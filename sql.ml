(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** PostgreSQL database operations via PGOCaml library. *)

module PGOCaml =
  PGOCaml_generic.Make (struct include Lwt include Lwt_chan end)

let () = PGOCaml.verbose := 2

open Lwt
open Ocsimore_lib
open CalendarLib


type db_t = (string, bool) Hashtbl.t PGOCaml.t

let connect () =
  PGOCaml.connect
    ~database:"ocsimore"
    ~user:Ocsimore_config.user
    ~password:!Ocsimore_config.password ()


let pool = Lwt_pool.create 16 connect

let transaction_block db f =
  PGOCaml.begin_work db >>= fun _ ->
  Lwt.catch
    (fun () ->
       (* DEBUG print_endline "SQL transaction"; *)
       f () >>= fun r ->
       PGOCaml.commit db >>= fun () ->
       Lwt.return r)
    (fun e ->
       PGOCaml.rollback db >>= fun () ->
       Lwt.fail e)

let full_transaction_block f =
  Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))


exception BadVersion

let current_version = Lwt_unix.run
  (Lwt.catch
     (fun () ->
        full_transaction_block
          (fun db ->
             PGSQL(db) "SELECT value FROM options WHERE name = 'dbversion'")
        >>= fun l -> Lwt.return (int_of_string (List.hd l)))
     (fun _ ->
        Ocsigen_messages.errlog "Incorrect database version for ocsimore. Correct the key 'dbversion' in the table 'options'";
        Lwt.fail BadVersion)
  )

let update_version db version =
  let ver = string_of_int version in
  PGSQL(db) "UPDATE options SET value = $ver WHERE name = 'dbversion'"

let update version f =
  if current_version < version then
    full_transaction_block
      (fun db ->
         Ocsigen_messages.warning
           (Printf.sprintf "Updating Ocsimore database to version %d" version);
         f db >>= fun () ->
         update_version db version)
  else
    Lwt.return ()


let () = Lwt_unix.run
 begin

   update 2
     (fun db -> PGSQL(db) "ALTER TABLE options ADD PRIMARY KEY (name)")
   >>= fun () ->

   Lwt.return ()

 end

