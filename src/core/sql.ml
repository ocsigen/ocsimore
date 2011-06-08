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

open Parse_config (* To force Parse_config to be linked early enough, as it
                     triggers the reading of the password file *)


module PGOCaml =
  PGOCaml_generic.Make (struct include Lwt include Lwt_chan end)

let () = PGOCaml.verbose := 2

open Lwt


type db_t = (string, bool) Hashtbl.t PGOCaml.t

let connect () =
  PGOCaml.connect
    ~database:!Ocsimore_config.db_name
    ~user:!Ocsimore_config.db_user
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

 
