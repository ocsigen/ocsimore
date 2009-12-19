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

open Sql
let (>>=) = Lwt.bind


let current_version = Lwt_unix.run
  (Lwt.catch
     (fun () ->
        full_transaction_block
          (fun db ->
             PGSQL(db) "SELECT value FROM options WHERE name = 'dbversion'")
        >>= fun l -> Lwt.return (int_of_string (List.hd l)))
     (fun e ->
        Lwt.fail (Failure (Printf.sprintf "Error while reading database version \
                                           for ocsimore: '%s'"
                             (Printexc.to_string e)));
     )
  )

let update_version db version =
  let ver = string_of_int version in
  PGSQL(db) "UPDATE options SET value = $ver WHERE name = 'dbversion'"

let update version f =
  if current_version < version then
    full_transaction_block
      (fun db ->
         Printf.eprintf "Updating Ocsimore database to version %d\n%!" version;
         f db >>= fun () ->
         update_version db version)
  else
    Lwt.return ()


let () = Lwt_unix.run
 begin

   update 2
     (fun db -> PGSQL(db) "ALTER TABLE options ADD PRIMARY KEY (name)")
   >>= fun () ->

   update 3
     (fun db -> PGSQL(db) "ALTER TABLE wikis ADD COLUMN hostid text")
   >>= fun () ->

   update 4
     (fun db -> PGSQL(db) "ALTER TABLE wikis RENAME COLUMN hostid TO siteid")
   >>= fun () ->

   update 5
     (fun db -> PGSQL(db) "ALTER TABLE wikiboxescontent ADD COLUMN ip text")
   >>= fun () ->

   update 6
     (fun db -> PGSQL(db) "ALTER TABLE wikis DROP CONSTRAINT wikis_title_key"
                >>= fun () ->
                PGSQL(db) "ALTER TABLE wikis ADD CONSTRAINT wikis_title_unique
                                             UNIQUE (title,siteid)")
   >>= fun () ->

   Lwt.return ()

 end
