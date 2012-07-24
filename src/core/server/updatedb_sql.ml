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

open Ocsi_sql

let (>|=) m f = Lwt.map f m
let (>>=) = Lwt.bind

(* BEWARE: This is going to raise an exception ! *)
let options = <:table< options (
  name text NOT NULL,
  value text NOT NULL
) >>

let current_version = Lwt_main.run
  (try_lwt
     lwt l =
       full_transaction_block
         (fun db -> PGOCamlQuery.view db (
           <:view< {opt.value} | opt in $options$; opt.name = "dbversion" >>))
   in
    Lwt.return (int_of_string ((List.hd l)#!value))
   with exc ->
      Lwt.fail (Failure (Printf.sprintf "Error while reading database version \
                                         for ocsimore: '%s'"
                           (Printexc.to_string exc))))

let update_version db version =
  let ver = string_of_int version in
  PGOCamlQuery.query db (<:update< opt in $options$ := { value = $string:ver$ } | opt.name = "dbversion">>)

let update version f =
  if current_version < version then
    full_transaction_block
      (fun db ->
         Printf.eprintf "Updating Ocsimore database to version %d\n%!" version;
         lwt () = f db in
         update_version db version)
  else
    Lwt.return ()

let alter db query =
  let name = "query" in
  PGOCaml.prepare db ~query ~name () >>= (fun () ->
    PGOCaml.execute db ~name ~params: [] () >>= (fun _ ->
      PGOCaml.close_statement db ~name ()
    )
  )

let () =
  Lwt_main.run begin
    update 2 (fun db ->
      alter db "ALTER TABLE options ADD PRIMARY KEY(name)"
    ) >>= fun () ->
    update 3 (fun db ->
      alter db "ALTER TABLE wikis ADD COLUMN hostid text"
    ) >>= fun () ->
    update 4 (fun db ->
      alter db "ALTER TABLE wikis RENAME COLUMN hostid TO siteid"
    ) >>= fun () ->
    update 5 (fun db ->
      alter db "ALTER TABLE wikiboxescontent ADD COLUMN ip text"
    ) >>= fun () ->
    update 6 (fun db ->
      alter db "ALTER TABLE wikis DROP CONSTRAINT wikis_title_key" >>= (fun () ->
        alter db "ALTER TABLE wikis ADD CONSTRAINT wikis_title_unique UNIQUE (title,siteid)"
      )
    )
  end
