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

open Eliom_lib
open Parse_config (* To force Parse_config to be linked early enough, as it
                     triggers the reading of the password file *)

module Lwt_fonct = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_fonct)
module Lwt_Query = Query.Make_with_Db(Lwt_fonct)(Lwt_PGOCaml)

let () = Lwt_PGOCaml.verbose := 2

open Lwt_ops

type db_t = Lwt_PGOCaml.pa_pg_data Lwt_PGOCaml.t

let connect () =
  Lwt_PGOCaml.connect
    ?host:!Ocsimore_config.db_host
    ?port:!Ocsimore_config.db_port
    ?unix_domain_socket_dir:!Ocsimore_config.db_unix_domain_socket_dir
    ?password:!Ocsimore_config.db_password ()
    ~database:!Ocsimore_config.db_name
    ~user:!Ocsimore_config.db_user

let validate db =
  try
    lwt () = Lwt_PGOCaml.ping db in
    Lwt.return true
  with _ ->
    Lwt.return false

let pool = Lwt_pool.create 16 ~validate connect

let transaction_block db f =
  Lwt_PGOCaml.begin_work db >>= fun _ ->
  try_lwt
     (* DEBUG print_endline "SQL transaction"; *)
     lwt r = f () in
     lwt () = Lwt_PGOCaml.commit db in
     Lwt.return r
  with e ->
     lwt () = Lwt_PGOCaml.rollback db in
     Lwt.fail e

let full_transaction_block f =
  Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))

let exec f x = Lwt_pool.use pool (fun db -> f db x)

let view x = exec (fun db x -> Lwt_Query.view db x) x
let view_one x = exec (fun db x -> Lwt_Query.view_one db x) x
let view_opt x = exec (fun db x -> Lwt_Query.view_opt db x) x
let query x = exec (fun db x -> Lwt_Query.query db x) x
let value x = exec (fun db x -> Lwt_Query.value db x) x
