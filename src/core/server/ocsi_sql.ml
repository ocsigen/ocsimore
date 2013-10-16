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

include Macaque_lwt
include Macaque_lwt.Utils
include Macaque_lwt.Make(struct
    let connect () =
      Lwt_PGOCaml.connect
        ?host:!Ocsimore_config.db_host
        ?port:!Ocsimore_config.db_port
        ?unix_domain_socket_dir:!Ocsimore_config.db_unix_domain_socket_dir
        ?password:!Ocsimore_config.db_password ()
        ~database:!Ocsimore_config.db_name
        ~user:!Ocsimore_config.db_user
    let pool_number = 16
  end)

let () = Lwt_PGOCaml.verbose := 2

let full_transaction_block f = transaction_block (fun ?log:_ -> f)
