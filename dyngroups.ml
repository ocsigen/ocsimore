(* Ocsimore
 * Copyright (C) 2008 Vincent Balat
 * Laboratoire PPS - Université Paris Diderot - CNRS
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

(**
User management

@author Vincent Balat
*)

let (>>=) = Lwt.bind

let _ =
  let rec parse_config = function
    | [] -> Lwt.return ()
    | (Simplexmlparser.Element ("group", [("name", name)], [s]))::l -> 
        let test = Accesscontrol.parse_condition s in
        Users.create_user
          ~name
          ~pwd:None
          ~fullname:name
          ~email:None
          ~groups:[]
          ~test:(fun ~sp ~sd -> Lwt.return (test (Eliom_sessions.get_ri ~sp)))
          () >>= fun _ ->
        Lwt.return ()
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                    "Unexpected content inside ocsimore config")
  in
  let c = Ocsigen_extensions.get_config () in
  Lwt_unix.run (parse_config c)

  
