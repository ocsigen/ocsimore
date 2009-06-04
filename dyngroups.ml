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
Creating dynamic groups if needed

@author Vincent Balat
*)

let (>>=) = Lwt.bind

let rec aux = function
  | [] -> Lwt.return ()
  | (name, s)::l ->
      aux l >>= fun () ->
      let test = Accesscontrol.parse_condition s in
      User.create_user
        ~name
        ~pwd:User_sql.Types.Connect_forbidden
        ~fullname:name
        ~test:(fun ~sp -> Lwt.return (test (Eliom_sessions.get_ri ~sp)))
        () >>= fun _ ->
      Lwt.return ()


let _ =
  Lwt_unix.run (aux !Ocsimore_config.dyngroupstobecreated);
  Ocsimore_config.dyngroupstobecreated := [] (* for GC *)

