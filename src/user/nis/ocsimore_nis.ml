(* Ocsimore
 * Copyright (C) 2008
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
   @author Vincent Balat
*)

let (>>=) = Lwt.bind

let nis_auth ~name ~pwd =
  try
    Nis_chkpwd.check_nis ~login:name ~passwd:pwd >>= function
      | true -> Lwt.return ()
      | false -> Lwt.fail User.BadPassword
  with
    | User.BadPassword ->
      Lwt.fail User.BadPassword
    | exn ->
      Lwt_log.ign_debug_f ~section ~exn "Ocsimore_nis" ;
      Lwt.fail exn

let () =
  User_external_auth.add_external_auth {
    User_external_auth.ext_auth_authenticate = nis_auth;
    ext_user_exists = (fun user ->
      Nis_chkpwd.userinfo user >>= function
        | None -> Lwt.return false
        | Some _ -> Lwt.return true
    );
  }
