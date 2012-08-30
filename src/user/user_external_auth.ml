(* Ocsimore
 * Copyright (C) 2005
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
   @author Boris Yakobowski
*)

let (>>=) = Lwt.bind


type external_auth = {
  (** A function returning unit if the given user can be authentified by
      the given password, or failing with [BadUser] *)
  ext_auth_authenticate: name:string -> pwd:string -> unit Lwt.t;

  (** The fullname of the user whose login is the argument *)
  ext_auth_fullname: string -> string Lwt.t;

  (** The argument is the username *)
  get_and_create_user : string -> User_sql.Types.userid option Lwt.t;
}

(** Functions to add and get an external authentication method(s)
    Used by LDAP and PAM *)
let get_other_external_auth, add_other_external_auth =
  let r : external_auth list ref = ref [] in
  (fun () -> !r), (fun x -> r := x :: !r)

(** NIS authentification *)
let external_auth_nis = {
  ext_auth_authenticate = Ocsimore_nis.nis_auth;
  ext_auth_fullname = (fun usr -> Nis_chkpwd.userinfo usr >>= function
                           | None -> Lwt.return usr
                           | Some { Unix.pw_gecos = v } -> Lwt.return v);
  get_and_create_user = (fun user ->
    Nis_chkpwd.userinfo user >>= function
      | None -> Lwt.return None
      | Some userdata ->
        User.create_user ~name:user
          ~pwd:User_sql.Types.External_Auth
          ~fullname:userdata.Unix.pw_gecos
          ~email:(user ^ "@localhost")
          ()
        >>= fun userdata ->
        Lwt.return (Some userdata)
  );
}
