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
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
   @author Boris Yakobowski
*)

open User_sql.Types
open Ocsimore_lib
open Eliom_lib.Lwt_ops

let valid_username usr =
  Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0

let valid_emailaddr email =
  Str.string_match
    (Str.regexp
       ("^[A-Za-z0-9\\._-]+@\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$"))
    email 0

let generate_password () =
  let chars = "0123456789"^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"^
    "abcdefghijklmnopqrstuvwxyz" in
  let pwd = String.create 8 in
    for i = 0 to 7 do
      pwd.[i] <- String.get chars (Random.int (10+2*26))
    done;
    pwd

(** {2 User creation} *)

(** Options for the creation of a new user *)
type user_creation = {
  non_admin_can_create: bool;
  mail_from: string;
  mail_addr: string;
  mail_subject: string;
  new_user_groups: User_sql.Types.user list;
}

let can_create_user ~options =
  if options.non_admin_can_create then
    Lwt.return true
  else
    User.in_group ~group:User.group_can_create_users ()


let can_create_group () =
  User.in_group ~group:User.group_can_create_groups ()

let can_admin_users () =
  User.in_group ~group:User.group_can_admin_users ()


let create_group ~name ~descr =
  can_create_group () >>= function
    | true ->
        if not (valid_username name) then
          Lwt.fail (Failure "ERROR: Bad character(s) in group name!")
        else
          lwt groupid =
            User.create_fresh_user
              ~name
              ~fullname:descr
              ~pwd:Connect_forbidden ()
          in
          lwt userid = User.get_user_id () in
          lwt () =
            User.add_to_group
              ~user:(basic_user userid)
              ~group:(User.group_can_admin_group $ groupid)
          in
          Lwt.return groupid
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied


(** {2 Change user information} *)

let can_view_users () =
  User.in_group ~group:User.group_can_create_users ()
let can_view_groups () =
  User.in_group ~group:User.group_can_create_groups ()
let can_view_roles () =
  User.in_group ~group:User.group_can_create_groups ()

let can_change_user_data_by_userid userid =
  lwt lu = User.get_user_id () in
  Lwt.return ((lu = userid && lu <> User.nobody) || lu = User.admin)

let can_change_user_data_by_user user =
  lwt ud = User_sql.get_user_data user in
  can_change_user_data_by_userid ud.user_id

let change_user_data ~userid ~pwd:(pwd, pwd2) ~fullname ~email =
  can_change_user_data_by_userid userid >>= function
    | true ->
        if email <> "" && valid_emailaddr email = false then
          Lwt.fail (Failure "ERROR: Ill-formed e-mail address!")
        else if pwd <> pwd2 then
          Lwt.fail (Failure "ERROR: Passwords don't match!")
        else
          lwt user = User_sql.get_basicuser_data userid in
          Lwt_log.ign_debug_f ~section "Updating user '%s'" fullname;
          let pwd =
            match user.user_pwd with
              | Connect_forbidden (* Should never happen, the user cannot
                                     be logged *) -> None

              | External_Auth (* We cannot change this password, we should not
                                 leave the possibility to the user. The
                                 form does not allow this anyhow *) ->
                  failwith
                       "ERROR: Cannot change NIS or PAM users from Ocsimore!"
              | Ocsimore_user_plain _
              | Ocsimore_user_crypt _
              | Ocsimore_user_safe _ ->
                  (* We always use crypted passwords, even if the user
                     previously unencrypted ones *)
                  let hash = Bcrypt.hash pwd in
                  if pwd = "" then None else Some (Ocsimore_user_safe hash)
          in
          User_sql.update_data ~userid ~fullname ~email: (Some email)
            ?password:pwd()

    | false -> Lwt.fail Ocsimore_common.Permission_denied


(** {2 Edition of groups} *)


let can_admin_group, add_group_admin_function =
  let hooks_admin = ref [] in
  (fun ?user ~group () ->
     (match user with
        | None -> User.get_user_id () >>= fun u ->
            Lwt.return (basic_user u)
        | Some u -> Lwt.return u
     ) >>= fun user ->
     (match is_basic_user group with
        | None -> Lwt.return (user = basic_user User.admin)
        | Some group ->
            User.in_group ~user
              ~group:(User.group_can_admin_group $ group) ()
     ) >>= function
       | true -> Lwt.return true
       | false ->
           let rec aux = function
             | [] -> Lwt.return false
             | f :: q ->
                 f ~user ~group >>= function
                   | true -> Lwt.return true
                   | false -> aux q
           in aux !hooks_admin
  ),
  (fun f -> hooks_admin := f :: !hooks_admin)

let add_remove_user_from_group ~group ~user f =
  User.get_user_by_name group
  >>= fun group ->
  can_admin_group ~group ()
  >>= function
    | true ->
        (match user with
          | "" -> Lwt.return ()
          | user ->
              User.get_user_by_name user
              >>= fun user ->
              f ~user ~group
        )
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied

let add_user_from_group ~group ~user () =
  add_remove_user_from_group
    ~group
    ~user
    User.add_to_group

let remove_user_from_group ~group ~user () =
  add_remove_user_from_group
    ~group
    ~user
    User.remove_from_group

(* (* No longer used; deactivated as the permissions check is a bit coarse *)
let add_remove_user_from_groups sp user (add, rem) =
  User.get_user_id sp >>= fun cuser ->
  if cuser = User.admin then
    User.get_user_by_name user
    >>= fun user ->
    User.get_user_by_name add >>= fun add ->
    User.get_user_by_name rem >>= fun rem ->
    User.add_to_group ~user ~group:add >>= fun () ->
    User.remove_from_group ~user ~group:rem
  else
    Lwt.fail Ocsimore_common.Permission_denied
*)


(** {2 Login and logout} *)

open User_external_auth

let logout () =
  Eliom_state.discard ~scope:Eliom_common.default_session_scope () >>= fun () ->
  Eliom_state.discard ~scope:Eliom_common.request_scope () >>= fun () ->
  Lwt.return ()

(**/**)
module Throttle = Lwt_throttle.Make(struct
                                      type t = string
                                      let equal (x: string) y = x = y
                                      let hash = Hashtbl.hash
                                    end)
(**/**)


let th_login = Throttle.create ~rate:1 ~max:1 ~n:10
let th_ip = Throttle.create ~rate:1 ~max:1 ~n:10

let login ~name ~pwd =
  lwt () = Eliom_state.discard ~scope:Eliom_common.default_session_scope () in
  (* XXX improve Lwt_throttle *)
  lwt b1 = Throttle.wait th_login name in
  lwt b2 = Throttle.wait th_ip (Eliom_request_info.get_remote_ip ()) in
  if b1 && b2 then
    let check_auth () =
      User_external_auth.iter_external_auths
        (fun x ->
          try_lwt
            lwt () = x.ext_auth_authenticate ~name ~pwd in
            Lwt.return true
          with User.BadPassword -> Lwt.return false
        )
    in
    lwt user =
      try_lwt
        lwt u = User.authenticate ~name ~pwd in
        Lwt.return u.user_id
      with
        | User.UseAuth u ->
            (* check external pwd *)
            lwt () = check_auth () in
            Lwt.return u
        | User.BadUser ->
            (* check external pwd, and create user if ok *)
            lwt () = check_auth () in
            User.create_external_user name
        | e -> Lwt.fail e
    in
    Eliom_request_info.clean_request_cache ();
    User.set_session_data (user, name)
  else
    Lwt.fail User.ConnectionRefused

(** {2 Login error tracking} *)

(* Used to store the fact that a login error has occurred, so that
   pages can display an appropriate message *)
let login_error_eref : exn list Eliom_reference.eref =
  Eliom_reference.eref ~scope:Eliom_common.request_scope []

let get_login_error () = Eliom_reference.get login_error_eref
let add_login_error exn =
  flip eref_modify login_error_eref
    (cons exn)
