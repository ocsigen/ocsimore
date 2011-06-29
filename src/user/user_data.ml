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

let (>>=) = Lwt.bind

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


let create_group ~name ~descr =
  can_create_group () >>= function
    | true ->
        if not (valid_username name) then
          Lwt.fail (Failure "ERROR: Bad character(s) in group name!")
        else
          User.create_fresh_user ~name ~fullname:descr
            ~pwd:Connect_forbidden ()
          >>= fun groupid ->
          User.get_user_id () >>= fun userid ->
          User.add_to_group ~user:(basic_user userid)
            ~group:(User.group_can_admin_group $ groupid) >>= fun () ->
          Lwt.return groupid
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied


(** Change user information *)

let can_change_user_data_by_userid userid =
  User.get_user_id () >>= fun lu ->
  Lwt.return ((lu = userid && lu <> User.nobody) || lu = User.admin)

let can_change_user_data_by_user user =
  User_sql.get_user_data user >>= fun ud ->
  can_change_user_data_by_userid ud.user_id


let change_user_data ~userid ~pwd:(pwd, pwd2) ~fullname ~email =
  can_change_user_data_by_userid userid >>= function
    | true ->
        if email <> "" && valid_emailaddr email = false then
          Lwt.fail (Failure "ERROR: Ill-formed e-mail address!")
        else if pwd <> pwd2 then
          Lwt.fail (Failure "ERROR: Passwords don't match!")
        else
          User_sql.get_basicuser_data userid >>= fun user ->
          Ocsigen_messages.debug2 (Printf.sprintf "Updating user '%s'"fullname);
          let pwd = match user.user_pwd with
            | Connect_forbidden (* Should never happen, the user cannot
                                   be logged *) -> None

            | External_Auth (* We cannot change this password, we should not
                               leave the possibility to the user. The
                               form does not allow this anyhow *) ->
                failwith
                     "ERROR: Cannot change NIS or PAM users from Ocsimore!"
            | Ocsimore_user_plain _ | Ocsimore_user_crypt _ ->
                (* We always use crypted passwords, even if the user
                   previously unencrypted ones *)
                if pwd = "" then None else Some (Ocsimore_user_crypt pwd)
          in
          User_sql.update_data ~userid ~fullname ~email
            ?password:pwd()

    | false -> Lwt.fail Ocsimore_common.Permission_denied


(** Edition of groups *)


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



let add_remove_users_from_group group (add, rem) =
  User.get_user_by_name group >>= fun group ->
  can_admin_group ~group () >>= function
    | true ->
        (if add <> "" then
           User.get_user_by_name add >>= fun add ->
           User.add_to_group ~user:add ~group
         else
           Lwt.return ()
        ) >>= fun () ->
        (if rem <> "" then
           User.get_user_by_name rem >>= fun rem ->
           User.remove_from_group ~user:rem ~group
         else
           Lwt.return ()
        )
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied

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


(** Login and logout *)
open User_external_auth

let logout () =
  Eliom_state.discard ~scope:Eliom_common.session () >>= fun () ->
  Eliom_request_info.clean_request_cache ();
  Lwt.return ()

module Throttle = Lwt_throttle.Make(struct
                                      type t = string
                                      let equal (x: string) y = x = y
                                      let hash = Hashtbl.hash
                                    end)


let th_login = Throttle.create ~rate:1 ~max:1 ~n:10
let th_ip = Throttle.create ~rate:1 ~max:1 ~n:10


let login ~name ~pwd ~external_auth =
  Eliom_state.discard ~scope:Eliom_common.session () >>= fun () ->
  (* XXX improve Lwt_throttle *)
  Throttle.wait th_login name >>= fun b1 ->
  Throttle.wait th_ip (Eliom_request_info.get_remote_ip ()) >>= fun b2 ->
  if b1 && b2 then
    Lwt.catch
      (fun () ->
         User.authenticate ~name ~pwd >>= fun u ->
         Lwt.return u.user_id)
      (fun e ->
         match external_auth with
           | None -> Lwt.fail e
           | Some ea -> match e with
               | User.UseAuth u ->
                   (* check external pwd *)
                   ea.ext_auth_authenticate ~name ~pwd >>= fun () ->
                   Lwt.return u
               | User.BadUser ->
                   (* check external pwd, and create user if ok *)
                   ea.ext_auth_authenticate ~name ~pwd >>= fun () ->
                   ea.ext_auth_fullname name >>= fun fullname ->
                   User.create_user ~pwd:User_sql.Types.External_Auth
                     ~name ~fullname ()
                   (* Do we need to actualize the fullname every time
                      the user connects? *)
               | e -> Lwt.fail e)
    >>= fun user ->
    Eliom_request_info.clean_request_cache ();
    User.set_session_data (user, name)

  else
    Lwt.fail User.ConnectionRefused



(* Used to store the fact that a login error has occurred, so that
   pages can display an appropriate message *)
let login_error_key : exn list Polytables.key = Polytables.make_key ()

let get_login_error () =
  try
    Polytables.get
      ~table:(Eliom_request_info.get_request_cache ())
      ~key:login_error_key
  with Not_found -> []
