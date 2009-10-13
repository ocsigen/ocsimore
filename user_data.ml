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




let mail_password ~name ~password ~from_name ~from_addr ~subject =
  Lwt.catch
    (fun () ->
       User.get_basicuser_by_login name
       >>= fun u ->
       User_sql.get_basicuser_data u
       >>= fun user ->
       Lwt_preemptive.detach
         (fun () ->
            match user.user_email with
              | Some email ->
                  ignore(Netaddress.parse email);
                  Netsendmail.sendmail
                    ~mailer:"/usr/sbin/sendmail"
                    (Netsendmail.compose
                       ~from_addr:(from_name, from_addr)
                       ~to_addrs:[(user.user_fullname, email)]
                       ~subject
                       ("This is an auto-generated message. "
                        ^ "Please do not reply to it.\n"
                        ^ "\n"
                        ^ "Your account is:\n"
                        ^ "\tUsername:\t" ^ name ^ "\n"
                        ^ "\tPassword:\t" ^ password ^ "\n"));
                  true
              | None -> false) ())
    (function _ -> Lwt.return false)


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

let can_create_user ~sp ~options =
  User.get_user_id sp >>= fun u ->
  Lwt.return (u = User.admin || options.non_admin_can_create)

let create_user ~sp ~name ~fullname ~email ~options =
  can_create_user ~sp ~options >>= function
    | true ->
        if not (valid_username name) then
          Lwt.fail (Failure "ERROR: Bad character(s) in login name!")
        else if not (valid_emailaddr email) then
          Lwt.fail (Failure "ERROR: Bad formed e-mail address!")
        else
          let pwd = generate_password () in
          Lwt.catch (fun () ->
              User.create_fresh_user ~name ~fullname ~email
                ~pwd:(User_sql.Types.Ocsimore_user_crypt pwd) ()
              >>= fun userid ->
              User.add_to_groups (basic_user userid) options.new_user_groups
              >>= fun () ->
              mail_password ~name ~password:pwd ~subject:options.mail_subject
                ~from_name:options.mail_from ~from_addr:options.mail_addr
              >>= function
                | true -> Lwt.return ()
                | false ->
                    User_sql.delete_user ~userid:userid >>= fun () ->
                    Lwt.fail (Failure
                          "Registration failed: cannot send confirmation email")
          )
          (function
             | User.BadUser ->
                 Lwt.fail (Failure "ERROR: This login already exists")
             | e -> Lwt.fail e)
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied


(** Change user information *)

let can_change_user_data_by_userid sp userid =
  User.get_user_id sp >>= fun lu ->
  Lwt.return ((lu = userid && lu <> User.nobody) || lu = User.admin)

let can_change_user_data_by_user sp user =
  User_sql.get_user_data user >>= fun ud ->
  can_change_user_data_by_userid sp ud.user_id


let change_user_data ~sp ~userid ~pwd:(pwd, pwd2) ~fullname ~email =
  can_change_user_data_by_userid sp userid >>= function
    | true ->
        if email <> "" && valid_emailaddr email = false then
          Lwt.fail (Failure "ERROR: Ill-formed e-mail address!")
        else if pwd <> pwd2 then
          Lwt.fail (Failure "ERROR: Passwords don't match!")
        else
          User.get_user_data sp >>= fun user ->
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
                if pwd = "" then None else Some (Ocsimore_user_plain pwd)
          in
          User_sql.update_data ~userid:user.user_id ~fullname ~email
            ?password:pwd()

    | false -> Lwt.fail Ocsimore_common.Permission_denied


(** Edition of groups *)

let add_remove_users_from_group sp g (add, rem) =
  User.get_user_id sp >>= fun user ->
  if user = User.admin then
    User.get_user_by_name g
    >>= fun group ->
    User.GroupsForms.add_remove_users_from_group add rem group
  else
    Lwt.fail Ocsimore_common.Permission_denied


let add_remove_user_from_groups sp u (add, rem) =
  User.get_user_id sp >>= fun user ->
  if user = User.admin then
    User.get_user_by_name u
    >>= fun group ->
    User.GroupsForms.user_add_remove_from_groups group add rem
  else
    Lwt.fail Ocsimore_common.Permission_denied



(** Login and logout *)
open User_external_auth

let logout ~sp =
  Eliom_sessions.close_session ~sp () >>= fun () ->
  Eliom_sessions.clean_request_cache ~sp;
  Lwt.return ()

module Throttle = Lwt_throttle.Make(struct
                                      type t = string
                                      let equal (x: string) y = x = y
                                      let hash = Hashtbl.hash
                                    end)


let th_login = Throttle.create ~rate:1 ~max:1 ~n:10
let th_ip = Throttle.create ~rate:1 ~max:1 ~n:10


let login ~sp ~name ~pwd ~external_auth =
  Eliom_sessions.close_session ~sp () >>= fun () ->
  (* XXX improve Lwt_throttle *)
  Throttle.wait th_login name >>= fun b1 ->
  Throttle.wait th_ip (Eliom_sessions.get_remote_ip sp) >>= fun b2 ->
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
    Eliom_sessions.clean_request_cache ~sp;
    User.set_session_data sp (user, name)

  else
    Lwt.fail User.ConnectionRefused

