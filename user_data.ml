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
       Users.get_basicuser_by_login name
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


let can_change_user_data_by_userid sp userid =
  Users.get_user_id sp >>= fun lu ->
  Lwt.return ((lu = userid && lu <> Users.nobody) || lu = Users.admin)

let can_change_user_data_by_user sp user =
  User_sql.get_user_data user >>= fun ud ->
  can_change_user_data_by_userid sp ud.user_id


let change_user_data ~sp ~userid ~pwd:(pwd, pwd2) ~fullname ~email =
  can_change_user_data_by_userid sp userid >>= function
    | true ->
        if not (valid_emailaddr email) then
          failwith "ERROR: Bad formed e-mail address!"
        else if pwd <> pwd2 then
          failwith "ERROR: Passwords don't match!"
        else
          Users.get_user_data sp >>= fun user ->
          Ocsigen_messages.debug2 (Printf.sprintf "Updating user '%s'"fullname);
          let pwd = match user.user_pwd with
            | Connect_forbidden (* Should never happen, the user cannot
                                   be logged *) -> None

            | External_Auth (* We cannot change this password, we should not
                               leave the possibility to the user  *) ->
              failwith"ERROR: Cannot change NIS or PAM passwords from Ocsimore!"
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
  Users.get_user_id sp >>= fun user ->
  if user = Users.admin then
    Users.get_user_by_name g
    >>= fun group ->
    Users.GroupsForms.add_remove_users_from_group add rem group
  else
    Lwt.fail Ocsimore_common.Permission_denied


let add_remove_user_from_groups sp u (add, rem) =
  Users.get_user_id sp >>= fun user ->
  if user = Users.admin then
    Users.get_user_by_name u
    >>= fun group ->
    Users.GroupsForms.user_add_remove_from_groups group add rem
  else
    Lwt.fail Ocsimore_common.Permission_denied



(** Login and logout *)
open User_external_auth

let logout ~sp =
  Eliom_sessions.close_session ~sp () >>= fun () ->
  Eliom_sessions.clean_request_cache ~sp;
  Lwt.return ()

let login ~sp ~name ~pwd ~external_auth =
  Eliom_sessions.close_session ~sp () >>= fun () ->
  Lwt.catch
    (fun () ->
       Users.authenticate ~name ~pwd >>= fun u ->
       Lwt.return u.user_id)
    (fun e ->
       match external_auth with
         | None -> Lwt.fail e
         | Some ea -> match e with
             | Users.UseAuth u ->
                 (* check external pwd *)
                 ea.ext_auth_authenticate ~name ~pwd >>= fun () ->
                   Lwt.return u
             | Users.BadUser ->
                 (* check external pwd, and create user if ok *)
                 ea.ext_auth_authenticate ~name ~pwd >>= fun () ->
                 ea.ext_auth_fullname name >>= fun fullname ->
                 Users.create_user ~pwd:User_sql.Types.External_Auth
                   ~name ~fullname ()
                 (* Do we need to actualize the fullname every time
                    the user connects? *)
              | e -> Lwt.fail e)
  >>= fun user ->
  Eliom_sessions.clean_request_cache ~sp;
  Users.set_session_data sp user

