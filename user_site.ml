(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2008-2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**
   @author Vincent Balat
   @author Boris Yakobowski
*)

open User_sql.Types
open Lwt

let ( ** ) = Eliom_parameters.prod


type user_creation =
  | NoUserCreation
  | BasicUserCreation of User_widgets.basic_user_creation

type external_auth = NoExternalAuth | Nis | Pam of string option

let default_data = (NoExternalAuth, NoUserCreation, true)


let (auth, basicusercreation, force_secure) =
  let rec find_data ((auth, basicusercreation, secure) as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("nis", [], []))::l ->
        find_data (Nis, basicusercreation, secure) l

    | (Simplexmlparser.Element ("pam", ["service", s], []))::l ->
        find_data (Pam (Some s), basicusercreation, secure) l

    | (Simplexmlparser.Element ("pam", [], []))::l ->
        find_data (Pam None, basicusercreation, secure) l

    | (Simplexmlparser.Element ("notsecure", [], []))::l ->
        find_data (auth, basicusercreation, true) l

    | (Simplexmlparser.Element ("basicusercreation", atts, []))::l ->
        let registration_mail_from =
          Ocsimore_lib.list_assoc_exn "registration_mail_from" atts
            (Ocsigen_config.Config_file_error
               "Missing registration_mail_from attribute inside <basicusercreation>")
        and registration_mail_addr =
          Ocsimore_lib.list_assoc_exn "registration_mail_addr" atts
            (Ocsigen_config.Config_file_error
               "Missing registration_mail_addr attribute inside <basicusercreation>")
        and registration_mail_subject =
          Ocsimore_lib.list_assoc_default "registration_mail_subject" atts
            "Ocsimore registration"
        in
        (try
          Users.user_list_of_string (List.assoc "groups" atts)
        with Not_found -> Lwt.return [basic_user Users.authenticated_users])
        >>= fun default_groups ->
        find_data
          (auth,
           BasicUserCreation {
             User_widgets.mail_from = registration_mail_from;
             mail_addr = registration_mail_addr;
             mail_subject = registration_mail_subject;
             new_user_groups = default_groups
           },
           secure
          )
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_data default_data c)


let user_widget =
  let sm =
    let sm_aux = new Session_manager.sessionmanager ~force_secure in
    match auth with
      | NoExternalAuth ->
          sm_aux ~external_auth:None ()
      | Nis ->
          sm_aux ~external_auth:(Some Session_manager.external_auth_nis) ()
      | Pam service ->
          match !Session_manager.external_auth_pam with
            | None -> raise
                (Ocsigen_config.Config_file_error
                   "Ocsimore compiled without PAM support")
            | Some pam ->
                sm_aux ~external_auth:(Some (pam ?service)) ()

  in
  (* Creation of the login box. This register some services, in the
     initializers of User_widgets.login_widget  *)
  (match basicusercreation with
     | BasicUserCreation buc ->
         (new User_widgets.login_widget_basic_user_creation sm buc
          :> User_widgets.login_widget)
     | NoUserCreation ->
         new User_widgets.login_widget sm
  )

let () = User_ext.register_user_extensions
  Wiki_syntax.wikicreole_parser user_widget
