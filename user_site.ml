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
  | UserCreation of User_services.user_creation

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
        find_data (auth, basicusercreation, false) l

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
           UserCreation {
             User_services.mail_from = registration_mail_from;
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


let external_auth = match auth with
  | NoExternalAuth -> None
  | Nis -> Some User_external_auth.external_auth_nis
  | Pam service ->
      match !User_external_auth.external_auth_pam with
        | Some pam -> Some (pam ?service)
        | None -> raise (Ocsigen_config.Config_file_error
                           "Ocsimore compiled without PAM support")

let (
    action_login,
    action_logout,
    action_logout_get,
    service_edit_user_data,
    action_edit_user_data,
    action_add_remove_users_from_group,
    action_add_remove_user_from_groups,
    service_view_group,
    service_view_groups
  as user_services) =
  User_services.services ~external_auth ~force_secure

let user_widget =
  match basicusercreation with
    | NoUserCreation ->
        new User_widgets.user_widget force_secure user_services

    | UserCreation user_creation_options ->
        (* We create some services specific to the creation of user *)
        let (service_create_new_user, action_create_new_user
               as creation_services) = User_services.services_user_creation ()
        in
        let user_widget_creation =
          new User_widgets.user_widget_user_creation
            ~force_secure ~user_services
            ~user_creation_options ~creation_services in

        (* We register the services above *)
        Eliom_duce.Xhtml.register ~service:service_create_new_user
          (fun sp () () -> user_widget_creation#display_user_creation
             ~err:"" ~sp);
        Eliom_duce.Xhtml.register ~service:action_create_new_user
          user_widget_creation#display_user_creation_done;

        (user_widget_creation :> User_widgets.user_widget)



let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)
  Eliom_duce.Xhtml.register ~service:service_edit_user_data
    (user_widget#display_edit_user_data ~err:"");
  Eliom_duce.Xhtml.register ~service:action_edit_user_data
    (fun sp _ args ->
       (* The first userid is always ignored, but is needed because we use as
          fallback a service that takes this argument *)
       user_widget#display_edit_user_data_done sp args);

  Eliom_duce.Xhtml.register service_view_group
    (fun sp g () -> user_widget#display_group ~sp g);

  Eliom_duce.Xhtml.register service_view_groups
    (fun sp () () -> user_widget#display_all_groups ~sp);

  (* This service is provided as a commodity to the admin, but is not
     used otherwise *)
  ignore
    (Eliom_duce.Xhtml.register_new_service
       ~https:force_secure
       ~path:[Ocsimore_lib.ocsimore_admin_dir; "login"]
       ~get_params:Eliom_parameters.unit
       (fun sp () () ->
          user_widget#display_login_widget ~sp () >>= fun lb ->
          Ocsimore_common.html_page {{ [ lb ] }}
       )
    );

  (* We register the syntax extensions *)
  User_ext.register_user_extensions Wiki_syntax.wikicreole_parser user_widget


