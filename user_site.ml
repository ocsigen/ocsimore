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
  | UserCreation of User_data.user_creation

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
        and non_admin =
          Ocsimore_lib.list_assoc_default "non_admin" atts ""
        in
        (try
          User.user_list_of_string (List.assoc "groups" atts)
        with Not_found -> Lwt.return [basic_user User.authenticated_users])
        >>= fun default_groups ->
        find_data
          (auth,
           UserCreation {
             User_data.mail_from = registration_mail_from;
             mail_addr = registration_mail_addr;
             mail_subject = registration_mail_subject;
             new_user_groups = default_groups;
             non_admin_can_create = (non_admin = "true");
           },
           secure
          )
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside User_site config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_data default_data c)


let external_auth = match auth with
  | NoExternalAuth -> None
  | Nis -> Some User_external_auth.external_auth_nis
  | Pam service ->
      match !User_external_auth.external_auth_pam with
        | Some pam -> Some (pam ?service ())
        | None -> raise (Ocsigen_config.Config_file_error
                           "Ocsimore compiled without PAM support")

module Services = struct

let (
    action_login,
    action_logout,
    action_logout_get,
    action_edit_user_data,
    action_add_remove_users_from_group,
    action_add_remove_user_from_groups,
    service_view_group,
    service_view_groups,
    service_login,
    service_create_new_group,
    action_create_new_group
  as user_services) =
  User_services.services ~external_auth ~force_secure

end
include Services

let user_widgets, service_user_creation =
  let module Widget = User_widgets.MakeWidget(Page_site)(Services) in
  match basicusercreation with
    | NoUserCreation ->
        new Widget.user_widget force_secure,
        None

    | UserCreation user_creation_options ->
        (* We create some services specific to the creation of user *)
        let module ServicesCreation = struct
          let (service_create_new_user, action_create_new_user
                 as creation_services) = User_services.services_user_creation ()
        end in
        let module WidgetCreation =
          Widget.MakeWidgetCreation(ServicesCreation) in
        let user_widget_creation =
          new WidgetCreation.user_widget_user_creation
            ~force_secure ~user_creation_options in

        (* We register the services above *)
        Eliom_duce.Xhtml.register
          ~service:ServicesCreation.service_create_new_user
          (fun sp () () ->
             user_widget_creation#display_user_creation ~err:"" ~sp
             >>= (Page_site.admin_page ~sp
                    ~service:ServicesCreation.service_create_new_user)
          );

        Eliom_duce.Xhtml.register
          ~service:ServicesCreation.action_create_new_user
          (fun sp () args ->
             user_widget_creation#display_user_creation_done sp () args
             >>= (Page_site.admin_page ~sp
                    ~service:ServicesCreation.service_create_new_user)
          );

        (user_widget_creation :> Widget.user_widget),
        Some ServicesCreation.service_create_new_user


let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Eliom_duce.Xhtml.register service_view_group
    (fun sp g () ->
       user_widgets#display_group ~sp g >>= fun body ->
       Page_site.admin_page ~sp ~service:service_view_groups body
    );

  Eliom_duce.Xhtml.register service_view_groups
    (fun sp () () ->
       user_widgets#display_all_groups ~sp >>= fun body ->
       Page_site.admin_page ~sp ~service:service_view_groups body
);

  Eliom_duce.Xhtml.register service_login
    (fun sp () () ->
       user_widgets#display_login_widget ~sp () >>= fun body ->
       Page_site.admin_page ~sp ~service:service_login
         {{ [ <h1>"Login page"
              body ] }}
    );

  Eliom_duce.Xhtml.register
    ~service:service_create_new_group
    (fun sp () () ->
       user_widgets#display_group_creation ~err:"" ~sp
       >>= (Page_site.admin_page ~sp
              ~service:service_create_new_group)
    );

  Eliom_duce.Xhtml.register
          ~service:action_create_new_group
    (fun sp () args ->
       user_widgets#display_group_creation_done sp () args
       >>= (Page_site.admin_page ~sp
              ~service:service_create_new_group)
    );


  (* We register the syntax extensions *)
  User_ext.register_user_extensions user_widgets



let () = Page_site.add_to_admin_menu "Users"
  (["Login", service_login;
    "Groups edition", service_view_groups;
    "Groups creation", service_create_new_group;
   ] @
     (match service_user_creation with
        | None -> []
        | Some service -> ["User creation", service])
  )


let () = Page_site.add_status_function user_widgets#status_text
