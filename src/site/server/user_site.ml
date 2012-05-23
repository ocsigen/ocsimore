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

open Eliom_content
open User_sql.Types
open Lwt

let ( ** ) = Eliom_parameter.prod

let user_widget =
  match User_services.basicusercreation with
    | User_services.NoUserCreation ->
        Eliom_output.Html5.register
          ~service:User_services.service_create_new_user
          (Page_site.admin_body_content_with_permission_handler
             ~title:(fun () () -> Lwt.return "Create new user")
             ~permissions:(fun () () -> Lwt.return true)
             ~display:(fun () () ->
                         Lwt.return Html5.F.([
                           h2 [pcdata "Error"];
                           p [pcdata "User creation is disabled"];
                         ])));
        (new User_widgets.user_widget : User_widgets.user_widget_class)
    | User_services.UserCreation options ->
        let user_widget_creation =
          object
            inherit User_widgets.user_widget
            inherit User_widgets.user_widget_user_creation options
          end
        in
        Eliom_output.Html5.register
          ~service:User_services.service_create_new_user
          (Page_site.admin_body_content_with_permission_handler
             ~title:(fun () () -> Lwt.return "Create new user")
             ~permissions:(fun () () -> User_data.can_create_user ~options)
             ~display:(fun () () -> user_widget_creation#display_user_creation ()));
        Eliom_output.Html5.register
          ~service:User_services.action_create_new_user
          (Page_site.admin_body_content_with_permission_handler
             ~title:(fun () _ -> Lwt.return "Created new user")
             ~service:(fun () _ -> Lwt.return User_services.service_create_new_user)
             ~permissions:(fun () _ -> User_data.can_create_user ~options)
             ~display:(fun () (name, (fullname, (email, pwd))) ->
                         user_widget_creation#display_user_creation_done
                           ~name ~fullname ~email ~pwd));
        (user_widget_creation :> User_widgets.user_widget_class)


let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Eliom_output.Html5.register User_services.service_view_group
    (let service g _ =
       User.get_user_by_name g >>= User_sql.user_type >|= function 
         | `Group -> User_services.service_view_groups
         | `User -> User_services.service_view_users
         | `Role -> User_services.service_view_roles
     in
     let display g _ =
       lwt group = User.get_user_by_name g in
       if group = basic_user User.nobody && g <> User.nobody_login then
         Lwt.fail (Failure ("Unknown group "^g))
       else
         user_widget#display_group (group, g)
     in
     Page_site.admin_body_content_with_permission_handler
       ~title:(fun g _ -> Lwt.return (Printf.sprintf "View group %S" g))
       ~permissions:(fun _ _ -> User_data.can_view_groups ())
       ~service
       ~display);

  Eliom_output.Html5.register User_services.service_view_groups
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View groups")
       ~permissions:(fun _ _ -> User_data.can_view_groups ())
       ~display:(fun _ _ -> user_widget#display_groups));

  Eliom_output.Html5.register User_services.service_view_users
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View groups")
       ~permissions:(fun _ _ -> User_data.can_view_users ())
       ~display:(fun _ _ -> user_widget#display_users));

  Eliom_output.Html5.register User_services.service_view_roles
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View roles")
       ~permissions:(fun _ _ -> User_data.can_view_roles ())
       ~display:(fun _ _ -> user_widget#display_roles));

  Eliom_output.Html5.register User_services.service_login
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "Login")
       ~permissions:(fun _ _ -> Lwt.return true)
       ~display:(fun _ _ -> user_widget#display_login_widget ()));

  Eliom_output.Html5.register ~service:User_services.service_create_new_group
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "Create new group")
       ~permissions:(fun _ _ -> User_data.can_create_group ())
       ~display:(fun _ _ -> user_widget#display_group_creation ()));

  Eliom_output.Html5.register ~service:User_services.action_create_new_group
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return"Create new group")
       ~permissions:(fun _ _ -> User_data.can_create_group ())
       ~display:(user_widget#display_group_creation_done)
       ~service:(fun _ _ -> Lwt.return User_services.service_create_new_group));

  (* We register the syntax extensions *)
  User_ext.register_user_extensions user_widget



(* We create the admin menu for the extension *)

let users_root =
  Eliom_service.service
    ~path:[!Ocsimore_config.admin_dir;"users"]
    ~get_params:Eliom_parameter.unit ()

let () = Eliom_output.Html5.register users_root
  (fun () () ->
     Page_site.admin_page
       ~title:"Ocsimore - Users module"
       Html5.F.([
         p [pcdata "This is the Ocsimore admin page for the users module." ];
       ]))




let () =
  Page_site.add_to_admin_menu ~root:users_root ~name:"Users"
  ~links:([
    "View users", User_services.service_view_users, (fun _ -> Lwt.return true);
    "View groups", User_services.service_view_groups,(fun _ -> Lwt.return true);
    "View roles", User_services.service_view_roles, (fun _ -> Lwt.return true);
    "Users creation", User_services.service_create_new_user,
            (fun _ -> match User_services.basicusercreation with
               | User_services.UserCreation options ->
                   User_data.can_create_user ~options
               | User_services.NoUserCreation -> Lwt.return false);
    "Groups creation", User_services.service_create_new_group,
            (fun _ -> User_data.can_create_group ());
          ]
  )


let () =
  Page_site.add_status_function
    (fun () ->
       user_widget#status_text >|= fun f ->
       Html5.F.div f
    )

