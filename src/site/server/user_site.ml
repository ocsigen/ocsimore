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

let () =
  let user_creation_disabled =
    Lwt.return Html5.F.([
      h2 [pcdata "Error"];
      p [pcdata "User creation is disabled"];
    ]) in
  Ocsimore_appl.register
    ~service:User_services.service_create_new_user
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun () () -> Lwt.return "Create new user")
       ~display:(fun () () ->
         User_services.basicusercreation () >>= (function
           | User_services.NoUserCreation -> user_creation_disabled
           | User_services.UserCreation options ->
             let user_widget_creation = object
               inherit User_widgets.user_widget
               inherit! User_widgets.user_widget_user_creation options
             end in
             user_widget_creation#display_user_creation ()
         )
       )
       ~permissions:(fun () () ->
         User_services.basicusercreation () >>= (function
           | User_services.NoUserCreation -> Lwt.return true
           | User_services.UserCreation options ->
             User_data.can_create_user ~options
         )
       )
    );
  Ocsimore_appl.register
    ~service:User_services.action_create_new_user
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun () _ -> Lwt.return "Created new user")
       ~service:(fun () _ -> Lwt.return User_services.service_create_new_user)
       ~permissions:(fun () _ ->
         User_services.basicusercreation () >>= (function
           | User_services.NoUserCreation -> Lwt.return false
           | User_services.UserCreation options ->
             User_data.can_create_user ~options
         )
       )
       ~display:(fun () (name, (fullname, (email, pwd))) ->
         User_services.basicusercreation () >>= (function
           | User_services.NoUserCreation -> user_creation_disabled
           | User_services.UserCreation options ->
             let user_widget_creation = object
               inherit User_widgets.user_widget
               inherit! User_widgets.user_widget_user_creation options
             end in
             user_widget_creation#display_user_creation_done
               ~name ~fullname ~email ~pwd
         )
       )
    )


let user_widget =
  (new User_widgets.user_widget : User_widgets.user_widget_class)

let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Ocsimore_appl.register ~service:User_services.service_view_group
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
         user_widget#display_group group
     in
     let permissions user _ =
       User_data.can_view_groups () >>= function
         | true -> Lwt.return true
         | false ->
             User.get_user_name () >>= fun current_user ->
             Lwt.return (current_user = user && user <> User.anonymous_login)
     in
     Page_site.admin_body_content_with_permission_handler
       ~title:(fun g _ -> Lwt.return (Printf.sprintf "View group %S" g))
       ~permissions
       ~service
       ~display);

  Ocsimore_appl.register
    ~service:User_services.service_users_settings
    (Page_site.admin_body_content_with_permission_handler
       ~title: (fun _ _ -> Lwt.return "Users settings")
       ~permissions: (fun _ _ -> User_data.can_admin_users ())
       ~display: (fun _ _ -> user_widget#display_users_settings)
    );

  Ocsimore_appl.register
    ~service:User_services.action_users_settings
    (Page_site.admin_body_content_with_permission_handler
       ~title: (fun _ _ -> Lwt.return "Users settings")
       ~permissions: (fun _ _ -> User_data.can_admin_users ())
       ~display: (user_widget#display_users_settings_done)
    );

  Ocsimore_appl.register
    ~service:User_services.service_view_groups
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View groups")
       ~permissions:(fun _ _ -> User_data.can_view_groups ())
       ~display:(fun _ _ -> user_widget#display_groups));

  Ocsimore_appl.register
    ~service:User_services.service_view_users
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View groups")
       ~permissions:(fun _ _ -> User_data.can_view_users ())
       ~display:(fun _ _ -> user_widget#display_users));

  Ocsimore_appl.register
    ~service:User_services.service_view_roles
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "View roles")
       ~permissions:(fun _ _ -> User_data.can_view_roles ())
       ~display:(fun _ _ -> user_widget#display_roles));

  Ocsimore_appl.register
    ~service:User_services.service_login
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "Login")
       ~permissions:(fun _ _ -> Lwt.return true)
       ~display:(fun _ _ -> user_widget#display_login_widget ()));

  Ocsimore_appl.register ~service:User_services.service_create_new_group
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return "Create new group")
       ~permissions:(fun _ _ -> User_data.can_create_group ())
       ~display:(fun _ _ -> user_widget#display_group_creation ()));

  Ocsimore_appl.register ~service:User_services.action_create_new_group
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun _ _ -> Lwt.return"Create new group")
       ~permissions:(fun _ _ -> User_data.can_create_group ())
       ~display:(user_widget#display_group_creation_done)
       ~service:(fun _ _ -> Lwt.return User_services.service_create_new_group));

  (* We register the syntax extensions *)
  User_ext.register_user_extensions user_widget



(* We create the admin menu for the extension *)

let users_root = User_services.service_view_users

let () =
  Page_site.add_to_admin_menu ~root:users_root ~name:"Users"
  ~links:([
    "View users", User_services.service_view_users, (fun _ -> Lwt.return true);
    "Users settings", User_services.service_users_settings, (fun _ -> Lwt.return true);
    "View groups", User_services.service_view_groups,(fun _ -> Lwt.return true);
    "View roles", User_services.service_view_roles, (fun _ -> Lwt.return true);
    "Users creation", User_services.service_create_new_user,
            (fun _ -> User_services.basicusercreation () >>= (function
              | User_services.UserCreation options ->
                User_data.can_create_user ~options
              | User_services.NoUserCreation -> Lwt.return false
            ));
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
