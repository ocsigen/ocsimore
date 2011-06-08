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

open Eliom_pervasives
open User_sql.Types
open Lwt

let ( ** ) = Eliom_parameters.prod


let user_widgets = new User_widgets.user_widget

let body_to_div x =
  (x : HTML5_types.body_content HTML5.M.elt list
     :> HTML5_types.flow5 HTML5.M.elt list
  )

let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Eliom_output.Html5.register User_services.service_view_group
    (fun g () ->
       User.get_user_by_name g  >>= fun group ->
       (if group = basic_user User.nobody && g <> User.nobody_login then
          Lwt.return
            [HTML5.M.p
               ~a:[HTML5.M.a_class ["errmsg"]]
               [HTML5.M.pcdata ("Unknown group " ^ g)]
            ]
        else
          user_widgets#display_group (group, g)
       ) >>= fun body ->
       User_sql.user_type group >>= fun gtype ->
       let service = match gtype with
         | `Group -> User_services.service_view_groups
         | `User -> User_services.service_view_users
         | `Role -> User_services.service_view_roles
       in
       Page_site.admin_page ~service (body_to_div body)
    );

  Eliom_output.Html5.register User_services.service_view_groups
    (fun () () ->
       user_widgets#display_groups >|= body_to_div
       >>= Page_site.admin_page
    );

  Eliom_output.Html5.register User_services.service_view_users
    (fun () () ->
       user_widgets#display_users >|= body_to_div
       >>= Page_site.admin_page
    );

  Eliom_output.Html5.register User_services.service_view_roles
    (fun () () ->
       user_widgets#display_roles >|= body_to_div
       >>= Page_site.admin_page
    );

  Eliom_output.Html5.register User_services.service_login
    (fun () () ->
       user_widgets#display_login_widget () >>= fun body ->
       Page_site.admin_page ~allow_unlogged:true
         [HTML5.M.h1 [HTML5.M.pcdata "Login page"];
          (body: HTML5_types.form_content HTML5.M.elt
               :> HTML5_types.flow5 HTML5.M.elt)
         ]
    );

  Eliom_output.Html5.register ~service:User_services.service_create_new_group
    (fun () () ->
       user_widgets#display_group_creation ~err:"" () >|= body_to_div
       >>= Page_site.admin_page
    );

  Eliom_output.Html5.register ~service:User_services.action_create_new_group
    (fun () args ->
       user_widgets#display_group_creation_done () args >|= body_to_div
       >>= (Page_site.admin_page
              ~service:User_services.service_create_new_group)
    );


  (* We register the syntax extensions *)
  User_ext.register_user_extensions user_widgets



let user_creation_widgets = match User_services.basicusercreation with
  | User_services.UserCreation user_creation_options ->
      let user_widget_creation = new User_widgets.user_widget_user_creation
        user_creation_options in

        (* We register the user creation services *)
        Eliom_output.Html5.register ~service:User_services.service_create_new_user
          (fun () () ->
             user_widget_creation#display_user_creation ~err:"" () >|= body_to_div
             >>= Page_site.admin_page
          );

        Eliom_output.Html5.register ~service:User_services.action_create_new_user
          (fun () (name, (fullname, (email, pwd))) ->
             user_widget_creation#display_user_creation_done
               ~name ~fullname ~email ~pwd >|= body_to_div
             >>= (Page_site.admin_page
                    ~service:User_services.service_create_new_user)
          );

        Some user_widget_creation

    | User_services.NoUserCreation ->

        Eliom_output.Html5.register ~service:User_services.service_create_new_user
          (fun () () ->
             Page_site.admin_page
               [HTML5.M.h1 [HTML5.M.pcdata "Error"];
                HTML5.M.p [HTML5.M.pcdata "User creation is disabled"];
               ]
          );

None




(* We create the admin menu for the extension *)

let users_root =
  Eliom_services.service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"users"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_output.Html5.register users_root
  (fun () () ->
     Page_site.admin_page
       ~title:"Ocsimore - Users module"
       [HTML5.M.h1 [HTML5.M.pcdata "Users module"];
        HTML5.M.p [HTML5.M.pcdata "This is the Ocsimore admin page for the \
                                   users module." ];
       ]
  )




let () = Page_site.add_to_admin_menu ~root:users_root ~name:"Users"
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
       user_widgets#status_text >|= fun f ->
       HTML5.M.div f
    )

