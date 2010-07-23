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


let user_widgets = new User_widgets.user_widget

let body_to_div x =
  (x : Xhtmltypes.body_content XHTML.M.elt list
     :> Xhtmltypes.div_content XHTML.M.elt list
  )

let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Eliom_predefmod.Xhtml.register User_services.service_view_group
    (fun sp g () ->
       User.get_user_by_name g  >>= fun group ->
       (if group = basic_user User.nobody && g <> User.nobody_login then
          Lwt.return
            [XHTML.M.p
               ~a:[XHTML.M.a_class ["errmsg"]]
               [XHTML.M.pcdata ("Unknown group " ^ g)]
            ]
        else
          user_widgets#display_group ~sp (group, g)
       ) >>= fun body ->
       User_sql.user_type group >>= fun gtype ->
       let service = match gtype with
         | `Group -> User_services.service_view_groups
         | `User -> User_services.service_view_users
         | `Role -> User_services.service_view_roles
       in
       Page_site.admin_page ~sp ~service (body_to_div body)
    );

  Eliom_predefmod.Xhtml.register User_services.service_view_groups
    (fun sp () () ->
       user_widgets#display_groups ~sp >|= body_to_div
       >>= (Page_site.admin_page ~sp)
    );

  Eliom_predefmod.Xhtml.register User_services.service_view_users
    (fun sp () () ->
       user_widgets#display_users ~sp >|= body_to_div
       >>= (Page_site.admin_page ~sp)
    );

  Eliom_predefmod.Xhtml.register User_services.service_view_roles
    (fun sp () () ->
       user_widgets#display_roles ~sp >|= body_to_div
       >>= (Page_site.admin_page ~sp)
    );

  Eliom_predefmod.Xhtml.register User_services.service_login
    (fun sp () () ->
       user_widgets#display_login_widget ~sp () >>= fun body ->
       Page_site.admin_page ~sp ~allow_unlogged:true
         [XHTML.M.h1 [XHTML.M.pcdata "Login page"];
          (body: Xhtmltypes.form_content XHTML.M.elt
               :> Xhtmltypes.div_content XHTML.M.elt)
         ]
    );

  Eliom_predefmod.Xhtml.register ~service:User_services.service_create_new_group
    (fun sp () () ->
       user_widgets#display_group_creation ~err:"" ~sp >|= body_to_div
       >>= (Page_site.admin_page ~sp)
    );

  Eliom_predefmod.Xhtml.register ~service:User_services.action_create_new_group
    (fun sp () args ->
       user_widgets#display_group_creation_done sp () args >|= body_to_div
       >>= (Page_site.admin_page ~sp
              ~service:User_services.service_create_new_group)
    );


  (* We register the syntax extensions *)
  User_ext.register_user_extensions user_widgets



let user_creation_widgets = match User_services.basicusercreation with
  | User_services.UserCreation user_creation_options ->
      let user_widget_creation = new User_widgets.user_widget_user_creation
        user_creation_options in

        (* We register the user creation services *)
        Eliom_predefmod.Xhtml.register ~service:User_services.service_create_new_user
          (fun sp () () ->
             user_widget_creation#display_user_creation ~err:"" ~sp >|= body_to_div
             >>= (Page_site.admin_page ~sp)
          );

        Eliom_predefmod.Xhtml.register ~service:User_services.action_create_new_user
          (fun sp () (name, (fullname, (email, pwd))) ->
             user_widget_creation#display_user_creation_done ~sp
               ~name ~fullname ~email ~pwd >|= body_to_div
             >>= (Page_site.admin_page ~sp
                    ~service:User_services.service_create_new_user)
          );

        Some user_widget_creation

    | User_services.NoUserCreation ->

        Eliom_predefmod.Xhtml.register ~service:User_services.service_create_new_user
          (fun sp () () ->
             Page_site.admin_page ~sp
               [XHTML.M.h1 [XHTML.M.pcdata "Error"];
                XHTML.M.p [XHTML.M.pcdata "User creation is disabled"];
               ]
          );

None




(* We create the admin menu for the extension *)

let users_root =
  Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"users"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_predefmod.Xhtml.register users_root
  (fun sp () () ->
     Page_site.admin_page ~sp
       ~title:"Ocsimore - Users module"
       [XHTML.M.h1 [XHTML.M.pcdata "Users module"];
        XHTML.M.p [XHTML.M.pcdata "This is the Ocsimore admin page for the \
                                   users module." ];
       ]
  )




let () = Page_site.add_to_admin_menu ~root:users_root ~name:"Users"
  ~links:([
    "View users", User_services.service_view_users, (fun _ -> Lwt.return true);
    "View groups", User_services.service_view_groups,(fun _ -> Lwt.return true);
    "View roles", User_services.service_view_roles, (fun _ -> Lwt.return true);
    "Users creation", User_services.service_create_new_user,
            (fun sp -> match User_services.basicusercreation with
               | User_services.UserCreation options ->
                   User_data.can_create_user ~sp ~options
               | User_services.NoUserCreation -> Lwt.return false);
    "Groups creation", User_services.service_create_new_group,
            (fun sp -> User_data.can_create_group sp);
          ]
  )


let () =
  Page_site.add_status_function
    (fun ~sp ->
       user_widgets#status_text ~sp >|= fun f ->
       XHTML.M.div f
    )

