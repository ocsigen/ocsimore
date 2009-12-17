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


let () =
  (* We register all the (non-creation related) services that depend on the
     rendering widget *)

  Eliom_duce.Xhtml.register User_services.service_view_group
    (fun sp g () ->
       User.get_user_by_name g  >>= fun group ->
       (if group = basic_user User.nobody && g <> User.nobody_login then
          let msg = Ocamlduce.Utf8.make ("Unknown group " ^ g) in
          Lwt.return {{ [<p class="errmsg">msg] }}
        else
          user_widgets#display_group ~sp (group, g)
       ) >>= fun body ->
       User_sql.user_type group >>= fun gtype ->
       let service = match gtype with
         | `Group -> User_services.service_view_groups
         | `User -> User_services.service_view_users
         | `Role -> User_services.service_view_roles
       in
       Page_site.admin_page ~sp ~service body
    );

  Eliom_duce.Xhtml.register User_services.service_view_groups
    (fun sp () () ->
       user_widgets#display_groups ~sp >>= fun body ->
       Page_site.admin_page ~sp body
    );

  Eliom_duce.Xhtml.register User_services.service_view_users
    (fun sp () () ->
       user_widgets#display_users ~sp >>= fun body ->
       Page_site.admin_page ~sp body
    );

  Eliom_duce.Xhtml.register User_services.service_view_roles
    (fun sp () () ->
       user_widgets#display_roles ~sp >>= fun body ->
       Page_site.admin_page ~sp body
    );

  Eliom_duce.Xhtml.register User_services.service_login
    (fun sp () () ->
       user_widgets#display_login_widget ~sp () >>= fun body ->
       Page_site.admin_page ~sp {{ [ <h1>"Login page" body ] }}
    );

  Eliom_duce.Xhtml.register ~service:User_services.service_create_new_group
    (fun sp () () ->
       user_widgets#display_group_creation ~err:"" ~sp
       >>= (Page_site.admin_page ~sp)
    );

  Eliom_duce.Xhtml.register ~service:User_services.action_create_new_group
    (fun sp () args ->
       user_widgets#display_group_creation_done sp () args
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
        Eliom_duce.Xhtml.register ~service:User_services.service_create_new_user
          (fun sp () () ->
             user_widget_creation#display_user_creation ~err:"" ~sp
             >>= (Page_site.admin_page ~sp)
          );

        Eliom_duce.Xhtml.register ~service:User_services.action_create_new_user
          (fun sp () (name, (fullname, (email, pwd))) ->
             user_widget_creation#display_user_creation_done ~sp
               ~name ~fullname ~email ~pwd
             >>= (Page_site.admin_page ~sp
                    ~service:User_services.service_create_new_user)
          );

        Some user_widget_creation

    | _ ->

        Eliom_duce.Xhtml.register ~service:User_services.service_create_new_user
          (fun sp () () ->
             Page_site.admin_page ~sp
               {{ [ <h1>"Error" <p>"User creation is disabled" ] }}
          );

None




(* We create the admin menu for the extension *)

let users_root =
  Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"users"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_duce.Xhtml.register users_root
  (fun sp () () ->
     Page_site.admin_page ~sp
       ~title:"Ocsimore - Users module"
       {{ [<h1>"Users module"
           <p>"This is the Ocsimore admin page for the users module." ]}}
  )




let () = Page_site.add_to_admin_menu ~root:users_root ~name:"Users"
  ~links:(["Login", User_services.service_login;
    "View users", User_services.service_view_users;
    "View groups", User_services.service_view_groups;
    "View roles", User_services.service_view_roles;
    "Groups creation", User_services.service_create_new_group;
   ] @
     (match User_services.service_create_new_user with
(*        | None -> []
        | Some*) service -> ["User creation", service])
  )


let () = Page_site.add_status_function user_widgets#status_text

