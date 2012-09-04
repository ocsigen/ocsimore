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
   @author Boris Yakobowski
*)

type user_creation =
  | NoUserCreation
  | UserCreation of User_data.user_creation

type external_auth =
    NoExternalAuth | OtherExternalAuth

let default_data = (NoExternalAuth, true)

let (>>=) = Lwt.bind

let (auth, force_secure) =
  let rec find_data ((auth, secure) as data) = function
    | [] -> Lwt.return data
    | (Simplexmlparser.Element ("notsecure", [], []))::l ->
        find_data (auth, false) l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                    ("Unexpected content inside User_site config")) in
  let c = Eliom_config.get_config () in
  Lwt_main.run (find_data default_data c)

let basicusercreation () =
  User_sql.get_users_settings () >>= (fun data ->
    if data.User_sql.basicusercreation then (
      (match data.User_sql.groups with
        | "" ->
          Lwt.return [
            User_sql.Types.basic_user User.authenticated_users
          ]
        | groups -> User.user_list_of_string groups
      ) >>= (fun groups ->
        Lwt.return (UserCreation {
          User_data.mail_from = data.User_sql.registration_mail_from;
          mail_addr = data.User_sql.registration_mail_addr;
          mail_subject = data.User_sql.registration_mail_subject;
          new_user_groups = groups;
          non_admin_can_create = data.User_sql.non_admin_can_create
        })
       )
    )
    else
      Lwt.return NoUserCreation
  )

let external_auth = match auth with
  | NoExternalAuth -> None
  | OtherExternalAuth ->
    let rec inner = function
      | [] -> None
      | auth_method::xs ->
        try Some auth_method
        with exn -> inner xs
    in
    inner (User_external_auth.get_external_auths ())



open Eliom_content
open Eliom_parameter
open Lwt
open User_sql.Types

(**/**)
let eliom_user : userid Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32 "userid"
(**/**)

let action_login =
  Eliom_registration.Any.register_post_coservice'  ~https:force_secure
    ~name:"login"  ~keep_get_na_params:false
    ~post_params:(string "usr" ** string "pwd")
    (fun () (name, pwd) ->
       try_lwt
          lwt () = User_data.login ~name ~pwd ~external_auth:external_auth in
          Eliom_registration.Redirection.send Eliom_service.void_hidden_coservice'
       with exc ->
          lwt () = User_data.add_login_error exc in
          Eliom_registration.Action.send ())

and action_logout =
  Eliom_registration.Any.register_post_coservice'
    ~name:"logoutpost" ~post_params:unit
    ~keep_get_na_params:false
    (fun () () ->
       lwt () = User_data.logout () in
       Eliom_registration.Redirection.send
         Eliom_service.void_hidden_coservice')

and action_logout_get =
  Eliom_registration.Redirection.register_coservice'
    ~name:"logout" ~get_params:unit
    (fun () () ->
       User_data.logout () >>= fun () ->
       Lwt.return Eliom_service.void_coservice')

and action_edit_user_data =
  Eliom_registration.Any.register_post_coservice'
    ~https:force_secure
    ~post_params:(eliom_user ** (string "pwd" **
                                   (string "pwd2" **
                                      (string "descr" ** string "email"))))
    (fun () (userid, (pwd, (pwd2, (descr, email)))) ->
       lwt () =
         Ocsimore_common.catch_action_failure
           (fun () ->
              lwt () =
                User_data.change_user_data
                  ~userid ~pwd:(pwd, pwd2)
                  ~fullname:descr ~email
              in
              Ocsimore_common.(set_action_failure Ok))
       in
       Eliom_registration.Action.send ())

(** Groups-related services *)

and action_add_remove_users_from_group =
  Eliom_registration.Any.register_post_coservice'
    ~name:"add_remove_users_from_group"
    ~post_params:(Eliom_parameter.string "group" **
                    (Eliom_parameter.string "add" **
                       Eliom_parameter.string "rem"))
    (fun () (g, (add, rem)) ->
       Ocsimore_common.catch_action_failure
         (fun () -> User_data.add_remove_users_from_group g (add, rem))
       >>= fun () -> Eliom_registration.Redirection.send
       Eliom_service.void_hidden_coservice'
    )

(*
  and action_add_remove_user_from_groups =
    Eliom_registration.Any.register_new_post_coservice'
      ~name:"add_remove_user_from_groups" ~post_params:params_groups
      (fun sp () (u, (add, rem)) ->
         Ocsimore_common.catch_action_failure ~sp
           (fun () -> User_data.add_remove_user_from_groups sp u (add, rem))
         >>= fun () -> Eliom_registration.Action.send ~sp ()
      )
*)

and service_view_group = Eliom_service.service
  ~path:[!Ocsimore_config.admin_dir; "view_group"]
  ~get_params:(Eliom_parameter.string "group") ()

and service_view_groups = Eliom_service.service
  ~path:[!Ocsimore_config.admin_dir; "view_groups"]
  ~get_params:(Eliom_parameter.unit) ()

and service_view_users = Eliom_service.service
  ~path:[!Ocsimore_config.admin_dir; "view_users"]
  ~get_params:(Eliom_parameter.unit) ()

and service_view_roles = Eliom_service.service
  ~path:[!Ocsimore_config.admin_dir; "view_roles"]
  ~get_params:(Eliom_parameter.unit) ()

and service_login = Eliom_service.service
  ~https:force_secure
  ~path:[!Ocsimore_config.admin_dir; "login"]
  ~get_params:Eliom_parameter.unit
  ()

and service_create_new_group =
  Eliom_service.service
    ~path:([!Ocsimore_config.admin_dir; "create_group"])
    ~get_params:unit ()

let action_create_new_group =
  Eliom_service.post_coservice
    ~fallback:service_create_new_group
    ~post_params:(string "usr" ** string "descr") ()


(** User creation. *)
(* Ideally, those services should not be created if user creation is disabled.
   To be done when Ocaml has first-class modules, by returning a module
   option *)

let service_create_new_user = Eliom_service.service
  ~path:([!Ocsimore_config.admin_dir; "create_user"])
  ~get_params:unit ()

let action_create_new_user =
  Eliom_service.post_coservice
    ~fallback:service_create_new_user
    ~https:force_secure
    ~post_params:(string "usr" ** (string "descr" **
                                     (string "email" **
                                        string "pass1" ** string "pass2"))) ()

let service_users_settings = Eliom_service.service
  ~path: [!Ocsimore_config.admin_dir; "users_settings"]
  ~get_params: unit ()

let action_users_settings =
  Eliom_service.post_coservice
    ~fallback:service_users_settings
    ~post_params:((bool "enable") **
                    (string "mail_from") **
                    (string "mail_addr") **
                    (string "mail_subject") **
                    (string "groups") **
                    (bool "non_admin")) ()



(*

module type ServicesCreationUser = sig

val service_create_new_user :
  (unit, unit,
   [> `Attached of
        [> `Internal of [> `Service ] * [> `Get ] ] Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_service.service

val action_create_new_user :
  (unit, string * (string * string),
   [> `Attached of
        [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_service.a_s ],
   [ `WithoutSuffix ], unit,
   [ `One of string ] Eliom_parameter.param_name *
   ([ `One of string ] Eliom_parameter.param_name *
    [ `One of string ] Eliom_parameter.param_name),
   [> `Registrable ])
  Eliom_service.service

end
*)


(* In this module instead of user_data because we use service_create_new_user
   as fallback... *)

let mail_user_creation ~name ~email ~from_name ~from_addr ~subject ~uri =
  (* TODO with fork ou mieux en utilisant l'event loop de ocamlnet *)
  try_lwt
    ignore (Netaddress.parse email);
    Netsendmail.sendmail
      ~mailer:!Ocsimore_config.mailer
      (Netsendmail.compose
         ~from_addr:(from_name, from_addr)
         ~to_addrs:[(name, email)]
         ~subject
         ("This is an auto-generated message. "
          ^ "Please do not reply to it.\n"
          ^ "\n"
          ^ "To activate your Ocsimore account, please visit the \
             following link: " ^ uri));
    Lwt.return true
  with _ ->
    Lwt.return false


let create_user ~name ~fullname ~email ?pwd ~options () =
  User_data.can_create_user ~options >>= function
    | true ->
        if not (User_data.valid_username name) then
          Lwt.fail (Failure "ERROR: Bad character(s) in login name!")
        else if not (User_data.valid_emailaddr email) then
          Lwt.fail (Failure "ERROR: Bad formed e-mail address!")
        else
          let pwd = match pwd with
            | None -> User_data.generate_password ()
            | Some pwd -> pwd
          in
          let service = Eliom_service.coservice ~max_use:1
            ~fallback:service_create_new_user
            ~get_params:Eliom_parameter.unit ()
          in
          Eliom_registration.Html5.register ~service
            (Page_site.admin_body_content_with_permission_handler
               ~title:(fun () () -> Lwt.return "Ocsimore - User creation")
               ~permissions:(fun () () -> Lwt.return true)
               ~display:(fun () () ->
                 try_lwt
                   lwt userid =
                     let hash = Bcrypt.hash pwd in
                     let pwd = User_sql.Types.Ocsimore_user_safe hash in
                     User.create_fresh_user ~name ~fullname ~email ~pwd ()
                   in
                   lwt () =
                     User.add_to_groups
                       (User_sql.Types.basic_user userid)
                       options.User_data.new_user_groups
                   in
                   Lwt.return Html5.F.([
                     h2 [pcdata "User created"];
                     p [ pcdata "Your account has been created, and you can now ";
                         a ~service:service_login [Html5.F.pcdata "login"] ()];
                   ])
                 with User.BadUser ->
                   Lwt.return Html5.F.([
                     h2 [pcdata "Error while creating"];
                     p [ pcdata "Bad user (the login may be already existing)"; ]
                   ])
               ));
          let uri =
            Html5.F.make_string_uri ~service ~absolute:true ()
          in
          begin try_lwt
            mail_user_creation ~name ~email ~uri
              ~subject:options.User_data.mail_subject
              ~from_name:options.User_data.mail_from
              ~from_addr:options.User_data.mail_addr
            >>= function
              | true -> Lwt.return ()
              | false ->
                  Lwt.fail (Failure
                        "Registration failed: cannot send confirmation email")
          with
             | User.BadUser ->
                 Lwt.fail (Failure "ERROR: This login already exists")
          end
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied
