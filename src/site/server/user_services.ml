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
    NoExternalAuth | Nis | Pam of string option | Ldap of (string * string)

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

    | (Simplexmlparser.Element ("ldap", [("base", b); ("uri", u)], []))::l ->
        find_data (Ldap (b, u), basicusercreation, secure) l

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
        and non_admin_can_create =
          Ocsimore_lib.list_assoc_default "non_admin" atts "" = "true"
        in
        lwt default_groups =
          try
            User.user_list_of_string (List.assoc "groups" atts)
          with Not_found -> Lwt.return [User_sql.Types.basic_user User.authenticated_users]
        in
        let data =
          auth,
          UserCreation {
            User_data.mail_from = registration_mail_from;
            mail_addr = registration_mail_addr;
            mail_subject = registration_mail_subject;
            new_user_groups = default_groups;
            non_admin_can_create;
          },
          secure
        in
        find_data data l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside User_site config"))
  in
  let c = Eliom_config.get_config () in
  Lwt_unix.run (find_data default_data c)


let external_auth = match auth with
  | NoExternalAuth -> None
  | Nis -> Some User_external_auth.external_auth_nis
  | Pam service ->
      (match !User_external_auth.external_auth_pam with
        | Some pam -> Some (pam ?service ())
        | None -> raise (Ocsigen_config.Config_file_error
                           "Ocsimore compiled without PAM support"))
  | Ldap (base, uri) ->
      (match !User_external_auth.external_auth_ldap with
        | Some f -> Some (f base uri)
        | None -> raise (Ocsigen_config.Config_file_error
                           "Ocsimore compiled without LDAP support"))



open Eliom_pervasives
open Eliom_parameters
open Lwt
open User_sql.Types

(**/**)
let eliom_user : userid Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32 "userid"
(**/**)

let action_login =
  Eliom_output.Any.register_post_coservice'  ~https:force_secure
    ~name:"login"  ~keep_get_na_params:false
    ~post_params:(string "usr" ** string "pwd")
    (fun () (name, pwd) ->
       try_lwt
          lwt () = User_data.login ~name ~pwd ~external_auth:external_auth in
          Eliom_output.Redirection.send Eliom_services.void_hidden_coservice'
       with exc ->
          lwt () = User_data.add_login_error exc in
          Eliom_output.Action.send ())

and action_logout =
  Eliom_output.Any.register_post_coservice'
    ~name:"logoutpost" ~post_params:unit
    ~keep_get_na_params:false
    (fun () () ->
       lwt () = User_data.logout () in
       Eliom_output.Redirection.send
         Eliom_services.void_hidden_coservice')

and action_logout_get =
  Eliom_output.Redirection.register_coservice'
    ~name:"logout" ~get_params:unit
    (fun () () ->
       User_data.logout () >>= fun () ->
       Lwt.return Eliom_services.void_coservice')

and action_edit_user_data =
  Eliom_output.Any.register_post_coservice'
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
       Eliom_output.Action.send ())

(** Groups-related services *)

and action_add_remove_users_from_group =
  Eliom_output.Any.register_post_coservice'
    ~name:"add_remove_users_from_group"
    ~post_params:(Eliom_parameters.string "group" **
                    (Eliom_parameters.string "add" **
                       Eliom_parameters.string "rem"))
    (fun () (g, (add, rem)) ->
       Ocsimore_common.catch_action_failure
         (fun () -> User_data.add_remove_users_from_group g (add, rem))
       >>= fun () -> Eliom_output.Redirection.send
       Eliom_services.void_hidden_coservice'
    )

(*
  and action_add_remove_user_from_groups =
    Eliom_output.Any.register_new_post_coservice'
      ~name:"add_remove_user_from_groups" ~post_params:params_groups
      (fun sp () (u, (add, rem)) ->
         Ocsimore_common.catch_action_failure ~sp
           (fun () -> User_data.add_remove_user_from_groups sp u (add, rem))
         >>= fun () -> Eliom_output.Action.send ~sp ()
      )
*)

and service_view_group = Eliom_services.service
  ~path:[!Ocsimore_config.admin_dir; "view_group"]
  ~get_params:(Eliom_parameters.string "group") ()

and service_view_groups = Eliom_services.service
  ~path:[!Ocsimore_config.admin_dir; "view_groups"]
  ~get_params:(Eliom_parameters.unit) ()

and service_view_users = Eliom_services.service
  ~path:[!Ocsimore_config.admin_dir; "view_users"]
  ~get_params:(Eliom_parameters.unit) ()

and service_view_roles = Eliom_services.service
  ~path:[!Ocsimore_config.admin_dir; "view_roles"]
  ~get_params:(Eliom_parameters.unit) ()

and service_login = Eliom_services.service
  ~https:force_secure
  ~path:[!Ocsimore_config.admin_dir; "login"]
  ~get_params:Eliom_parameters.unit
  ()

and service_create_new_group =
  Eliom_services.service
    ~path:([!Ocsimore_config.admin_dir; "create_group"])
    ~get_params:unit ()

let action_create_new_group =
  Eliom_services.post_coservice
    ~fallback:service_create_new_group
    ~post_params:(string "usr" ** string "descr") ()


(** User creation. *)
(* Ideally, those services should not be created if user creation is disabled.
   To be done when Ocaml has first-class modules, by returning a module
   option *)

let service_create_new_user = Eliom_services.service
  ~path:([!Ocsimore_config.admin_dir; "create_user"])
  ~get_params:unit ()

let action_create_new_user =
  Eliom_services.post_coservice
    ~fallback:service_create_new_user
    ~https:force_secure
    ~post_params:(string "usr" ** (string "descr" **
                                     (string "email" **
                                        string "pass1" ** string "pass2"))) ()



(*

module type ServicesCreationUser = sig

val service_create_new_user :
  (unit, unit,
   [> `Attached of
        [> `Internal of [> `Service ] * [> `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_services.service

val action_create_new_user :
  (unit, string * (string * string),
   [> `Attached of
        [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit,
   [ `One of string ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    [ `One of string ] Eliom_parameters.param_name),
   [> `Registrable ])
  Eliom_services.service

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
          let service = Eliom_services.coservice ~max_use:1
            ~fallback:service_create_new_user
            ~get_params:Eliom_parameters.unit ()
          in
          Eliom_output.Html5.register ~service
            (Page_site.admin_body_content_with_permission_handler
               ~title:(fun () () -> Lwt.return "Ocsimore - User creation")
               ~permissions:(fun () () -> Lwt.return true)
               ~display:(fun () () ->
                 try_lwt
                   lwt userid =
                     let pwd = User_sql.Types.Ocsimore_user_crypt pwd in
                     User.create_fresh_user ~name ~fullname ~email ~pwd ()
                   in
                   lwt () =
                     User.add_to_groups
                       (User_sql.Types.basic_user userid)
                       options.User_data.new_user_groups
                   in
                   Lwt.return HTML5.M.([
                     h2 [pcdata "User created"];
                     p [ pcdata "Your account has been created, and you can now ";
                         Eliom_output.Html5.a
                           ~service:service_login
                           [HTML5.M.pcdata "login"] ()];
                   ])
                 with User.BadUser ->
                   Lwt.return HTML5.M.([
                     h2 [pcdata "Error while creating"];
                     p [ pcdata "Bad user (the login may be already existing)"; ]
                   ])
               ));
          let uri =
            Eliom_output.Html5.make_string_uri ~service ~absolute:true ()
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
