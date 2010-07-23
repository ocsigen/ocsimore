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



open Eliom_parameters
open Lwt
open User_sql.Types


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




let eliom_user : userid Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32 "userid"



let action_login =
  Eliom_predefmod.Any.register_new_post_coservice'  ~https:force_secure
    ~name:"login"  ~keep_get_na_params:false
    ~post_params:(string "usr" ** string "pwd")
    (fun sp () (name, pwd) ->
       Lwt.catch
         (fun () ->
            User_data.login ~sp ~name ~pwd ~external_auth >>= fun () ->
            Eliom_predefmod.Redirection.send ~sp 
              Eliom_services.void_hidden_coservice')
         (fun e ->
            Polytables.set (Eliom_sessions.get_request_cache sp)
              User_data.login_error_key [e];
            Eliom_predefmod.Action.send ~sp ()))

and action_logout =
  Eliom_predefmod.Any.register_new_post_coservice'
    ~name:"logoutpost" ~post_params:unit
    ~keep_get_na_params:false
    (fun sp () () ->
       User_data.logout sp >>= fun () ->
       Eliom_predefmod.Redirection.send ~sp
         Eliom_services.void_hidden_coservice'
    )

and action_logout_get =
  Eliom_predefmod.Redirection.register_new_coservice'
    ~name:"logout" ~get_params:unit
    (fun sp () () ->
       User_data.logout sp >>= fun () ->
       Lwt.return Eliom_services.void_coservice')

and action_edit_user_data =
  Eliom_predefmod.Any.register_new_post_coservice'
    ~https:force_secure
    ~post_params:(eliom_user ** (string "pwd" **
                                   (string "pwd2" **
                                      (string "descr" ** string "email"))))
    (fun sp () (userid, (pwd, (pwd2, (descr, email)))) ->
       Ocsimore_common.catch_action_failure sp
         (fun () ->
            User_data.change_user_data ~sp ~userid ~pwd:(pwd, pwd2)
              ~fullname:descr ~email >>= fun () ->
            Polytables.set (Eliom_sessions.get_request_cache sp)
              Ocsimore_common.action_failure_key Ocsimore_common.Ok;
            Lwt.return ()
         )
       >>= fun () ->
       Eliom_predefmod.Action.send ~sp ())

(** Groups-related services *)

and action_add_remove_users_from_group =
  Eliom_predefmod.Any.register_new_post_coservice'
    ~name:"add_remove_users_from_group"
    ~post_params:(Eliom_parameters.string "group" **
                    (Eliom_parameters.string "add" **
                       Eliom_parameters.string "rem"))
    (fun sp () (g, (add, rem)) ->
       Ocsimore_common.catch_action_failure ~sp
         (fun () -> User_data.add_remove_users_from_group sp g (add, rem))
       >>= fun () -> Eliom_predefmod.Redirection.send ~sp
       Eliom_services.void_hidden_coservice'
    )

(*
  and action_add_remove_user_from_groups =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_user_from_groups" ~post_params:params_groups
      (fun sp () (u, (add, rem)) ->
         Ocsimore_common.catch_action_failure ~sp
           (fun () -> User_data.add_remove_user_from_groups sp u (add, rem))
         >>= fun () -> Eliom_predefmod.Action.send ~sp ()
      )
*)

and service_view_group = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_group"]
  ~get_params:(Eliom_parameters.string "group") ()

and service_view_groups = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_groups"]
  ~get_params:(Eliom_parameters.unit) ()

and service_view_users = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_users"]
  ~get_params:(Eliom_parameters.unit) ()

and service_view_roles = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_roles"]
  ~get_params:(Eliom_parameters.unit) ()

and service_login = Eliom_services.new_service
  ~https:force_secure
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "login"]
  ~get_params:Eliom_parameters.unit
  ()

and service_create_new_group =
  Eliom_services.new_service
    ~path:([Ocsimore_lib.ocsimore_admin_dir; "create_group"])
    ~get_params:unit ()

let action_create_new_group =
  Eliom_services.new_post_coservice
    ~fallback:service_create_new_group
    ~post_params:(string "usr" ** string "descr") ()


(** User creation. *)
(* Ideally, those services should not be created if user creation is disabled.
   To be done when Ocaml has first-class modules, by returning a module
   option *)

let service_create_new_user = Eliom_services.new_service
  ~path:([Ocsimore_lib.ocsimore_admin_dir; "create_user"])
  ~get_params:unit ()

let action_create_new_user =
  Eliom_services.new_post_coservice ~fallback:service_create_new_user
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
  Lwt.catch
    (fun () ->
       Lwt_preemptive.detach
         (fun () ->
            ignore(Netaddress.parse email);
            Netsendmail.sendmail
              ~mailer:"/usr/sbin/sendmail"
              (Netsendmail.compose
                 ~from_addr:(from_name, from_addr)
                 ~to_addrs:[(name, email)]
                 ~subject
                 ("This is an auto-generated message. "
                  ^ "Please do not reply to it.\n"
                  ^ "\n"
                  ^ "To activate your Ocsimore account, please visit the \
                     following link: " ^ uri));
            true
         ) ())
    (function _ -> Lwt.return false)


let create_user ~sp ~name ~fullname ~email ?pwd ~options () =
  User_data.can_create_user ~sp ~options >>= function
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
          let service = Eliom_services.new_coservice ~max_use:1
            ~fallback:service_create_new_user
            ~get_params:Eliom_parameters.unit ()
          in
          Eliom_predefmod.Xhtml.register ~sp ~service
            (fun _sp () () ->
               User.create_fresh_user ~name ~fullname ~email
                 ~pwd:(User_sql.Types.Ocsimore_user_crypt pwd) ()
               >>= fun userid ->
               User.add_to_groups (basic_user userid)
                 options.User_data.new_user_groups
               >>= fun () ->
               Page_site.admin_page ~sp
                 ~title:"Ocsimore - User creation"
                 [ XHTML.M.h1 [XHTML.M.pcdata "User created"];
                   XHTML.M.p [
                     XHTML.M.pcdata "Your account has been created, and you \
                                     can now ";
                     Eliom_predefmod.Xhtml.a
                       ~sp
                       ~service:service_login
                       [XHTML.M.pcdata "login"] ();
                   ];
                 ]
            );
          let uri =
            Eliom_predefmod.Xhtml.make_string_uri ~service ~absolute:true ~sp ()
          in
          Lwt.catch (fun () ->
              mail_user_creation ~name ~email ~uri
                ~subject:options.User_data.mail_subject
                ~from_name:options.User_data.mail_from
                ~from_addr:options.User_data.mail_addr
              >>= function
                | true -> Lwt.return ()
                | false ->
                    Lwt.fail (Failure
                          "Registration failed: cannot send confirmation email")
          )
          (function
             | User.BadUser ->
                 Lwt.fail (Failure "ERROR: This login already exists")
             | e -> Lwt.fail e)
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied
