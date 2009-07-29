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



type external_auth = {
  (** A function returning unit if the given user can be authentified by
      the given password, or failing with [BadUser] *)
  ext_auth_authenticate: name:string -> pwd:string -> unit Lwt.t;

  (** The fullname of the user whose login is the argument *)
  ext_auth_fullname: string -> string Lwt.t;
}


let login_error_key = Polytables.make_key ()

let get_login_error ~sp =
  try
    Polytables.get
      ~table:(Eliom_sessions.get_request_cache ~sp)
      ~key:login_error_key
  with Not_found -> []


let eliom_user : userid Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32 "userid"



(*
  action_login:
    (unit,
     string * string,
     [`Nonattached of [`Post] na_s],
     [`WithoutSuffix],
     unit,
     [`One of string] param_name * [`One of string] param_name,
     [`Registrable]) service

 action_logout:
    (unit,
     unit,
     [`Nonattached of [`Post] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service

  action_logout_get:
    (unit,
     unit,
     [`Nonattached of [`Get] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service
*)

let services ~external_auth ~force_secure =
  let action_login =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~https:force_secure
      ~name:"login"
      ~keep_get_na_params:false
      ~post_params:(string "usr" ** string "pwd")
  (fun sp () (name, pwd) ->
    Lwt.catch
      (fun () ->
         User_data.login ~sp ~name ~pwd ~external_auth >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp
           Eliom_services.void_hidden_coservice')
      (fun e ->
         Polytables.set (Eliom_sessions.get_request_cache sp)
           login_error_key [e];
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

  in let action_edit_user_data =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~https:force_secure
      ~post_params:(eliom_user ** (string "pwd" **
                      (string "pwd2" ** (string "descr" ** string "email"))))
      (fun sp () (userid, (pwd, (pwd2, (descr, email)))) ->
         Ocsimore_common.catch_action_failure sp
           (fun () ->
              User_data.change_user_data ~sp ~userid ~pwd:(pwd, pwd2)
                ~fullname:descr ~email)
        >>= fun () ->
        Eliom_predefmod.Action.send ~sp ())
  in

  (** Groups-related services *)
  let params_groups = Eliom_parameters.string "group" **
    (Eliom_parameters.string "add" ** Eliom_parameters.string "rem") in

  let action_add_remove_users_from_group =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_users_from_group" ~post_params:params_groups
      (fun sp () (g, (add, rem)) ->
         Ocsimore_common.catch_action_failure ~sp
           (fun () -> User_data.add_remove_users_from_group sp g (add, rem))
         >>= fun () -> Eliom_predefmod.Action.send ~sp ()
      )
  and action_add_remove_user_from_groups =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_user_from_groups" ~post_params:params_groups
      (fun sp () (u, (add, rem)) ->
         Ocsimore_common.catch_action_failure ~sp
           (fun () -> User_data.add_remove_user_from_groups sp u (add, rem))
         >>= fun () -> Eliom_predefmod.Action.send ~sp ()
      )

  and service_view_group = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_group"]
    ~get_params:(Eliom_parameters.string "group") ()

  and service_view_groups = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_groups"]
    ~get_params:(Eliom_parameters.unit) ()

  and service_login = Eliom_services.new_service
    ~https:force_secure
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "login"]
    ~get_params:Eliom_parameters.unit
    ()

  in (
    action_login,
    action_logout,
    action_logout_get,
    action_edit_user_data,
    action_add_remove_users_from_group,
    action_add_remove_user_from_groups,
    service_view_group,
    service_view_groups,
    service_login
  )


let services_user_creation () =
  let service_create_new_user =
    Eliom_services.new_service
      ~path:([Ocsimore_lib.ocsimore_admin_dir; "create_user"])
      ~get_params:unit () in
  let action_create_new_user =
    Eliom_services.new_post_coservice
      ~fallback:service_create_new_user
      ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
  in
  (service_create_new_user,
   action_create_new_user)




module type Services = sig

val action_login :
  (unit, string * string,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of string ] Eliom_parameters.param_name *
   [ `One of string ] Eliom_parameters.param_name, [> `Registrable ])
  Eliom_services.service

val action_logout :
  (unit, unit, [> `Nonattached of [> `Post ] Eliom_services.na_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_services.service

val action_logout_get :
  (unit, unit, [> `Nonattached of [> `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_services.service

val action_edit_user_data :
  (unit, User_sql.Types.userid * (string * (string * (string * string))),
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of User_sql.Types.userid ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    ([ `One of string ] Eliom_parameters.param_name *
     ([ `One of string ] Eliom_parameters.param_name *
      [ `One of string ] Eliom_parameters.param_name))),
   [> `Registrable ])
  Eliom_services.service

val action_add_remove_users_from_group :
  (unit, string * (string * string),
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of string ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    [ `One of string ] Eliom_parameters.param_name),
   [> `Registrable ])
  Eliom_services.service

val action_add_remove_user_from_groups :
  (unit, string * (string * string),
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of string ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    [ `One of string ] Eliom_parameters.param_name),
   [> `Registrable ])
  Eliom_services.service

val service_view_group :
  (string, unit,
   [> `Attached of
        [> `Internal of [> `Service ] * [> `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name, 
   unit, [> `Registrable ])
  Eliom_services.service

val service_view_groups :
  (unit, unit,
   [> `Attached of
        [> `Internal of [> `Service ] * [> `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_services.service

val service_login :
  (unit, unit,
   [> `Attached of
        [> `Internal of [> `Service ] * [> `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [> `Registrable ])
  Eliom_services.service

end

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
