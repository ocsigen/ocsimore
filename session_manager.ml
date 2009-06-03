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



let login_error_key = Polytables.make_key ()

let get_login_error ~sp =
  try
    Polytables.get
      ~table:(Eliom_sessions.get_request_cache ~sp)
      ~key:login_error_key
  with Not_found -> []

(* use https for login? *)
let secure = ref true
let set_secure b = secure := b
let get_secure () = !secure



class sessionmanager_aux f_auth ?sp ()  =
  let internal_act_login =
    Eliom_services.new_post_coservice'
      ~https:!secure
      ~name:"login"
      ~keep_get_na_params:false
      ~post_params:(string "usr" ** string "pwd") ()
  and internal_act_logout =
    Eliom_services.new_post_coservice'
      ~name:"logoutpost"
      ~keep_get_na_params:false
      ~post_params:unit ()
  and internal_act_logout_get =
    Eliom_services.new_coservice' ~name:"logout" ~get_params:unit ()
(*VVV I add this GET service because it is not possible to make a link
  towards a POST service ... I use a redirection instead of an action *)
  in

object (self)

  method act_login :
    (unit, string * string,
     [`Nonattached of [`Post] Eliom_services.na_s],
     [`WithoutSuffix], unit,
     [`One of string] Eliom_parameters.param_name *
       [`One of string] Eliom_parameters.param_name,
     [`Registrable]) Eliom_services.service
    = internal_act_login

  method act_logout :
    (unit,
     unit,
     [`Nonattached of [`Post] Eliom_services.na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_act_logout

  method act_logout_get :
    (unit,
     unit,
     [`Nonattached of [`Get] Eliom_services.na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_act_logout_get


  method private mk_act_login sp () (usr, pwd) =
    Eliom_sessions.close_session ~sp () >>= fun () ->
    Lwt.catch
      (fun () ->
         f_auth ~name:usr ~pwd  >>= fun user ->
         Eliom_sessions.clean_request_cache ~sp;
         Users.set_session_data sp user >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp
           Eliom_services.void_hidden_coservice')
      (fun e ->
         Polytables.set (Eliom_sessions.get_request_cache sp)
           login_error_key [e];
         Eliom_predefmod.Action.send ~sp ())


  method private mk_act_logout sp () () =
    Eliom_sessions.close_session ~sp () >>= fun () ->
    Eliom_sessions.clean_request_cache ~sp;
    Eliom_predefmod.Redirection.send ~sp Eliom_services.void_hidden_coservice'


  initializer
    begin
      Eliom_predefmod.Any.register
        ?sp ~service:internal_act_login self#mk_act_login;
      Eliom_predefmod.Any.register
        ?sp ~service:internal_act_logout self#mk_act_logout;
      Eliom_predefmod.Redirection.register ?sp ~service:internal_act_logout_get
        (fun sp () () ->
           ignore (self#mk_act_logout sp () ());
           Lwt.return Eliom_services.void_coservice'
        );
    end
end;;


class sessionmanager = sessionmanager_aux
  (fun ~name ~pwd -> Users.authenticate ~name ~pwd
                     >>= fun u -> Lwt.return u.user_id)


let pam_auth :
    (?service:string -> name:string -> pwd:string -> unit Lwt.t) ref =
  ref (fun ?service:_service ~name:_name ~pwd:_pwd ->
         raise Users.BadUser)

let pam_loaded = ref false

let set_pam_auth f =
  pam_loaded := true;
  pam_auth := f

let pam_loaded () = !pam_loaded



(* Authentification by external means *)
let external_auth f_auth f_fullname ~name ~pwd =
  Lwt.catch
    (fun () -> Users.authenticate ~name ~pwd >>= fun u -> 
               Lwt.return u.user_id)
    (function
       | Users.UseAuth u ->
           (* check external pwd *)
           f_auth ~name ~pwd >>= fun () ->
           Lwt.return u
       | Users.BadUser ->
           (* check external pwd, and create user if ok *)
           f_auth ~name ~pwd >>= fun () ->
           f_fullname name >>= fun fullname ->
           Users.create_user ~pwd:User_sql.Types.External_Auth
             ~name
             ~fullname
             (* Do we need to actualize the fullanem every time the user
                connects? *)
             ()
        | e -> Lwt.fail e)



class sessionmanager_pam pam_service = sessionmanager_aux
  (external_auth (!pam_auth ?service:pam_service) (* XXX find full name using PAM *) (fun n -> Lwt.return n))

class sessionmanager_nis =
  sessionmanager_aux (external_auth Ocsimore_nis.nis_auth
                        (fun usr -> Nis_chkpwd.userinfo usr >>= function
                           | None -> Lwt.return usr
                           | Some { Unix.pw_gecos = v } -> Lwt.return v))
