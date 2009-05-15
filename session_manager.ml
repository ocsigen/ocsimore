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
*)

open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_predefmod
open Eliom_duce.Xhtml
open Lwt
open User_sql.Types
open Users


type sessionmanager_in = 
{
  url: string list;
  login_actions: server_params -> userid -> unit Lwt.t;
  logout_actions: server_params -> unit Lwt.t;
  administrator: userid;
}


(* use https for login? *)
let secure = ref true
let set_secure b = secure := b
let get_secure () = !secure


class sessionmanager ?sp ~(sessionmanagerinfo: sessionmanager_in) =
  let internal_act_login = 
    new_post_coservice'
      ~https:!secure
      ~name:"login"
      ~keep_get_na_params:false
      ~post_params:(string "usr" ** string "pwd") () 
  and internal_act_logout = 
    new_post_coservice'
      ~name:"logoutpost"
      ~keep_get_na_params:false
      ~post_params:unit ()
  and internal_act_logout_get = 
    new_coservice' ~name:"logout" ~get_params:unit ()
(*VVV I add this GET service because it is not possible to make a link 
  towards a POST service ... I use a redirection instead of an action *)
  in
    
object (self)
  
  val widget_types = Hashtbl.create 1
    
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
     [`Registrable]) service
    = internal_act_logout

  method act_logout_get :
    (unit, 
     unit, 
     [`Nonattached of [`Get] Eliom_services.na_s],
     [`WithoutSuffix], 
     unit, 
     unit, 
     [`Registrable]) service
    = internal_act_logout_get



  method private add_parameter_handler _user _sp () (_url, _param_name) =
    (* if in_group user sessionmanagerinfo.administrator then
       begin
       Ocsigen_messages.debug2 "[add_parameter_handler] user is an administrator.";
       add_parameter ~url ~param:{ name=param_name } >>=
       fun _ -> return []
       end
       else *)
    return [NotAllowed]
      
      
  val mutable all_login_actions = sessionmanagerinfo.login_actions
  val mutable all_logout_actions = sessionmanagerinfo.logout_actions
    
  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>= fun () -> 
    close_session ~sp () >>= fun () -> 
    Lwt.catch
      (fun () -> 
         authenticate ~name:usr ~pwd  >>= fun user -> 
         Eliom_sessions.clean_request_cache ~sp;
         Users.set_session_data sp user.user_id >>= fun () -> 
         all_login_actions sp user.user_id >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp 
           Eliom_services.void_hidden_coservice'
         (* was: Lwt.return [Ocsimore_common.Session_data sd] *)) 
      (fun e -> 
       Polytables.set
         (Eliom_sessions.get_request_cache sp)
         Ocsimore_common.tmp 
         [e];
         Eliom_predefmod.Action.send ~sp ())
        
  method add_login_actions f =
    let old_la = all_login_actions in
    all_login_actions <- fun sp u -> 
    old_la sp u >>= (fun () -> f sp u)
                
  method private mk_act_logout sp () () = 
    all_logout_actions sp >>= fun () ->
    close_session ~sp () >>= fun () -> 
    Eliom_sessions.clean_request_cache ~sp;
    Eliom_predefmod.Redirection.send ~sp Eliom_services.void_hidden_coservice'
(* was:    return [] (* do not send sd here! *) *)
      
  method add_logout_actions f =
    let old_la = all_logout_actions in
    all_logout_actions <- fun sp -> 
    old_la sp >>= fun () -> f sp
  
(*VVV not implemented!
  method safer_mk_act_login sp () (user, pwd) =
  Il faut empêcher un utilisateur ou IP
  qui vient d'essayer de se connecter de recommencer avant 2s???
  cf lwt_lib
*)


  initializer
    begin
      Any.register ?sp ~service:internal_act_login self#mk_act_login;
      Any.register ?sp ~service:internal_act_logout self#mk_act_logout;
      Redirection.register ?sp ~service:internal_act_logout_get
        (fun sp () () ->
           ignore (self#mk_act_logout sp () ());
           Lwt.return Eliom_services.void_coservice'
        );
    end
end;;



let pam_auth :
    (?service:string -> name:string -> pwd:string -> unit -> unit Lwt.t) ref =
  ref (fun ?service:_service ~name:_name ~pwd:_pwd () ->
         raise Users.BadUser)

let pam_loaded = ref false

let set_pam_auth f =
  pam_loaded := true;
  pam_auth := f

let pam_loaded () = !pam_loaded



(*VVV à revoir *)
class sessionmanager_pam pam_service ?sp ~(sessionmanagerinfo: sessionmanager_in) =
object
  inherit sessionmanager ?sp ~sessionmanagerinfo

  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>= fun () -> 
    close_session ~sp () >>= fun () -> 
    Lwt.catch
      (fun () -> 
         Lwt.catch
           (fun () -> Users.authenticate ~name:usr ~pwd
                      >>= fun u -> Lwt.return u.user_id)
           (function
              | Users.UseAuth u -> 
                  (* check PAM pwd *)
                  !pam_auth ?service:pam_service ~name:usr ~pwd () >>= fun () ->
                  Lwt.return u
              | Users.BadUser -> 
                  (* check PAM pwd, and create user if ok *)
                  !pam_auth ?service:pam_service ~name:usr ~pwd () >>= fun () ->
                  Users.create_user
                    ~name:usr
                    ~pwd:User_sql.Types.External_Auth
                    ~fullname:usr
(*VVV Can we get the full name from PAM? 
  If yes, do we need to actualize it every time the user connects? *)
(* XXX useful ??   ~groups:[basic_user Users.authenticated_users] *)
                    ()
              | e -> Lwt.fail e)
         >>= fun user -> 
         Eliom_sessions.clean_request_cache ~sp;
         Users.set_session_data sp user >>= fun () -> 
         all_login_actions sp user >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp 
           Eliom_services.void_hidden_coservice'
      ) 
      (fun e -> 
         Polytables.set
           (Eliom_sessions.get_request_cache sp)
           Ocsimore_common.tmp 
           [e];
         Eliom_predefmod.Action.send ~sp ())

end



(*VVV à revoir *)
class sessionmanager_nis ?sp ~(sessionmanagerinfo: sessionmanager_in) =
object
  inherit sessionmanager ?sp ~sessionmanagerinfo

  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>= fun () -> 
    close_session ~sp () >>= fun () -> 
    Lwt.catch
      (fun () -> 
         Lwt.catch
           (fun () -> Users.authenticate ~name:usr ~pwd
                      >>= fun u -> Lwt.return u.user_id)
           (function
              | Users.UseAuth u -> 
                  (* check NIS pwd *)
                  Ocsimore_nis.nis_auth ~name:usr ~pwd () >>= fun () ->
                  Lwt.return u
              | Users.BadUser -> 
                  (* check NIS pwd, and create user if ok *)
                  Ocsimore_nis.nis_auth ~name:usr ~pwd () >>= fun () ->
                  Users.create_user
                    ~name:usr
                    ~pwd:User_sql.Types.External_Auth
                    ~fullname:usr
(*VVV Can we get the full name from NIS? 
  If yes, do we need to actualize it every time the user connects? *)
(*                    ~groups:[basic_user Users.authenticated_users] *)
                    ()
              | e -> Lwt.fail e)
         >>= fun user -> 
         Eliom_sessions.clean_request_cache ~sp;
         Users.set_session_data sp user >>= fun () -> 
         all_login_actions sp user >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp 
           Eliom_services.void_hidden_coservice'
         (* was: Lwt.return [Ocsimore_common.Session_data sd] *)) 
      (fun e -> 
         Polytables.set
           (Eliom_sessions.get_request_cache sp)
           Ocsimore_common.tmp 
           [e];
         Eliom_predefmod.Action.send ~sp ())

end

