open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions 
open Eliom_duce.Xhtml

type sessionmanager_in = 
{
  url: string list;
  default_groups: User_sql.userid list;
  login_actions: server_params -> Users.userdata -> unit Lwt.t;
  logout_actions: server_params -> unit Lwt.t;
  registration_mail_from: string * string;
  registration_mail_subject: string;
  administrator: Users.userdata;
}



      
class sessionmanager: sessionmanagerinfo: sessionmanager_in ->
object

  method act_login: 
    (unit, 
     string * string, 
     [`Nonattached of [`Post] na_s], 
     [`WithoutSuffix],
     unit,
     [`One of string] param_name * [`One of string] param_name,
     [`Registrable]) service

  method act_logout: 
    (unit, 
     unit,
     [`Nonattached of [`Post] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service

  method act_logout_get: 
    (unit, 
     unit,
     [`Nonattached of [`Get] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service
(** Use GET service if you want to make a link 
    towards a POST service ... It uses a redirection instead of an action. *)

  method srv_register: 
    (unit, 
     unit, 
     get_service_kind, 
     [`WithoutSuffix],
     unit, 
     unit, 
     [`Registrable ]) service

  method srv_reminder: 
    (unit, 
     unit, 
     get_service_kind,
     [`WithoutSuffix], 
     unit, 
     unit,
     [`Registrable ]) service
    
  method srv_edit: 
    (unit, 
     unit, 
     get_service_kind, 
     [`WithoutSuffix], 
     unit, 
     unit,
     [`Registrable ]) service
    
  method container: sp:server_params -> sd:Ocsimore_common.session_data ->
    contents:Xhtmltypes_duce.blocks -> Xhtmltypes_duce.html Lwt.t

  method add_login_actions: 
      (server_params -> Users.userdata -> unit Lwt.t) -> unit

  method add_logout_actions: 
      (server_params -> unit Lwt.t) -> unit

end;;

val connect:
  sessionmanager ->
  ('get, 
   'post,
   internal_service_kind,
   [`WithoutSuffix], 
   'gn,
   'pn,
   [`Registrable]) service ->
  (sp:server_params -> 
    sd:Ocsimore_common.session_data -> 
    contents: Xhtmltypes_duce.blocks -> 
    Eliom_duce.Xhtml.page Lwt.t) ->
  ('get -> 'post -> (sp:server_params -> Xhtmltypes_duce._div Lwt.t) list)
  -> unit
