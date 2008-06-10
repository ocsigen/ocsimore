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
open Eliom_duce.Xhtml

type sessionmanager_in = 
{
  url: string list;
  login_actions: server_params -> Users.userdata -> unit Lwt.t;
  logout_actions: server_params -> unit Lwt.t;
  administrator: Users.userdata;
}


(** do you want https for login/password? 
   (see option <notsecure/> in ocsisite) *)
val set_secure : bool -> unit
val get_secure : unit -> bool
      
class sessionmanager : sessionmanagerinfo: sessionmanager_in ->
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


class sessionmanager_pam : sessionmanagerinfo: sessionmanager_in ->
  sessionmanager
