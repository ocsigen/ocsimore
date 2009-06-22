(* Ocsimore
 * Copyright (C) 2008
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
   @author Vincent Balat
*)

(** Exception raised by modules when a function tries to read or write
    data in the database without permission.
*)
exception Permission_denied

(** A key that can be used to find inside the session cache an
    exception is raised in an action *)
val action_failure_key : exn Polytables.key

val catch_action_failure :
  sp:Eliom_sessions.server_params ->
  ?f_exn:(exn -> exn) ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t

val get_action_failure :
  sp:Eliom_sessions.server_params ->
  exn option


(** Exception raised when a service is called with incorrect or
    incoherent arguments *)
exception Incorrect_argument


(** Generic headers for an html page. Currently no CSS is added *)
val html_page :
  sp:Eliom_sessions.server_params ->
  ?css:{{ [ Xhtmltypes_duce.link* ] }} ->
  ?title:string ->
  Xhtmltypes_duce.blocks ->
  Xhtmltypes_duce.html Lwt.t


type 'a eliom_usertype =
    ('a, [ `WithoutSuffix ], [ `One of 'a ] Eliom_parameters.param_name)
    Eliom_parameters.params_type

val eliom_opaque_int32 :
  string ->
  'a Opaque.int32_t eliom_usertype


val input_opaque_int32 :
  ?value:'a Opaque.int32_t ->
  ?hidden:bool ->
  [< 'a Opaque.int32_t Eliom_parameters.setoneradio ]
  Eliom_parameters.param_name -> {{Eliom_duce.Blocks.input_elt}}


(** Registers the service that answers for admin files.
    This function must be called at eliom initialisation time,
    as it registers another service automatically *)
val set_service_for_static_files :
  ((string list, unit, Eliom_services.get_service_kind, [ `WithSuffix ],
    [ `One of string list ] Eliom_parameters.param_name, unit,
    [ `Registrable ])
   Eliom_services.service) -> unit
