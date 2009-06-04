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


(** The list of login errors *)
val get_login_error : sp:server_params -> exn list


(** Authentification with external methods *)
type external_auth = {
  (** A function returning unit if the given user can be authentified by
      the given password, or failing with [BadUser] *)
  ext_auth_authenticate: name:string -> pwd:string -> unit Lwt.t;

  (** The fullname of the user whose login is the argument *)
  ext_auth_fullname: string -> string Lwt.t;
}


class sessionmanager :
external_auth:external_auth option ->
(** do you want https for login? (see option <notsecure/> in User_site) *)
force_secure:bool ->
?sp:server_params -> unit ->
object

  method force_secure:bool

  method action_login:
    (unit,
     string * string,
     [`Nonattached of [`Post] na_s],
     [`WithoutSuffix],
     unit,
     [`One of string] param_name * [`One of string] param_name,
     [`Registrable]) service

  method action_logout:
    (unit,
     unit,
     [`Nonattached of [`Post] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service

  method action_logout_get:
    (unit,
     unit,
     [`Nonattached of [`Get] na_s],
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) service
(** Use GET service if you want to make a link
    towards a POST service ... It uses a redirection instead of an action. *)


end;;

(** Authentification using PAM. May not be available if Ocsimore is
    compiled without Ocsimore_pam *)
val external_auth_pam : (?service:string -> external_auth) option ref

(** NIS authentification. *)
val external_auth_nis : external_auth

