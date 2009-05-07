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

(** The type of session data. It is a polytable, so that any module
    (like Forum or Wiki) can save its own data.
*)
type session_data = Polytables.t

(** Put this exception in the return value of your actions to transmit
    the session data to the subsequent service.
*)
exception Session_data of session_data

(** Call this function at the beginning of your services to get the
    session data sent by the previous action, or create a new (empty) 
    one otherwise.
*)
val get_sd : sp:Eliom_sessions.server_params -> session_data

(** Call this function only if you do not want to use the session data sent
    by previous action. (For example login action).
*)
val create_empty_sd : unit -> session_data

(** Call this function just after closing the session
*)
val clear_sd : sd:session_data -> unit



(** The type of the functions taking sd ans sp as arguments *)
type 'a sp_sd =
  sp:Eliom_sessions.server_params ->
  sd:session_data ->
  'a
