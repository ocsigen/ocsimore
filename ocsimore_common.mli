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

(** Exception raised when a service is called with incorrect or
    incoherent arguments *)
exception Incorrect_argument


(** Generic headers for an html page. Currently no CSS is added *)
val html_page :
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
