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
   Filtering wiki syntax to perform special actions
   (before saving it to the database)
   @author Vincent Balat
*)

(** The filters registered with that function are called on
    the wikibox content before registering it in the database.
*)
val add_preparser_extension : 
  string -> 
  (int32 ->
     (Eliom_sessions.server_params * 
        Ocsimore_common.session_data) ->
       (string * string) list -> 
         string option -> 
           string option Lwt.t) -> unit


(** Filters the wiki syntax and replace extensions according to 
    preparsers recorded with [add_preparser_extension].
*)
val preparse_extension :
  (Eliom_sessions.server_params * 
     Ocsimore_common.session_data) ->
  int32 -> 
  string -> string Lwt.t
