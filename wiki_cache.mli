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

val get_wikibox_data : 
  ?version:int32 ->
  wikibox:(Wiki_sql.wiki * int32) ->
  unit ->
  (string * User_sql.userid * string * CalendarLib.Calendar.t) option Lwt.t

(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
val update_wikibox :
  wiki:Wiki_sql.wiki ->
  wikibox:int32 ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  ?readers:User_sql.userid list ->
  ?writers:User_sql.userid list ->
  ?rights_adm:User_sql.userid list ->
  ?wikiboxes_creators:User_sql.userid list ->
  unit ->
  int32 Lwt.t


(** return the box corresponding to a wikipage *)
val get_box_for_page : wiki:Wiki_sql.wiki -> page:string -> int32 Lwt.t

(** sets the box corresponding to a wikipage *)
val set_box_for_page : wiki:int32 -> id:int32 -> page:string -> unit Lwt.t

(** *)
val find_wiki : Wiki_sql.wiki -> 
  (Wiki_sql.wiki * string * string * string list option * bool) Lwt.t

(**/**)
val get_readers_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_writers_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_rights_adm_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_wikiboxes_creators_ : int32 * int32 -> User_sql.userid list Lwt.t


