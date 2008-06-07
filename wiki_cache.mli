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
  int32 Lwt.t


(** return the box corresponding to a wikipage *)
val get_box_for_page : wiki:Wiki_sql.wiki -> page:string -> int32 Lwt.t

(** sets the box corresponding to a wikipage *)
val set_box_for_page : wiki:int32 -> id:int32 -> page:string -> unit Lwt.t

(** returns the css for a page or fails with [Not_found] if it does not exist *)
val get_css_for_page : wiki:int32 -> page:string -> string Lwt.t

(** Sets the css for a wikipage *)
val set_css_for_page : wiki:int32 -> page:string -> string -> unit Lwt.t

(** returns the gloabl css for a wiki
    or fails with [Not_found] if it does not exist *)
val get_css_for_wiki : wiki:int32 -> string Lwt.t

(** Sets the gloabl css for a wiki *)
val set_css_for_wiki : wiki:int32 -> string -> unit Lwt.t


(** *)
val find_wiki : Wiki_sql.wiki -> 
  (Wiki_sql.wiki * string * string * bool * bool * int32 ref) Lwt.t

val populate_readers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val populate_writers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val populate_rights_adm : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val populate_wikiboxes_creators : 
  int32 -> int32 -> int32 list -> unit Lwt.t

val remove_readers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val remove_writers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val remove_rights_adm : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val remove_wikiboxes_creators : 
  int32 -> int32 -> int32 list -> unit Lwt.t

(**/**)
val get_readers_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_writers_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_rights_adm_ : int32 * int32 -> User_sql.userid list Lwt.t
val get_wikiboxes_creators_ : int32 * int32 -> User_sql.userid list Lwt.t


