(* Ocsimore
 * Copyright (C) 2009
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
   @author Boris Yakobowski
   @author Vincent Balat
*)

open User_sql.Types
open Wiki_types



(** Eliom parameter type for wikis *)
val eliom_wiki :
  string -> (wiki, [`WithoutSuffix], [`One of wiki] Eliom_parameters.param_name) Eliom_parameters.params_type


(** inserts a new wiki, creating on the fly the container wikibox
    (which is returned along the index of the new wiki). The [author]
    argument is used when creating the wikibox for the container. *)
val new_wiki :
  title:string -> 
  descr:string -> 
  pages:string option ->
  boxrights:bool ->
  staticdir:string option ->
  container_text:string ->
  author:userid ->
  model:Wiki_types.wiki_model ->
  unit ->
  (wiki * wikibox_id) Lwt.t

(** Inserts a new wikibox in an existing wiki and return the id of the 
    wikibox. *)
val new_wikibox :
  ?db: Sql.db_t ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  content_type:Wiki_types.content_type ->
  unit ->
  wikibox_id Lwt.t

(** return the history of a wikibox. *)
val get_history : wikibox:wikibox ->
  (int32 * string * (* userid *) int32 * CalendarLib.Calendar.t) list Lwt.t


(** Wikipages *)

(** return the information for a wikipage *)
val get_wikipage_info : wiki:wiki -> page:string -> wikipage_info Lwt.t

(** sets the box corresponding to a wikipage. The previous entry is
    entirely overwritten. If [destwiki] is not supplied, it is set
    equal to [sourcewiki]. If [title] is not supplied, it is set equal
    to NULL (and the title for the wiki will be used).
*)
val set_box_for_page :
  sourcewiki:wiki -> page:string -> ?destwiki:wiki -> wbid:wikibox_id -> ?title:string -> unit -> unit Lwt.t



(** returns the css of a wikipage or [None] if  the page has no css *)
val get_css_for_wikipage : wiki:wiki -> page:string -> string option Lwt.t

(** Sets the css for a wikipage *)
val set_css_for_wikipage :
  wiki:wiki -> page:string -> author:userid -> string option -> unit Lwt.t


(** returns the global css of a wiki, or [None] if the wiki has no such css *)
val get_css_for_wiki : wiki:wiki -> string option Lwt.t

val set_css_for_wiki : wiki:wiki -> author:userid -> string option -> unit Lwt.t


(** returns the wikibox for the css of a page or [None] if the page has no css *)
val get_css_wikibox_for_wikipage :
  wiki:wiki -> page:string -> wikibox option Lwt.t

(** returns the wikibox for the global css of a wiki, or [None] if the wiki
    has no such css *)
val get_css_wikibox_for_wiki : wiki:wiki -> wikibox option Lwt.t



(** Find wiki information for a wiki, given its id *)
val get_wiki_info_by_id : id:wiki -> wiki_info Lwt.t

(** Find wiki information for a wiki, given its name *)
val get_wiki_info_by_name : name:string -> wiki_info Lwt.t

(** looks for a wikibox and returns [Some (comment, author, content, datetime,
    content_type, version)], or [None] if the page doesn't exist. *)
val get_wikibox_data : 
  ?version:int32 ->
  wikibox:wikibox ->
  unit ->
  (string * userid * string option * CalendarLib.Calendar.t * Wiki_types.content_type * int32) option Lwt.t

val set_wikibox_special_rights:
  wikibox -> bool -> unit Lwt.t

val wikibox_from_uid: wikibox_uid -> wikibox Lwt.t

(** Current revision number of a wikibox *)
val current_wikibox_version : wikibox:wikibox -> Int32.t option Lwt.t


(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
val update_wikibox :
  wikibox:wikibox ->
  author:userid ->
  comment:string ->
  content:string option ->
  content_type:Wiki_types.content_type ->
  int32 Lwt.t

(** Update the information of a wiki. All arguments not passed are left
    unchanged *)
val update_wiki :
  ?container_id:wikibox_id ->
  ?staticdir:string option ->
  ?pages:string option ->
  wiki -> unit Lwt.t


(** Iterator on all the wikis  *)
val iter_wikis : (wiki_info -> unit Lwt.t) -> unit Lwt.t



(** **)



val get_wikibox_info : wikibox -> wikibox_info Lwt.t
