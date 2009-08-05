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




(** inserts a new wiki, creating on the fly the container wikibox
    (which is returned along the index of the new wiki). The [author]
    argument is used when creating the wikibox for the container. *)
val new_wiki :
  ?db: Sql.db_t ->
  title:string ->
  descr:string ->
  pages:string option ->
  boxrights:bool ->
  staticdir:string option ->
  ?container_text:string ->
  author:userid ->
  model:Wiki_types.wiki_model ->
  unit ->
  (wiki * wikibox option) Lwt.t

(** Inserts a new wikibox in an existing wiki and return the id of the
    wikibox. *)
val new_wikibox :
  ?db: Sql.db_t ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  content_type:'a Wiki_types.content_type ->
  unit ->
  wikibox Lwt.t

(** return the history of a wikibox. *)
val get_wikibox_history : wb:wikibox ->
  (int32 * string * (* userid *) int32 * CalendarLib.Calendar.t) list Lwt.t


(** Wikipages *)

(** Links the wikibox [wb] to the page [page] of wiki [wiki]. Fails
    if there already exists a wikibox linked to this wikipage *)
val create_wikipage :
  ?db: Sql.db_t ->
  wiki:wiki ->
  page:string ->
  wb:wikibox ->
  unit Lwt.t

(** return the information for a wikipage *)
val get_wikipage_info : wiki:wiki -> page:string -> wikipage_info Lwt.t

(** Sets the info for a wikipage. All parameters not passed are left
    unchanged. If [title] is [Some ""], it will be set to NULL
    (just as if it is [None]). If [wb] is [None], the page is deleted.
*)
val set_wikipage_properties :
  ?db: Sql.db_t ->
  wiki:wiki ->
  page:string ->
  ?title:string ->
  ?newpage:string ->
  ?wb:wikibox option ->
  unit -> unit Lwt.t



(** returns the css of a wikipage or a wiki, or [None] if no CSS is
    associated to wiki or the wikipage *)
val get_css_for_wikipage : wiki:wiki -> page:string -> string option Lwt.t
val get_css_for_wiki : wiki:wiki -> string option Lwt.t


(** Sets the css for a wikipage or a wiki *)
val set_css_for_wikipage :
  ?db: Sql.db_t ->
  wiki:wiki ->
  page:string ->
  author:userid ->
  string option ->
  unit Lwt.t

val set_css_for_wiki :
  ?db: Sql.db_t ->
  wiki:wiki ->
  author:userid ->
  string option ->
  unit Lwt.t

(** Sets the wikibox for the CSS of a wikipage or a wiki. If the wikibox
   argument is [None], the CSS is deleted. (But not the underlying content
   of the wikibox.) *)
val set_wikibox_css_wiki :
  ?db: Sql.db_t ->
  wiki:wiki ->
  wikibox option ->
  unit Lwt.t
val set_wikibox_css_wikipage :
  ?db: Sql.db_t ->
  wiki:wiki ->
  page:string ->
  wikibox option ->
  unit Lwt.t


(** returns the wikibox for the css of a page or [None] if the page has no css*)
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
val get_wikibox_content :
  ?version:int32 ->
  wikibox ->
  (string * userid * string option * CalendarLib.Calendar.t *
     'a Wiki_types.content_type * int32) option Lwt.t


(** Does the wikibox have special permission rights *)
val set_wikibox_special_rights:
  ?db: Sql.db_t ->
  wb:wikibox ->
  bool ->
  unit Lwt.t


(** Wiki in which the wikibox currently resides *)
val wikibox_wiki: wikibox -> wiki Lwt.t


(** Current revision number of a wikibox *)
val current_wikibox_version :
  wikibox -> Int32.t option Lwt.t


(** Inserts a new version of an existing wikibox in a wiki
    and return its version number. *)
val update_wikibox :
  ?db: Sql.db_t ->
  author:userid ->
  comment:string ->
  content:string option ->
  content_type:'a Wiki_types.content_type ->
  wikibox ->
  int32 Lwt.t

(** Update the information of a wiki. All arguments not passed are left
    unchanged *)
val update_wiki :
  ?db: Sql.db_t ->
  ?container:wikibox option ->
  ?staticdir:string option ->
  ?path:string option ->
  ?descr:string ->
  ?boxrights:bool ->
  wiki -> unit Lwt.t


(** Iterator on all the wikis  *)
val iter_wikis :
  ?db: Sql.db_t ->
  (wiki_info -> unit Lwt.t) ->
  unit Lwt.t


val get_wikibox_info : wikibox -> wikibox_info Lwt.t


(** This function updates the content of all the wikiboxes stored in
    the database (including the old versions) according to the function
    passed as argument, which must return the updated content, or None *)
val update_wikiboxes :
  ?db: Sql.db_t ->
  (wikibox:wikibox ->
   version:int32 ->
   content:string option ->
   content_type:'a content_type ->
   string option Lwt.t) ->
  unit Lwt.t


val rewrite_wikipages :
  ?db: Sql.db_t ->
  oldwiki:wiki ->
  newwiki:wiki ->
  path:string ->
  unit Lwt.t


(** / **)
(* This function can be used to convert from the all wikiboxes ids to
   the (new) uids. *)
val wikibox_new_id: wiki:wiki -> wb_old_id:int32 -> wikibox Lwt.t



