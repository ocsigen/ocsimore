
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

   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)

(** Abstract type for a wiki *)
type wiki = [`Wiki] Opaque.int32_t

(** Conversions from a wiki index *)
val wiki_id_s : wiki -> string
val s_wiki_id : string -> wiki

(** Data stored in a wikibox *)
type wikibox_content_type =
  | Css
  | Wiki
  | Deleted

exception IncorrectWikiboxContentType of string

val wikibox_content_type_of_string : string -> wikibox_content_type

val string_of_wikibox_content_type : wikibox_content_type -> string


(** Direct reading of wikis from eliom *)
val eliom_wiki :
  string -> (wiki, [`WithoutSuffix], [`One of wiki] Eliom_parameters.param_name) Eliom_parameters.params_type


(** inserts a new wiki, creating on the fly the container wikibox
    (which is returned along the index of the new wiki). *)
val new_wiki :
  title:string -> 
  descr:string -> 
  pages:string option ->
  boxrights:bool ->
  staticdir:string option ->
  container_page:string ->
  unit ->
  (wiki * int32) Lwt.t

(** Inserts a new wikibox in an existing wiki and return the id of the 
    wikibox. *)
val new_wikibox :
  wiki:wiki ->
  ?box:int32 ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  content_type:wikibox_content_type ->
  ?rights:User_sql.userid list * User_sql.userid list * User_sql.userid list * User_sql.userid list ->
  unit ->
  int32 Lwt.t

(** return the history of a wikibox. *)
val get_history : 
  wiki:wiki -> 
  id:int32 ->
  (int32 * string * User_sql.userid * CalendarLib.Calendar.t) list Lwt.t


(** Wikipages *)

type wikipage = {
  wikipage_source_wiki: wiki;
  wikipage_page: string;
  wikipage_dest_wiki: wiki;
  wikipage_wikibox: int32;
  wikipage_title: string option;
}

(** return the box corresponding to a wikipage *)
val get_box_for_page : wiki:wiki -> page:string -> wikipage Lwt.t

(** sets the box corresponding to a wikipage. The previous entry is
    entirely overwritten. If [destwiki] is not supplied, it is set
    equal to [sourcewiki]. If [title] is not supplied, it is set equal
    to NULL (and the title for the wiki will be used).
*)
val set_box_for_page :
  sourcewiki:wiki -> page:string -> ?destwiki:wiki -> wikibox:int32 -> ?title:string -> unit -> unit Lwt.t

(** returns the css for a page or fails with [Not_found] if it does not exist *)
val get_css_for_page : wiki:wiki -> page:string -> string Lwt.t

(** Sets the css for a wikipage *)
val set_css_for_page : wiki:wiki -> page:string -> string -> unit Lwt.t

(** returns the global css for a wiki 
    or fails with [Not_found] if it does not exist *)
val get_css_for_wiki : wiki:wiki -> string Lwt.t

(** Sets the global css for a wiki *)
val set_css_for_wiki : wiki:wiki -> string -> unit Lwt.t


(** Fields for a wiki *)
type wiki_info = {
  id : wiki;
  title : string;
  descr : string;
  boxrights : bool;
  pages : string option;
  container_id : int32;
  staticdir : string option; (** if static dir is given,
                                ocsimore will serve static pages if present,
                                instead of wiki pages *)
}


(** Find wiki information for a wiki, given its id *)
val get_wiki_by_id : id:wiki -> wiki_info Lwt.t

(** Find wiki information for a wiki, given its name *)
val get_wiki_by_name : name:string -> wiki_info Lwt.t

(** looks for a wikibox and returns [Some (comment, author, content, datetime,
    content_type, version)], or [None] if the page doesn't exist. *)
val get_wikibox_data : 
  ?version:int32 ->
  wikibox:(wiki * int32) ->
  unit ->
  (string * User_sql.userid * string * CalendarLib.Calendar.t * wikibox_content_type * int32) option Lwt.t


(** Current revision number of a wikibox *)
val current_wikibox_version : wikibox:(wiki * int32) -> Int32.t option Lwt.t


(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
val update_wikibox :
  wiki:wiki ->
  wikibox:int32 ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  content_type:wikibox_content_type ->
  int32 Lwt.t

(** Update container_id (only, for now). *)
val update_wiki :
  wiki_id:wiki ->
  container_id:int32 ->
  unit ->
  unit Lwt.t

val populate_readers :
  wiki -> int32 -> int32 list -> unit Lwt.t
val populate_writers :
  wiki -> int32 -> int32 list -> unit Lwt.t
val populate_rights_adm :
  wiki -> int32 -> int32 list -> unit Lwt.t
val populate_wikiboxes_creators :
  wiki -> int32 -> int32 list -> unit Lwt.t

val remove_readers :
  wiki -> int32 -> int32 list -> unit Lwt.t
val remove_writers :
  wiki -> int32 -> int32 list -> unit Lwt.t
val remove_rights_adm :
  wiki -> int32 -> int32 list -> unit Lwt.t
val remove_wikiboxes_creators :
  wiki -> int32 -> int32 list -> unit Lwt.t


val get_readers : (wiki * int32) -> User_sql.userid list Lwt.t
val get_writers : (wiki * int32) -> User_sql.userid list Lwt.t
val get_rights_adm : (wiki * int32) -> User_sql.userid list Lwt.t
val get_wikiboxes_creators : (wiki * int32) -> User_sql.userid list Lwt.t


(** Path associated to a wiki. Does not currently return a list
    of (wiki * _) because of limitations in our phantom types based
    representation *)
val wikis_path : unit -> (int32 * string option) list Lwt.t
