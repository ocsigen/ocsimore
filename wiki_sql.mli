
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

module Types : sig

(** Semi-abstract type for a wiki *)
type wiki = [`Wiki] Opaque.int32_t

(** Conversions from a wiki index *)
val wiki_id_s : wiki -> string
val s_wiki_id : string -> wiki


type wikibox_id = int32
type wikibox = wiki * wikibox_id
type wikipage = wiki * string


(** Fields for a wiki *)
type wiki_info = {
  wiki_id : wiki;
  wiki_title : string;
  wiki_descr : string;
  wiki_boxrights : bool;
  wiki_pages : string option;
  wiki_container : wikibox_id;
  wiki_staticdir : string option (** if static dir is given,
                                ocsimore will serve static pages if present,
                                instead of wiki pages *);
}
end
open Types


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
  (wiki * wikibox_id) Lwt.t

(** Inserts a new wikibox in an existing wiki and return the id of the 
    wikibox. *)
val new_wikibox :
  wiki:wiki ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  content_type:wikibox_content_type ->
  ?rights:User_sql.userid list * User_sql.userid list * User_sql.userid list * User_sql.userid list ->
  unit ->
  wikibox_id Lwt.t

(** return the history of a wikibox. *)
val get_history : wikibox:wikibox ->
  (int32 * string * User_sql.userid * CalendarLib.Calendar.t) list Lwt.t


(** Wikipages *)

type wikipage = {
  wikipage_source_wiki: wiki;
  wikipage_page: string;
  wikipage_dest_wiki: wiki;
  wikipage_wikibox: wikibox_id;
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
  sourcewiki:wiki -> page:string -> ?destwiki:wiki -> wbid:wikibox_id -> ?title:string -> unit -> unit Lwt.t



(** returns the wikibox for the css of a page or [None] if the page has no css *)
val get_css_wikibox_for_wikipage :
  wiki:wiki -> page:string -> wikibox_id option Lwt.t

(** returns the css of a wikipage or [None] if  the page has no css *)
val get_css_for_wikipage : wiki:wiki -> page:string -> string option Lwt.t

(** Sets the css for a wikipage *)
val set_css_for_wikipage :
  wiki:wiki -> page:string -> author:User_sql.userid -> string -> unit Lwt.t


(** returns the wikibox for the global css of a wiki, or [None] if the wiki
    has no such css *)
val get_css_wikibox_for_wiki : wiki:wiki -> wikibox_id option Lwt.t

(** returns the global css of a wiki, or [None] if the wiki has no such css *)
val get_css_for_wiki : wiki:wiki -> string option Lwt.t

val set_css_for_wiki : wiki:wiki -> author:User_sql.userid -> string -> unit Lwt.t


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
  (string * User_sql.userid * string * CalendarLib.Calendar.t * wikibox_content_type * int32) option Lwt.t


(** Current revision number of a wikibox *)
val current_wikibox_version : wikibox:wikibox -> Int32.t option Lwt.t


(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
val update_wikibox :
  wikibox:wikibox ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  content_type:wikibox_content_type ->
  int32 Lwt.t

(** Update the information of a wiki. All arguments not passed are left
    unchanged *)
val update_wiki :
  ?container_id:wikibox_id ->
  ?staticdir:string option ->
  ?pages:string option ->
  wiki -> unit Lwt.t


val populate_readers : wikibox -> int32 list -> unit Lwt.t
val populate_writers : wikibox -> int32 list -> unit Lwt.t
val populate_rights_adm : wikibox -> int32 list -> unit Lwt.t
val populate_wikiboxes_creators : wikibox -> int32 list -> unit Lwt.t

val remove_readers : wikibox -> int32 list -> unit Lwt.t
val remove_writers : wikibox -> int32 list -> unit Lwt.t
val remove_rights_adm : wikibox -> int32 list -> unit Lwt.t
val remove_wikiboxes_creators : wikibox -> int32 list -> unit Lwt.t


val get_readers : wikibox -> User_sql.userid list Lwt.t
val get_writers : wikibox -> User_sql.userid list Lwt.t
val get_rights_adm : wikibox -> User_sql.userid list Lwt.t
val get_wikiboxes_creators : wikibox -> User_sql.userid list Lwt.t


(** Iterator on all the wikis  *)
val iter_wikis : (wiki_info -> unit Lwt.t) -> unit Lwt.t
