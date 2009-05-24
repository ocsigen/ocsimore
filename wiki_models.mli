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

(** Table of wiki models.
    Each wikis belongs to a "model" describing 
    - the default wikisyntax name
    - the right model
    - the widgets
    
    Shall we put also error_box? yes I think.
*)

exception Wiki_model_does_not_exist

val register_wiki_model :
  name:string ->
  content_type:Wiki_types.content_type ->
  rights:Wiki_types.wiki_rights ->
  widgets:Wiki_widgets_interface.interactive_wikibox -> Wiki_types.wiki_model

val get_rights : Wiki_types.wiki_model -> Wiki_types.wiki_rights
val get_default_content_type : Wiki_types.wiki_model -> Wiki_types.content_type
val get_widgets : Wiki_types.wiki_model -> Wiki_widgets_interface.interactive_wikibox

(** Table of wiki syntaxes. *)
exception Content_type_does_not_exist

type wiki_preparser = 
    Eliom_sessions.server_params * Wiki_types.wikibox -> string -> string Lwt.t

type wiki_parser =
    Wiki_widgets_interface.box_info -> string -> 
  Xhtmltypes_duce.flows Lwt.t(* pretty printer *) 

val register_wiki_parser : 
  name:string -> 
  preparser:wiki_preparser -> 
  parser:wiki_parser -> Wiki_types.content_type

val get_wiki_parser : 
  Wiki_types.content_type -> wiki_parser

val get_wiki_preparser : 
  Wiki_types.content_type -> wiki_preparser

(** default wikiparser for one wiki model *)
val get_default_wiki_parser : Wiki_types.wiki_model -> wiki_parser
val get_default_wiki_preparser : Wiki_types.wiki_model -> wiki_preparser

val css_content_type : Wiki_types.content_type
