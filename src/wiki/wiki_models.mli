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

open Eliom_pervasives

exception Wiki_model_does_not_exist of string

val register_wiki_model :
  name:string ->
  content_type: [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type ->
  rights:Wiki_types.wiki_rights ->
  widgets:Wiki_widgets_interface.interactive_wikibox ->
  Wiki_types.wiki_model

val get_rights : Wiki_types.wiki_model -> Wiki_types.wiki_rights
val get_default_content_type :
  Wiki_types.wiki_model ->
  [> HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type
val get_widgets : Wiki_types.wiki_model -> Wiki_widgets_interface.interactive_wikibox

(** Table of wiki syntaxes. *)
exception Content_type_does_not_exist of string

type wiki_preprocessor = (module Wiki_syntax_types.Preprocessor)
val identity_preprocessor : wiki_preprocessor

(** See [Wiki_syntax_types.Preprocessor.preparse_string] *)
val preparse_string:
  ?href_action:Wiki_syntax_types.link_action ->
  ?link_action:Wiki_syntax_types.link_action ->
  wiki_preprocessor -> Wiki_types.wikibox -> string -> string Lwt.t

(** See [Wiki_syntax_types.Preprocessor.desugar_string] *)
val desugar_string : wiki_preprocessor -> Wiki_syntax_types.desugar_param -> string -> string Lwt.t

type +'res wiki_parser =
  Wiki_widgets_interface.box_info -> string -> 'res Lwt.t (* pretty printer *)

val register_flows_wiki_parser :
  name:string ->
  preprocessor:wiki_preprocessor ->
  parser_:[< HTML5_types.flow5] HTML5.M.elt list wiki_parser ->
    [> HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type

val register_flows_wiki_parser' :
  name:string ->
  preprocessor:wiki_preprocessor ->
  parser_:[< HTML5_types.flow5_without_header_footer] HTML5.M.elt list wiki_parser ->
    [> HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type

(** will also register a flows parser by adding a <div> around the result *)
val register_phrasings_wiki_parser :
  name:string ->
  preprocessor:wiki_preprocessor ->
  parser_:[< HTML5_types.phrasing] HTML5.M.elt list wiki_parser ->
    [> HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type

val get_flows_wiki_parser :
  [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type ->
  [> HTML5_types.flow5] HTML5.M.elt list wiki_parser

val get_flows_wiki_parser' :
  [< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type ->
  [> HTML5_types.flow5_without_header_footer] HTML5.M.elt list wiki_parser

val get_phrasings_wiki_parser :
  [< HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type ->
  [> HTML5_types.phrasing] HTML5.M.elt list wiki_parser

val get_flows_wiki_preprocessor :
  [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type -> wiki_preprocessor

val get_flows_wiki_preprocessor' :
  [< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type -> wiki_preprocessor

val get_phrasings_wiki_preprocessor :
  [< HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type -> wiki_preprocessor

(** default wikiparser for one wiki model *)
val get_default_wiki_parser :
  Wiki_types.wiki_model -> [> HTML5_types.flow5] HTML5.M.elt list wiki_parser
val get_default_wiki_preprocessor : Wiki_types.wiki_model -> wiki_preprocessor

val css_content_type : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type
