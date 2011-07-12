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
   Wiki AST to OcamlDuce
   @author Vincent Balat
*)

open Eliom_pervasives
open Wiki_types


(** The type for a function acting as a syntax extension *)
type ('res, 'phrasing_without_interactive, 'href) syntax_extension =
  (Wiki_widgets_interface.box_info, 'res, 'phrasing_without_interactive, 'href) Wikicreole.plugin


(** The abstract type of the objects able to parse wiki creole syntax,
    possibly with extensions. Those objects are passed as arguments
    to all displaying functions *)
type ('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser

type ('a,'b) wiki_service =
    ('a, unit,
     Eliom_services.get_service_kind,
     Eliom_services.suff,
     'b, unit,
     Eliom_services.registrable,
     Eliom_output.appl_service ) Eliom_services.service

type service_href

type href =
  | String_href of string
  | Service_href of service_href

val service_href : ?fragment:string -> ?https:bool -> ('a,'b) wiki_service -> 'a -> service_href

val a_link_of_href : service_href ->
  ?a:HTML5_types.a_attrib HTML5.M.attrib list ->
  'a Eliom_pervasives.HTML5.M.elt list ->
  [> 'a HTML5_types.a ] Eliom_pervasives.HTML5.M.elt

val uri_of_href : href -> string

(** Add a syntax extension to an existing syntax parser.
    If [wiki_content] is [true], it means that your extension
    may contain wikisyntax after "|" (that will be preparsed).
*)
val add_extension :
  wp:('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  name:string ->
  ?wiki_content:bool ->
  ('res, 'phrasing_without_interactive, 'href) syntax_extension ->
  unit


(** The default syntax parser. It parses wiki creole syntax, as well
    as div, span, wikiname, raw, content, menu and cond tags.
    Default (and full) wiki parser.
*)
val wikicreole_parser :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser
(* Currently modified in Wiki_widgets and User_widgets *)

(** The same, without subwikiboxes and containers (content).
    Used for example for forum messages.
*)
val reduced_wikicreole_parser0 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser

(** The same, without images, objects, subwikiboxes and containers (content).
    Used for example for forum messages with restricted features.
*)
val reduced_wikicreole_parser1 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser

(** The same, without images, objects, titles, tables, lists,
    subwikiboxes and containers (content). *)
val reduced_wikicreole_parser2 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser

(** For button content. *)
val reduced_wikicreole_parser_button_content :
  (HTML5_types.button_content HTML5.M.elt list Lwt.t,
   HTML5_types.button_content HTML5.M.elt list Lwt.t,
   HTML5_types.button_content HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser

(** Parser for phrasing wikicreole. *)
val phrasing_wikicreole_parser :
  (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   href
  ) wikicreole_parser

(** Parser for menu *)
val menu_parser :
  ([ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
      Eliom_pervasives.HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing Eliom_pervasives.HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive
     Eliom_pervasives.HTML5.M.elt list Lwt.t, href) wikicreole_parser

(** the content type for wikicreole boxes: *)
val wikicreole_content_type : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser0: *)
val reduced_wikicreole_content_type0 : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser1: *)
val reduced_wikicreole_content_type1 : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type

(** the content type for reduced_wikicreole_parser2: *)
val reduced_wikicreole_content_type2 : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type

(** the content type for raw text boxes: *)
val rawtext_content_type : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type

(** the content type for wikicreole phrasing content.
    It is using [phrasing_wikicreole_parser]. *)
val wikicreole_phrasing_content_type :
  HTML5_types.phrasing HTML5.M.elt list Wiki_types.content_type

(** Return a copy of a parser. The calls to [add_extension] on one of the
    copy will not be visible on the other *)
val copy_parser :
  ('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  ('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser



(** Functions called to transform some wikicreole text *)
val add_preparser_extension :
  wp:('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  name:string ->
  (Wiki_types.wikibox,
    string option Lwt.t)
  Wikicreole.plugin_args ->
  unit

val preparse_extension :
  ('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  Wiki_types.wikibox ->
  string -> string Lwt.t


(** Sets the extension which will be called on links *)
val set_link_extension :
  wp:('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  (string ->
   string option ->
   Wikicreole.attribs ->
   Wiki_types.wikibox ->
   string option Lwt.t) ->
  unit


(** **)

(** Functions displaying wikicreole code *)


(** Returns the HTML5 corresponding to a wiki page *)
val xml_of_wiki :
  ('res_pre list Lwt.t,
   'phrasing,
   [> `PCDATA ] HTML5.M.elt list Lwt.t,
   'href
  ) wikicreole_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  'res_pre list Lwt.t

(** returns only the content of the first paragraph of a wiki text. *)
val phrasing_of_wiki :
  Wiki_widgets_interface.box_info ->
  string ->
  HTML5_types.phrasing HTML5.M.elt list Lwt.t

(** returns only the content of the first paragraph of a wiki text,
    after having removed links. *)
val phrasing_without_interactive_of_wiki :
  Wiki_widgets_interface.box_info ->
  string ->
  HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension :
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id]) *)
val parse_common_attribs :
  (string * string) list -> HTML5_types.common HTML5.M.attrib list

(** returns the type of URL.
    [Page] means a page in current wiki ([wiki:page], or [page]),
    [Wiki_page] means a page in another wiki ([wiki(num):page]),
    [Site] means an URL relative to the root of the site ([site:href]),
    [Absolute] means an absolute URL ([<otherscheme>:href]).
*)
type force_https = bool option

type link_kind =
  | Absolute of string
  | Page of string * force_https
  | Wiki_page of Wiki_types.wiki * string * force_https
  | Site of string * force_https

val link_kind : string -> link_kind

val make_href :
  Wiki_widgets_interface.box_info ->
  link_kind -> string option -> href


(** The class to use to denote the fact that the content comes
    from the specified wikibox *)
val class_wikibox: wikibox -> string



val translate_link :
  oldwiki:wiki ->
  newwiki:wiki ->
  newwikipath:string ->
  string ->
  string option ->
  Wikicreole.attribs ->
  wikibox ->
  string option Lwt.t
