(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Universit� Paris Diderot - CNRS
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
   Pretty print wiki to HTML5 using Eliom's TyXML
   @author Vincent Balat
*)

open Eliom_pervasives
open Wiki_types


(** The abstract type of the objects able to parse wiki creole syntax,
    possibly with extensions. Those objects are passed as arguments
    to all displaying functions *)
type 'res wikicreole_parser

(** The abstract type for extensible parser. See
    [register_simple_extension] and [register_wiki_extension].*)
type ('res,
      'res_without_interactive,
      'content_link) ext_wikicreole_parser

(** Cast an extensible parser to a raw parser to be used with
    [xml_of_wiki]. *)
val cast_wp : ('a, 'b, 'c) ext_wikicreole_parser -> 'a wikicreole_parser

(** Cast an extensible parser to the associated non_interactive parser
    to be used with [xml_of_wiki]. *)
val cast_niwp : ('a, 'b, 'c) ext_wikicreole_parser -> 'b wikicreole_parser


type ('a,'b, 'kind, 'suff, 'reg, 'appl) wiki_service =
    ('a, unit,
     [< Eliom_services.get_service_kind] as 'kind,
     [< Eliom_services.suff] as 'suff,
     'b, unit,
     [< Eliom_services.registrable] as 'reg,
     [< Eliom_output.appl_service] as 'appl) Eliom_services.service

type service_href

type href =
  | String_href of string
  | Service_href of service_href

val service_href : ?fragment:string -> ?https:bool -> ('a,'b,'c,'d,'e,'f) wiki_service -> 'a -> service_href

val a_link_of_href : service_href ->
  ?a:HTML5_types.a_attrib HTML5.M.attrib list ->
  'a Eliom_pervasives.HTML5.M.elt list ->
  [> 'a HTML5_types.a ] Eliom_pervasives.HTML5.M.elt

val uri_of_href : href -> HTML5.M.uri

(** Add a syntax extension to an existing parser. *)

(** The type of extension that can be registred into both the
    interactive and non_interactive variant and of a parser. *)
type (+'flow,
      +'flow_without_interactive,
      +'phrasing_without_interactive) interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
     ('flow,
      (href * Wikicreole.attribs *' flow_without_interactive),
      'phrasing_without_interactive,
      (href * Wikicreole.attribs * 'phrasing_without_interactive))
       Wikicreole.ext_kind) Wikicreole.plugin

(** The type of extension that can be registred into the interactive
    variant of a parser. *)
type (+'flow_without_interactive,
      +'phrasing_without_interactive) non_interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
     [ `Flow5 of 'flow_without_interactive
     | `Phrasing_without_interactive of 'phrasing_without_interactive])
      Wikicreole.plugin

type preparser =
    Wiki_types.wikibox ->
    Wikicreole.attribs ->
    string option ->
    string option Lwt.t

(* Register an extension whose content does not follow the wiki
   syntax. *)
val register_simple_extension :
  wp:('res,
      'flow_without_interactive,
      'phrasing_without_interactive) ext_wikicreole_parser ->
  name:string ->
  ?preparser:preparser ->
  ?ni_plugin:
    ('flow_without_interactive,
     'phrasing_without_interactive) non_interactive_simple_plugin ->
  ('res,
   'flow_without_interactive,
   'phrasing_without_interactive) interactive_simple_plugin ->
  unit

type (-'content,
      +'flow_without_interactive,
      +'phrasing_without_interactive)
  wiki_plugin =
    Wiki_widgets_interface.box_info ->
    Wikicreole.attribs ->
    'content option ->
    [ `Flow5 of 'flow_without_interactive
    | `Phrasing_without_interactive of 'phrasing_without_interactive]

(* Register an extension whose content follow the wiki syntax. *)
val register_wiki_extension :
  wp:('res,
      'flow_without_interactive,
      'phrasing_without_interactive) ext_wikicreole_parser ->
  name:string ->
  wp_rec:('a HTML5.M.elt list Lwt.t,
          'b HTML5.M.elt list Lwt.t,
          'c HTML5.M.elt list Lwt.t) ext_wikicreole_parser ->
  ?preparser:preparser ->
  ?ni_plugin:
    ('b HTML5.M.elt list Lwt.t,
     'flow_without_interactive,
     'phrasing_without_interactive) wiki_plugin ->
  ('a HTML5.M.elt list Lwt.t,
   'res,
   'phrasing_without_interactive) wiki_plugin ->
  unit

type (-'content,
      +'flow_without_interactive,
      +'phrasing_without_interactive)
  link_plugin =
    Wiki_widgets_interface.box_info ->
    Wikicreole.attribs ->
    'content option ->
    [ `Flow5_link of (href * Wikicreole.attribs * 'flow_without_interactive)
    | `Phrasing_link of (href * Wikicreole.attribs * 'phrasing_without_interactive)]

(* Register an extension whose content follow the wiki syntax. The
   content is parsed with the non interactive variant of [wp_rec]. *)
val register_link_extension :
  wp:('res,
      'flow_without_interactive,
      'phrasing_without_interactive) ext_wikicreole_parser ->
  name:string ->
  wp_rec:('a HTML5.M.elt list Lwt.t,
          'b HTML5.M.elt list Lwt.t,
          'c HTML5.M.elt list Lwt.t) ext_wikicreole_parser ->
  ?preparser:(Wiki_types.wikibox ->
              Wikicreole.attribs ->
              string option -> string option Lwt.t) ->
  ('b HTML5.M.elt list Lwt.t,
   'flow_without_interactive,
   'phrasing_without_interactive) link_plugin ->
  unit

(* Register an extension whose content does follow the wiki syntax. (low-level variant) *)
val raw_register_wiki_extension:
  wp:('res,
      'flow_without_interactive,
      'phrasing_without_interactive) ext_wikicreole_parser ->
  name:string ->
  ?preparser:preparser ->
  ?ni_plugin:
    ('flow_without_interactive,
     'phrasing_without_interactive) non_interactive_simple_plugin ->
  ('res,
   'flow_without_interactive,
   'phrasing_without_interactive) interactive_simple_plugin ->
  unit

(* Add a "flow" syntax extension to all predefined parser
   (that accept flow !) *)

val register_simple_flow_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  ([< HTML5_types.flow5_without_interactive_header_footer] HTML5.M.elt list Lwt.t,
   [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t)
     non_interactive_simple_plugin ->
  unit

val register_interactive_simple_flow_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  ([< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Lwt.t,
   [< HTML5_types.flow5_without_interactive_header_footer] HTML5.M.elt list Lwt.t,
   [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t )
  interactive_simple_plugin ->
  unit

type (+'without_interactive) link_simple_plugin =
    (Wiki_widgets_interface.box_info,
     href * Wikicreole.attribs * 'without_interactive)
      Wikicreole.plugin

val register_link_simple_flow_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:Wiki_syntax_types.preparser ->
  ([< HTML5_types.flow5_without_interactive_header_footer ] HTML5.M.elt list Lwt.t)
    link_simple_plugin ->
  unit

type wiki_flow_pplugin = {
  fpp: 'flow.
    ('flow HTML5_types.between_flow5_and_flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
     'flow HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
    wiki_plugin
}

val register_wiki_flow_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  wiki_flow_pplugin ->
  unit

type interactive_wiki_flow_pplugin = {
  ifpp: 'flow 'flow_without_interactive.
    (('flow, 'flow_without_interactive)
        HTML5_types.between_flow5_and_flow5_without_header_footer
        HTML5.M.elt list Lwt.t,
     'flow HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
    wiki_plugin
}

val register_interactive_wiki_flow_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  interactive_wiki_flow_pplugin ->
  unit

type link_wiki_flow_pplugin = {
  lfpp: 'flow_without_interactive.
    Wiki_widgets_interface.box_info ->
      Wikicreole.attribs ->
      ([> HTML5_types.flow5_without_interactive_header_footer] as 'flow_without_interactive)
        HTML5.M.elt list Lwt.t option ->
      (href * Wikicreole.attribs * 'flow_without_interactive HTML5.M.elt list Lwt.t)
}

val register_link_flow_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  link_wiki_flow_pplugin ->
  unit


(* Add a "phrasing" syntax extension to all predefined parser *)

val register_simple_phrasing_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  ([< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t,
   [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t)
    non_interactive_simple_plugin ->
  unit

val register_interactive_simple_phrasing_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:preparser ->
    (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
      interactive_simple_plugin ->
  unit

val register_link_simple_phrasing_extension :
  name:string ->
  ?reduced:bool ->
  ?preparser:Wiki_syntax_types.preparser ->
  ([< HTML5_types.phrasing_without_interactive ] HTML5.M.elt list Lwt.t)
    link_simple_plugin ->
  unit

type wiki_phrasing_pplugin = {
  ppp: 'phrasing 'phrasing_without_interactive.
    (('phrasing, 'phrasing_without_interactive)
       HTML5_types.between_phrasing_and_phrasing_without_interactive
       HTML5.M.elt list Lwt.t,
     'phrasing HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
    wiki_plugin
}

val register_wiki_phrasing_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  wiki_phrasing_pplugin ->
  unit

val register_interactive_wiki_phrasing_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  wiki_phrasing_pplugin ->
  unit

type link_wiki_phrasing_pplugin =
    Wiki_widgets_interface.box_info ->
    Wikicreole.attribs ->
    HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t option ->
    (href * Wikicreole.attribs * HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)

val register_link_phrasing_extension :
  name: string ->
  ?reduced:bool ->
  ?preparser:preparser ->
  link_wiki_phrasing_pplugin ->
  unit


(******)


(** The default syntax parser. It parses wiki creole syntax, as well
    as div, span, wikiname, raw, content, menu and cond tags.
    Default (and full) wiki parser.
*)

val wikicreole_parser :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.flow5_without_interactive HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser
(* Currently modified in Wiki_widgets and User_widgets *)

(** The same parser as [wikicreole_parser] but with a more precise type. *)
val wikicreole_parser_without_header_footer :
  (HTML5_types.flow5_without_header_footer HTML5.M.elt list Lwt.t,
   HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

(** The same, without subwikiboxes and containers (content).
    Used for example for forum messages.
*)
val reduced_wikicreole_parser0 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.flow5_without_interactive HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

(** The same, without images, objects, subwikiboxes and containers (content).
    Used for example for forum messages with restricted features.
*)
val reduced_wikicreole_parser1 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.flow5_without_interactive HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

(** The same, without images, objects, titles, tables, lists,
    subwikiboxes and containers (content). *)
val reduced_wikicreole_parser2 :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.flow5_without_interactive HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

(** For button content. *)
val reduced_wikicreole_parser_button_content :
  (HTML5_types.button_content HTML5.M.elt list Lwt.t,
   HTML5_types.button_content HTML5.M.elt list Lwt.t,
   HTML5_types.button_content HTML5.M.elt list Lwt.t) ext_wikicreole_parser

(** Parser for phrasing wikicreole. *)
val phrasing_wikicreole_parser :
  (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

(** Parser for menu *)
val menu_parser :
  ([ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] HTML5.M.elt list Lwt.t,
   [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  ) ext_wikicreole_parser

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

val preprocess_extension : 'res wikicreole_parser -> Wiki_models.wiki_preprocessor

(*
(** Sets the extension which will be called on links *)
val set_link_extension :
  wp:('res, 'flow_without_interactive, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser ->
  (string ->
   string option ->
   Wikicreole.attribs ->
   Wiki_types.wikibox ->
   string option Lwt.t) ->
  unit
*)

(** **)

(** Functions displaying wikicreole code *)


(** Returns the HTML5 corresponding to a wiki page *)
val xml_of_wiki :
  'res HTML5.M.elt list Lwt.t wikicreole_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  'res HTML5.M.elt list Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension :
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id], [style]) *)
val parse_common_attribs :
  ?classes:HTML5_types.nmtokens -> Wikicreole.attribs -> [> HTML5_types.common ] HTML5.M.attrib list

type force_https = bool option

(** Returns the type of URL.
    # [Wiki_page (None, path, _)] means the page of the currently display wiki at the given path.
    # [Wiki_page (Some wiki, path, _)] means a page in the providede wiki at the given path.
    # [Href path] means a link relative to the the domain if starting with a '/' or relative to the current URL
      otherwise.
    # [Site path] means a link relative the Ocsigen application site.
    # [Absolute] means an absolute URL ([<otherscheme>:href]).
*)
type link_kind =
  | Wiki_page of Wiki_types.wiki option * string * force_https
  | Href of string * force_https
  | Site of string * force_https
  | Absolute of string

val link_kind : string -> link_kind

val make_href :
  Wiki_widgets_interface.box_info ->
  link_kind -> string option -> href


(** The class to use to denote the fact that the content comes
    from the specified wikibox *)
val class_wikibox: wikibox -> string


(*
val translate_link :
  oldwiki:wiki ->
  newwiki:wiki ->
  newwikipath:string ->
  string ->
  string option ->
  Wikicreole.attribs ->
  wikibox ->
  string option Lwt.t
*)

