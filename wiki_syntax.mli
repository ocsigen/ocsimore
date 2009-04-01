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



(** find services for each wiki *)
val find_naservpage : Wiki_sql.wiki ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service

val find_servpage : Wiki_sql.wiki ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service option

val find_servwikicss : Wiki_sql.wiki ->
  (unit, unit,
   [ `Attached of
       [> `Internal of [> `Service ] * [ `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [ `Registrable ])
    Eliom_services.service option

val add_naservpage : Wiki_sql.wiki ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service -> unit

val add_servpage : Wiki_sql.wiki ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service -> unit

val add_servwikicss : Wiki_sql.wiki ->
  (unit, unit,
   [ `Attached of
      [ `Internal of [ `Service ] * [ `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [ `Registrable ])
    Eliom_services.service -> unit


(** Define new extensions to the wiki syntax. *)
val add_block_extension : 
  string ->
  (Wiki_sql.wiki ->
     Wiki_widgets_interface.box_info ->
     (string * string) list -> 
       string option -> 
         Xhtmltypes_duce.flows Lwt.t) -> 
  unit

val add_a_content_extension : 
  string -> 
  (Wiki_sql.wiki ->
     Wiki_widgets_interface.box_info ->
       (string * string) list -> 
         string option -> 
           {{[ Xhtmltypes_duce.a_content* ]}} Lwt.t) -> 
  unit

val add_link_extension : 
  string -> 
  (Wiki_sql.wiki -> 
     Wiki_widgets_interface.box_info ->
     (string * string) list -> 
       string option -> 
         string * Wikicreole.attribs * 
           {{[ Xhtmltypes_duce.a_content* ]}} Lwt.t) -> 
  unit


(** Returns the XHTML corresponding to a wiki page.
    The int32 is the id of the wiki (a wikibox may contain another one,
    and the default wiki id is the same as the one of the surrounding box).
*)
val xml_of_wiki :
  Wiki_sql.wiki -> 
  Wiki_widgets_interface.box_info ->
  string -> 
  Xhtmltypes_duce.flows Lwt.t

(** returns only the content of the first paragraph of a wiki text.
*)
val inline_of_wiki :
  Wiki_sql.wiki -> 
  Wiki_widgets_interface.box_info ->
  string -> 
  Xhtmltypes_duce.inlines Lwt.t

(** returns only the content of the first paragraph of a wiki text,
    after having removed links.
*)
val a_content_of_wiki :
  Wiki_sql.wiki -> 
  Wiki_widgets_interface.box_info ->
  string -> 
  {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension : 
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id]) *)
val parse_common_attribs : (string * string) list -> Xhtmltypes_duce.coreattrs

(** returns true if the string is an absolute URL (http://...) *)
val is_absolute_link : string -> bool


(** To be passed as information inside [sd] for evaluating conditions
<<cond http_code='404'| >> *)

type page_displayable =
  | Page_displayable
  | Page_404
  | Page_403

val page_displayable: Polytables.t -> page_displayable

val set_page_displayable: Polytables.t -> page_displayable -> unit
