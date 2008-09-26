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


(** Type used to avoid wikibox loops *)
type ancestors

val no_ancestors : ancestors

val in_ancestors : (int32 * int32) -> ancestors -> bool

val add_ancestor : (int32 * int32) -> ancestors -> ancestors


(** find services for each wiki *)
val find_naservpage : int32 ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service

val find_servpage : int32 ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service option

val add_naservpage : int32 ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service -> unit

val add_servpage : int32 ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service -> unit


(** Define new extensions to the wiki syntax. *)
val add_block_extension : 
  string -> 
  (int32 ->
     (Eliom_sessions.server_params * 
      Ocsimore_common.session_data *
      (Xhtmltypes_duce.flows option * ancestors)) ->
     (string * string) list -> 
       string option -> 
         Xhtmltypes_duce.flows Lwt.t) -> unit

val add_a_content_extension : 
  string -> 
  (int32 ->
     (Eliom_sessions.server_params * 
      Ocsimore_common.session_data *
      (Xhtmltypes_duce.flows option * ancestors)) ->
     (string * string) list -> 
       string option -> 
         {{[ Xhtmltypes_duce.a_content* ]}} Lwt.t) -> unit

val add_link_extension : 
  string -> 
  (int32 ->
     (Eliom_sessions.server_params * 
      Ocsimore_common.session_data *
      (Xhtmltypes_duce.flows option * ancestors)) ->
     (string * string) list -> 
       string option -> 
         string * Wikicreole.attribs * {{[ Xhtmltypes_duce.a_content* ]}} Lwt.t) -> unit


(** Returns the XHTML corresponding to a wiki page.
    The int32 is the id of the wiki (a wikibox may contain another one,
    and the default wiki id is the same as the one of the surrounding box).
*)
val xml_of_wiki :
  ?subbox: Xhtmltypes_duce.flows ->
  ancestors:ancestors ->
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  int32 ->
  string -> 
  Xhtmltypes_duce.flows Lwt.t

(** returns only the content of the first paragraph of a wiki text.
*)
val inline_of_wiki :
  ?subbox: Xhtmltypes_duce.flows ->
  ancestors:ancestors ->
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  int32 ->
  string -> 
  Xhtmltypes_duce.inlines Lwt.t

(** returns only the content of the first paragraph of a wiki text,
    after having removed links.
*)
val a_content_of_wiki :
  ?subbox: Xhtmltypes_duce.flows ->
  ancestors:ancestors ->
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  int32 ->
  string -> 
  {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension : 
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id]) *)
val parse_common_attribs : (string * string) list -> Xhtmltypes_duce.coreattrs
