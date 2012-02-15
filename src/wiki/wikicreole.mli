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
   Parser for Wikicreole
   @author Jérôme Vouillon
   @author Vincent Balat
   @author Boris Yakobowski
*)

(** Xml-like attributes for the extension (eg val='foo') *)
type attribs = (string * string) list

module type RawBuilder = sig

  type href
  type param
  type phrasing_without_interactive
  type phrasing
  type flow
  type flow_without_interactive
  type uo_list

  val chars : string -> phrasing_without_interactive
  val strong_elem : attribs -> phrasing list -> phrasing_without_interactive
  val em_elem : attribs -> phrasing list -> phrasing_without_interactive
  val br_elem : attribs -> phrasing_without_interactive
  val img_elem : attribs -> href -> string -> phrasing_without_interactive
  val tt_elem : attribs -> phrasing list -> phrasing_without_interactive
  val monospace_elem : attribs -> phrasing list -> phrasing_without_interactive
  val underlined_elem : attribs -> phrasing list -> phrasing_without_interactive
  val linethrough_elem : attribs -> phrasing list -> phrasing_without_interactive
  val subscripted_elem : attribs -> phrasing list -> phrasing_without_interactive
  val superscripted_elem : attribs -> phrasing list -> phrasing_without_interactive
  val nbsp : phrasing_without_interactive
  val endash : phrasing_without_interactive
  val emdash : phrasing_without_interactive
  val a_elem_phrasing : attribs -> href -> phrasing_without_interactive list -> phrasing
  val a_elem_flow : attribs -> href -> flow_without_interactive list -> flow
  val make_href : param -> string -> string option -> href
  (** the string option is the fragment part of the URL (#...)*)
  val p_elem : attribs -> phrasing list -> flow_without_interactive
  val pre_elem : attribs -> string list -> flow_without_interactive
  val h1_elem : attribs -> phrasing list -> flow_without_interactive
  val h2_elem : attribs -> phrasing list -> flow_without_interactive
  val h3_elem : attribs -> phrasing list -> flow_without_interactive
  val h4_elem : attribs -> phrasing list -> flow_without_interactive
  val h5_elem : attribs -> phrasing list -> flow_without_interactive
  val h6_elem : attribs -> phrasing list -> flow_without_interactive
  val section_elem : attribs -> flow list -> flow_without_interactive

  val ul_elem : attribs -> (phrasing list * uo_list option * attribs) list -> uo_list
  val ol_elem : attribs -> (phrasing list * uo_list option * attribs) list -> uo_list

  val dl_elem : attribs -> (bool * phrasing list * attribs) list -> flow_without_interactive
  val hr_elem : attribs -> flow_without_interactive

  val table_elem : attribs ->
      ((bool * attribs * phrasing list) list * attribs) list -> flow_without_interactive

  val phrasing : phrasing_without_interactive -> phrasing
  val flow : flow_without_interactive -> flow
  val list : uo_list -> flow_without_interactive

  val error : string -> phrasing_without_interactive

end

(** *)
type (-'param, +'res) plugin = 'param -> attribs -> string option -> 'res

type plugin_resolver = Resolver of (string -> plugin_resolver option)

module type Builder = sig

  include RawBuilder

  type plugin_content =
    [ `Flow5_link of (href * attribs * flow_without_interactive)
    | `Phrasing_link of (href * attribs * phrasing_without_interactive)
    | `Flow5 of flow
    | `Phrasing_without_interactive of phrasing_without_interactive ]

  val plugin:
    string -> plugin_resolver option * (param, plugin_content) plugin
  val plugin_action: string -> int -> int -> (param, unit) plugin
  val link_action:
    string -> string option -> attribs -> int * int -> param -> unit
  val href_action:
    string -> string option -> attribs -> int * int -> param -> unit

end

type ('param, 'res) builder =
    (module Builder with type param = 'param and type flow = 'res)

val from_string :
  ?sectioning:bool -> 'param ->
  ('param, 'res) builder -> string -> 'res list
