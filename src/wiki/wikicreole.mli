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


type attribs = (string * string) list

type ('a, 'b, 'c) ext_kind =
  | Flow5 of 'a
  | Phrasing_without_interactive of 'b
  | Link_plugin of 'c


(** Arguments for the extension mechanisme, after '<<' *)
type ('param, 'a) plugin_args =
    'param ->
    attribs -> (** Xml-like attributes for the extension (eg val='foo') *)
    string option -> (** content for the extension, after the '|' *)
    'a

type ('param, 'flow, 'phrasing_without_interactive, 'href) plugin =
    ('param,
     ('flow, 'phrasing_without_interactive, ('href * attribs * 'phrasing_without_interactive)) ext_kind
    ) plugin_args


type ('flow, 'phrasing, 'phrasing_without_interactive, 'param, 'href) builder =
  { chars : string -> 'phrasing_without_interactive;
    strong_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    em_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    br_elem : attribs -> 'phrasing_without_interactive;
    img_elem : attribs -> 'href -> string -> 'phrasing_without_interactive;
    tt_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    monospace_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    underlined_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    linethrough_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    subscripted_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    superscripted_elem : attribs -> 'phrasing list -> 'phrasing_without_interactive;
    nbsp : 'phrasing_without_interactive;
    endash : 'phrasing_without_interactive;
    emdash : 'phrasing_without_interactive;
    a_elem : attribs -> 'href -> 'phrasing_without_interactive list -> 'phrasing;
    make_href : 'param -> string -> string option -> 'href;
    (** the string option is the fragment part of the URL (#...)*)
    p_elem : attribs -> 'phrasing list -> 'flow;
    pre_elem : attribs -> string list -> 'flow;
    h1_elem : attribs -> 'phrasing list -> 'flow;
    h2_elem : attribs -> 'phrasing list -> 'flow;
    h3_elem : attribs -> 'phrasing list -> 'flow;
    h4_elem : attribs -> 'phrasing list -> 'flow;
    h5_elem : attribs -> 'phrasing list -> 'flow;
    h6_elem : attribs -> 'phrasing list -> 'flow;
    ul_elem : attribs -> ('phrasing list * 'flow option * attribs) list -> 'flow;
    ol_elem : attribs -> ('phrasing list * 'flow option * attribs) list -> 'flow;
    dl_elem : attribs -> (bool * 'phrasing list * attribs) list -> 'flow;
    hr_elem : attribs -> 'flow;
    table_elem : attribs ->
      ((bool * attribs * 'phrasing list) list * attribs) list -> 'flow;
    phrasing : 'phrasing_without_interactive -> 'phrasing;
(** The syntax of plugins is [<<name arg1='value1' ... argn="valuen' >>] or
[<<name arg1='value1' ... argn="valuen' |content>> ] *)
(** Must display sthg (error message?) if the name does not exist. *)
    plugin : string -> bool * ('param, 'flow, 'phrasing_without_interactive, 'href) plugin;
    plugin_action :  string -> int -> int -> ('param, unit) plugin_args;
    link_action : string -> string option -> attribs -> int * int -> 'param -> unit;
    error : string -> 'phrasing_without_interactive;
  }

(*
val from_channel :
  'param ->
  ('flow, 'phrasing, 'phrasing_without_interactive, 'param) builder -> in_channel -> 'flow list Lwt.t
*)

val from_string :
  'param ->
  ('flow, 'phrasing, 'phrasing_without_interactive, 'param, 'href) builder -> string -> 'flow list Lwt.t

(*
val from_lexbuf :
  'param ->
  ('flow, 'phrasing, 'phrasing_without_interactive, 'param) builder -> Lexing.lexbuf -> 'flow list Lwt.t
*)
