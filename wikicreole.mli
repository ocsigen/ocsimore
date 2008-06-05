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
*)


type ('b, 'i) ext_kind

type ('flow, 'inline, 'a_content, 'param) builder =
  { chars : string -> 'a_content;
    strong_elem : 'inline list -> 'a_content;
    em_elem : 'inline list -> 'a_content;
    br_elem : unit -> 'a_content;
    img_elem : string -> string -> 'a_content;
    tt_elem : 'inline list -> 'a_content;
    nbsp : 'a_content;
    a_elem : string -> 'a_content list -> 'inline;
    p_elem : 'inline list -> 'flow;
    pre_elem : string list -> 'flow;
    h1_elem : 'inline list -> 'flow;
    h2_elem : 'inline list -> 'flow;
    h3_elem : 'inline list -> 'flow;
    h4_elem : 'inline list -> 'flow;
    h5_elem : 'inline list -> 'flow;
    h6_elem : 'inline list -> 'flow;
    ul_elem : ('inline list * 'flow option) list -> 'flow;
    ol_elem : ('inline list * 'flow option) list -> 'flow;
    hr_elem : unit -> 'flow;
    table_elem : (bool * 'inline list) list list -> 'flow;
    inline : 'a_content -> 'inline;
    inline_plugin : 
      string ->
      'param -> (string * string) list -> string option -> 'a_content;
(** Syntax of plugins is [<<name arg1='value1' ... argn="valuen' >>] or
[<<name arg1='value1' ... argn="valuen' | content >> ] *)
    block_plugin : 
      string ->
     'param -> (string * string) list -> 
                  string option -> 'flow;
    (** Must raise [Not_found] if the name does not exist.
        In that case, will try [inline_plugin].
    *)
    plugin_action : 
      string -> int -> int -> 
      'param -> (string * string) list -> string option -> unit;
    error : string -> 'a_content;
 }

val from_channel :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> in_channel -> 'flow list
val from_string :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> string -> 'flow list
val from_lexbuf :
  'param ->
  ('flow, 'inline, 'a_content, 'param) builder -> Lexing.lexbuf -> 'flow list
