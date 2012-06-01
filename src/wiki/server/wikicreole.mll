{
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
*)


exception Eof
exception Unrecognized_char

type attribs = (string * string) list

type style =
    Bold | Italic | Underlined | Linethrough |
        Monospace | Superscripted | Subscripted

type list_kind = Unordered | Ordered


(* module type BuilderType = sig *)


(* end *)

module type RawBuilder = sig

  type href
  type param
  type phrasing_without_interactive
  type phrasing
  type flow
  type flow_without_interactive
  type uo_list
  (* module BuilderType : BuilderType *)
  (* open BuilderType *)

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
  val string_of_href : href -> string
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

(* REMOVE TODO *)
let resolve_plugin (Resolver f) name = f name

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
  val href_action :
    string -> string option -> attribs -> int * int -> param -> unit

end

module Make(B : Builder) = struct

  type stack =
    | Style of style * B.phrasing list * attribs * stack
    | Link of B.href * attribs * stack
  (* Not that we do not save anything in the case of links, as
     links cannot be nested *)
    | Paragraph of attribs
    | Heading of int * attribs
    | List_item of attribs * stack
    | List of
	list_kind * (B.phrasing list * B.uo_list option * attribs) list
      * attribs * stack
    | Descr_def of attribs * stack
    | Descr_title of attribs * stack
    | Descr of attribs * stack
    | Table of ((bool * attribs * B.phrasing list) list * attribs) list * attribs
    | Row of (bool * attribs * B.phrasing list) list * attribs * stack
    | Entry of bool * attribs * stack

  type ctx =
      { param : B.param;
	mutable italic : bool;
	mutable bold : bool;
	mutable monospace : bool;
	mutable underlined : bool;
	mutable linethrough : bool;
	mutable subscripted : bool;
	mutable superscripted : bool;
	mutable heading : bool;
	mutable link : bool;
	mutable list_level : int;
	mutable phrasing_mix : B.phrasing list;
	mutable link_content : B.phrasing_without_interactive list;
	mutable pre_content : string list;
	mutable list : (B.phrasing list * B.uo_list option * attribs) list;
	mutable descr : (bool * B.phrasing list * attribs) list;
	mutable flow : (int * B.flow list) list;
	mutable stack : stack;
	mutable sectioning: bool;
      }

  let count c s =
    let n = ref 0 in
    for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
    !n

  let push c v =
    match c.stack with
      | Link _ -> c.link_content <- v :: c.link_content
      | _      -> c.phrasing_mix <- B.phrasing v :: c.phrasing_mix

let push_string c s = push c (B.chars s)

let push_chars c lexbuf = push_string c (Lexing.lexeme lexbuf)

let push_flow c e = match c.flow with
  | (lvl, flow) :: flows -> c.flow <- (lvl, e :: flow) :: flows
  | [] -> assert false

let rec end_section c lvl =
  match c.flow with
  | (lvl', flow) :: flows when lvl <= lvl' ->
      c.flow <- flows;
      push_flow c (B.flow (B.section_elem [(* TODO attribs ?? *)] (List.rev flow)));
      end_section c lvl
  | _ -> ()

let get_section_lvl c = match c.flow with
  | (lvl, _) :: _ -> lvl
  | [] -> assert false

let push_section c lvl =
  assert (get_section_lvl c < lvl);
  if c.sectioning then c.flow <- (lvl,[]) :: c.flow

let read_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 1 [] [] c lexbuf with
      | [] -> []
      | a::_ -> a
    else []
  with Eof -> [] (*VVV ??? *)

let read_list_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 2 [] [] c lexbuf with
      | [] -> ([], [])
      | a::b::_ -> (b, a)
      | [a] -> ([], a)
    else ([], [])
  with Eof -> ([], []) (*VVV ??? *)

let read_table_attribs att parse_attribs c lexbuf =
  try
    if att = "@@"
    then match parse_attribs 3 [] [] c lexbuf with
      | [] -> ([], [], [])
      | a::b::c::_ -> (c, b, a)
      | [a; b] -> ([], b, a)
      | [a] -> ([], [], a)
    else ([], [], [])
  with Eof -> ([], [], []) (*VVV ??? *)

let get_style c style =
  match style with
    | Bold -> c.bold
    | Italic -> c.italic
    | Monospace -> c.monospace
    | Underlined -> c.underlined
    | Linethrough -> c.linethrough
    | Subscripted -> c.subscripted
    | Superscripted -> c.superscripted

let set_style c style v =
  match style with
    | Bold -> c.bold <- v
    | Italic -> c.italic <- v
    | Monospace -> c.monospace <- v
    | Underlined -> c.underlined <- v
    | Linethrough -> c.linethrough <- v
    | Subscripted -> c.subscripted <- v
    | Superscripted -> c.superscripted <- v

let pop_style c style phrasing attribs stack =
  let elt =
    match style with
      | Bold   -> B.strong_elem attribs
      | Italic -> B.em_elem attribs
      | Monospace -> B.monospace_elem attribs
      | Underlined -> B.underlined_elem attribs
      | Linethrough -> B.linethrough_elem attribs
      | Subscripted -> B.subscripted_elem attribs
      | Superscripted -> B.superscripted_elem attribs
  in
  let phrasing' = c.phrasing_mix in
  c.stack <- stack;
  c.phrasing_mix <- phrasing;
  push c (elt (List.rev phrasing'));
  set_style c style false

let style_change c style att parse_attribs lexbuf =
  let atts = read_attribs att parse_attribs c lexbuf in
  if get_style c style then begin
    match c.stack with
      Style (s, phrasing, attribs, stack) when s = style ->
        pop_style c style phrasing attribs stack;
    | _ ->
        push_string c "**";
        push_string c att
  end else begin
    c.stack <- Style (style, c.phrasing_mix, atts, c.stack);
    c.phrasing_mix <- [];
    set_style c style true
  end

let pop_link c addr attribs stack =
  c.stack <- stack;
  c.phrasing_mix <-
    B.a_elem_phrasing attribs addr (List.rev c.link_content) :: c.phrasing_mix;
  c.link_content <- [];
  c.link <- false

let close_entry c =
  match c.stack with
    Entry (heading, attribs, Row (entries, row_attribs, stack)) ->
      c.stack <- Row ((heading, attribs, List.rev c.phrasing_mix) :: entries,
                      row_attribs,
                      stack);
      c.phrasing_mix <- [];
      true
  | Row _ ->
      true
  | Table _ ->
      (*VVV attribs? *)
      c.stack <- Row ([(false, [], List.rev c.phrasing_mix)], [], c.stack);
      c.phrasing_mix <- [];
      true
  | _ ->
      false

let close_row c =
  close_entry c &&
  match c.stack with
    Row (entries, row_attribs, Table (rows, table_attribs)) ->
      c.stack <- Table (((List.rev entries, row_attribs) :: rows),
                        table_attribs);
      true
  | Table _ ->
      true
  | _ ->
      assert false

let close_descr_entry c =
  match c.stack with
    | Descr_def (attribs, stack) ->
        c.stack <- stack;
        c.descr <- (false, List.rev c.phrasing_mix, attribs) :: c.descr;
        c.phrasing_mix <- [];
        true
    | Descr_title (attribs, stack) ->
        c.stack <- stack;
        c.descr <- (true, List.rev c.phrasing_mix, attribs) :: c.descr;
        c.phrasing_mix <- [];
        true
    | _ ->
        false


let rec end_paragraph c lev =
  match c.stack with
    Style (style, phrasing, attribs, stack) ->
      pop_style c style phrasing attribs stack;
      end_paragraph c lev
  | Link (addr, attribs, stack) ->
      pop_link c addr attribs stack;
      end_paragraph c lev
  | Paragraph attribs ->
      if c.phrasing_mix <> [] then begin
        push_flow c (B.flow (B.p_elem attribs (List.rev c.phrasing_mix)));
        c.phrasing_mix <- []
      end;
      c.stack <- Paragraph []
  | Heading (l, attribs) ->
      let f =
        match l with
          | 1 -> B.h1_elem
          | 2 -> B.h2_elem
          | 3 -> B.h3_elem
          | 4 -> B.h4_elem
          | 5 -> B.h5_elem
          | _ -> B.h6_elem
      in
      push_flow c (B.flow (f attribs (List.rev c.phrasing_mix)));
      c.phrasing_mix <- [];
      c.heading <- false;
      c.stack <- Paragraph []
  | List_item (attribs, stack) ->
      c.list <- (List.rev c.phrasing_mix, None, attribs) :: c.list;
      c.stack <- stack;
      c.phrasing_mix <- [];
      end_paragraph c lev
  | List (kind, lst, attribs, stack) ->
      if lev < c.list_level then begin
        c.list_level <- c.list_level - 1;
        let elt =
          match kind with
            Unordered -> B.ul_elem
          | Ordered   -> B.ol_elem
        in
        let cur_lst = elt attribs (List.rev c.list) in
        if c.list_level = 0 then
          push_flow c (B.flow (B.list cur_lst))
        else begin
          match lst with
            (l, None, attribs) :: rem ->
              c.list <- (l, Some cur_lst, attribs) :: rem;
          | _                -> assert false
        end;
        c.stack <- stack;
        end_paragraph c lev
      end
  | Descr_def (attribs, stack) ->
      c.descr <- (false, List.rev c.phrasing_mix, attribs) :: c.descr;
      c.stack <- stack;
      c.phrasing_mix <- [];
      end_paragraph c lev
  | Descr_title (attribs, stack) ->
      c.descr <- (true, List.rev c.phrasing_mix, attribs) :: c.descr;
      c.stack <- stack;
      c.phrasing_mix <- [];
      end_paragraph c lev
  | Descr (attribs, stack) ->
      push_flow c (B.flow (B.dl_elem attribs (List.rev c.descr)));
      c.stack <- stack;
      end_paragraph c lev
  | Entry _ ->
      ignore (close_row c);
      end_paragraph c lev
  | Row _ ->
      assert false
  | Table (rows, attribs) ->
      push_flow c (B.flow (B.table_elem attribs (List.rev rows)));
      c.stack <- Paragraph []

let rec correct_kind_rec stack kind n =
  match stack with
    List_item (_, stack) ->
      correct_kind_rec stack kind n
  | List (k, _lst, _, stack) ->
      if n = 0 then k = kind else
      correct_kind_rec stack kind (n - 1)
  | Style (_, _, _, stack) ->
      correct_kind_rec stack kind n
  | Link _ | Heading _ | Paragraph _ | Entry _ | Row _ | Table _
  | Descr _ | Descr_title _ | Descr_def _ ->
      assert false

let correct_kind c kind lev =
  lev = c.list_level + 1
    ||
  (lev <= c.list_level &&
   correct_kind_rec c.stack kind (c.list_level - lev))

let start_list_item c kind lev att parse_attribs lexbuf =
  let correct = correct_kind c kind lev in
  if lev = 1 || correct then begin
    (* If we have an item of a different kind at level 1, we close the
       previous list and start a new one of the right kind *)
    end_paragraph c (if correct then lev else 0);
    let (list_attribs, item_attribs) =
      read_list_attribs att parse_attribs c lexbuf
    in
    if lev = c.list_level then begin
      c.stack <- List_item (item_attribs, c.stack)
    end else (* if lev = c.list_level + 1 then *) begin
      c.list_level <- lev;
      c.stack <- List_item (item_attribs,
                            List (kind, c.list, list_attribs, c.stack));
      c.list <- []
    end;
    true
  end else
    false

let start_table_row c heading (table_attribs, row_attribs, entry_attribs) =
  if not (close_row c) then begin
    end_paragraph c 0;
    c.stack <- Table ([], table_attribs)
  end;
  c.stack <- Entry (heading,
                    entry_attribs,
                    Row ([], row_attribs, c.stack))

}

let line_break = '\n' | '\r' | "\r\n"
let white_space = [ ' ' '\t' ]
(* XXX Should we consider form feed and zero-width space as white
   spaces as well ? *)

let not_line_break = [^ '\n' '\r']
let reserved_chars = [ '*' '/' '\\' '=' '[' ']' '{' '~' '|' 'h' 'f' '<' '-' '#' '_' '^' ',' ';' ':' ]
let punctuation = [ ',' '.' '?' '!' ':' ';' '"' '\'' ]

let first_char = (not_line_break # ['~' '|']) | ('=' +)
let next_chars = not_line_break # reserved_chars

(* begin of line *)
rule parse_bol c =
  parse
    line_break {
      end_paragraph c 0;
      parse_bol c lexbuf
    }
  | white_space * ("=" | "==" | "===" | "====" | "=====" | "======")
      (("@@" ?) as att) {
      end_paragraph c 0;
      let l = count '=' (Lexing.lexeme lexbuf) in
      end_section c l;
      push_section c l;
      assert (match c.stack with Paragraph _ -> true | _ -> false);
      c.stack <- Heading (l, read_attribs att parse_attribs c lexbuf);
      c.heading <- true;
      parse_rem c lexbuf
    }
  | white_space * "*" + (("@@" ?) as att) {
      let lev = count '*' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Unordered lev att parse_attribs lexbuf)
      then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.index s '*' in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 - 1 do
          style_change c Bold "" parse_attribs lexbuf
        done;
        if lev land 1 = 1
        then begin
          style_change c Bold "" parse_attribs lexbuf;
          push_string c "*";
          push_string c att;
        end
        else
          style_change c Bold att parse_attribs lexbuf
      end;
      parse_rem c lexbuf
    }
  | white_space * "#" + (("@@" ?) as att) {
      let lev = count '#' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Ordered lev att parse_attribs lexbuf)
      then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.index s '#' in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 - 1 do
          style_change c Monospace "" parse_attribs lexbuf
        done;
        if lev land 1 = 1
        then begin
          style_change c Monospace "" parse_attribs lexbuf;
          push_string c "#";
          push_string c att;
        end
        else
          style_change c Bold att parse_attribs lexbuf
      end;
      parse_rem c lexbuf
    }
  | white_space * ";" (("@@" ?) as att) {
      let (list_attribs, item_attribs) =
        read_list_attribs att parse_attribs c lexbuf
      in
      if close_descr_entry c
      then c.stack <- Descr_title (item_attribs, c.stack)
      else begin
        end_paragraph c 0;
        c.stack <- Descr_title (item_attribs, Descr (list_attribs, c.stack));
        c.descr <- []
      end;
      parse_rem c lexbuf
    }
  | white_space * ":" (("@@" ?) as att) {
      let (list_attribs, item_attribs) =
        read_list_attribs att parse_attribs c lexbuf
      in
      if close_descr_entry c
      then c.stack <- Descr_def (item_attribs, c.stack)
      else begin
        end_paragraph c 0;
        c.stack <- Descr_def (item_attribs, Descr (list_attribs, c.stack));
        c.descr <- []
      end;
      parse_rem c lexbuf
    }
  | white_space * "----" (("@@" ?) as att) white_space * (line_break | eof) {
      end_paragraph c 0;
      push_flow c
	(B.flow
	   (B.hr_elem
              (read_attribs att parse_attribs c lexbuf)));
      parse_bol c lexbuf
    }
  | white_space * "{{{" (("@@" ?) as att) (line_break | eof) {
      end_paragraph c 0;
      parse_nowiki c (read_attribs att parse_attribs c lexbuf) lexbuf
    }
  | white_space * "|" (("@@" ?) as att) {
      start_table_row c false
        (read_table_attribs att parse_attribs c lexbuf);
      parse_rem c lexbuf
    }
  | white_space * "|=" (("@@" ?) as att) {
      start_table_row c true
        (read_table_attribs att parse_attribs c lexbuf);
      parse_rem c lexbuf
    }
  | white_space * (("@@" ?) as att) {
      let attribs = read_attribs att parse_attribs c lexbuf in
      if attribs <> []
      then
	(match c.stack with
           | Paragraph _ -> c.stack <- Paragraph attribs
           | _ -> ());
      parse_rem c lexbuf
    }
  | "" {
      parse_rem c lexbuf
    }

and parse_rem c =
  parse
    line_break {
      (* Headings are single lines *)
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | "**" (("@@" ?) as att) {
      style_change c Bold att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "//" (("@@" ?) as att) {
      style_change c Italic att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "##" (("@@" ?) as att) {
      style_change c Monospace att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "^^" (("@@" ?) as att) {
      style_change c Superscripted att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | ",," (("@@" ?) as att) {
      style_change c Subscripted att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "__" (("@@" ?) as att) {
      style_change c Underlined att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "/-" (("@@" ?) as att) {
      style_change c Linethrough att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "-/" (("@@" ?) as att) {
      style_change c Linethrough att parse_attribs lexbuf;
      parse_rem c lexbuf
    }
  | "=" + white_space * (line_break | eof) {
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | "[[" (("@@" ?) as att)
      { parse_link (Lexing.lexeme_start lexbuf) "" None c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | "]]" {
      begin match c.stack with
        Link (addr, attribs, stack) ->
          pop_link c addr attribs stack
      | _ ->
          push_chars c lexbuf
      end;
      parse_rem c lexbuf
    }
  | ("http:" | "https:" | "ftp:") (not_line_break # white_space) *
    (not_line_break # white_space # punctuation) {
      if c.link then
        push_chars c lexbuf
      else
	let addr_string = Lexing.lexeme lexbuf in
	let addr = B.make_href c.param addr_string None in
        c.phrasing_mix <-
          B.a_elem_phrasing [] addr [B.chars addr_string] :: c.phrasing_mix;
      parse_rem c lexbuf
  }
  | "\\\\" (("@@" ?) as att) {
      push c (B.br_elem (read_attribs att parse_attribs c lexbuf));
      parse_rem c lexbuf
    }
  | "{{" (("@@" ?) as att)
      { parse_image (Lexing.lexeme_start lexbuf) c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | "<<" ((not_line_break # white_space) # ['|' '>']) * {
      let s = Lexing.lexeme lexbuf in
      let l = String.length s in
      let name = String.sub s 2 (l - 2) in
      let start = Lexing.lexeme_start lexbuf in
      let (plugin_resolver, plugin_builder) = B.plugin name in
      let content, args =
        parse_extension start name plugin_resolver [] c lexbuf in
      B.plugin_action name start (Lexing.lexeme_end lexbuf)
        c.param (List.rev args) content;
      match plugin_builder c.param args content with
      | `Phrasing_without_interactive i ->
          push c i;
          parse_rem c lexbuf
      | `Phrasing_link (addr, attribs, content) ->
          c.link_content <- [ content ];
          pop_link c addr attribs c.stack;
          parse_rem c lexbuf
      | `Flow5 b ->
          end_paragraph c 0;
          push_flow c b;
          parse_bol c lexbuf
      | `Flow5_link (addr, attribs, content) ->
          end_paragraph c 0;
	  push_flow c (B.a_elem_flow attribs addr [content]);
          parse_bol c lexbuf
    }
  | "{{{" (("@@" ?) as att)
      { parse_tt c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | '~' (not_line_break # white_space) {
      let s = Lexing.lexeme lexbuf in
      (* It amounts to the same to quote a UTF-8 char or its first byte *)
      push_string c (String.sub s 1 1);
      parse_rem c lexbuf
    }
  | "~ " {
      push c B.nbsp;
      parse_rem c lexbuf
    }
  | "--" {
      push c B.endash;
      parse_rem c lexbuf
    }
  | "---" {
      push c B.emdash;
      parse_rem c lexbuf
    }
  | '|' white_space* (line_break | eof) {
      (match c.stack with
         | Entry _ | Row _ | Table _ -> ()
             (* we skip ending | if in table row *)
         | _ -> push_chars c lexbuf);
      parse_bol c lexbuf
    }
  | '|' (("@@" ?) as att) {
      if close_entry c then
        c.stack <- Entry (false,
                          read_attribs att parse_attribs c lexbuf,
                          c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | "|=" (("@@" ?) as att) {
      if close_entry c then
        c.stack <- Entry (true,
                          read_attribs att parse_attribs c lexbuf,
                          c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | first_char next_chars * | '~' {
      push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | _ {
     Ocsigen_messages.warning
       ("Wikicreole: Unrecognized char "^(Lexing.lexeme lexbuf)^".");
     raise Unrecognized_char
  }
  | eof {
      end_paragraph c 0;
      end_section c 1
    }

and parse_link beg begaddr fragment c attribs =
    parse
  | "" (* Must be before the case ]?(...)* !! *) {
      push_string c "[["; (*VVV We loose attributes *)
      push_string c begaddr;
      (match fragment with
         | None -> ()
         | Some f ->
             push_string c "#";
             push_string c f);
      parse_rem c lexbuf
    }
  | (']' ? (not_line_break # [ ']' '|' '~' '#' ])) *
          { match fragment with
              | None ->
                  parse_link beg (begaddr^Lexing.lexeme lexbuf) None c attribs lexbuf
              | Some f ->
                  parse_link beg begaddr (Some (f^Lexing.lexeme lexbuf)) c attribs lexbuf
          }
  | "]]" | '|' {
      let lb = Lexing.lexeme lexbuf in
      if c.link then begin
        push_string c "[["; (*VVV We loose attributes *)
        push_string c begaddr;
        (match fragment with
          | None -> ()
          | Some f ->
              push_string c "#";
              push_string c f);
        push_string c lb
      end
      else begin
        B.href_action begaddr fragment attribs
          (beg+2, Lexing.lexeme_start lexbuf) c.param;
        B.link_action begaddr fragment attribs
          (beg+2, Lexing.lexeme_start lexbuf) c.param;
        let addr = B.make_href c.param begaddr fragment in
        if lb = "|"
        then begin
          c.stack <- Link (addr, attribs, c.stack);
          c.link <- true
        end
        else
          c.phrasing_mix <-
            B.a_elem_phrasing attribs addr [B.chars (B.string_of_href addr)]
              :: c.phrasing_mix;
      end;
      parse_rem c lexbuf
  }
  | '~' not_line_break {
      let s = Lexing.lexeme lexbuf in
      let char = String.sub s 1 1 in
      let char =
        if char = "]" || char = "|" || char = "#" || char = "~"
        then char
        else "~"^char
      in
      (* It amounts to the same to quote a UTF-8 char or its first byte *)
       match fragment with
         | None -> parse_link beg (begaddr^char) fragment c attribs lexbuf
         | Some f -> parse_link beg begaddr (Some (f^char)) c attribs lexbuf
    }
  | '#' { let begaddr = match fragment with
            | None -> begaddr
            | Some f -> begaddr^"#"^f
          in parse_link beg begaddr (Some "") c attribs lexbuf }


and parse_image beg c attribs =
    parse
     (not_line_break # ['|' '{']) (not_line_break # '|') * '|'
         ('}' ? (not_line_break # '}')) * "}}" {
      let s = Lexing.lexeme lexbuf in
      let i = String.index s '|' in
      let addr = String.sub s 0 i in
      let url = B.make_href c.param addr None in
      let alt = String.sub s (i + 1) (String.length s - i - 3) in
      (let begaddr, fragment =
         try
           let i = String.index addr '#' in
           String.sub addr 0 i, Some (String.sub addr (succ i) (String.length addr - i))
         with Not_found ->
           addr, None
       in
       let beg = beg + 2 in
       let end_ = beg + String.length addr in
       B.href_action begaddr fragment attribs (beg, end_) c.param);
      push c (B.img_elem attribs url alt);
      parse_rem c lexbuf
    }
  | "" {
      push_string c "{{"; (*VVV We loose attributes *)
      parse_rem c lexbuf
    }

and parse_tt c attribs =
    parse
        ('}' ? '}' ? (not_line_break # '}')) * '}' * "}}}" {
      let s = Lexing.lexeme lexbuf in
      let txt = String.sub s 0 (String.length s - 3) in
      push c (B.tt_elem attribs [B.phrasing (B.chars txt)]);
      parse_rem c lexbuf
    }
  | "" {
      push_string c "{{{"; (*VVV We loose attributes *)
      parse_rem c lexbuf
    }

and parse_nowiki c attribs =
  parse
    white_space + "}}}" (line_break | eof) {
      let s = Lexing.lexeme lexbuf in
      c.pre_content <- String.sub s 1 (String.length s - 1) :: c.pre_content;
      parse_nowiki c attribs lexbuf
    }
  | ("}}}" (line_break | eof)) | eof {
      push_flow c (B.flow (B.pre_elem attribs (List.rev c.pre_content)));
      c.pre_content <- [];
      parse_bol c lexbuf
    }
  | not_line_break * (line_break | eof) {
      c.pre_content <- Lexing.lexeme lexbuf :: c.pre_content;
      parse_nowiki c attribs lexbuf
    }

and parse_extension start name rec_plugin args c =
    parse
    | '|' {
        match rec_plugin with
        | None ->
            ((parse_extension_content_nowiki start true "" c lexbuf), args)
        | Some rec_plugin ->
            ((parse_extension_content_wiki start rec_plugin [] "" c lexbuf), args)
      }
    | (">>" | eof) {
        (None, args)
      }
    |  ';'* | (white_space *) | (line_break *) {
        parse_extension start name rec_plugin args c lexbuf
      }
    | (not_line_break # white_space # '=' # '>') * '='
        ((white_space | line_break) *) (('\'' | '"') as quote) {
        let s = Lexing.lexeme lexbuf in
        let i = String.index s '=' in
        let arg_name = String.sub s 0 i in
        let arg_value = parse_arg_value quote "" c lexbuf in
        parse_extension start name rec_plugin
          ((arg_name, arg_value)::args) c lexbuf
      }
    | _ {
        ignore
          (match rec_plugin with
           | None ->
             ((parse_extension_content_nowiki start true "" c lexbuf), args)
           | Some rec_plugin ->
             ((parse_extension_content_wiki start rec_plugin [] "" c lexbuf), args));
      (Some ("Syntax error in extension "^name), args)
      }

and parse_extension_content_wiki_nowiki str =
  parse
    | "}}}" {
      str^Lexing.lexeme lexbuf
    }
    | _ {
      let s = Lexing.lexeme lexbuf in
      parse_extension_content_wiki_nowiki (str^s) lexbuf
    }

and parse_extension_content_wiki start rec_plugin lev beg c =
    parse
      | '~' (('<' | '>' | '~') as ch) {
          parse_extension_content_wiki
            start rec_plugin lev (beg^"~"^(String.make 1 ch)) c lexbuf
        }
      | "<<" ((not_line_break # white_space) # ['|' '>']) * {
          let s = Lexing.lexeme lexbuf in
          let l = String.length s in
          let name = String.sub s 2 (l - 2) in
          match resolve_plugin rec_plugin name with
          | Some rec_plugin' ->
              parse_extension_content_wiki
                start rec_plugin' (rec_plugin::lev) (beg^s) c lexbuf
          | None ->
              let s =
                parse_extension_content_nowiki start false (beg^s) c lexbuf
              in
              let s = match s with
                | None -> ">>"
                | Some s -> s^">>"
              in
              parse_extension_content_wiki start rec_plugin lev s c lexbuf
        }
      | "{{{" {
        let s = Lexing.lexeme lexbuf in
        let nowiki = parse_extension_content_wiki_nowiki "" lexbuf in
        parse_extension_content_wiki start rec_plugin lev (beg^s^nowiki) c lexbuf
        }
      | "}}}" {
(*VVV Warning: not quotable! *)
          parse_extension_content_wiki start rec_plugin lev (beg^"}}}") c lexbuf
        }
      | ">>" {
          match lev with
          | [] -> Some beg
          | rec_plugin :: lev ->
              parse_extension_content_wiki
                start rec_plugin lev (beg^">>") c lexbuf
        }
      | eof {
          Some (beg^" syntax error in wikisyntax") (* or error ?? *)
        }
      | [^ '~' '>' '<' '{' '}' ]+ {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start rec_plugin lev (beg^s) c lexbuf
        }
      | [ '>' '<' '~' '{' '}' ] {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start rec_plugin lev (beg^s) c lexbuf
        }
      | _ {
          Ocsigen_messages.warning
            ("Wikicreole: Unrecognized char in extension: "^
               (Lexing.lexeme lexbuf)^".");
          raise Unrecognized_char
        }

and parse_extension_content_nowiki start unquote beg c =
    parse
      | "~>>" {
          let s = if unquote then ">>" else "~>>" in
          parse_extension_content_nowiki start unquote (beg^s) c lexbuf
        }
      | (">>" | eof) {
          Some beg
        }
      | [^ '~' '>' ]+ {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_nowiki start unquote (beg^s) c lexbuf
        }
      | [ '>' '~' ] {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_nowiki start unquote (beg^s) c lexbuf
        }
      | _ {
          Ocsigen_messages.warning
            ("Wikicreole: Unrecognized char in extension "^
               (Lexing.lexeme lexbuf)^".");
          raise Unrecognized_char
        }


and parse_arg_value quote beg c =
    parse
      eof | '~' eof {
        raise Eof
      }
    | '~' ('\'' | '"' | '~' as ch) {
        parse_arg_value quote (beg^(String.make 1 ch)) c lexbuf
      }
    | ('\'' | '"' | '~') as ch {
        if ch = quote
        then beg
        else parse_arg_value quote (beg^(String.make 1 ch)) c lexbuf
      }
    | [^ '~' '\'' '"' ] * {
        let s = Lexing.lexeme lexbuf in
        parse_arg_value quote (beg^s) c lexbuf
      }

and parse_attribs depth args oldargs c =
    parse
     "" {
        args::oldargs
      }
    | ';'* | (white_space *) | (line_break *) {
        parse_attribs depth args oldargs c lexbuf
      }
    | (not_line_break # white_space # '=' # '@') * '='
          ((white_space | line_break) *) (('\'' | '"') as quote) {
            let s = Lexing.lexeme lexbuf in
            let i = String.index s '=' in
            let arg_name = String.sub s 0 i in
            let arg_value = parse_arg_value quote "" c lexbuf in
            parse_attribs depth ((arg_name, arg_value)::args) oldargs c lexbuf
          }
    | "@@" { args::oldargs }
    | "@" {
        if depth > 1
        then parse_attribs (depth - 1) [] (args::oldargs) c lexbuf
        else args::oldargs
      }
    | "@@@" { []::args::oldargs }
    | "@@@@" { []::[]::args::oldargs }
    | eof {
        raise Eof
      }


{

let context sectioning param =
  { param;
    italic = false;
    bold = false;
    monospace = false;
    underlined = false;
    linethrough = false;
    subscripted = false;
    superscripted = false;
    heading = false;
    link = false;
    list_level = 0;
    phrasing_mix = [];
    link_content = [];
    pre_content = [];
    list = [];
    descr = [];
    flow = [(0, [])];
    stack = Paragraph [];
    sectioning;
  }

let from_lexbuf_no_preempt sectioning param lexbuf =
  let c = context sectioning param in
  parse_bol c lexbuf;
  match c.flow with
  | [(0, flow)] -> List.rev flow
  | _ -> assert false

end

type ('param, 'res) builder =
    (module Builder with type param = 'param and type flow = 'res)

let from_string (type param) (type flow) ?(sectioning = false) param builder s =
  let module B = (val builder : Builder with type param = param and type flow = flow) in
  let module Wikicreole = Make(B) in
  Wikicreole.from_lexbuf_no_preempt sectioning param (Lexing.from_string s)

}
