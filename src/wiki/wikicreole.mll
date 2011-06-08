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
    plugin : string -> bool * ('param, 'flow, 'phrasing_without_interactive, 'href) plugin;
    plugin_action :  string -> int -> int -> ('param, unit) plugin_args;
    link_action : string -> string option -> attribs -> int * int -> 'param -> unit;
    error : string -> 'phrasing_without_interactive;
  }

type style =
    Bold | Italic | Underlined | Linethrough |
        Monospace | Superscripted | Subscripted

type list_kind = Unordered | Ordered

type ('phrasing, 'flow, 'href) stack =
    Style of style * 'phrasing list * attribs * ('phrasing, 'flow, 'href) stack
  | Link of 'href * attribs * ('phrasing, 'flow, 'href) stack
      (* Not that we do not save anything in the case of links, as
         links cannot be nested *)
  | Paragraph of attribs
  | Heading of int * attribs
  | List_item of attribs * ('phrasing, 'flow, 'href) stack
  | List of
      list_kind * ('phrasing list * 'flow option * attribs) list
      * attribs * ('phrasing, 'flow, 'href) stack
  | Descr_def of attribs * ('phrasing, 'flow, 'href) stack
  | Descr_title of attribs * ('phrasing, 'flow, 'href) stack
  | Descr of attribs * ('phrasing, 'flow, 'href) stack
  | Table of ((bool * attribs * 'phrasing list) list * attribs) list * attribs
  | Row of (bool * attribs * 'phrasing list) list * attribs * ('phrasing, 'flow, 'href) stack
  | Entry of bool * attribs * ('phrasing, 'flow, 'href) stack

type ('flow, 'phrasing, 'phrasing_without_interactive, 'param, 'href) ctx =
  { build : ('flow, 'phrasing, 'phrasing_without_interactive, 'param, 'href) builder;
    param : 'param;
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
    mutable phrasing_mix : 'phrasing list;
    mutable link_content : 'phrasing_without_interactive list;
    mutable pre_content : string list;
    mutable list : ('phrasing list * 'flow option * attribs) list;
    mutable descr : (bool * 'phrasing list * attribs) list;
    mutable flow : 'flow list;
    mutable stack : ('phrasing, 'flow, 'href) stack }

let count c s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
  !n

let push c v =
  match c.stack with
    Link _ -> c.link_content <- v :: c.link_content
  | _      -> c.phrasing_mix <- c.build.phrasing v :: c.phrasing_mix

let push_string c s = push c (c.build.chars s)

let push_chars c lexbuf = push_string c (Lexing.lexeme lexbuf)

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
      | Bold   -> c.build.strong_elem attribs
      | Italic -> c.build.em_elem attribs
      | Monospace -> c.build.monospace_elem attribs
      | Underlined -> c.build.underlined_elem attribs
      | Linethrough -> c.build.linethrough_elem attribs
      | Subscripted -> c.build.subscripted_elem attribs
      | Superscripted -> c.build.superscripted_elem attribs
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
    c.build.a_elem attribs addr (List.rev c.link_content) :: c.phrasing_mix;
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
        c.flow <- c.build.p_elem attribs (List.rev c.phrasing_mix) :: c.flow;
        c.phrasing_mix <- []
      end;
      c.stack <- Paragraph []
  | Heading (l, attribs) ->
      let f =
        match l with
          | 1 -> c.build.h1_elem
          | 2 -> c.build.h2_elem
          | 3 -> c.build.h3_elem
          | 4 -> c.build.h4_elem
          | 5 -> c.build.h5_elem
          | _ -> c.build.h6_elem
      in
      c.flow <- f attribs (List.rev c.phrasing_mix) :: c.flow;
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
            Unordered -> c.build.ul_elem
          | Ordered   -> c.build.ol_elem
        in
        let cur_lst = elt attribs (List.rev c.list) in
        if c.list_level = 0 then
          c.flow <- cur_lst :: c.flow
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
      let lst = c.build.dl_elem attribs (List.rev c.descr) in
      c.flow <- lst :: c.flow;
      c.stack <- stack;
      end_paragraph c lev
  | Entry _ ->
      ignore (close_row c);
      end_paragraph c lev
  | Row _ ->
      assert false
  | Table (rows, attribs) ->
      c.flow <- c.build.table_elem attribs (List.rev rows) :: c.flow;
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


rule parse_bol c =
  parse
    line_break {
      end_paragraph c 0;
      parse_bol c lexbuf
    }
  | white_space * ("=" | "==" | "===" | "====" | "=====" | "======") 
      (("@@" ?) as att) {
      end_paragraph c 0;
      assert (match c.stack with Paragraph _ -> true | _ -> false);
      let l = count '=' (Lexing.lexeme lexbuf) in
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
      c.flow <- c.build.hr_elem 
        (read_attribs att parse_attribs c lexbuf) :: c.flow;
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
  | ("http:" | "ftp:") (not_line_break # white_space) *
    (not_line_break # white_space # punctuation) {
      if c.link then
        push_chars c lexbuf
      else
	let addr_string = Lexing.lexeme lexbuf in
	let addr = c.build.make_href c.param addr_string None in
        c.phrasing_mix <-
          c.build.a_elem [] addr [c.build.chars addr_string] :: c.phrasing_mix;
      parse_rem c lexbuf
  }
  | "\\\\" (("@@" ?) as att) {
      push c (c.build.br_elem (read_attribs att parse_attribs c lexbuf));
      parse_rem c lexbuf
    }
  | "{{" (("@@" ?) as att)
      { parse_image c (read_attribs att parse_attribs c lexbuf) lexbuf }
  | "<<" ((not_line_break # white_space) # ['|' '>']) * {
      let s = Lexing.lexeme lexbuf in
      let l = String.length s in
      let name = String.sub s 2 (l - 2) in
      let start = Lexing.lexeme_start lexbuf in
      let (wiki_content, ext_info) = c.build.plugin name in
      let content, args =
        parse_extension start name wiki_content [] c lexbuf in
      c.build.plugin_action name start (Lexing.lexeme_end lexbuf)
        c.param (List.rev args) content;
      match ext_info c.param args content with
      | Phrasing_without_interactive i ->
          push c i;
          parse_rem c lexbuf
      | Link_plugin (addr, attribs, content) ->
          c.link_content <- [ content ];
          pop_link c addr attribs c.stack;
          parse_rem c lexbuf
      | Flow5 b ->
          end_paragraph c 0;
          c.flow <- b :: c.flow;
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
      push c c.build.nbsp;
      parse_rem c lexbuf
    }
  | "--" {
      push c c.build.endash;
      parse_rem c lexbuf
    }
  | "---" {
      push c c.build.emdash;
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
      end_paragraph c 0
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
        c.build.link_action begaddr fragment attribs
          (beg+2, Lexing.lexeme_start lexbuf) c.param;
        let addr = c.build.make_href c.param begaddr fragment in
        if lb = "|"
        then begin
          c.stack <- Link (addr, attribs, c.stack);
          c.link <- true
        end
        else
          let text = match fragment with
            | None -> begaddr
            | Some f -> begaddr^"#"^f
          in
          c.phrasing_mix <-
            c.build.a_elem
            attribs addr [c.build.chars text] :: c.phrasing_mix;
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


and parse_image c attribs =
    parse
     (not_line_break # ['|' '{']) (not_line_break # '|') * '|'
         ('}' ? (not_line_break # '}')) * "}}" {
      let s = Lexing.lexeme lexbuf in
      let i = String.index s '|' in
      let url = c.build.make_href c.param (String.sub s 0 i) None in
      let alt = String.sub s (i + 1) (String.length s - i - 3) in
      push c (c.build.img_elem attribs url alt);
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
      push c (c.build.tt_elem attribs [c.build.phrasing (c.build.chars txt)]);
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
      c.flow <- c.build.pre_elem attribs (List.rev c.pre_content) :: c.flow;
      c.pre_content <- [];
      parse_bol c lexbuf
    }
  | not_line_break * (line_break | eof) {
      c.pre_content <- Lexing.lexeme lexbuf :: c.pre_content;
      parse_nowiki c attribs lexbuf
    }

and parse_extension start name wiki_content args c =
    parse
    | '|' {
        if wiki_content
        then ((parse_extension_content_wiki start 0 false "" c lexbuf), args)
        else ((parse_extension_content_nowiki start true "" c lexbuf), args)
      }
    | (">>" | eof) {
        (None, args)
      }
    |  ';'* | (white_space *) | (line_break *) {
        parse_extension start name wiki_content args c lexbuf
      }
    | (not_line_break # white_space # '=' # '>') * '='
        ((white_space | line_break) *) (('\'' | '"') as quote) {
        let s = Lexing.lexeme lexbuf in
        let i = String.index s '=' in
        let arg_name = String.sub s 0 i in
        let arg_value = parse_arg_value quote "" c lexbuf in
        parse_extension start name wiki_content
          ((arg_name, arg_value)::args) c lexbuf
      }
    | _ {
        ignore
          (if wiki_content
           then ((parse_extension_content_wiki start 0 false "" c lexbuf), args)
           else
             ((parse_extension_content_nowiki start true "" c lexbuf), args));
        (Some ("Syntax error in extension "^name), args)
      }

and parse_extension_content_wiki start lev nowiki beg c =
    parse
      | '~' (('<' | '>' | '~') as ch) {
          parse_extension_content_wiki
            start lev nowiki (beg^"~"^(String.make 1 ch)) c lexbuf
        }
      | "<<" ((not_line_break # white_space) # ['|' '>']) * {
          let s = Lexing.lexeme lexbuf in
          if nowiki
          then parse_extension_content_wiki start lev nowiki (beg^s) c lexbuf
          else
            let l = String.length s in
            let name = String.sub s 2 (l - 2) in
            let (wiki_content, _) = c.build.plugin name in
            if wiki_content
            then
              parse_extension_content_wiki
                start (lev+1) false (beg^s) c lexbuf
            else
              let s =
                match
                  parse_extension_content_nowiki start false (beg^s) c lexbuf
                with None -> ">>"
                  | Some s -> s^">>"
              in
              parse_extension_content_wiki start lev false s c lexbuf
        }
      | "{{{" {
          parse_extension_content_wiki
            start lev true (beg^"{{{") c lexbuf
        }
      | "}}}" {
(*VVV Warning: not quotable! *)
          parse_extension_content_wiki start lev false (beg^"}}}") c lexbuf
        }
      | ">>" {
          if nowiki
          then
            parse_extension_content_wiki start lev nowiki (beg^">>") c lexbuf
          else
            if lev>0
            then
              parse_extension_content_wiki
                start (lev-1) nowiki (beg^">>") c lexbuf
            else Some beg
        }
      | eof {
          Some (beg^" syntax error in wikisyntax") (* or error ?? *)
        }
      | [^ '~' '>' '<' '{' '}' ]+ {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start lev nowiki (beg^s) c lexbuf
        }
      | [ '>' '<' '~' '{' '}' ] {
          let s = Lexing.lexeme lexbuf in
          parse_extension_content_wiki start lev nowiki (beg^s) c lexbuf
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

let context param b =
  { build = b; 
    param = param;
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
    flow = [];
    stack = Paragraph [] }

(*
let from_lexbuf param b lexbuf =
  Lwt_preemptive.detach
    (fun () ->
       let c = context param b in
       parse_bol c lexbuf;
       List.rev c.flow)
    ()

let from_channel param b ch = from_lexbuf param b (Lexing.from_channel ch)
*)

let from_lexbuf_no_preempt param b lexbuf =
  let c = context param b in
  parse_bol c lexbuf;
  Lwt.return (List.rev c.flow)

let from_string param b s = from_lexbuf_no_preempt param b (Lexing.from_string s)

}
