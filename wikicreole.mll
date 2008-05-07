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
*)

type ('flow, 'inline, 'a, 'pre) builder =
  { chars : string -> 'inline;
    strong_elem : 'inline list -> 'inline;
    em_elem : 'inline list -> 'inline;
    a_elem : string -> 'a list -> 'inline;
    br_elem : unit -> 'inline;
    img_elem : string -> string -> 'inline;
    tt_elem : 'inline list -> 'inline;
    p_elem : 'inline list -> 'flow;
    pre_elem : 'pre list -> 'flow;
    h1_elem : int -> 'inline list -> 'flow;
    h2_elem : int -> 'inline list -> 'flow;
    h3_elem : int -> 'inline list -> 'flow;
    h4_elem : int -> 'inline list -> 'flow;
    h5_elem : int -> 'inline list -> 'flow;
    h6_elem : int -> 'inline list -> 'flow;
    ul_elem : ('inline list * 'flow option) list -> 'flow;
    ol_elem : ('inline list * 'flow option) list -> 'flow;
    hr_elem : unit -> 'flow;
    table_elem : (bool * 'inline list) list list -> 'flow;
    rev_pre : 'inline list -> 'pre list;
    rev_a : 'inline list -> 'a list;
  }

type style = Bold | Italic

type list_kind = Unordered | Ordered

type ('inline, 'flow, 'a, 'pre) stack =
    Style of style * 'inline list * ('inline, 'flow, 'a, 'pre) stack
  | Link of string * 'inline list * ('inline, 'flow, 'a, 'pre) stack
  | Paragraph
  | Heading of int
  | Preformatted
  | List_item of ('inline, 'flow, 'a, 'pre) stack
  | List of
      list_kind * ('inline list * 'flow option) list * ('inline, 'flow, 'a, 'pre) stack
  | Table of (bool * 'inline list) list list
  | Row of (bool * 'inline list) list * 
      ('inline, 'flow, 'a, 'pre) stack
  | Entry of bool * ('inline, 'flow, 'a, 'pre) stack

type ('flow, 'inline, 'a, 'pre) ctx =
  { build : ('flow, 'inline, 'a, 'pre) builder;
    mutable italic : bool;
    mutable bold : bool;
    mutable heading : bool;
    mutable link : bool;
    mutable list_level : int;
    mutable inline : 'inline list;
    mutable list : ('inline list * 'flow option) list;
    mutable flow : 'flow list;
    mutable stack : ('inline, 'flow, 'a, 'pre) stack }

let count c s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
  !n

let push_string c s = c.inline <- c.build.chars s :: c.inline

let push_chars c lexbuf = push_string c (Lexing.lexeme lexbuf)

let get_style c style =
  match style with Bold -> c.bold | Italic -> c.italic

let set_style c style v =
  match style with Bold -> c.bold <- v | Italic -> c.italic <- v

let pop_style c style inline stack =
  let elt =
    match style with
      Bold   -> c.build.strong_elem
    | Italic -> c.build.em_elem
  in
  c.inline <- elt (List.rev c.inline) :: inline;
  c.stack <- stack;
  set_style c style false

let style_change c style =
  if get_style c style then begin
    match c.stack with
      Style (s, inline, stack) when s = style ->
        pop_style c style inline stack
    | _ ->
        push_string c "**"
  end else begin
    c.stack <- Style (style, c.inline, c.stack);
    c.inline <- [];
    set_style c style true
  end

let pop_link c addr inline stack =
  c.inline <- c.build.a_elem addr (c.build.rev_a c.inline) :: inline;
  c.stack <- stack;
  c.link <- false

let close_entry c =
  match c.stack with
    Entry (heading, Row (entries, stack)) ->
      c.stack <- Row ((heading, List.rev c.inline) :: entries, stack);
      c.inline <- [];
      true
  | Row _ | Table _ ->
      true
  | _ ->
      false

let close_row c =
  close_entry c &&
  match c.stack with
    Row (entries, Table rows) ->
      c.stack <- Table (List.rev entries :: rows);
      true
  | Table _ ->
      true
  | _ ->
      assert false

let rec end_paragraph c lev =
  match c.stack with
    Style (style, inline, stack) ->
      pop_style c style inline stack;
      end_paragraph c lev
  | Link (addr, inline, stack) ->
      pop_link c addr inline stack;
      end_paragraph c lev
  | Paragraph ->
      if c.inline <> [] then begin
        c.flow <- c.build.p_elem (List.rev c.inline) :: c.flow;
        c.inline <- []
      end
  | Heading l ->
      let f = 
        match l with
          | 1 -> c.build.h1_elem
          | 2 -> c.build.h2_elem
          | 3 -> c.build.h3_elem
          | 4 -> c.build.h4_elem
          | 5 -> c.build.h5_elem
          | _ -> c.build.h6_elem
      in
      c.flow <- f l (List.rev c.inline) :: c.flow;
      c.inline <- [];
      c.heading <- false;
      c.stack <- Paragraph
  | Preformatted ->
      c.flow <- c.build.pre_elem (c.build.rev_pre c.inline) :: c.flow;
      c.inline <- [];
      c.stack <- Paragraph
  | List_item stack ->
      c.list <- (List.rev c.inline, None) :: c.list;
      c.stack <- stack;
      c.inline <- [];
      end_paragraph c lev
  | List (kind, lst, stack) ->
      if lev < c.list_level then begin
        c.list_level <- c.list_level - 1;
        let elt =
          match kind with
            Unordered -> c.build.ul_elem
          | Ordered   -> c.build.ol_elem
        in
        let cur_lst = elt (List.rev c.list) in
        if c.list_level = 0 then
          c.flow <- cur_lst :: c.flow
        else begin
          match lst with
            (l, None) :: rem -> c.list <- (l, Some cur_lst) :: rem;
          | _                -> assert false
        end;
        c.stack <- stack;
        end_paragraph c lev
      end
  | Entry _ ->
      ignore (close_row c);
      end_paragraph c lev
  | Row _ ->
      assert false
  | Table rows ->
      c.flow <- c.build.table_elem (List.rev rows) :: c.flow;
      c.stack <- Paragraph

let rec correct_kind_rec stack kind n =
  match stack with
    List_item stack ->
      correct_kind_rec stack kind n
  | List (k, lst, stack) ->
      if n = 0 then k = kind else
      correct_kind_rec stack kind (n - 1)
  | Style (_, _, stack) ->
      correct_kind_rec stack kind n
  | Link _ | Heading _ | Paragraph | Preformatted
  | Entry _ | Row _ | Table _ ->
      assert false

let correct_kind c kind lev =
  lev = c.list_level + 1
    ||
  (lev <= c.list_level &&
   correct_kind_rec c.stack kind (c.list_level - lev))

let start_list_item c kind lev =
  let correct = correct_kind c kind lev in
  if lev = 1 || correct then begin
    (* If we have an item of a different kind at level 1, we close the
       previous list and start a new one of the right kind *)
    end_paragraph c (if correct then lev else 0);
    if lev = c.list_level then begin
      c.stack <- List_item c.stack
    end else (* if lev = c.list_level + 1 then *) begin
      c.list_level <- lev;
      c.stack <- List_item (List (kind, c.list, c.stack));
      c.list <- []
    end;
    true
  end else
    false

let start_table_row c heading =
  if not (close_row c) then begin
    end_paragraph c 0;
    c.stack <- Table []
  end;
  c.stack <- Entry (heading, Row ([], c.stack))

}

let line_break = '\n' | '\r' | "\r\n"
let white_space = [ ' ' '\t' ]
(* XXX Should we consider form feed and zero-width space as white
   spaces as well ? *)

let not_line_break = [^ '\n' '\r']
let reserved_chars = [ '*' '/' '\\' '=' '[' ']' '{' '~' '|' 'h' 'f' ]
let punctuation = [ ',' '.' '?' '!' ':' ';' '"' '\'' ]

let first_char = (not_line_break # ['~' '|']) | ('=' +)
let next_chars = not_line_break # reserved_chars

rule parse_bol c =
  parse
    line_break {
      end_paragraph c 0;
      parse_bol c lexbuf
    }
  | white_space * ("=" | "==" | "===" | "====" | "=====" | "======") {
      end_paragraph c 0;
      assert (c.stack = Paragraph);
      c.stack <- Heading (count '=' (Lexing.lexeme lexbuf));
      c.heading <- true;
      parse_rem c lexbuf
    }
  | white_space * "*" + {
      let lev = count '*' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Unordered lev) then begin
        let s = Lexing.lexeme lexbuf in
        let l = String.length s - lev in
        if l > 0 then push_string c (String.sub s 0 l);
        for i = 1 to lev / 2 do
          style_change c Bold
        done;
        if lev land 1 = 1 then push_string c "*"
      end;
      parse_rem c lexbuf
    }
  | white_space * "#" + {
      let lev = count '#' (Lexing.lexeme lexbuf) in
      if not (start_list_item c Ordered lev) then
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | white_space * "----" white_space * (line_break | eof) {
      end_paragraph c 0;
      c.flow <- c.build.hr_elem () :: c.flow;
      parse_bol c lexbuf
    }
  | white_space * "{{{" (line_break | eof) {
      assert (c.stack = Paragraph);
      c.stack <- Preformatted;
      parse_nowiki c lexbuf
    }
  | white_space * "|" {
      start_table_row c false;
      parse_rem c lexbuf
    }
  | white_space * "|=" {
      start_table_row c true;
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
  | "**" {
      style_change c Bold;
      parse_rem c lexbuf
    }
  | "//" {
      style_change c Italic;
      parse_rem c lexbuf
    }
  | "=" + white_space * (line_break | eof) {
      if c.heading then
        end_paragraph c 0
      else
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | "[[" (']' ? (not_line_break # [ ']' '|' ])) + "]]" {
      if c.link then
        push_chars c lexbuf
      else
        let s = Lexing.lexeme lexbuf in
        let addr = String.sub s 2 (String.length s - 4) in
        c.inline <- c.build.a_elem addr (c.build.rev_a [c.build.chars addr]) :: c.inline;
      parse_rem c lexbuf
  }
  | "[[" (']' ? (not_line_break # [ ']' '|' ])) + "|" {
      if c.link then
        push_chars c lexbuf
      else begin
        let s = Lexing.lexeme lexbuf in
        let addr = String.sub s 2 (String.length s - 3) in
        c.stack <- Link (addr, c.inline, c.stack);
        c.inline <- [];
        c.link <- true
      end;
      parse_rem c lexbuf
  }
  | "]]" {
      begin match c.stack with
        Link (addr, inline, stack) ->
          pop_link c addr inline stack
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
        let addr = Lexing.lexeme lexbuf in
        c.inline <- c.build.a_elem addr (c.build.rev_a [c.build.chars addr]) :: c.inline;
      parse_rem c lexbuf
  }
  | "\\\\" {
      c.inline <- c.build.br_elem () :: c.inline;
      parse_rem c lexbuf
    }
  | "{{" (not_line_break # ['|' '{'])  (not_line_break # '|') * '|'
         ('}' ? (not_line_break # '}')) * "}}" {
      let s = Lexing.lexeme lexbuf in
      let i = String.index s '|' in
      let url = String.sub s 2 (i - 2) in
      let alt = String.sub s (i + 1) (String.length s - i - 3) in
      c.inline <- c.build.img_elem url alt :: c.inline;
      parse_rem c lexbuf
    }
  | "{{{" ('}' ? '}' ? (not_line_break # '}')) * '}' * "}}" {
      let s = Lexing.lexeme lexbuf in
      let txt = String.sub s 3 (String.length s - 6) in
      c.inline <- c.build.tt_elem [c.build.chars txt] :: c.inline;
      parse_rem c lexbuf
    }
  | '~' (not_line_break # white_space) {
      let s = Lexing.lexeme lexbuf in
      (* It amounts to the same to quote a UTF-8 char or its first byte *)
      push_string c (String.sub s 1 1);
      parse_rem c lexbuf
    }
  | '|' white_space* (line_break | eof) {
      if not (close_row c) then
        push_chars c lexbuf;
      parse_bol c lexbuf
    }
  | '|' {
      if close_entry c then
        c.stack <- Entry (false, c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | "|=" {
      if close_entry c then
        c.stack <- Entry (true, c.stack)
      else
        push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | first_char next_chars * | '~' {
      push_chars c lexbuf;
      parse_rem c lexbuf
    }
  | _ {
     Format.eprintf "Unrecognized char '%s'@." (Lexing.lexeme lexbuf);
     exit 1
  }
  | eof {
      end_paragraph c 0
    }

and parse_nowiki c =
  parse
    white_space + "}}}" (line_break | eof) {
      let s = Lexing.lexeme lexbuf in
      push_string c (String.sub s 1 (String.length s - 1));
      parse_nowiki c lexbuf
    }
  | ("}}}" (line_break | eof)) | eof {
      end_paragraph c 0;
      parse_bol c lexbuf
    }
  | not_line_break * (line_break | eof) {
      push_chars c lexbuf;
      parse_nowiki c lexbuf
    }

{

let context b =
  { build = b; italic = false; bold = false;
    heading = false; link = false; list_level = 0;
    inline = []; list = []; flow = []; stack = Paragraph }

let parse b lexbuf =
  let c = context b in
  parse_bol c lexbuf;
  List.rev c.flow

let from_channel b ch = parse b (Lexing.from_channel ch)

let from_string b s = parse b (Lexing.from_string s)

}
