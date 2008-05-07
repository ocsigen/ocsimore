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

module W = Wikicreole

type pre_cont = 
  {{
     (Char | Xhtmltypes_duce.a | Xhtmltypes_duce.fontstyle
     | Xhtmltypes_duce.phrase | Xhtmltypes_duce.special_pre
     | Xhtmltypes_duce.misc_inline
     | Xhtmltypes_duce.inline_forms)
   }}

type pre_content = 
  {{
     [ pre_cont* ]
   }}


let make_string s = Ocamlduce.Utf8.make s

let element (c : Xhtmltypes_duce.inlines list) = {{ (map {: c :} with i -> i) }}

let element2 (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) = {{ (map {: c :} with i -> i) }}

let elementt (c : pre_content list) = {{ (map {: c :} with i -> i) }}

let element4 c = {{ (map {: c :} with i -> i) }}

let list_builder = function
  | [] -> {{ [ <li>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (c, 
             (l : Xhtmltypes_duce.block option)) =
        {{ <li>[ !(element c)  
                   !{: match l with
                       | Some v -> [v]
                       | None -> [] :}  ] }}
      in
      {{ [ (f a) !{: List.map f l :} ] }}

let rev_pre l = 
  let rec aux acc = function 
  | [] -> acc
  | a::l -> match a with
      | {{ [ a::pre_cont* ] }} -> aux (a::acc) l
      | {{ _ }} -> aux acc l (*VVV ??? *)
  in
  aux [] l

let rev_a l =
  let rec aux (acc : {{ [ Xhtmltypes_duce.a_content* ]}} list) = function 
  | [] -> acc
  | a::l -> match a with
      | {{ [ a::Xhtmltypes_duce.a_content* ] }} -> aux (a::acc) l
      | {{ _ }} -> aux acc l (*VVV ??? *)
  in
  aux [] l

let builder =
  { W.chars = make_string;
    W.strong_elem = (fun a -> {{ [<strong>(element a) ] }});
    W.em_elem = (fun a -> {{ [<em>(element a) ] }});
    W.a_elem =
      (fun addr 
         (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) -> 
         {{ [ <a href={: addr :}>{: element2 c :} ] }});
    W.br_elem = (fun () -> {{ [<br>[]] }});
    W.img_elem =
      (fun addr alt -> 
         {{ [<img src={: addr :} alt={: alt :}>[] ] }});
    W.tt_elem = (fun a -> {{ [<tt>(element a) ] }});
    W.p_elem = (fun a -> {{ <p>(element a) }});
    W.pre_elem = (fun a -> {{ <pre>(elementt a) }});
    W.h1_elem = (fun l a -> {{ <h1>(element a) }});
    W.h2_elem = (fun l a -> {{ <h2>(element a) }});
    W.h3_elem = (fun l a -> {{ <h3>(element a) }});
    W.h4_elem = (fun l a -> {{ <h4>(element a) }});
    W.h5_elem = (fun l a -> {{ <h5>(element a) }});
    W.h6_elem = (fun l a -> {{ <h6>(element a) }});
    W.ul_elem = (fun a -> {{ <ul>(list_builder a) }});
    W.ol_elem = (fun a -> {{ <ol>(list_builder a) }});
    W.hr_elem = (fun () -> {{ <hr>[] }});
    W.table_elem =
      (function 
         | [] -> {{ <p>[] }} (*VVV ??? *)
         | row::rows ->
             let f (h, c) =
               if h 
               then {{ <th>(element4 c) }}
               else {{ <td>(element4 c) }}
             in
             let f2 = function
               | [] -> {{ <tr>[<td>[]] }} (*VVV ??? *)
               | a::l -> {{ <tr>[ (f a) !{: List.map f l :} ] }}
             in
             let row = f2 row in
             let rows = List.map f2 rows in
             {{ <table>[<tbody>[ row !{: rows :} ] ] }});
    rev_pre = rev_pre;
    rev_a = rev_a;
  }

let xml_of_wiki s = {{ {: Wikicreole.from_string builder s :} }}
