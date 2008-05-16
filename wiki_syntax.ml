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

let (>>=) = Lwt.bind

module W = Wikicreole

module H = Hashtbl.Make(struct
                          type t = string
                          let equal = (=)
                          let hash = Hashtbl.hash 
                        end)

let block_extension_table = H.create 8
let inline_extension_table = H.create 8

let add_block_extension k f = H.add block_extension_table k f

let add_inline_extension k f = H.add inline_extension_table k f

let make_string s = Lwt.return (Ocamlduce.Utf8.make s)

let element (c : Xhtmltypes_duce.inlines Lwt.t list) = 
  Lwt_util.map (fun x -> x) c >>= fun c ->
  Lwt.return {{ (map {: c :} with i -> i) }}

let element2 (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) = 
  {{ (map {: c :} with i -> i) }}

let elementt (c : string list) = {{ (map {: c :} with i -> i) }}

let list_builder = function
  | [] -> Lwt.return {{ [ <li>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (c, 
             (l : Xhtmltypes_duce.block Lwt.t option)) =
        element c >>= fun r ->
        (match l with
          | Some v -> v >>= fun v -> Lwt.return [v]
          | None -> Lwt.return []) >>= fun l ->
        Lwt.return
          {{ <li>[ !r
                   !{: l :} ] }}
      in
      f a >>= fun r ->
      Lwt_util.map f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let inline (x : Xhtmltypes_duce.a_content)
    : Xhtmltypes_duce.inlines
    = {{ {: [ x ] :} }}

let builder =
  { W.chars = make_string;
    W.strong_elem = (fun a -> 
                       element a >>= fun r ->
                       Lwt.return {{ [<strong>r ] }});
    W.em_elem = (fun a -> 
                   element a >>= fun r ->
                   Lwt.return {{ [<em>r ] }});
    W.a_elem =
      (fun addr 
         (c : {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t list) -> 
           Lwt_util.map (fun x -> x) c >>= fun c ->
           Lwt.return 
             {{ [ <a href={: Ocamlduce.Utf8.make addr :}>{: element2 c :} ] }});
    W.br_elem = (fun () -> Lwt.return {{ [<br>[]] }});
    W.img_elem =
      (fun addr alt -> 
         Lwt.return 
           {{ [<img
                  src={: Ocamlduce.Utf8.make addr :} 
                  alt={: Ocamlduce.Utf8.make alt :}>[] ] }});
    W.tt_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<tt>r ] }});
    W.p_elem = (fun a -> 
                  element a >>= fun r ->
                  Lwt.return {{ <p>r }});
    W.pre_elem = (fun a ->  Lwt.return {{ <pre>(elementt a) }});
    W.h1_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h1>r }});
    W.h2_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h2>r }});
    W.h3_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h3>r }});
    W.h4_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h4>r }});
    W.h5_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h5>r }});
    W.h6_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ <h6>r }});
    W.ul_elem = (fun a ->
                   list_builder a >>= fun r ->
                   Lwt.return {{ <ul>r }});
    W.ol_elem = (fun a ->
                   list_builder a >>= fun r ->
                   Lwt.return {{ <ol>r }});
    W.hr_elem = (fun () -> Lwt.return {{ <hr>[] }});
    W.table_elem =
      (function 
         | [] -> Lwt.return {{ <p>[] }} (*VVV ??? *)
         | row::rows ->
             let f (h, c) =
               element c >>= fun r ->
               Lwt.return
                 (if h 
                 then {{ <th>r }}
                 else {{ <td>r }})
             in
             let f2 = function
               | [] -> Lwt.return {{ <tr>[<td>[]] }} (*VVV ??? *)
               | a::l -> 
                   f a >>= fun r ->
                   Lwt_util.map f l >>= fun l ->
                   Lwt.return {{ <tr>[ r !{: l :} ] }}
             in
             f2 row >>= fun row ->
             Lwt_util.map f2 rows >>= fun rows ->
             Lwt.return {{ <table>[<tbody>[ row !{: rows :} ] ] }});
    W.inline = (fun x -> x >>= fun x -> Lwt.return x);
    W.block_plugin = H.find block_extension_table;
    W.inline_plugin =
      (fun name param args content -> 
         let f = 
           try H.find inline_extension_table name
           with Not_found -> 
             (fun _ _ _ ->
                Lwt.return
                  {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                  !{: name :} ] ] ] }})
         in f param args content);
(*
      (fun name param args content -> 
         let s = "PLUGIN "^name^"\n"^
           (List.fold_left
              (fun beg (n, v) -> beg^" "^n^"='"^v^"'") "" args)^
           (match content with
              | None -> "" 
              | Some content -> "\n|\n"^content)
         in
         Lwt.return {{ [ <b>{: s :} ] }});
*)
    W.error = (fun s -> Lwt.return {{ [ <b>{: s :} ] }});
  }

let xml_of_wiki ~sp ~sd s = 
  Lwt_util.map (fun x -> x) (Wikicreole.from_string (sp, sd) builder s) 
  >>= fun r ->
  Lwt.return {{ {: r :} }}
