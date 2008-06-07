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
   Pretty print wiki to OcamlDuce
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

(** Type used to avoid wikibox loops *)
type ancestors = (int32 * int32) list

let in_ancestors box ancestors =
  List.mem box ancestors

let no_ancestors = []

let add_ancestor x a = x::a

let add_block_extension k f = H.add block_extension_table k f

let add_inline_extension k f = H.add inline_extension_table k f

let make_string s = Lwt.return (Ocamlduce.Utf8.make s)

let element (c : Xhtmltypes_duce.inlines Lwt.t list) = 
  Lwt_util.map_serial (fun x -> x) c >>= fun c ->
  Lwt.return {{ (map {: c :} with i -> i) }}

let element2 (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) = 
  {{ (map {: c :} with i -> i) }}

let elementt (c : string list) = {{ (map {: c :} with i -> i) }}

let list_builder = function
  | [] -> Lwt.return {{ [ <li>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (c, 
             (l : Xhtmltypes_duce.flows Lwt.t option)) =
        element c >>= fun r ->
        (match l with
          | Some v -> v >>= fun v -> Lwt.return v
          | None -> Lwt.return {{ [] }}) >>= fun l ->
        Lwt.return
          {{ <li>[ !r
                   !l ] }}
      in
      f a >>= fun r ->
      Lwt_util.map_serial f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let inline (x : Xhtmltypes_duce.a_content)
    : Xhtmltypes_duce.inlines
    = {{ {: [ x ] :} }}

let builder wiki_id =
  { W.chars = make_string;
    W.strong_elem = (fun a -> 
                       element a >>= fun r ->
                       Lwt.return {{ [<strong>r ] }});
    W.em_elem = (fun a -> 
                   element a >>= fun r ->
                   Lwt.return {{ [<em>r] }});
    W.a_elem =
      (fun addr 
         (c : {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t list) -> 
           Lwt_util.map_serial (fun x -> x) c >>= fun c ->
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
    W.nbsp = Lwt.return {{ [ ' ' ] }};
    W.p_elem = (fun a -> 
                  element a >>= fun r ->
                  Lwt.return {{ [<p>r] }});
    W.pre_elem = (fun a ->  Lwt.return {{ [<pre>(elementt a)] }});
    W.h1_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h1>r] }});
    W.h2_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h2>r] }});
    W.h3_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h3>r] }});
    W.h4_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h4>r] }});
    W.h5_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h5>r] }});
    W.h6_elem = (fun a ->
                   element a >>= fun r ->
                   Lwt.return {{ [<h6>r] }});
    W.ul_elem = (fun a ->
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ul>r] }});
    W.ol_elem = (fun a ->
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ol>r] }});
    W.hr_elem = (fun () -> Lwt.return {{ [<hr>[]] }});
    W.table_elem =
      (function 
         | [] -> Lwt.return {{ [] }}
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
                   Lwt_util.map_serial f l >>= fun l ->
                   Lwt.return {{ <tr>[ r !{: l :} ] }}
             in
             f2 row >>= fun row ->
             Lwt_util.map_serial f2 rows >>= fun rows ->
             Lwt.return {{ [<table>[<tbody>[ row !{: rows :} ] ] ] }});
    W.inline = (fun x -> x >>= fun x -> Lwt.return x);
    W.block_plugin = 
      (fun name -> H.find block_extension_table name wiki_id);
    W.inline_plugin =
      (fun name param args content -> 
         let f = 
           try H.find inline_extension_table name
           with Not_found -> 
             (fun _ _ _ _ ->
                Lwt.return
                  {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                  !{: name :} ] ] ] }})
         in 
         f wiki_id param args content);
    W.plugin_action = (fun _ _ _ _ _ _ -> ());
    W.error = (fun s -> Lwt.return {{ [ <b>{: s :} ] }});
  }

let xml_of_wiki ?subbox ~ancestors ~sp ~sd wiki_id s = 
  Lwt_util.map_serial
    (fun x -> x) 
    (Wikicreole.from_string (sp, sd, (subbox, ancestors)) (builder wiki_id) s) 
  >>= fun r ->
  Lwt.return {{ (map {: r :} with i -> i) }}

let string_of_extension name args content =
  "<<"^name^
    (List.fold_left
       (fun beg (n, v) -> beg^" "^n^"='"^v^"'") "" args)^
    (match content with
       | None -> "" 
       | Some content -> "|"^content)^">>"
  
let _ =

  add_block_extension "div"
    (fun w (sp, sd, (subbox, ancestors)) args c -> 
       let content = match c with
         | Some c -> c
         | None -> ""
       in
       xml_of_wiki ?subbox ~ancestors ~sp ~sd w content >>= fun content ->
       let classe = 
         try
           let a = List.assoc "class" args in
           {{ { class={: a :} } }} 
         with Not_found -> {{ {} }} 
       in
       let id = 
         try
           let a = List.assoc "id" args in
           {{ { id={: a :} } }} 
         with Not_found -> {{ {} }} 
       in
       Lwt.return 
         {{ [ <div (classe ++ id) >content ] }}
    );

  Wiki_filter.add_preparser_extension "div"
(*VVV may be done automatically for all extensions with wiki content
  (with an optional parameter of add_block_extension/add_inline_extension?) *)
    (fun w param args -> function
       | None -> Lwt.return None
       | Some c ->
           Wiki_filter.preparse_extension param w c >>= fun c ->
           Lwt.return (Some (string_of_extension "div" args (Some c)))
    )
  ;


  add_block_extension "raw"
    (fun _ (sp, sd, _) args content ->
       let s = string_of_extension "raw" args content in
       Lwt.return {{ [ <p>[ <b>{: s :} ] ] }});

  Wiki_filter.add_preparser_extension "raw"
(*VVV may be done automatically for all extensions with wiki content 
  (with an optional parameter of add_block_extension/add_inline_extension?) *)
    (fun w param args -> function
       | None -> Lwt.return None
       | Some c ->
           Wiki_filter.preparse_extension param w c >>= fun c ->
           Lwt.return (Some (string_of_extension "raw" args (Some c)))
    )
  ;

  add_block_extension "content"
    (fun _ (sp, sd, (subbox, ancestors)) args c -> 
       let classe = 
         try
           let a = List.assoc "class" args in
           {{ { class={: a :} } }} 
         with Not_found -> {{ {} }} 
       in
       let id = 
         try
           let a = List.assoc "id" args in
           {{ { id={: a :} } }} 
         with Not_found -> {{ {} }} 
       in
       match subbox with
         | None -> Lwt.return {{ [ <div (classe ++ id) >
                                     [<strong>[<em>"<<content>>"]]] }}
         | Some subbox -> Lwt.return {{ [ <div (classe ++ id) >subbox ] }}
    );

  add_block_extension "menu"
    (fun _ (sp, sd, _) args c -> 
       let classe = 
         let c =
           "wikimenu"^
             (try
                " "^(List.assoc "class" args)
              with Not_found -> "")
         in {{ { class={: c :} } }} 
       in
       let id = 
         try
           let a = List.assoc "id" args in
           {{ { id={: a :} } }} 
         with Not_found -> {{ {} }} 
       in
       let f ?classe s =
         let link, text = 
           try 
             Ocsigen_lib.sep '|' s 
           with Not_found -> s, s
         in
         let text2 = Ocamlduce.Utf8.make text in
         if Eliom_sessions.get_current_sub_path_string sp = link
         then 
           let classe = match classe with
             | None -> {{ { class="wikimenu_current" } }}
             | Some c -> 
                 let c = Ocamlduce.Utf8.make ("wikimenu_current "^c) in
                 {{ { class=c } }}
           in
           {{ <li (classe)>text2}}
         else
           let link = if link = "" then "./" else link in
           let link2 = Ocamlduce.Utf8.make link in
           let classe = match classe with
             | None -> {{ {} }}
             | Some c -> 
                 let c = Ocamlduce.Utf8.make c in
                 {{ { class=c } }}
           in
           {{ <li (classe)>[<a href=link2>text2]}}
       in
       let rec mapf = function
           | [] -> []
           | [a] -> [f ~classe:"wikimenu_last" a]
           | a::ll -> (f a)::mapf ll
       in
       match
         List.rev
           (List.fold_left
              (fun beg (n, v) -> if n="item" then v::beg else beg)
              [] args)
       with
         | [] -> Lwt.return {: [] :}
         | [a] ->  
             let first = f ~classe:"wikimenu_first wikimenu_last" a in
             Lwt.return {{ [ <ul (classe ++ id) >[ {: first :} ] ] }}
         | a::ll -> 
             let first = f ~classe:"wikimenu_first" a in
             let poi = mapf ll in
             Lwt.return 
               {{ [ <ul (classe ++ id) >[ first !{: poi :} ] ] }}
    );


  add_block_extension "cond"
    (fun w (sp, sd, (subbox, ancestors)) args c -> 
       let content = match c with
         | Some c -> c
         | None -> ""
       in
       (match args with
         | [("ingroup", g)] -> 
             Users.get_user_id_by_name g >>= fun group ->
             Users.in_group ~sp ~sd ~group ()
         | [("notingroup", g)] ->
             Users.get_user_id_by_name g >>= fun group ->
             Users.in_group ~sp ~sd ~group ()
             >>= fun b ->
             Lwt.return (not b)
         | _ -> Lwt.return false)
(*VVV syntax??? How to do a OR or AND?? a THEN? *)
       >>= function
         | true -> xml_of_wiki ?subbox ~ancestors ~sp ~sd w content
         | false -> Lwt.return {{ [] }}
    );

  Wiki_filter.add_preparser_extension "cond"
(*VVV may be done automatically for all extensions with wiki content 
  (with an optional parameter of add_block_extension/add_inline_extension?) *)
    (fun w param args -> function
       | None -> Lwt.return None
       | Some c ->
           Wiki_filter.preparse_extension param w c >>= fun c ->
           Lwt.return (Some (string_of_extension "cond" args (Some c)))
    )
  ;

  add_inline_extension "username"
    (fun w (sp, sd, (subbox, ancestors)) args c -> 
       Users.get_user_data ~sp ~sd >>= fun ud ->
       Lwt.return (Ocamlduce.Utf8.make ud.Users.fullname)
    );


