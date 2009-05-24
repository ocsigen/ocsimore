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
   @author Boris Yakobowski
*)

open Wiki_types
open Wiki_widgets_interface
let (>>=) = Lwt.bind



let string_of_extension name args content =
  "<<"^name^
    (List.fold_left
       (fun beg (n, v) -> beg^" "^n^"='"^v^"'") "" args)^
    (match content with
       | None -> "" 
       | Some content -> "|"^content)^">>"



(***)
let make_string s = Lwt.return (Ocamlduce.Utf8.make s)

let element (c : Xhtmltypes_duce.inlines Lwt.t list) = 
  Lwt_util.map_serial (fun x -> x) c >>= fun c ->
  Lwt.return {{ (map {: c :} with i -> i) }}

let element2 (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) = 
  {{ (map {: c :} with i -> i) }}


(* I don't know how to do a function for 
    try
      let c = Ocamlduce.Utf8.make (List.assoc name attribs) in
      {{ {...=c} }}
    with Not_found -> atts
that typechecks with ocamlduce ... ?
*)
let parse_common_attribs attribs =
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc "class" attribs) in
      {{ {class=c} }}
    with Not_found -> {{ {} }}
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "id" attribs) in
      {{ {id=c}++atts }}
    with Not_found -> atts
  in
  atts

let parse_table_attribs attribs =
  let atts = parse_common_attribs attribs in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "border" attribs) in
      {{ {border=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "cellpadding" attribs) in
      {{ {cellpadding=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "cellspacing" attribs) in
      {{ {cellspacing=c}++atts }}
    with Not_found -> atts
  in
(*  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "frame" attribs) in
      {{ {frame=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "rules" attribs) in
      {{ {rules=c}++atts }}
    with Not_found -> atts
  in*)
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "summary" attribs) in
      {{ {summary=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "width" attribs) in
      {{ {width=c}++atts }}
    with Not_found -> atts
  in
  atts

let parse_valign_attrib attribs : {{ { valign =? Xhtmltypes_duce.valign } }} =
  try
    let c = List.assoc "valign" attribs in
    if c="top"
    then {{ {valign="top"} }}
    else
    if c="middle"
    then {{ {valign="middle"} }}
    else
    if c="bottom"
    then {{ {valign="bottom"} }}
    else
    if c="baseline"
    then {{ {valign="baseline"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_align_attrib attribs : {{ { align =? Xhtmltypes_duce.align } }} =
  try
    let c = List.assoc "align" attribs in
    if c="left"
    then {{ {align="left"} }}
    else
    if c="center"
    then {{ {align="center"} }}
    else
    if c="right"
    then {{ {align="right"} }}
    else
    if c="justify"
    then {{ {align="justify"} }}
    else
    if c="char"
    then {{ {align="char"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_scope_attrib attribs : {{ { scope =? Xhtmltypes_duce.scope } }} =
  try
    let c = List.assoc "scope" attribs in
    if c="row"
    then {{ {scope="row"} }}
    else
    if c="col"
    then {{ {scope="col"} }}
    else
    if c="rowgroup"
    then {{ {scope="rowgroup"} }}
    else
    if c="colgroup"
    then {{ {scope="colgroup"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_table_row_attribs attribs : Xhtmltypes_duce.align_attrs =
  let atts = parse_common_attribs attribs in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "char" attribs) in
      {{ {char=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "charoff" attribs) in
      {{ {charoff=c}++atts }}
    with Not_found -> atts
  in
  let atts2 = parse_valign_attrib attribs in
  let atts3 = parse_align_attrib attribs in
  {{ atts2++atts3++atts }}

let parse_table_cell_attribs attribs =
  let atts = parse_common_attribs attribs in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "abbr" attribs) in
      {{ {abbr=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "axis" attribs) in
      {{ {axis=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "char" attribs) in
      {{ {char=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "charoff" attribs) in
      {{ {charoff=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "colspan" attribs) in
      {{ {colspan=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "headers" attribs) in
      {{ {headers=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "rowspan" attribs) in
      {{ {rowspan=c}++atts }}
    with Not_found -> atts
  in
  let atts2 = parse_valign_attrib attribs in
  let atts3 = parse_align_attrib attribs in
  let atts4 = parse_scope_attrib attribs in
  {{ atts++atts2++atts3++atts4 }}

let list_builder = function
  | [] -> Lwt.return {{ [ <li>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (c, 
             (l : Xhtmltypes_duce.flows Lwt.t option),
             attribs) =
        let atts = parse_common_attribs attribs in
        element c >>= fun r ->
        (match l with
          | Some v -> v >>= fun v -> Lwt.return v
          | None -> Lwt.return {{ [] }}) >>= fun l ->
        Lwt.return
          {{ <li (atts)>[ !r
                   !l ] }}
      in
      f a >>= fun r ->
      Lwt_util.map_serial f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let descr_builder = function
  | [] -> Lwt.return {{ [ <dt>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (istitle, d, attribs) =
        let atts = parse_common_attribs attribs in
        element d >>= fun d ->
        if istitle
        then Lwt.return {{ <dt (atts)>[ !d ] }}
        else Lwt.return {{ <dd (atts)>[ !d ] }}
      in
      f a >>= fun r ->
      Lwt_util.map_serial f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let inline (x : Xhtmltypes_duce.a_content)
    : Xhtmltypes_duce.inlines
    = {{ {: [ x ] :} }}

let absolute_link_regexp = Netstring_pcre.regexp "[a-z|A-Z|0-9]+:"

let is_absolute_link addr = 
  (Netstring_pcre.string_match absolute_link_regexp addr 0) <> None




type syntax_extension =
    (Wiki_widgets_interface.box_info,
     Xhtmltypes_duce.flows Lwt.t,
     Eliom_duce.Blocks.a_content_elt_list Lwt.t)
   Wikicreole.plugin


type 'a plugin_hash = (string, 'a) Hashtbl.t

type wikicreole_parser = {
  builder: (Xhtmltypes_duce.flows Lwt.t,
            Xhtmltypes_duce.inlines Lwt.t,
            {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t,
            box_info,
            Eliom_sessions.server_params)
    Wikicreole.builder;

  plugin_assoc: (bool * syntax_extension) plugin_hash;

  plugin_action_assoc:
     ((Eliom_sessions.server_params * Wiki_types.wikibox,
       string option Lwt.t)
        Wikicreole.plugin_args)
    plugin_hash;
}

let copy_parser wp = {
  wp with
    plugin_assoc = Hashtbl.copy wp.plugin_assoc;
    plugin_action_assoc = Hashtbl.copy wp.plugin_action_assoc;
}

let builder_from_builder_ext bext =
  { bext.builder with
      Wikicreole.plugin =
      (fun name ->
         try Hashtbl.find bext.plugin_assoc name
         with Not_found ->
           (false,
            (fun _ _ _ ->
               Wikicreole.A_content
                 (Lwt.return
                    {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                    !{: name :} ] ] ] }})))
      );
  }





let add_preparser_extension ~wp ~name f =
  Hashtbl.add wp.plugin_action_assoc name f

let nothing _ _ = ()
let nothing1 _ = ()

let make_plugin_action wp =
  let subst = ref [] in
  ((fun name start end_ params args content ->
      subst := (start,
                end_,
                (try
                   Hashtbl.find wp.plugin_action_assoc name params args content
                 with Not_found -> Lwt.return None))::!subst)
   ,
   fun () -> !subst
  )

let builder wp plugin_action =
  { Wikicreole.chars = nothing1;
    strong_elem = nothing;
    em_elem = nothing;
    a_elem = (fun _ _ _ _ -> ());
    make_href = (fun _ _ a -> a);
    br_elem = nothing1;
    img_elem = (fun _ _ _ -> ());
    tt_elem = nothing;
    monospace_elem = nothing;
    underlined_elem = nothing;
    linethrough_elem = nothing;
    subscripted_elem = nothing;
    superscripted_elem = nothing;
    nbsp = ();
    p_elem = nothing;
    pre_elem = nothing;
    h1_elem = nothing;
    h2_elem = nothing;
    h3_elem = nothing;
    h4_elem = nothing;
    h5_elem = nothing;
    h6_elem = nothing;
    ul_elem = nothing;
    ol_elem = nothing;
    dl_elem = nothing;
    hr_elem = nothing1;
    table_elem = nothing;
    inline = nothing1;
    plugin =
      (fun name -> 
         let wiki_content =
           try fst (Hashtbl.find wp.plugin_assoc name)
           with Not_found -> false
         in (wiki_content, (fun _ _ _ -> Wikicreole.A_content ())));
    plugin_action = plugin_action;
    error = nothing1;
  }

let preparse_extension
    wp (sp, wb : Eliom_sessions.server_params * Wiki_types.wikibox)
    content =
  let (plugin_action, get_subst) = make_plugin_action wp in
  let builder = builder wp plugin_action in
  ignore (Wikicreole.from_string sp (sp, wb) builder content);
  let buf = Buffer.create 1024 in
  Lwt_util.fold_left
    (fun pos (start, end_, replacement) -> 
       replacement >>= function
         | None -> Lwt.return pos;
         | Some replacement ->
             Buffer.add_substring buf content pos (start - pos);
             Buffer.add_string buf replacement;
             Lwt.return end_
    )
    0
    (List.rev (get_subst ()))
  >>= fun pos ->
  let l = String.length content in
  if pos < l 
  then Buffer.add_substring buf content pos (l - pos);
  Lwt.return (Buffer.contents buf)


let default_builder =
  { Wikicreole.chars = make_string;
    strong_elem = (fun attribs a -> 
                       let atts = parse_common_attribs attribs in
                       element a >>= fun r ->
                       Lwt.return {{ [<strong (atts)>r ] }});
    em_elem = (fun attribs a -> 
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<em (atts)>r] }});
    monospace_elem = (fun attribs a -> 
                          let atts = parse_common_attribs attribs in
                          element a >>= fun r ->
                            Lwt.return {{ [<tt (atts)>r] }});
    underlined_elem = (fun attribs a -> 
                           let atts = parse_common_attribs attribs in
                           element a >>= fun r ->
                           Lwt.return {{ [<span ({class="underlined"} ++
                                             atts)>r] }});
    linethrough_elem = (fun attribs a -> 
                           let atts = parse_common_attribs attribs in
                           element a >>= fun r ->
                           Lwt.return {{ [<span ({class="linethrough"} ++
                                             atts)>r] }});
    subscripted_elem = (fun attribs a -> 
                            let atts = parse_common_attribs attribs in
                            element a >>= fun r ->
                              Lwt.return {{ [<sub (atts)>r] }});
    superscripted_elem = (fun attribs a -> 
                              let atts = parse_common_attribs attribs in
                              element a >>= fun r ->
                                Lwt.return {{ [<sup (atts)>r] }});
    a_elem =
      (fun attribs _sp addr 
         (c : {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t list) -> 
           let atts = parse_common_attribs attribs in
           Lwt_util.map_serial (fun x -> x) c >>= fun c ->
           Lwt.return
             {{ [ <a ({href={: Ocamlduce.Utf8.make addr :}}++atts)>{: element2 c :} ] }});
    make_href =
      (fun sp bi addr ->
         let wiki_id = bi.Wiki_widgets_interface.bi_root_wiki in
         let servpage = Wiki_widgets_interface.find_servpage wiki_id in
         match servpage, is_absolute_link addr with
           | (Some servpage, false) ->
               let addr =
                 Ocsigen_lib.remove_slash_at_beginning (Neturl.split_path addr)
               in
               Eliom_predefmod.Xhtml.make_string_uri servpage sp addr
           | _ -> addr);
    br_elem = (fun attribs -> 
                   let atts = parse_common_attribs attribs in
                   Lwt.return {{ [<br (atts)>[]] }});
    img_elem =
      (fun attribs addr alt -> 
         let atts = parse_common_attribs attribs in
         Lwt.return 
           {{ [<img
                  ({src={: Ocamlduce.Utf8.make addr :} 
                    alt={: Ocamlduce.Utf8.make alt :}}
                   ++
                    atts)
                  >[] ] }});
    tt_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<tt (atts)>r ] }});
    nbsp = Lwt.return {{" "}};
    p_elem = (fun attribs a -> 
                  let atts = parse_common_attribs attribs in
                  element a >>= fun r ->
                  Lwt.return {{ [<p (atts)>r] }});
    pre_elem = (fun attribs a ->
       let atts = parse_common_attribs attribs in
       Lwt.return
         {{ [<pre (atts)>{:Ocamlduce.Utf8.make (String.concat "" a):}] }});
    h1_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h1 (atts)>r] }});
    h2_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h2 (atts)>r] }});
    h3_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h3 (atts)>r] }});
    h4_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h4 (atts)>r] }});
    h5_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h5 (atts)>r] }});
    h6_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h6 (atts)>r] }});
    ul_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ul (atts)>r] }});
    ol_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ol (atts)>r] }});
    dl_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   descr_builder a >>= fun r ->
                   Lwt.return {{ [<dl (atts)>r] }});
    hr_elem = (fun attribs -> 
                   let atts = parse_common_attribs attribs in
                   Lwt.return {{ [<hr (atts)>[]] }});
    table_elem =
      (fun attribs l ->
         let atts = parse_table_attribs attribs in
         match l with
         | [] -> Lwt.return {{ [] }}
         | row::rows ->
             let f (h, attribs, c) =
               let atts = parse_table_cell_attribs attribs in
               element c >>= fun r ->
               Lwt.return
                 (if h 
                 then {{ <th (atts)>r }}
                 else {{ <td (atts)>r }})
             in
             let f2 (row, attribs) = match row with
               | [] -> Lwt.return {{ <tr>[<td>[]] }} (*VVV ??? *)
               | a::l -> 
                   let atts = parse_table_row_attribs attribs in
                   f a >>= fun r ->
                   Lwt_util.map_serial f l >>= fun l ->
                   Lwt.return {{ <tr (atts)>[ r !{: l :} ] }}
             in
             f2 row >>= fun row ->
             Lwt_util.map_serial f2 rows >>= fun rows ->
             Lwt.return {{ [<table (atts)>[<tbody>[ row !{: rows :} ] ] ] }});
    inline = (fun x -> x >>= fun x -> Lwt.return x);
    plugin = (fun name ->
                (false,
                 (fun _ _ _ ->
                    Wikicreole.A_content 
                      (Lwt.return
                         {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                         !{: name :} ] ] ] }})))
      );
    plugin_action = (fun _ _ _ _ _ _ -> ());
    error = (fun s -> Lwt.return {{ [ <b>{: s :} ] }});
  }

let wikicreole_parser = {
  builder = default_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
}

let xml_of_wiki wp bi s = 
  Lwt_util.map_serial
    (fun x -> x)
    (Wikicreole.from_string bi.Wiki_widgets_interface.bi_sp bi
       (builder_from_builder_ext wp) s)
  >>= fun r ->
  Lwt.return {{ (map {: r :} with i -> i) }}

let wikicreole_content_type = 
  Wiki_models.register_wiki_parser "wikicreole" 
    (preparse_extension wikicreole_parser)
    (xml_of_wiki wikicreole_parser)

let inline_of_wiki builder bi s : Xhtmltypes_duce.inlines Lwt.t =
  match Wikicreole.from_string bi.Wiki_widgets_interface.bi_sp bi
    (builder_from_builder_ext builder) s
  with
    | [] -> Lwt.return {{ [] }}
    | a::_ -> a >>= (function
                       | {{ [ <p>l _* ] }} -> Lwt.return l
(*VVV What can I do with trailing data? *)
                       | {{ _ }} -> Lwt.return {{ [ <b>"error" ] }})

let a_content_of_wiki builder bi s =
  inline_of_wiki builder bi s >>= fun r ->
  Lwt.return
    {{ map r with
         |  <a (Xhtmltypes_duce.a_attrs)>l -> l
         | p -> [p] }}




(*********************************************************************)

let add_extension ~wp ~name ?(wiki_content=true) f =
  Hashtbl.add wp.plugin_assoc name (wiki_content, f);
  if wiki_content then
    Hashtbl.add wp.plugin_action_assoc name
      (fun (sp, wb) args -> function
         | None -> Lwt.return None
         | Some c ->
             preparse_extension wp (sp, wb) c >>= fun c ->
             Lwt.return (Some (string_of_extension name args (Some c)))
      )


let () =
  let add_extension_aux = add_extension ~wp:wikicreole_parser in

  add_extension_aux ~name:"div" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.Block 
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          xml_of_wiki wikicreole_parser bi content >>= fun content ->
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
            {{ [ <div (classe ++ id) >content ] }})
    );


  add_extension_aux ~name:"span" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.A_content
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          inline_of_wiki wikicreole_parser bi content >>= fun content ->
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
            {{ [ <span (classe ++ id) >content ] }}
         )
    );

  add_extension_aux ~name:"wikiname" ~wiki_content:true
    (fun bi _ _ ->
       Wikicreole.A_content
         (let wid = fst bi.Wiki_widgets_interface.bi_box in
          Wiki_sql.get_wiki_info_by_id wid
          >>= fun wiki_info ->
          Lwt.return {{ {: Ocamlduce.Utf8.make (wiki_info.wiki_descr) :} }})
    );


  add_extension_aux ~name:"raw" ~wiki_content:false
    (fun _ args content ->
       Wikicreole.A_content
         (let s = string_of_extension "raw" args content in
          Lwt.return {{ [ <b>{: s :} ] }}));

  add_extension_aux ~name:"" ~wiki_content:false
    (fun _ _ _ ->
       Wikicreole.A_content (Lwt.return {{ [] }})
    );

  add_extension_aux ~name:"content"
    (fun bi args _ -> 
       Wikicreole.Block
         (let classe = 
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
          match bi.Wiki_widgets_interface.bi_subbox with
            | None -> Lwt.return {{ [ <div (classe ++ id) >
                                        [<strong>[<em>"<<content>>"]]] }}
            | Some subbox -> Lwt.return {{ [ <div (classe ++ id) >subbox ] }}
         )
    );

  add_extension_aux ~name:"menu"
    (fun bi args _ ->
       let wiki_id = fst bi.Wiki_widgets_interface.bi_box in
       Wikicreole.Block
         (let classe = 
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
            Wiki_sql.get_wiki_info_by_id wiki_id >>= fun wiki_info ->
            a_content_of_wiki wikicreole_parser bi text >>= fun text2 ->
            let b = 
              match wiki_info.Wiki_types.wiki_pages with
                | Some dir ->
                    Eliom_sessions.get_current_sub_path_string
                      bi.Wiki_widgets_interface.bi_sp = dir^"/"^link
                | None -> false
            in
            if b
            then 
              let classe = match classe with
                | None -> {{ { class="wikimenu_current" } }}
                | Some c -> 
                    let c = Ocamlduce.Utf8.make ("wikimenu_current "^c) in
                    {{ { class=c } }}
              in
              Lwt.return {{ <li (classe)>text2}}
            else
              let href = 
                if is_absolute_link link
                then link
                else 
                  match Wiki_widgets_interface.find_servpage
                    bi.bi_root_wiki with
                    | Some servpage -> 
                        let path =
                          Ocsigen_lib.remove_slash_at_beginning
                            (Neturl.split_path link)
                        in
                        Eliom_duce.Xhtml.make_uri
                          ~service:servpage
                          ~sp:bi.Wiki_widgets_interface.bi_sp
                          path
                    | _ -> link
              in
              let link2 = Ocamlduce.Utf8.make href in
              let classe = match classe with
                | None -> {{ {} }}
                | Some c -> 
                    let c = Ocamlduce.Utf8.make c in
                    {{ { class=c } }}
              in
              Lwt.return {{ <li (classe)>[<a href=link2>text2]}}
          in
          let rec mapf = function
            | [] -> Lwt.return []
            | [a] -> f ~classe:"wikimenu_last" a >>= fun b -> Lwt.return [b]
              | a::ll -> f a >>= fun b -> mapf ll >>= fun l -> Lwt.return (b::l)
          in
          match
            List.rev
              (List.fold_left
                 (fun beg (n, v) -> if n="item" then v::beg else beg)
                 [] args)
          with
            | [] -> Lwt.return {: [] :}
            | [a] ->  
                f ~classe:"wikimenu_first wikimenu_last" a >>= fun first ->
                  Lwt.return {{ [ <ul (classe ++ id) >[ {: first :} ] ] }}
                | a::ll -> 
                    f ~classe:"wikimenu_first" a >>= fun first ->
                      mapf ll >>= fun poi ->
                        Lwt.return 
                          {{ [ <ul (classe ++ id) >[ first !{: poi :} ] ] }}
         )
    );


  add_extension_aux ~name:"cond" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.Block
         (let sp = bi.Wiki_widgets_interface.bi_sp in
          let content = match c with
            | Some c -> c
            | None -> ""
          in
          (let rec eval_cond = function
             | ("error", "autherror") ->
                 Lwt.return
                   (List.exists
                      (fun e -> e = Users.BadPassword || e = Users.BadUser)
                      (Ocsimore_common.get_exn ~sp))
             | ("ingroup", g) ->
                 Lwt.catch
                   (fun () ->
                      Users.get_user_by_name g >>= fun group ->
                      Users.in_group ~sp ~group ())
                   (function _ -> Lwt.return false)
             | ("http_code", "404") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp =
                     Wiki_widgets_interface.Page_404)
             | ("http_code", "403") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp =
                     Wiki_widgets_interface.Page_403)
             | ("http_code", "40?") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp <>
                               Wiki_widgets_interface.Page_displayable)
             | (err, value) when String.length err >= 3 &&
                 String.sub err 0 3 = "not" ->
                 let cond' = (String.sub err 3 (String.length err - 3), value) in
                 eval_cond cond' >>=
                   fun b -> Lwt.return (not b)
                   | _ -> Lwt.return false
           in
           (match args with
              | [c] -> eval_cond c
              | _ -> Lwt.return false)
         >>= function
           | true -> xml_of_wiki wikicreole_parser bi content
           | false -> Lwt.return {{ [] }}
          )
         )
    );

