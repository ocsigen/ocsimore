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
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


let class_wikibox wb = Printf.sprintf "wikiboxcontent%s" (string_of_wikibox wb)


let string_of_extension name args content =
    "<<"^name^" "
  ^ (String.concat " " (List.map (fun (n, v) -> n^"='"^v^"'") args))
  ^ (match content with | None -> "" | Some content -> "|"^content)
  ^ ">>"

let opt_list = function | [] -> None | _::_ as l -> Some l


(***)
let element c = (* : 'a Lwt.t list Lwt.t -> 'a list Lwt.t *)
  let rec aux = function
    | [] -> Lwt.return []
    | x::xs -> x      >>= fun y ->
               aux xs >|= fun ys ->
               y::ys
  in aux c

let length_of_string s = (*FIXME?*)
  let re = Netstring_pcre.regexp "(\\d+) *(px|%)" in
  match Netstring_pcre.string_match re s 0 with
    | None -> failwith "Not a valid length identifier"
    | Some r -> match Netstring_pcre.matched_group r 1 s with
        | "%" -> `Percent (int_of_string (Netstring_pcre.matched_group r 0 s))
        | "px" -> `Pixels (int_of_string (Netstring_pcre.matched_group r 0 s))
        | _ -> (assert false)


let rec filter_raw = function (*/!\ NOT TAIL REC /!\*)
  | [] -> []
  | None :: xs -> filter_raw xs
  | Some x :: xs -> x :: filter_raw xs

let apply_opt f = function
  | None -> None
  | Some x -> Some (f x)

let unopt ~def = function
  | None -> def
  | Some x -> x

let unopt_string s = unopt ~def:"" s


let parse_common_attribs attribs =
  let at1 =
    try Some (XHTML.M.a_class [List.assoc "class" attribs])
    with Not_found -> None
  and at2 =
    try Some (XHTML.M.a_id (List.assoc "id" attribs))
    with Not_found -> None
  in
  filter_raw [at1; at2]

let parse_table_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try Some (XHTML.M.a_border (int_of_string (List.assoc "border" attribs)))
    with Not_found | Failure _ -> None
  and at2 =
    try Some (XHTML.M.a_cellpadding (length_of_string (List.assoc "cellpadding" attribs)))
    with Not_found | Failure _ -> None
  and at3 =
    try Some (XHTML.M.a_cellspacing (length_of_string (List.assoc "cellspacing" attribs)))
    with Not_found | Failure _ -> None
  and at4 =
    try Some (XHTML.M.a_summary (List.assoc "summary" attribs))
    with Not_found -> None
(*and at5 =
    try Some (XHTML.M.a_width (length_of_string (List.assoc "width" attribs)))
    with Not_found | Failure _ -> None*)
  in
  atts @ filter_raw [at1; at2; at3; at4;]

let parse_valign_attrib attribs =
  try
    Some (XHTML.M.a_valign (match List.assoc "valign" attribs with
                              |"top" -> `Top
                              |"middle" -> `Middle
                              |"bottom" -> `Bottom
                              |"baseline" -> `Baseline
                              | _ -> raise Not_found
    ))
  with Not_found -> None

let parse_align_attrib attribs =
  try
    Some (XHTML.M.a_align (match List.assoc "align" attribs with
                              |"left" -> `Left
                              |"right" -> `Right
                              |"center" -> `Center
                              |"justify" -> `Justify
                              |"char" -> `Char
                              | _ -> raise Not_found
    ))
  with Not_found -> None

let parse_scope_attrib attribs =
  try
    Some (XHTML.M.a_scope (match List.assoc "scope" attribs with
                             |"row" -> `Row
                             |"col" -> `Col
                             |"rowgroup" -> `Rowgroup
                             |"colgroup" -> `Colgroup
                             | _ -> raise Not_found
    ))
  with Not_found -> None

let parse_table_row_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try Some (XHTML.M.a_char (List.assoc "char" attribs).[0])
    with Not_found | Invalid_argument _ -> None
(*
  and at2 =
    try Some (XHTML.M.a_width (length_of_string (List.assoc "charoff" attribs)))
    with Not_found | Failure _ -> None
 *)
  and at3 = parse_valign_attrib attribs
  and at4 = parse_align_attrib attribs
  in
  atts @ filter_raw [at1; at3; at4]

let parse_table_cell_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try Some (XHTML.M.a_char (List.assoc "char" attribs).[0])
    with Not_found | Failure _ -> None
  and at2  =
    try Some (XHTML.M.a_charoff (length_of_string (List.assoc "charoff" attribs)))
    with Not_found | Failure _ -> None
  and at3  =
    try Some (XHTML.M.a_abbr (List.assoc "abbr" attribs))
    with Not_found -> None
  and at4 =
    try Some (XHTML.M.a_axis (List.assoc "axis" attribs))
    with Not_found -> None
  and at5 =
    try Some (XHTML.M.a_colspan (int_of_string (List.assoc "colspan" attribs)))
    with Not_found | Failure _ -> None
  and at6 =
    try Some (XHTML.M.a_headers [List.assoc "headers" attribs])
    with Not_found -> None
  and at7 =
    try Some (XHTML.M.a_rowspan ( int_of_string (List.assoc "rowspan" attribs)))
    with Not_found | Failure _ -> None
  and at8 = parse_valign_attrib attribs
  and at9 = parse_align_attrib attribs
  and at10 = parse_scope_attrib attribs in
  atts @ filter_raw [at1; at2; at3; at4; at5; at6; at7]

let list_builder = function
  | [] -> Lwt.return (XHTML.M.li [], [])
  | x::xs ->
      let f (c, l, attribs) =
        let a = opt_list (parse_common_attribs attribs) in
        element c >|= List.flatten   >>= fun r ->
        unopt ~def:(Lwt.return []) l >|= fun l ->
        XHTML.M.li ?a ((r : Xhtmltypes.inlinemix XHTML.M.elt list :> Xhtmltypes.div_content XHTML.M.elt list) @ l)
      in
      f x                   >>= fun y ->
      Lwt_list.map_s f xs   >|= fun ys ->
      (y, ys)

let descr_builder = function
  | [] -> Lwt.return (XHTML.M.dt [], [])
  | x::xs ->
      let f (istitle, d, attribs) =
        let a = opt_list (parse_common_attribs attribs) in
        element d >|= fun d ->
        if istitle
        then (XHTML.M.dt ?a (List.flatten d))
        else (XHTML.M.dd ?a (List.flatten d))
      in
      f x                  >>= fun y ->
      Lwt_list.map_s f xs  >|= fun ys ->
      (y, ys)

let inline (x : Xhtmltypes.inlinemix XHTML.M.elt list) : Xhtmltypes.inlinemix XHTML.M.elt list =
  [XHTML.M.span x]

let link_regexp =
  Netstring_pcre.regexp "(http\\+|https\\+)?([a-z|A-Z|0-9]+)(\\((.*)\\))?:(.*)"

type force_https = bool option

type link_kind =
  | Absolute of string
  | Page of string * force_https
  | Wiki_page of Wiki_types.wiki * string * force_https
  | Site of string * force_https

let link_kind addr =
  match Netstring_pcre.string_match link_regexp addr 0 with
    | None -> Page (addr, None)
    | Some result ->
        let forceproto =
          try
            if Netstring_pcre.matched_group result 1 addr = "https+"
            then Some true
            else Some false
          with Not_found -> None
        in
        let proto = Netstring_pcre.matched_group result 2 addr in
        if proto = "wiki"
        then
          let page = Netstring_pcre.matched_group result 5 addr in
          try
            let wikinum = Netstring_pcre.matched_group result 4 addr in
            Wiki_page ((Wiki_types.wiki_of_sql (Int32.of_string wikinum)),
                       page,
                       forceproto)
          with Failure _ | Not_found -> Page (page, forceproto)
        else if proto = "site"
        then
          let page = Netstring_pcre.matched_group result 5 addr in
          Site (page, forceproto)
        else Absolute addr

let remove_first_slash s =
  let l = String.length s in
  if l = 0
  then s
  else
    if s.[0] = '/'
    then String.sub s 1 (l - 1)
    else s

(* This function is used to translate a sub-tree of a wiki into a new wiki *)
(* newwikipath is relative to oldwiki's path *)
let translate_link ~oldwiki ~newwiki ~newwikipath addr frag attribs (_sp, wb) =
  Wiki_sql.wikibox_wiki wb >>= fun currentwiki ->
  (* Prefix must end with '/' and start with '/' only if it is "/" : *)
  let newwikipath = match newwikipath with
    | "" -> "/"
    | "/" -> newwikipath
    | _ -> remove_first_slash newwikipath
  in
  let preflen = String.length newwikipath in
  let preflast = preflen - 1 in
  let newwikipath, preflen, preflast =
    if newwikipath.[preflast] = '/'
    then newwikipath, preflen, preflast
    else newwikipath^"/", preflen+1, preflen
  in
  let remove_prefix s =
    let s = remove_first_slash s in
    if preflen = 1
    then (* prefix is "/" *) Some s
    else
      let slen = String.length s in
      let first_diff = Ocsigen_lib.string_first_diff newwikipath s 0 preflast in
      if first_diff = preflen
      then Some (String.sub s preflen (slen - preflen))
      else if first_diff = preflast && slen = preflast
      then Some ""
      else None
  in
  let build_link wiki s b =
    let attrs =
      match
        String.concat " " (List.map (fun (n, v) -> n^"='"^v^"'") attribs)
      with
        | "" -> ""
        | s -> "@@"^s^"@@"
    in
    let r =
      attrs^
        (match b with
           | None -> ""
           | Some true -> "https+"
           | Some false -> "http+")^
        "wiki("^(Opaque.int32_t_to_string wiki)^"):"^s
    in
    match frag with
      | None -> r
      | Some f -> r^"#"^f
  in
  let aux s b =
    match remove_prefix s with
      | None -> (* prefix not found *) None
      | Some s -> Some (build_link newwiki s b)
  in
  Lwt.return
    (match link_kind addr with
       | Page (s, b) ->
           if currentwiki = oldwiki
           then aux s b
           else Some (build_link currentwiki s b)
       | Wiki_page (w, s, b) ->
           if w = oldwiki
           then aux s b
           else None
       | _ -> None)


type ('res, 'a_content) syntax_extension =
  (Wiki_widgets_interface.box_info, 'res, 'a_content) Wikicreole.plugin


type 'a plugin_hash = (string, 'a) Hashtbl.t

(* A wikicreole_parser is essentially a [Wikicreole.builder] object
   (contained in the field [builder] but easily extensible. That is,
   the fields [plugin] and [plugin_action] of [Wikicreole.builder],
   which are supposed to be functions, are here represented as
   association tables. Thus, it becomes easy (and, more importantly,
   not costly) to add extensions. When a [wikicreole_parser] must be
   used as a builder, we call the function [builder_from_wikicreole_parser]
   (defined below), which updates the field [plugin] of the builder.
   (This supposes that the builder is used to parse something. When
   used as a preparser, the field [plugin_assoc] is also overridden
   using a proper function. See [preparse_extension] below. BY
*)
type ('res, 'inline, 'a_content) wikicreole_parser = {
  builder: ('res, 'inline, 'a_content, box_info) Wikicreole.builder;

  plugin_assoc: (bool * ('res, 'a_content) syntax_extension) plugin_hash;

  plugin_action_assoc:
     ((Eliom_sessions.server_params * Wiki_types.wikibox,
       string option Lwt.t)
        Wikicreole.plugin_args)
    plugin_hash;

  link_action:
    (string ->
     string option ->
     Wikicreole.attribs ->
     Eliom_sessions.server_params * Wiki_types.wikibox ->
       string option Lwt.t) ref
}

let default_plugin =
  (fun (name : string) ->
    (true,
     (fun _ args content ->
        Wikicreole.A_content
          (let s = string_of_extension name args content in
           Lwt.return [(XHTML.M.pcdata s :> [>`PCDATA] XHTML.M.elt)]))
    )
  )

let plugin_function prsr name =
  try Hashtbl.find prsr.plugin_assoc name
  with Not_found -> default_plugin name

let builder_from_wikicreole_parser prsr =
  { prsr.builder with
      Wikicreole.plugin = plugin_function prsr
  }

let copy_parser wp = {
  wp with
    plugin_assoc = Hashtbl.copy wp.plugin_assoc;
    plugin_action_assoc = Hashtbl.copy wp.plugin_action_assoc;
    link_action = ref !(wp.link_action);
}


let add_preparser_extension ~wp ~name f =
  Hashtbl.add wp.plugin_action_assoc name f

let set_link_extension ~wp f =
    wp.link_action := f


let preparse_extension wp (sp, wb : Eliom_sessions.server_params * Wiki_types.wikibox) content =
  let subst = ref [] in
  let plugin_action name start end_ params args content =
    subst := (start,
              end_,
              (try
                 (Hashtbl.find wp.plugin_action_assoc)
                   name params args content
               with Not_found -> Lwt.return None))::!subst
  and link_action addr fragment attribs (start, end_) params =
    subst := (start,
              end_,
              try !(wp.link_action) addr fragment attribs params
              with _ -> Lwt.return None) ::!subst
  and nothing _ _ = ()
  and nothing1 _ = ()
  in
  (* This builder does essentially nothing, except on extensions
     where it calls plugin_action *)
  let preparse_builder = {
    Wikicreole.chars = nothing1;
    strong_elem = nothing;
    em_elem = nothing;
    a_elem = (fun _ _ _ -> ());
    make_href = (fun _ a fragment -> match fragment with
                   | None -> a
                   | Some f -> a ^"#"^f);
    br_elem = nothing1;
    img_elem = (fun _ _ _ -> ());
    tt_elem = nothing;
    monospace_elem = nothing;
    underlined_elem = nothing;
    linethrough_elem = nothing;
    subscripted_elem = nothing;
    superscripted_elem = nothing;
    nbsp = ();
    endash = ();
    emdash = ();
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
    link_action = link_action;
    error = nothing1;
  } in
  Wikicreole.from_string (sp, wb) preparse_builder content
  >>= fun (_ : unit list) ->
  let buf = Buffer.create 1024 in
  Lwt_list.fold_left_s
    (fun pos (start, end_, replacement) ->
       replacement >>= function
         | None -> Lwt.return pos;
         | Some replacement ->
             Buffer.add_substring buf content pos (start - pos);
             Buffer.add_string buf replacement;
             Lwt.return end_
    )
    0
    (List.rev !subst)
  >>= fun pos ->
  let l = String.length content in
  if pos < l
  then Buffer.add_substring buf content pos (l - pos);
  Lwt.return (Buffer.contents buf)

let site_url_syntax =
  {
    Neturl.url_enable_scheme = Neturl.Url_part_not_recognized;
    Neturl.url_enable_user = Neturl.Url_part_not_recognized;
    Neturl.url_enable_user_param = Neturl.Url_part_not_recognized;
    Neturl.url_enable_password = Neturl.Url_part_not_recognized;
    Neturl.url_enable_host = Neturl.Url_part_not_recognized;
    Neturl.url_enable_port = Neturl.Url_part_not_recognized;
    Neturl.url_enable_path = Neturl.Url_part_allowed;
    Neturl.url_enable_param = Neturl.Url_part_not_recognized;
    Neturl.url_enable_query = Neturl.Url_part_allowed;
    Neturl.url_enable_fragment = Neturl.Url_part_allowed;
    Neturl.url_enable_other = Neturl.Url_part_not_recognized;
    Neturl.url_accepts_8bits = true;
    Neturl.url_is_valid = (fun _ -> true);
    Neturl.url_enable_relative = true;
  }

let make_href bi addr fragment =
  let sp = bi.bi_sp in
  let aux ~fragment https wiki page =
    match Wiki_self_services.find_servpage wiki with
      | Some servpage ->
          let addr =
            Ocsigen_lib.remove_slash_at_beginning
              (Neturl.split_path page)
          in
          Eliom_predefmod.Xhtml.make_string_uri ?https
            ?fragment ~service:servpage ~sp addr
      | None -> "malformed link" (*VVV ??? *)
  in
  match addr with
    | Page (page, forceproto) ->
        let wiki = bi.Wiki_widgets_interface.bi_wiki in
        aux ~fragment forceproto wiki page
    | Absolute addr -> (match fragment with
                          | None -> addr
                          | Some fragment -> addr^"#"^fragment)
    | Wiki_page (wiki, page, forceproto) -> aux ~fragment forceproto wiki page
    | Site (href, forceproto) ->
        try
          let url = Neturl.url_of_string site_url_syntax href in
          let path = Neturl.url_path url in
          let path =
            (Eliom_sessions.get_site_dir sp) @
              (Ocsigen_lib.remove_slash_at_beginning path)
          in
          match forceproto with
            | None ->
                let path =
                  Eliom_uri.reconstruct_relative_url_path
                    (Eliom_sessions.get_original_full_path ~sp)
                    path
                in
                Neturl.string_of_url (Neturl.modify_url ?fragment ~path url)
            | Some https ->
                (Eliom_predefmod.Xhtml.make_proto_prefix ~sp https)^
                  (Neturl.string_of_url (Neturl.modify_url ?fragment ~path url))
        with Neturl.Malformed_URL ->
          "malformed link"

(********************************)
(* builders. Default functions: *)

let strong_elem =
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(XHTML.M.strong ?a r : [>`Strong] XHTML.M.elt)]
  )

let em_elem =
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(XHTML.M.em ?a r : [>`Em] XHTML.M.elt)]
  )

let monospace_elem =
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(XHTML.M.tt ?a r : [>`Tt] XHTML.M.elt)]
  )

let underlined_elem =
  (fun attribs content ->
    let a = XHTML.M.a_class ["underlined"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.span ~a r : [>`Span] XHTML.M.elt)]
  )

let linethrough_elem =
  (fun attribs content ->
    let a = XHTML.M.a_class ["linethrough"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.span ~a r : [>`Span] XHTML.M.elt)]
  )

let subscripted_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.sub ?a r : [>`Sub] XHTML.M.elt)]
  )

let superscripted_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.sup ?a r : [>`Sup] XHTML.M.elt)]
  )

let a_elem =
  (fun attribs addr
    (c : Xhtmltypes.a_content XHTML.M.elt list Lwt.t list) ->
       let a = parse_common_attribs attribs in
       Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
       [(XHTML.M.a ~a:(XHTML.M.a_href (XHTML.M.uri_of_string addr) :: a) c
        : [>`A] XHTML.M.elt)])

let default_make_href =
  (fun bi c fragment -> make_href bi (link_kind c) fragment)

let br_elem =
  (fun attribs ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(XHTML.M.br ?a () : [>`Br] XHTML.M.elt)])

let img_elem =
  (fun attribs addr alt ->
     let a = opt_list (parse_common_attribs attribs) in
     Lwt.return
       [(XHTML.M.img ~src:(XHTML.M.uri_of_string addr) ~alt:alt ?a ()
        : [>`Img] XHTML.M.elt)]
  )

let tt_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.tt ?a r : [>`Tt] XHTML.M.elt)])

let nbsp = Lwt.return [(XHTML.M.pcdata " " : [>`PCDATA] XHTML.M.elt)]

let endash = Lwt.return [(XHTML.M.pcdata "–" : [>`PCDATA] XHTML.M.elt)]

let emdash = Lwt.return [(XHTML.M.pcdata "—" : [>`PCDATA] XHTML.M.elt)]

let p_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.p ?a r : [>`P] XHTML.M.elt)])

let pre_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return
      [(XHTML.M.pre ?a [XHTML.M.pcdata (String.concat "" content)]
       : [>`Pre] XHTML.M.elt)]
  )

let h1_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h1 ?a r : [>`H1] XHTML.M.elt)])

let h2_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h2 ?a r : [>`H2] XHTML.M.elt)])

let h3_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h3 ?a r : [>`H3] XHTML.M.elt)])

let h4_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h4 ?a r : [>`H4] XHTML.M.elt)])

let h5_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h5 ?a r : [>`H5] XHTML.M.elt)])

let h6_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.h6 ?a r : [>`H6] XHTML.M.elt)])

let ul_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
    [(XHTML.M.ul ?a r rs : [>`Ul] XHTML.M.elt)])

let ol_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
    [(XHTML.M.ol ?a r rs : [>`Ol] XHTML.M.elt)])

let dl_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    descr_builder content >|= fun (r, rs) ->
    [(XHTML.M.dl ?a r rs : [>`Dl] XHTML.M.elt)])

let hr_elem =
  (fun attribs ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(XHTML.M.hr ?a () : [>`Hr] XHTML.M.elt)])

let table_elem =
  (fun attribs l ->
    let a = opt_list (parse_table_attribs attribs) in
     match l with
       | [] -> Lwt.return [XHTML.M.table ?a (XHTML.M.tr (XHTML.M.td []) []) []]
       | row::rows ->
           let f (h, attribs, c) =
             let a = opt_list (parse_table_cell_attribs attribs) in
             element c >|= List.flatten >|= fun r ->
             if h
             then XHTML.M.th ?a r
             else XHTML.M.td ?a r
           in
           let f2 (row, attribs) = match row with
             | [] -> Lwt.return (XHTML.M.tr (XHTML.M.td []) [])
             | x::xs ->
                 let a = opt_list (parse_table_row_attribs attribs) in
                 f x                 >>= fun y ->
                 Lwt_list.map_s f xs >|= fun ys ->
                 XHTML.M.tr ?a y ys
           in
           f2 row                 >>= fun row ->
           Lwt_list.map_s f2 rows >|= fun rows ->
           [(XHTML.M.table ?a row rows : [>`Table] XHTML.M.elt)])

let inline =
  (fun x ->
    (x
     : Xhtmltypes.a_content XHTML.M.elt list Lwt.t
     :> Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t)
  )

let plugin = default_plugin

let plugin_action = (fun _ _ _ _ _ _ -> ())

let link_action = (fun _ _ _ _ _ -> ())

let error =
  (fun (s : string) ->
    Lwt.return [(XHTML.M.strong [XHTML.M.pcdata s] : [>`Strong] XHTML.M.elt)])

let span_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(XHTML.M.span ?a r : [>`Span] XHTML.M.elt)])



(********************************)
(* Predefined builders.         *)

let inline_builder :
  (Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t,
   box_info)
     Wikicreole.builder = (* no images, no titles, no tables, no lists,
                             no subwikiboxes, no content, no objects,
                             no paragraph, no pre, ... *)
  { Wikicreole.chars = (fun s -> Lwt.return [XHTML.M.pcdata s]);
    strong_elem = strong_elem;
    em_elem = em_elem;
    monospace_elem = monospace_elem;
    underlined_elem = underlined_elem;
    linethrough_elem = linethrough_elem;
    subscripted_elem = subscripted_elem;
    superscripted_elem = superscripted_elem;
    a_elem = a_elem;
    make_href = default_make_href;
    br_elem =
      (fun _ ->
         Lwt.return
           [XHTML.M.em
              [XHTML.M.pcdata "Line breaks not enabled in this syntax"]]);
    img_elem = img_elem;
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = span_elem;
    pre_elem =
      (fun _ _ ->
         Lwt.return
          [XHTML.M.em
             [XHTML.M.pcdata "Blocks of code not enabled in this syntax"]]);
    h1_elem = span_elem;
    h2_elem = span_elem;
    h3_elem = span_elem;
    h4_elem = span_elem;
    h5_elem = span_elem;
    h6_elem = span_elem;
    ul_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    ol_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    dl_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    hr_elem =
      (fun _ ->
        Lwt.return
          [XHTML.M.em
             [XHTML.M.pcdata "Horizontal rules not enabled in this syntax"]]);
    table_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Tables not enabled in this syntax"]]);
    inline = inline;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }


let default_builder :
  (Xhtmltypes.div_content XHTML.M.elt list Lwt.t,
   Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
   Xhtmltypes.a_content XHTML.M.elt list Lwt.t,
   box_info
  ) Wikicreole.builder =

  { Wikicreole.chars = (fun s -> Lwt.return [XHTML.M.pcdata s]) ;
    strong_elem = strong_elem;
    em_elem = em_elem;
    monospace_elem = monospace_elem;
    underlined_elem = underlined_elem;
    linethrough_elem = linethrough_elem;
    subscripted_elem = subscripted_elem;
    superscripted_elem = superscripted_elem;
    a_elem = a_elem;
    make_href = default_make_href;
    br_elem = br_elem;
    img_elem = img_elem;
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = p_elem;
    pre_elem = pre_elem;
    h1_elem = h1_elem;
    h2_elem = h2_elem;
    h3_elem = h3_elem;
    h4_elem = h4_elem;
    h5_elem = h5_elem;
    h6_elem = h6_elem;
    ul_elem = ul_elem;
    ol_elem = ol_elem;
    dl_elem = dl_elem;
    hr_elem = hr_elem;
    table_elem = table_elem;
    inline = inline;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder = (* no images, no objects, no subwikiboxes, no content *)
  { Wikicreole.chars = (fun s -> Lwt.return [XHTML.M.pcdata s]) ;
    strong_elem = strong_elem;
    em_elem = em_elem;
    monospace_elem = monospace_elem;
    underlined_elem = underlined_elem;
    linethrough_elem = linethrough_elem;
    subscripted_elem = subscripted_elem;
    superscripted_elem = superscripted_elem;
    a_elem = a_elem;
    make_href = default_make_href;
    br_elem = br_elem;
    img_elem =
      (fun _ _ _ ->
         Lwt.return
           [XHTML.M.em [XHTML.M.pcdata "Images not enabled in this syntax"]]);
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = p_elem;
    pre_elem = pre_elem;
    h1_elem = h1_elem;
    h2_elem = h2_elem;
    h3_elem = h3_elem;
    h4_elem = h4_elem;
    h5_elem = h5_elem;
    h6_elem = h6_elem;
    ul_elem = ul_elem;
    ol_elem = ol_elem;
    dl_elem = dl_elem;
    hr_elem = hr_elem;
    table_elem = table_elem;
    inline = inline;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder2 = (* no images, no titles, no tables, no lists,
                          no subwikiboxes, no content, no objects *)
  { Wikicreole.chars = (fun s -> Lwt.return [XHTML.M.pcdata s]) ;
    strong_elem = strong_elem;
    em_elem = em_elem;
    monospace_elem = monospace_elem;
    underlined_elem = underlined_elem;
    linethrough_elem = linethrough_elem;
    subscripted_elem = subscripted_elem;
    superscripted_elem = superscripted_elem;
    a_elem = a_elem;
    make_href = default_make_href;
    br_elem = br_elem;
    img_elem =
      (fun _ _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Images not enabled in this syntax"]]);
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = p_elem;
    pre_elem = pre_elem;
    h1_elem = p_elem;
    h2_elem = p_elem;
    h3_elem = p_elem;
    h4_elem = p_elem;
    h5_elem = p_elem;
    h6_elem = p_elem;
    ul_elem =
      (fun _ _ ->
        Lwt.return
         [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    ol_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    dl_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Lists not enabled in this syntax"]]);
    hr_elem = hr_elem;
    table_elem =
      (fun _ _ ->
        Lwt.return
          [XHTML.M.em [XHTML.M.pcdata "Tables not enabled in this syntax"]]);
    inline = inline;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder_button :
  ([Xhtmltypes.button_content | `PCDATA] XHTML.M.elt list Lwt.t,
   [Xhtmltypes.button_content | `PCDATA] XHTML.M.elt list Lwt.t,
   [Xhtmltypes.button_content | `PCDATA] XHTML.M.elt list Lwt.t,
   box_info
  ) Wikicreole.builder =
  let forbid0 s =
    Lwt.return [(XHTML.M.em [XHTML.M.pcdata (s ^" not enabled in buttons")]
                : [Xhtmltypes.button_content | `PCDATA] XHTML.M.elt)]
  in
  let forbid1 s _ = forbid0 s in
  let forbid2 s _ _ = forbid0 s in
  let forbid3 s _ _ _ = forbid0 s in

  { Wikicreole.chars =
      (fun s -> Lwt.return [(XHTML.M.pcdata s : [>`PCDATA] XHTML.M.elt)]);
    strong_elem = (*strong_elem;*) forbid2 "toto";
    em_elem = (*em_elem;*) forbid2 "toto";
    monospace_elem = (*monospace_elem;*) forbid2 "toto";
    underlined_elem = (*underlined_elem;*) forbid2 "toto";
    linethrough_elem = (*linethrough_elem;*) forbid2 "toto";
    subscripted_elem = (*subscripted_elem;*) forbid2 "toto";
    superscripted_elem = (*superscripted_elem;*) forbid2 "toto";
    a_elem = forbid3 "links";
    make_href = default_make_href;
    br_elem = (*br_elem;*) forbid1 "toto";
    img_elem = (*img_elem ;*) forbid3 "toto";
    tt_elem = (*tt_elem;*) forbid2 "toto";
    nbsp = (*nbsp;*) forbid0 "toto";
    endash = (*endash;*) forbid0 "toto";
    emdash = (*emdash;*) forbid0 "toto";
    p_elem = (*p_elem;*) forbid2 "toto";
    pre_elem = (*pre_elem;*) forbid2 "toto";
    h1_elem = (*p_elem;*) forbid2 "toto";
    h2_elem = (*p_elem;*) forbid2 "toto";
    h3_elem = (*p_elem;*) forbid2 "toto";
    h4_elem = (*p_elem;*) forbid2 "toto";
    h5_elem = (*p_elem;*) forbid2 "toto";
    h6_elem = (*p_elem;*) forbid2 "toto";
    ul_elem = (*ul_elem;*) forbid2 "toto";
    ol_elem = (*ol_elem;*) forbid2 "toto";
    dl_elem = (*dl_elem;*) forbid2 "toto";
    hr_elem = (*hr_elem;*) forbid1 "toto";
    table_elem = (*table_elem;*) forbid2 "toto";
    inline = (*inline;*) forbid1 "toto";
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }



(********************************)
(* Default parsers:             *)

let void_plugin_action = fun _ _ _ _ -> Lwt.return None

let wikicreole_parser = {
  builder = default_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}

let reduced_wikicreole_parser0 = {
  builder = default_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}

let reduced_wikicreole_parser1 = {
  builder = reduced_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}

let reduced_wikicreole_parser2 = {
  builder = reduced_builder2;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}

let reduced_wikicreole_parser_button_content = {
  builder = reduced_builder_button;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}

let inline_wikicreole_parser = {
  builder = inline_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
  link_action = ref void_plugin_action;
}



(********************************)
(* Default parser functions:    *)

let xml_of_wiki wp bi s =
  Wikicreole.from_string bi (builder_from_wikicreole_parser wp) s
  >>= Lwt_list.map_s (fun x -> x)
  >|= List.flatten

let inline_of_wiki bi s : Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t =
  Wikicreole.from_string
    bi
    ({inline_builder with
       Wikicreole.plugin = plugin_function inline_wikicreole_parser
     } : (Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
          Xhtmltypes.inlinemix XHTML.M.elt list Lwt.t,
          Xhtmltypes.a_content XHTML.M.elt list Lwt.t,
          box_info
         ) Wikicreole.builder
    )
    s >>= function
      | [] -> Lwt.return []
      | a::_ -> a


(********************************)
(* Predefined content types:    *)

let wikicreole_content_type =
  Wiki_models.register_flows_wiki_parser "wikicreole"
    (preparse_extension wikicreole_parser)
    (xml_of_wiki wikicreole_parser)

let reduced_wikicreole_content_type0 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole0"
    (preparse_extension reduced_wikicreole_parser0)
    (xml_of_wiki reduced_wikicreole_parser0)

let reduced_wikicreole_content_type1 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole1"
    (preparse_extension reduced_wikicreole_parser1)
    (xml_of_wiki reduced_wikicreole_parser1)

let reduced_wikicreole_content_type2 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole2"
    (preparse_extension reduced_wikicreole_parser2)
    (xml_of_wiki reduced_wikicreole_parser2)

let wikicreole_inline_content_type =
  Wiki_models.register_inlines_wiki_parser "inline_wikicreole"
    (preparse_extension inline_wikicreole_parser)
    inline_of_wiki

let rawtext_content_type =
  Wiki_models.register_flows_wiki_parser "rawtext"
    (fun _ s -> Lwt.return s)
    (fun _bi s -> Lwt.return [XHTML.M.p [XHTML.M.pcdata s]])

let a_content_of_wiki bi s =
  inline_of_wiki bi s >>= fun r ->
  Lwt.fail (Failure "TODO: how to safely pattern match on XHTML.M values")
(*  Lwt.return
    {{ map r with
         |  <a (Xhtmltypes_duce.a_attrs)>l -> l
         | p -> [p] }}*)




(*********************************************************************)
(* Adding syntax extensions in predefined parsers:                   *)

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
  let add_extension_aux l ~name ?wiki_content f =
    List.iter (fun wp -> add_extension ~wp ~name ?wiki_content f) l
  in

  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"div" ~wiki_content:true
    (fun bi args c ->
       Wikicreole.Block
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          xml_of_wiki wikicreole_parser bi content >|= fun content ->
          let classe =
            try Some (XHTML.M.a_class [List.assoc "class" args])
            with Not_found -> None
          in
          let id =
            try Some (XHTML.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = opt_list (filter_raw [classe; id]) in
          [XHTML.M.div ?a content]
    ));


  let f = (fun bi args c ->
       Wikicreole.A_content
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          inline_of_wiki bi content >|= fun content ->
          let classe =
            try Some (XHTML.M.a_class [List.assoc "class" args])
            with Not_found -> None
          in
          let id =
            try Some (XHTML.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = opt_list (filter_raw [classe; id]) in
          [XHTML.M.span ?a content]
         )
    )
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"span" ~wiki_content:true
    f;
  add_extension ~wp:inline_wikicreole_parser ~name:"span" ~wiki_content:true f;

  let f =
    (fun bi _ _ ->
      Wikicreole.A_content
        (let wid = bi.Wiki_widgets_interface.bi_wiki in
         Wiki_sql.get_wiki_info_by_id wid >|= fun wiki_info ->
         [XHTML.M.pcdata wiki_info.wiki_descr])
    )
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"wikiname" ~wiki_content:true f;
  add_extension ~wp:inline_wikicreole_parser
    ~name:"wikiname" ~wiki_content:true f;


  let f =
    (fun _ args content ->
      Wikicreole.A_content
        (let s = string_of_extension "raw" args content in
         Lwt.return [XHTML.M.b [XHTML.M.pcdata s]]))
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"raw" ~wiki_content:false f;
  add_extension ~wp:inline_wikicreole_parser ~name:"raw" ~wiki_content:false f;

  let f = (fun _ _ _ -> Wikicreole.A_content (Lwt.return []))
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"" ~wiki_content:false f;
  add_extension ~wp:inline_wikicreole_parser ~name:"" ~wiki_content:false f;

  add_extension_aux
    [wikicreole_parser]
    ~name:"content"
    (fun bi args _ ->
       Wikicreole.Block
         (let classe =
            try Some (List.assoc "class" args)
            with Not_found -> None
          and id =
            try Some (XHTML.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          bi.Wiki_widgets_interface.bi_subbox bi.bi_menu_style >|= function
            | None ->
                let a = match classe with
                  | None -> opt_list (filter_raw [id])
                  | Some c -> Some (XHTML.M.a_class [c] :: filter_raw [id])
                in
                [XHTML.M.div ?a
                   [XHTML.M.strong [XHTML.M.em [XHTML.M.pcdata "<<conten>>"]]]
                ]
            | Some (wb, subbox) ->
                let classe = match wb with
                  | None -> apply_opt (fun c -> XHTML.M.a_class [c]) classe
                  | Some wb ->
                      Some
                        (XHTML.M.a_class
                           (class_wikibox wb :: filter_raw [classe]))
                in
                let a = opt_list (filter_raw [classe; id]) in
                [XHTML.M.div ?a subbox]
         )
    );

  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"menu"
    (fun bi args _ ->
       let wiki_id = bi.Wiki_widgets_interface.bi_wiki in
       Wikicreole.Block
         (let classe =
            XHTML.M.a_class
              (   "wikimenu"
               :: filter_raw [
                    try Some (List.assoc "class" args) with Not_found -> None
                  ])
          in
          let id =
            try Some (XHTML.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = Some (classe :: (filter_raw [id])) in
          let f ?classe s =
            let link, text =
              try Ocsigen_lib.sep '|' s
              with Not_found -> s, s
            in
            Wiki_sql.get_wiki_info_by_id wiki_id >>= fun wiki_info ->
            a_content_of_wiki bi text >>= fun text2 ->
            let b =
              match wiki_info.Wiki_types.wiki_pages with
                | Some dir ->
                    Eliom_sessions.get_current_sub_path_string
                      bi.Wiki_widgets_interface.bi_sp =
                      Ocsimore_lib.remove_begin_slash (dir^"/"^link)
                | None -> false
            in
            if b
            then
              let classe = match classe with
                | None   -> XHTML.M.a_class ["wikimenu_current"]
                | Some c -> XHTML.M.a_class ("wikimenu_current" :: c)
              in
              Lwt.return (XHTML.M.li ~a:[classe] text2)
            else
              let href =
                XHTML.M.a_href
                  (XHTML.M.uri_of_string
                     (make_href bi (link_kind link) None)
                  )
              in
              let classe = apply_opt XHTML.M.a_class classe in
              let a = apply_opt (fun x -> [x]) classe in
              Lwt.return (XHTML.M.li ?a [XHTML.M.a ~a:[href] text2])
          in
          let rec mapf = function
            | []    -> Lwt.return []
            | [x]   -> f ~classe:["wikimenu_last"] x >|= fun y -> [y]
            | x::xs -> f x     >>= fun y  ->
                       mapf xs >|= fun ys ->
                       (y::ys)
          in
          match List.fold_left
            (fun beg (n, v) -> if n="item" then v::beg else beg)
            [] args
          with
            | [] -> Lwt.return []
            | [x] ->
                f ~classe:["wikimenu_first"; "wikimenu_last"] x >|= fun y ->
                [XHTML.M.ul ?a y []]
            | x::xs ->
                f ~classe:["wikimenu_first"] x >>= fun y ->
                mapf xs                        >|= fun ys ->
                [XHTML.M.ul ?a y ys]
         )
    );


  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"cond" ~wiki_content:true
    (fun bi args c ->
       Wikicreole.Block
         (let sp = bi.Wiki_widgets_interface.bi_sp in
          let content = unopt_string c in
          let rec eval_cond = function
            | ("error", "autherror") ->
                Lwt_list.exists_s
                  (fun e ->
                    Lwt.return (e = User.BadPassword || e = User.BadUser))
                  (User_data.get_login_error ~sp)
            | ("ingroup", g) ->
                Lwt.catch
                  (fun () ->
                     User.get_user_by_name g >>= fun group ->
                     User.in_group ~sp ~group ())
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
                let not_cond =
                  (String.sub err 3 (String.length err - 3), value)
                in
                eval_cond not_cond >|= not
            | _ -> Lwt.return false
          in
          (match args with
             | [c] -> eval_cond c
             | _   -> Lwt.return false)
         >>= function
           | true -> xml_of_wiki wikicreole_parser bi content
           | false -> Lwt.return []
         )
    )

