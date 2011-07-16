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

open Eliom_pervasives
open Wiki_types
open Wiki_widgets_interface
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


let class_wikibox wb = Printf.sprintf "wikiboxcontent%s" (string_of_wikibox wb)


let string_of_extension name args content =
    "<<"^name^" "
  ^ (String.concat " " (List.map (fun (n, v) -> n^"=\""^v^"\"") args))
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
    try Some (HTML5.M.a_class [List.assoc "class" attribs])
    with Not_found -> None
  and at2 =
    try Some (HTML5.M.a_id (List.assoc "id" attribs))
    with Not_found -> None
  in
  filter_raw [at1; at2]

let parse_table_attribs attribs =
  let atts = parse_common_attribs attribs
(* not available in html5 anymore
  and at1 =
    try Some (HTML5.M.a_border (int_of_string (List.assoc "border" attribs)))
    with Not_found | Failure _ -> None
  and at2 =
    try Some (HTML5.M.a_cellpadding (length_of_string (List.assoc "cellpadding" attribs)))
    with Not_found | Failure _ -> None
  and at3 =
    try Some (HTML5.M.a_cellspacing (length_of_string (List.assoc "cellspacing" attribs)))
    with Not_found | Failure _ -> None*)
  and at4 =
    try Some (HTML5.M.a_summary (List.assoc "summary" attribs))
    with Not_found -> None
(*and at5 =
    try Some (HTML5.M.a_width (length_of_string (List.assoc "width" attribs)))
    with Not_found | Failure _ -> None*)
  in
  atts @ filter_raw [(*at1; at2; at3;*) at4;]

(* No more valign in html5
let parse_valign_attrib attribs =
  try
    Some (HTML5.M.a_valign (match List.assoc "valign" attribs with
                              |"top" -> `Top
                              |"middle" -> `Middle
                              |"bottom" -> `Bottom
                              |"baseline" -> `Baseline
                              | _ -> raise Not_found
    ))
  with Not_found -> None
*)

let parse_align_attrib attribs =
  try
    Some (HTML5.M.a_align (match List.assoc "align" attribs with
                              |"left" -> `Left
                              |"right" -> `Right
                              (* |"center" -> `Center no more in HTML5 *)
                              |"justify" -> `Justify
                              |"char" -> `Char
                              | _ -> raise Not_found
    ))
  with Not_found -> None

let parse_scope_attrib attribs =
  try
    Some (HTML5.M.a_scope (match List.assoc "scope" attribs with
                             |"row" -> `Row
                             |"col" -> `Col
                             |"rowgroup" -> `Rowgroup
                             |"colgroup" -> `Colgroup
                             | _ -> raise Not_found
    ))
  with Not_found -> None

(* no any of this in html5
let parse_table_row_attribs attribs =
  let atts = parse_common_attribs attribs
(*
  and at1 =
    try Some (HTML5.M.a_char (List.assoc "char" attribs).[0])
    with Not_found | Invalid_argument _ -> None *)
(*
  and at2 =
    try Some (HTML5.M.a_width (length_of_string (List.assoc "charoff" attribs)))
    with Not_found | Failure _ -> None
 *)
(* no more valign in HTML5
  and at3 = parse_valign_attrib attribs *)
(*  and at4 = parse_align_attrib attribs *)
  in
  atts @ filter_raw [at1; at3; at4 ]
*)

let parse_table_cell_attribs attribs =
  let atts = parse_common_attribs attribs
(* no more in html5
  and at1 =
    try Some (HTML5.M.a_char (List.assoc "char" attribs).[0])
    with Not_found | Failure _ -> None
  and at2  =
    try Some (HTML5.M.a_charoff (length_of_string (List.assoc "charoff" attribs)))
    with Not_found | Failure _ -> None *)
(* No more abbr attribute in HTML5 
  and at3  =
    try Some (HTML5.M.a_abbr (List.assoc "abbr" attribs))
    with Not_found -> None *)
(*
  and at4 =
    try Some (HTML5.M.a_axis (List.assoc "axis" attribs))
    with Not_found -> None *)
  and at5 =
    try Some (HTML5.M.a_colspan (int_of_string (List.assoc "colspan" attribs)))
    with Not_found | Failure _ -> None
  and at6 =
    try Some (HTML5.M.a_headers [List.assoc "headers" attribs])
    with Not_found -> None
  and at7 =
    try Some (HTML5.M.a_rowspan ( int_of_string (List.assoc "rowspan" attribs)))
    with Not_found | Failure _ -> None
(*  and at8 = parse_valign_attrib attribs
  and at9 = parse_align_attrib attribs
  and at10 = parse_scope_attrib attribs *) in
  atts @ filter_raw [(* at1; at2; at3; at4;*) at5; at6; at7]

let list_builder = function
  | [] -> Lwt.return (HTML5.M.li [], [])
  | x::xs ->
      let f (c, l, attribs) =
        let a = opt_list (parse_common_attribs attribs) in
        element c >|= List.flatten   >>= fun r ->
        unopt ~def:(Lwt.return []) l >|= fun l ->
        HTML5.M.li ?a ((r : [ HTML5_types.phrasing ] HTML5.M.elt list :> [ HTML5_types.li_content ] HTML5.M.elt list) @ l)
      in
      f x                   >>= fun y ->
      Lwt_list.map_s f xs   >|= fun ys ->
      (y, ys)

let descr_builder l =
  let f (istitle, (d : HTML5_types.phrasing HTML5.M.elt list Lwt.t list), attribs) =
    let a = opt_list (parse_common_attribs attribs) in
    element d >|= fun d ->
      if istitle
      then `Dt (HTML5.M.dt ?a (List.flatten d))
      else `Dd (HTML5.M.dd ?a ((List.flatten d:>HTML5_types.flow5 HTML5.M.elt list)))
  in
  let rec list_dt acc = function
    | [] -> None
    | (`Dd _)::_ as l ->
      let rest, dd = list_dd [] l in
      Some ((List.rev acc, dd),rest)
    | (`Dt v)::q -> list_dt (v::acc) q
  and list_dd acc = function
    | []
    | (`Dt _)::_ as l -> l,List.rev acc
    | (`Dd v)::q -> list_dd (v::acc) q
  in
  let rec combine acc l =
    match list_dt [] l with
      | None -> List.rev acc
      | Some ((dt,dd),rest) ->
	let dt = match dt with
	  | [] -> HTML5.M.dt [], []
	  | x::xs -> x,xs
	in
	let dd = match dd with
	  | [] -> HTML5.M.dd [], []
	  | x::xs -> x,xs
	in
	combine ((dt,dd)::acc) rest
  in
  lwt l = Lwt_list.map_s f l in
  Lwt.return (combine [] l)

let phrasing (x : HTML5_types.phrasing HTML5.M.elt list) : HTML5_types.phrasing_without_interactive HTML5.M.elt list =
  [HTML5.M.span x]

type ('a,'b) wiki_service =
    ('a, unit,
     Eliom_services.get_service_kind,
     Eliom_services.suff,
     'b, unit,
     Eliom_services.registrable,
     Eliom_output.appl_service ) Eliom_services.service

(* We need existential types to be able to parametrise service_href by
   the service and its parameter without showing the type of the
   parameter.

   Since this does not exists in ocaml, we need some encoding *)

module Appl_existential_services :
sig
  type 'a t

  val a_link :
    ?a:HTML5_types.a_attrib HTML5.M.attrib list ->
    'a Eliom_pervasives.HTML5.M.elt list ->
    [> 'a HTML5_types.a ] Eliom_pervasives.HTML5.M.elt t

  val uri : string t

  val run : 'b t -> ?fragment:string -> ?https:bool -> 'a -> ('a,'c) wiki_service -> 'b

end
  =
struct
  type 'b t = { t : 'a 'c. ?fragment:string -> ?https:bool -> 'a -> ('a,'c) wiki_service -> 'b }
  type ('a,'c,'b) apply_serv = 'b t * 'a * ('a,'c) wiki_service
  type ('b, 'z) apply_scope = { bind_apply : 'a 'c. ('a,'c, 'b) apply_serv -> 'z}
  type 'b packed_apply = { open_apply : 'z. ('b, 'z) apply_scope -> 'z}

  (* Packing and unpacking applications *)
  let pack_apply value serv f = { open_apply = fun scope -> scope.bind_apply (f,value,serv) }
  let with_packed_apply p e = p.open_apply e

  let a_link ?a c = { t = ( fun ?fragment ?https param service ->
    Eliom_output.Html5.a ~service ?a ?fragment ?https c param ) }
  let uri = { t = ( fun ?fragment ?https param service ->
    Uri.uri_of_string (Eliom_uri.make_string_uri ?fragment ?https ~service param) ) }

  let run t ?fragment ?https v s = with_packed_apply (pack_apply v s t) { bind_apply = fun (f,v,s) -> f.t ?fragment ?https v s }

end

type service_href = { apply_serv : 'a. 'a Appl_existential_services.t -> 'a }

type href =
  | String_href of string
  | Service_href of service_href

let service_href ?fragment ?https serv param =
  { apply_serv = (fun m -> Appl_existential_services.run m ?fragment ?https param serv) }

let a_link_of_href href ?a c = href.apply_serv
  (Appl_existential_services.a_link ?a c)

let uri_of_href href =
  match href with
    | String_href s -> Uri.uri_of_string s
    | Service_href href -> href.apply_serv (Appl_existential_services.uri)

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
let translate_link ~oldwiki ~newwiki ~newwikipath addr frag attribs wb =
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
      let first_diff = String.first_diff newwikipath s 0 preflast in
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


type ('res, 'phrasing_without_interactive, 'href) syntax_extension =
  (Wiki_widgets_interface.box_info, 'res, 'phrasing_without_interactive, 'href) Wikicreole.plugin


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
type ('res, 'phrasing, 'phrasing_without_interactive, 'href) wikicreole_parser = {
  builder: ('res, 'phrasing, 'phrasing_without_interactive, box_info, 'href) Wikicreole.builder;

  plugin_assoc: (bool * ('res, 'phrasing_without_interactive, 'href) syntax_extension) plugin_hash;

  plugin_action_assoc:
     ((Wiki_types.wikibox,
       string option Lwt.t)
        Wikicreole.plugin_args)
    plugin_hash;

  link_action:
    (string ->
     string option ->
     Wikicreole.attribs ->
     Wiki_types.wikibox ->
       string option Lwt.t) ref
}

let default_plugin =
  (fun (name : string) ->
    (true,
     (fun _ args content ->
        Wikicreole.Phrasing_without_interactive
          (let s = string_of_extension name args content in
           Lwt.return [(HTML5.M.pcdata s :> [>`PCDATA] HTML5.M.elt)]))
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


let preparse_extension wp (wb : Wiki_types.wikibox) content =
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
    phrasing = nothing1;
    plugin =
      (fun name ->
         let wiki_content =
           try fst (Hashtbl.find wp.plugin_assoc name)
           with Not_found -> false
         in (wiki_content, (fun _ _ _ -> Wikicreole.Phrasing_without_interactive ())));
    plugin_action = plugin_action;
    link_action = link_action;
    error = nothing1;
  } in
  Wikicreole.from_string wb preparse_builder content
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
  let aux ~fragment https wiki page =
    match Wiki_self_services.find_servpage wiki with
      | Some servpage ->
          let addr =
            Url.remove_slash_at_beginning
              (Neturl.split_path page)
          in
	  Service_href (service_href (servpage:>('a,'b) wiki_service) addr)
            (* Eliom_output.Html5.make_string_uri ?https
               ?fragment ~service:servpage addr *)
      | None -> String_href "malformed link" (*VVV ??? *)
  in
  match addr with
    | Page (page, forceproto) ->
        let wiki = bi.Wiki_widgets_interface.bi_wiki in
        aux ~fragment forceproto wiki page
    | Absolute addr -> (match fragment with
                          | None -> String_href addr
                          | Some fragment -> String_href (addr^"#"^fragment))
    | Wiki_page (wiki, page, forceproto) -> aux ~fragment forceproto wiki page
    | Site (href, forceproto) ->
      String_href (* CCC could we find a service for the site ? *)
        (try
          let url = Neturl.url_of_string site_url_syntax href in
          let path = Neturl.url_path url in
          let path =
            (Eliom_request_info.get_site_dir ()) @
              (Url.remove_slash_at_beginning path)
          in
          match forceproto with
            | None ->
                let path =
                  Eliom_uri.reconstruct_relative_url_path
                    (Eliom_request_info.get_original_full_path ())
                    path
                in
                Neturl.string_of_url (Neturl.modify_url ?fragment ~path url)
            | Some https ->
                (Eliom_output.Html5.make_proto_prefix https)^
                  (Neturl.string_of_url (Neturl.modify_url ?fragment ~path url))
        with Neturl.Malformed_URL ->
          "malformed link")

(********************************)
(* builders. Default functions: *)

let strong_elem =
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(HTML5.M.strong ?a r : [>`Strong] HTML5.M.elt)]
  )

let em_elem =
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(HTML5.M.em ?a r : [>`Em] HTML5.M.elt)]
  )

let monospace_elem =
(* No more tt in HTML5 
  (fun attribs content ->
     let a = opt_list (parse_common_attribs attribs) in
     element content >|= List.flatten >|= fun r ->
     [(HTML5.M.tt ?a r : [>`Tt] HTML5.M.elt)]
  ) *)
  (fun attribs content ->
    let a = HTML5.M.a_class ["monospace"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]
  )


let underlined_elem =
  (fun attribs content ->
    let a = HTML5.M.a_class ["underlined"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]
  )

let linethrough_elem =
  (fun attribs content ->
    let a = HTML5.M.a_class ["linethrough"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]
  )

let subscripted_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.sub ?a r : [>`Sub] HTML5.M.elt)]
  )

let superscripted_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.sup ?a r : [>`Sup] HTML5.M.elt)]
  )

let a_elem =
  (fun attribs addr
    (c : HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t list) ->
       let a = parse_common_attribs attribs in
       Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
	 match addr with
	   | String_href addr ->
	     [(HTML5.M.a ~a:(HTML5.M.a_href (Uri.uri_of_string addr) :: a) c
               :> HTML5_types.phrasing HTML5.M.elt)]
	   | Service_href href ->
	     [(a_link_of_href href ~a c :> HTML5_types.phrasing HTML5.M.elt)])

let default_make_href =
  (fun bi c fragment ->
    make_href bi (link_kind c) fragment )

let menu_make_href bi c fragment =
  (* Accept only simple page. Ignore fragment and anything else silently... *)
  match link_kind c with
  | Page (page, None) -> String_href page
  | Wiki_page (wiki,page,None) ->
      String_href ("wiki(" ^ Wiki_types.string_of_wiki wiki ^ "):" ^ page)
  | _ -> String_href ""

let br_elem =
  (fun attribs ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(HTML5.M.br ?a () : [>`Br] HTML5.M.elt)])

let img_elem =
  (fun attribs href alt ->
     let a = opt_list (parse_common_attribs attribs) in
     let src = uri_of_href href (* CCC https ? *) in
     Lwt.return
       [(HTML5.M.img ~src ~alt:alt ?a ()
        : [>`Img] HTML5.M.elt)]
  )

let tt_elem =
(* no more tt in HTML5
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.tt ?a r : [>`Tt] HTML5.M.elt)]) *)
  (fun attribs content ->
    let a = HTML5.M.a_class ["teletype"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]
  )


let nbsp = Lwt.return [(HTML5.M.pcdata " " : [>`PCDATA] HTML5.M.elt)]

let endash = Lwt.return [(HTML5.M.pcdata "–" : [>`PCDATA] HTML5.M.elt)]

let emdash = Lwt.return [(HTML5.M.pcdata "—" : [>`PCDATA] HTML5.M.elt)]

let p_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.p ?a r : [>`P] HTML5.M.elt)])

let pre_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return
      [(HTML5.M.pre ?a [HTML5.M.pcdata (String.concat "" content)]
       : [>`Pre] HTML5.M.elt)]
  )

let h1_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h1 ?a r : [>`H1] HTML5.M.elt)])

let h2_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h2 ?a r : [>`H2] HTML5.M.elt)])

let h3_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h3 ?a r : [>`H3] HTML5.M.elt)])

let h4_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h4 ?a r : [>`H4] HTML5.M.elt)])

let h5_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h5 ?a r : [>`H5] HTML5.M.elt)])

let h6_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.h6 ?a r : [>`H6] HTML5.M.elt)])

let ul_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
    [(HTML5.M.ul ?a (r::rs) : [>`Ul] HTML5.M.elt)])

let ol_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
    [(HTML5.M.ol ?a (r::rs) : [>`Ol] HTML5.M.elt)])

let dl_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    descr_builder content >|= fun r ->
    [(HTML5.M.dl ?a r : [>`Dl] HTML5.M.elt)])

let hr_elem =
  (fun attribs ->
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(HTML5.M.hr ?a () : [>`Hr] HTML5.M.elt)])

let table_elem =
  (fun attribs l ->
    let a = opt_list (parse_table_attribs attribs) in
     match l with
       | [] -> Lwt.return [HTML5.M.table ?a (HTML5.M.tr [HTML5.M.td []]) []]
       | row::rows ->
           let f (h, attribs, (c:HTML5_types.phrasing HTML5.M.elt list Lwt.t list)) =
             let a = opt_list (parse_table_cell_attribs attribs) in
             element c >|= List.flatten >|= fun r ->
             if h
             then HTML5.M.th ?a r
             else HTML5.M.td ?a (r:>HTML5_types.flow5 HTML5.M.elt list)
           in
           let f2 (row, attribs) = match row with
             | [] -> Lwt.return (HTML5.M.tr [HTML5.M.td []])
             | x::xs ->
                 let a = opt_list (parse_common_attribs attribs) in
                 (*let a = opt_list (parse_table_row_attribs attribs) in*)
                 f x                 >>= fun y ->
                 Lwt_list.map_s f xs >|= fun ys ->
                 HTML5.M.tr ?a (y::ys)
           in
           f2 row                 >>= fun row ->
           Lwt_list.map_s f2 rows >|= fun rows ->
           [(HTML5.M.table ?a row rows : [>`Table] HTML5.M.elt)])

let phrasing =
  (fun x ->
    (x
     : HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
     :> HTML5_types.phrasing HTML5.M.elt list Lwt.t)
  )

let plugin = default_plugin

let plugin_action = (fun _ _ _ _ _ _ -> ())

let link_action = (fun _ _ _ _ _ -> ())

let error =
  (fun (s : string) ->
    Lwt.return [(HTML5.M.strong [HTML5.M.pcdata s] : [>`Strong] HTML5.M.elt)])

let span_elem =
  (fun attribs content ->
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.span ?a r : [>`Span] HTML5.M.elt)])



(********************************)
(* Predefined builders.         *)

let phrasing_builder :
  (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   box_info,
   'href)
     Wikicreole.builder = (* no images, no titles, no tables, no lists,
                             no subwikiboxes, no content, no objects,
                             no paragraph, no pre, ... *)
  { Wikicreole.chars = (fun s -> Lwt.return [HTML5.M.pcdata s]);
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
           [HTML5.M.em
              [HTML5.M.pcdata "Line breaks not enabled in this syntax"]]);
    img_elem = img_elem;
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = span_elem;
    pre_elem =
      (fun _ _ ->
         Lwt.return
          [HTML5.M.em
             [HTML5.M.pcdata "Blocks of code not enabled in this syntax"]]);
    h1_elem = span_elem;
    h2_elem = span_elem;
    h3_elem = span_elem;
    h4_elem = span_elem;
    h5_elem = span_elem;
    h6_elem = span_elem;
    ul_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    ol_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    dl_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    hr_elem =
      (fun _ ->
        Lwt.return
          [HTML5.M.em
             [HTML5.M.pcdata "Horizontal rules not enabled in this syntax"]]);
    table_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Tables not enabled in this syntax"]]);
    phrasing = phrasing;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }


let default_builder :
  (HTML5_types.flow5 HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing HTML5.M.elt list Lwt.t,
   HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
   box_info,
   'href
  ) Wikicreole.builder =

  { Wikicreole.chars = (fun s -> Lwt.return [HTML5.M.pcdata s]) ;
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
    phrasing = phrasing;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder = (* no images, no objects, no subwikiboxes, no content *)
  { Wikicreole.chars = (fun s -> Lwt.return [HTML5.M.pcdata s]) ;
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
           [HTML5.M.em [HTML5.M.pcdata "Images not enabled in this syntax"]]);
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
    phrasing = phrasing;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder2 = (* no images, no titles, no tables, no lists,
                          no subwikiboxes, no content, no objects *)
  { Wikicreole.chars = (fun s -> Lwt.return [HTML5.M.pcdata s]) ;
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
          [HTML5.M.em [HTML5.M.pcdata "Images not enabled in this syntax"]]);
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
         [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    ol_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    dl_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]);
    hr_elem = hr_elem;
    table_elem =
      (fun _ _ ->
        Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Tables not enabled in this syntax"]]);
    phrasing = phrasing;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }


let menu_builder :
    ([ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
        Eliom_pervasives.HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing Eliom_pervasives.HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive
       Eliom_pervasives.HTML5.M.elt list Lwt.t, 'a, href)
         Wikicreole.builder =
 (* no images, no objects, no subwikiboxes, no content *)
  let nothing _ _ = Lwt.return []
  and nothing1 _ = Lwt.return [] in
  { Wikicreole.chars = (fun s -> Lwt.return [HTML5.M.pcdata s]) ;
    strong_elem = strong_elem;
    em_elem = em_elem;
    monospace_elem = monospace_elem;
    underlined_elem = underlined_elem;
    linethrough_elem = linethrough_elem;
    subscripted_elem = subscripted_elem;
    superscripted_elem = superscripted_elem;
    a_elem = a_elem;
    make_href = menu_make_href;
    br_elem = nothing1;
    img_elem = img_elem;
    tt_elem = tt_elem;
    nbsp = nbsp;
    endash = endash;
    emdash = emdash;
    p_elem = nothing;
    pre_elem = nothing;
    h1_elem = h1_elem;
    h2_elem = h2_elem;
    h3_elem = h3_elem;
    h4_elem = h4_elem;
    h5_elem = h5_elem;
    h6_elem = h6_elem;
    ul_elem = nothing;
    ol_elem = nothing;
    dl_elem = nothing;
    hr_elem = nothing1;
    table_elem = nothing;
    phrasing = phrasing;
    plugin = plugin;
    plugin_action = plugin_action;
    link_action = link_action;
    error = error;
  }

let reduced_builder_button :
  ([HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t,
   [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t,
   [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t,
   box_info,
   'href
  ) Wikicreole.builder =
  let forbid0 s =
    Lwt.return [(HTML5.M.em [HTML5.M.pcdata (s ^" not enabled in buttons")]
                : [HTML5_types.button_content | `PCDATA] HTML5.M.elt)]
  in
  let forbid1 s _ = forbid0 s in
  let forbid2 s _ _ = forbid0 s in
  let forbid3 s _ _ _ = forbid0 s in

  { Wikicreole.chars =
      (fun s -> Lwt.return [(HTML5.M.pcdata s : [>`PCDATA] HTML5.M.elt)]);
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
    phrasing = (*phrasing;*) forbid1 "toto";
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

let menu_parser = {
  builder = menu_builder;
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

let phrasing_wikicreole_parser = {
  builder = phrasing_builder;
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

let phrasing_of_wiki bi s : HTML5_types.phrasing HTML5.M.elt list Lwt.t =
  Wikicreole.from_string
    bi
    ({phrasing_builder with
       Wikicreole.plugin = plugin_function phrasing_wikicreole_parser
     } : (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
          HTML5_types.phrasing HTML5.M.elt list Lwt.t,
          HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
          box_info,
	  'href
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

let wikicreole_phrasing_content_type =
  Wiki_models.register_phrasings_wiki_parser "phrasing_wikicreole"
    (preparse_extension phrasing_wikicreole_parser)
    phrasing_of_wiki

(* For backward compatibility *)
let wikicreole_inline_content_type =
  Wiki_models.register_phrasings_wiki_parser "inline_wikicreole"
    (preparse_extension phrasing_wikicreole_parser)
    phrasing_of_wiki

let rawtext_content_type =
  Wiki_models.register_flows_wiki_parser "rawtext"
    (fun _ s -> Lwt.return s)
    (fun _bi s -> Lwt.return [HTML5.M.p [HTML5.M.pcdata s]])

let phrasing_without_interactive_of_wiki bi s =
  phrasing_of_wiki bi s >>= fun r ->
  (* CCC horrible: nettoyer ca *)
  Lwt.return (HTML5.M.totl (HTML5.M.toeltl r))
(*
  Lwt.fail (Failure "TODO: how to safely pattern match on HTML5.M values")
*)
(*  Lwt.return
    {{ map r with
         |  <a (HTML5_types_duce.a_attrs)>l -> l
         | p -> [p] }}*)




(*********************************************************************)
(* Adding syntax extensions in predefined parsers:                   *)

let add_extension ~wp ~name ?(wiki_content=true) f =
  Hashtbl.add wp.plugin_assoc name (wiki_content, f);
  if wiki_content then
    Hashtbl.add wp.plugin_action_assoc name
      (fun wb args -> function
         | None -> Lwt.return None
         | Some c ->
             preparse_extension wp wb c >>= fun c ->
             Lwt.return (Some (string_of_extension name args (Some c)))
      )


let () =
  let add_extension_aux l ~name ?wiki_content f =
    List.iter (fun wp -> add_extension ~wp ~name ?wiki_content (f wp)) l
  in

  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"div" ~wiki_content:true
    (fun wp bi args c ->
       Wikicreole.Flow5
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          lwt content = xml_of_wiki wp bi content in
          let classe =
            try Some (HTML5.M.a_class [List.assoc "class" args])
            with Not_found -> None
          in
          let id =
            try Some (HTML5.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = opt_list (filter_raw [classe; id]) in
          Lwt.return [HTML5.M.div ?a content]
    ));


  let f = (fun wp bi args c ->
       Wikicreole.Phrasing_without_interactive
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          phrasing_of_wiki bi content >|= fun content ->
          let classe =
            try Some (HTML5.M.a_class [List.assoc "class" args])
            with Not_found -> None
          in
          let id =
            try Some (HTML5.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = opt_list (filter_raw [classe; id]) in
          [HTML5.M.span ?a content]
         )
    )
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"span" ~wiki_content:true
    f;
  add_extension ~wp:phrasing_wikicreole_parser ~name:"span" ~wiki_content:true
    (f phrasing_wikicreole_parser);

  let f =
    (fun _ bi _ _ ->
      Wikicreole.Phrasing_without_interactive
        (let wid = bi.Wiki_widgets_interface.bi_wiki in
         Wiki_sql.get_wiki_info_by_id wid >|= fun wiki_info ->
         [HTML5.M.pcdata wiki_info.wiki_descr])
    )
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"wikiname" ~wiki_content:true f;
  add_extension ~wp:phrasing_wikicreole_parser
    ~name:"wikiname" ~wiki_content:true (f phrasing_wikicreole_parser);

  let f =
    (fun _ _ args content ->
      Wikicreole.Phrasing_without_interactive
        (let s = string_of_extension "raw" args content in
         Lwt.return [HTML5.M.b [HTML5.M.pcdata s]]))
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"raw" ~wiki_content:false f;
  add_extension ~wp:phrasing_wikicreole_parser ~name:"raw" ~wiki_content:false
    (f phrasing_wikicreole_parser);

  let f = (fun _ _ _ _ -> Wikicreole.Phrasing_without_interactive (Lwt.return []))
  in
  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"" ~wiki_content:false f;
  add_extension ~wp:phrasing_wikicreole_parser ~name:"" ~wiki_content:false
    (f phrasing_wikicreole_parser);

  add_extension_aux
    [wikicreole_parser]
    ~name:"content"
    (fun _ bi args _ ->
       Wikicreole.Flow5
         (let classe =
            try Some (List.assoc "class" args)
            with Not_found -> None
          and id =
            try Some (HTML5.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          bi.Wiki_widgets_interface.bi_subbox bi.bi_menu_style >|= function
            | None ->
                let a = match classe with
                  | None -> opt_list (filter_raw [id])
                  | Some c -> Some (HTML5.M.a_class [c] :: filter_raw [id])
                in
                [HTML5.M.div ?a
                   [HTML5.M.strong [HTML5.M.em [HTML5.M.pcdata "<<conten>>"]]]
                ]
            | Some (wb, subbox) ->
                let classe = match wb with
                  | None -> apply_opt (fun c -> HTML5.M.a_class [c]) classe
                  | Some wb ->
                      Some
                        (HTML5.M.a_class
                           (class_wikibox wb :: filter_raw [classe]))
                in
                let a = opt_list (filter_raw [classe; id]) in
                [HTML5.M.div ?a subbox]
         )
    );

  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"menu"
    (fun _ bi args _ ->
       let wiki_id = bi.Wiki_widgets_interface.bi_wiki in
       Wikicreole.Flow5
         (let classe =
            HTML5.M.a_class
              (   "wikimenu"
               :: filter_raw [
                    try Some (List.assoc "class" args) with Not_found -> None
                  ])
          in
          let id =
            try Some (HTML5.M.a_id (List.assoc "id" args))
            with Not_found -> None
          in
          let a = Some (classe :: (filter_raw [id])) in
          let f ?classe s =
            let link, text =
              try String.sep '|' s
              with Not_found -> s, s
            in
            Wiki_sql.get_wiki_info_by_id wiki_id >>= fun wiki_info ->
            phrasing_without_interactive_of_wiki bi text >>= fun text2 ->
            let b =
              match wiki_info.Wiki_types.wiki_pages with
                | Some dir ->
                    Eliom_request_info.get_current_sub_path_string ()
                  = Ocsimore_lib.remove_begin_slash (dir^"/"^link)
                | None -> false
            in
            if b
            then
              let classe = match classe with
                | None   -> HTML5.M.a_class ["wikimenu_current"]
                | Some c -> HTML5.M.a_class ("wikimenu_current" :: c)
              in
              Lwt.return (HTML5.M.li ~a:[classe] text2)
            else
	      let link =
		match make_href bi (link_kind link) None with
		  | String_href addr ->
		    (HTML5.M.a ~a:[HTML5.M.a_href (Uri.uri_of_string addr)] text2)
		  | Service_href href -> a_link_of_href href ~a:[] text2
              in
              let classe = apply_opt HTML5.M.a_class classe in
              let a = apply_opt (fun x -> [x]) classe in
              Lwt.return (HTML5.M.li ?a [link])
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
                [HTML5.M.ul ?a [y]]
            | x::xs ->
                f ~classe:["wikimenu_first"] x >>= fun y ->
                mapf xs                        >|= fun ys ->
                [HTML5.M.ul ?a (y::ys)]
         )
    );


  let f_cond wp bi args c =
    Wikicreole.Flow5
      (let content = unopt_string c in
       let rec eval_cond = function
         | ("error", "autherror") ->
           Lwt_list.exists_s
             (fun e ->
               Lwt.return (e = User.BadPassword || e = User.BadUser))
             (User_data.get_login_error ())
         | ("ingroup", g) ->
           Lwt.catch
             (fun () ->
               User.get_user_by_name g >>= fun group ->
               User.in_group ~group ())
             (function _ -> Lwt.return false)
         | ("http_code", "404") ->
           Lwt.return (Wiki_widgets_interface.page_displayable () =
               Wiki_widgets_interface.Page_404)
         | ("http_code", "403") ->
           Lwt.return (Wiki_widgets_interface.page_displayable () =
               Wiki_widgets_interface.Page_403)
         | ("http_code", "40?") ->
           Lwt.return (Wiki_widgets_interface.page_displayable () <>
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
             | true -> xml_of_wiki wp bi content
             | false -> Lwt.return []
      )
  in

  add_extension_aux
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"cond" ~wiki_content:true
     f_cond;

  add_extension_aux [menu_parser] ~name:"cond" ~wiki_content:true f_cond;
