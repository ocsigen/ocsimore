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
   Pretty print wiki to HTML5 using Eliom's TyXML
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Eliom_lib
open Lwt_ops
open Eliom_content
open Ocsimore_lib
open Wiki_types
open Wiki_syntax_types
open Wiki_widgets_interface

let class_wikibox wb = Printf.sprintf "wikiboxcontent%s" (string_of_wikibox wb)

let string_of_extension name args content =
  "<<"^name^" "
  ^ (String.concat " " (List.map (fun (n, v) -> n^"=\""^v^"\"") args))
  ^ (match content with | None -> "" | Some content -> "|"^content)
  ^ ">>"

let opt_list = function | [] -> None | _::_ as l -> Some l

(***)

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


let rec filter_raw = function (* /!\ NOT TAIL REC /!\ *)
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


let parse_common_attribs ?classes attribs =
  let at1 =
    try Some (Html5.F.a_class (String.split ',' (List.assoc "class" attribs) @ unopt ~def:[] classes))
    with Not_found -> Eliom_lib.Option.map Html5.F.a_class classes
  and at2 =
    try Some (Html5.F.a_id (List.assoc "id" attribs))
    with Not_found -> None
  and at3 =
    try Some (Html5.F.a_style (List.assoc "style" attribs))
    with Not_found -> None
  in
  filter_raw [at1; at2; at3]

let parse_table_attribs attribs =
  let atts = parse_common_attribs attribs
(* not available in html5 anymore
  and at1 =
    try Some (Html5.F.a_border (int_of_string (List.assoc "border" attribs)))
    with Not_found | Failure _ -> None
  and at2 =
    try Some (Html5.F.a_cellpadding (length_of_string (List.assoc "cellpadding" attribs)))
    with Not_found | Failure _ -> None
  and at3 =
    try Some (Html5.F.a_cellspacing (length_of_string (List.assoc "cellspacing" attribs)))
    with Not_found | Failure _ -> None*)
  and at4 =
    try Some (Html5.F.a_summary (List.assoc "summary" attribs))
    with Not_found -> None
(*and at5 =
    try Some (Html5.F.a_width (length_of_string (List.assoc "width" attribs)))
    with Not_found | Failure _ -> None*)
  in
  atts @ filter_raw [(*at1; at2; at3;*) at4;]

(* No more valign in html5
let parse_valign_attrib attribs =
  try
    Some (Html5.F.a_valign (match List.assoc "valign" attribs with
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
    Some (Html5.F.a_align (match List.assoc "align" attribs with
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
    Some (Html5.F.a_scope (match List.assoc "scope" attribs with
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
    try Some (Html5.F.a_char (List.assoc "char" attribs).[0])
    with Not_found | Invalid_argument _ -> None *)
(*
  and at2 =
    try Some (Html5.F.a_width (length_of_string (List.assoc "charoff" attribs)))
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
    try Some (Html5.F.a_char (List.assoc "char" attribs).[0])
    with Not_found | Failure _ -> None
  and at2  =
    try Some (Html5.F.a_charoff (length_of_string (List.assoc "charoff" attribs)))
    with Not_found | Failure _ -> None *)
(* No more abbr attribute in HTML5
  and at3  =
    try Some (Html5.F.a_abbr (List.assoc "abbr" attribs))
    with Not_found -> None *)
(*
  and at4 =
    try Some (Html5.F.a_axis (List.assoc "axis" attribs))
    with Not_found -> None *)
  and at5 =
    try Some (Html5.F.a_colspan (int_of_string (List.assoc "colspan" attribs)))
    with Not_found | Failure _ -> None
  and at6 =
    try Some (Html5.F.a_headers [List.assoc "headers" attribs])
    with Not_found -> None
  and at7 =
    try Some (Html5.F.a_rowspan ( int_of_string (List.assoc "rowspan" attribs)))
    with Not_found | Failure _ -> None
(*  and at8 = parse_valign_attrib attribs
  and at9 = parse_align_attrib attribs
  and at10 = parse_scope_attrib attribs *) in
  atts @ filter_raw [(* at1; at2; at3; at4;*) at5; at6; at7]

let item_builder
    ((c : Html5_types.phrasing Html5.F.elt list Lwt.t list), l, attribs) =
  let a = opt_list (parse_common_attribs attribs) in
  lwt r = element c >|= List.flatten in
  lwt l = unopt ~def:(Lwt.return []) l in
  Lwt.return
    (Html5.F.li ?a ((r :> Html5_types.li_content_fun Html5.F.elt list)
                    @ (l :> Html5_types.li_content_fun Html5.F.elt list)))

let item_builder =
  (item_builder (* opening types *)
     : (Html5_types.phrasing Html5.F.elt list Lwt.t list * _ * _ -> _)
     :> ([< Html5_types.phrasing ] Html5.F.elt list Lwt.t list * _ * _ -> _))

let list_builder xs = match xs with
  | [] -> Lwt.return (Html5.F.li [], [])
  | x :: xs ->
      lwt y = item_builder x in
      lwt ys = Lwt_list.map_s item_builder xs in
      Lwt.return (y, ys)

let ddt_builder
    (istitle, (d : Html5_types.phrasing Html5.F.elt list Lwt.t list), attribs) =
  let a = opt_list (parse_common_attribs attribs) in
  lwt d = element d in
  Lwt.return
    (if istitle
     then `Dt (Html5.F.dt ?a ((List.flatten d :> Html5_types.dt_content_fun Html5.F.elt list)))
     else `Dd (Html5.F.dd ?a ((List.flatten  d :> Html5_types.dd_content_fun Html5.F.elt list))))

let ddt_builder =
  (ddt_builder (* opening types *)
     : (_ * Html5_types.phrasing Html5.F.elt list Lwt.t list * _ -> _)
     :> (_ * [< Html5_types.phrasing ] Html5.F.elt list Lwt.t list * _ -> _))

let descr_builder l =
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
          | [] -> Html5.F.dt [], []
          | x::xs -> x,xs
        in
        let dd = match dd with
          | [] -> Html5.F.dd [], []
          | x::xs -> x,xs
        in
        combine ((dt,dd)::acc) rest
  in
  lwt l = Lwt_list.map_s ddt_builder l in
  Lwt.return (combine [] l)

let phrasing (x : Html5_types.phrasing Html5.F.elt list) : Html5_types.phrasing_without_interactive Html5.F.elt list =
  [Html5.F.span x]

type ('a,'b, 'kind, 'suff, 'reg, 'appl) wiki_service =
    ('a, unit,
     [< Eliom_service.get_service_kind] as 'kind,
     [< Eliom_service.suff] as 'suff,
     'b, unit,
     [< Eliom_service.registrable] as 'reg,
     [< Eliom_registration.appl_service] as 'appl) Eliom_service.service

(* We need existential types to be able to parametrise service_href by
   the service and its parameter without showing the type of the
   parameter.

   Since this does not exists in ocaml, we need some encoding *)

type service_href = Wiki_syntax_types.service_href
type href = Wiki_syntax_types.href =
  | String_href of string
  | Service_href of service_href

let service_href ?fragment ?https service param =
  let module Href = struct
    let uri = Eliom_content.Html5.F.make_uri ?fragment ?https ~service param
    let a_link ?a c =
      Eliom_content.Html5.F.a ~service ?a ?fragment ?https c param
  end in
  (module Href : Service_href)

let a_link_of_href href ?a c =
  let module Href = (val href : Service_href) in
  Href.a_link ?a c

let uri_of_href href =
  match href with
    | String_href s -> Html5.F.Raw.uri_of_string s
    | Service_href href ->
      let module Href = (val href : Service_href) in
      Href.uri

let link_regexp =
  Netstring_pcre.regexp "(http\\+|https\\+)?([a-z|A-Z|0-9]+)(\\((.*)\\))?:(.*)"
let wiki_title_regexp = Netstring_pcre.regexp "\"([a-z|A-Z|_][a-z|A-Z|_|0-9]*)\""
let protocol_group = 1
let prototype_group = 2
let wiki_id_parentheses_group = 3
let wiki_id_group = 4
let page_group = 5

type force_https = bool option

let replace_regexp_group ~str ~result ~group ~replacement =
  let open Netstring_pcre in
  let before =
    String.sub str 0 (group_beginning result group)
  in
  let after =
    let e = group_end result group in
    String.sub str e (String.length str - e)
  in
  before ^ replacement ^ after

let get_map_option ~default ~f = function
    None -> default
  | Some x -> f x

let sub_string ?from ?to_ str =
  let from = match from with Some ix -> ix | None -> 0 in
  let to_ = match to_ with Some ix -> ix | None -> String.length str in
  String.sub str from (to_ - from)

let has_prefix ?(offset=0) ~prefix str =
  String.(length str - offset > length prefix && sub str offset (length prefix) = prefix)

(** If [list] starts with [prefix] and ends with [res] (i.e. [list = prefix @ res]
    [list_suffix ~prefix list] return [Some res], otherwise [None].  *)
let list_suffix ~prefix list =
  let rec aux = function
      [], res -> Some res
    | x::xs, y::ys when x = y -> aux (xs, ys)
    | _ -> None
  in
  aux (prefix, list)

let normalize_link =
  let module Result = struct
    let success_replace' s = Lwt.return (Some s)
    let success_replace fmt = Printf.ksprintf success_replace' fmt
    let failure_malformed_link' pos desugar_param msg =
      Wiki_syntax_types.(desugar_param.dc_warnings <- (pos, "Malformed link: "^msg) :: desugar_param.dc_warnings);
      Lwt.return None
    let failure_malformed_link pos desugar_param fmt = Printf.ksprintf (failure_malformed_link' pos desugar_param) fmt
    let no_replacement = Lwt.return None
  end in
  fun pos addr fragment desugar_param ->
    let open Netstring_pcre in
    match string_match link_regexp addr 0 with
      | Some result when matched_group result prototype_group addr = "wiki" ->
          let wikinum = matched_group result wiki_id_group addr in
          begin match string_match wiki_title_regexp wikinum 0 with
            | Some title_result -> (* [[wiki("title"):path]] => [[wiki(ix):path]] *)
                let name = matched_group title_result 1 wikinum in
                begin try_lwt
                  Wiki_sql.get_wiki_info_by_name ~name >>= fun wiki_info ->
                  let wiki_id_string = Wiki_types.string_of_wiki wiki_info.Wiki_types.wiki_id in
                  let replacement =
                    replace_regexp_group ~str:addr ~result ~group:wiki_id_group ~replacement:wiki_id_string
                  in
                  Result.success_replace' replacement
                with
                  Not_found ->
                    Result.failure_malformed_link pos desugar_param "no wiki named %S" name
                end
            | None -> Result.no_replacement
          end
      | None -> (* [addr] is no [link_regexp] *)
          let replacement_addr =
            let page_wiki_id_string = Wiki_types.string_of_wiki desugar_param.dc_page_wiki in
            if String.length addr = 0 then (* [[]] => [[wiki(25):a/b/c]] *)
              Result.success_replace "wiki(%s):%s"
                page_wiki_id_string
                (get_map_option ~default:"" ~f:(String.concat "/") desugar_param.dc_page_path)
            else
              let path_or_link =
                if has_prefix ~prefix:"/" addr then (* [[/a/b/c]] => [[wiki(ix):e/f]] *)
                  match
                    list_suffix
                      ~prefix:(Eliom_request_info.get_site_dir ())
                      Neturl.(url_path (url_of_string site_url_syntax (sub_string ~from:1 addr)))
                  with
                    | Some path -> (* [addr = site_dir / path] *)
                        `Path path
                    | None -> (* addr does not denote something in the ocsigen site *)
                        `Link (Result.success_replace "href:%s" addr)
                else
                  let url =
                    let relative_url = Neturl.url_of_string site_url_syntax addr in
                    match desugar_param.Wiki_syntax_types.dc_page_path with
                      | Some page_path ->
                          Neturl.apply_relative_url
                            (Neturl.make_url ~path:page_path site_url_syntax)
                            relative_url
                      | None -> relative_url
                  in `Path (Neturl.url_path url)
              in
              match path_or_link with
                | `Path path -> (* [[xyz]] => [[wiki(25):a/b/xyz]] et al. *)
                    begin try
                      let page_wiki, page_path =
                        let wiki_page_for_path_option path =
                          try Some (Wiki_self_services.get_wiki_page_for_path path)
                          with Not_found -> None
                        in
                        match wiki_page_for_path_option path, wiki_page_for_path_option ("" :: path) with
                          | Some ((_, page_path) as page), Some ((_, page_path') as page') ->
                              if List.(length page_path < length page_path') then page else page'
                          | Some page, None | None, Some page -> page
                          | None, None -> raise Not_found
                      in
                      Result.success_replace "wiki(%s):%s"
                        (Wiki_types.string_of_wiki page_wiki)
                        (Url.string_of_url_path ~encode:false page_path)
                    with Not_found -> (* No wiki page at [path] *)
                      Result.success_replace "site:%s" (Url.string_of_url_path ~encode:false path)
                    end
                | `Link res -> res
          in
          let append_fragment addr = addr ^ get_map_option ~default:"" ~f:((^) "#") fragment in
          replacement_addr >|= Option.map append_fragment
      | _ -> Result.no_replacement

type link_kind =
  | Wiki_page of Wiki_types.wiki option * string * force_https
  | Href of string * force_https
  | Site of string * force_https
  | Absolute of string

let link_kind addr =
  match Netstring_pcre.string_match link_regexp addr 0 with
    | None ->
        failwith (Printf.sprintf "Not a valid link: %S" addr);
    | Some result ->
        let forceproto =
          try Some (Netstring_pcre.matched_group result protocol_group addr = "https+")
          with Not_found -> None
        in
        let page = Netstring_pcre.matched_group result page_group addr in
        begin match Netstring_pcre.matched_group result prototype_group addr with
          | "href" ->
              Href (page, forceproto)
          | "site" ->
              Site (page, forceproto)
          | "wiki" ->
              let has_id =
                try
                  ignore (Netstring_pcre.matched_group result wiki_id_parentheses_group addr);
                  true
                with Not_found -> false
              in
              if has_id then
                begin try
                  let wikinum = Netstring_pcre.matched_group result wiki_id_group addr in
                  let wiki = Wiki_types.wiki_of_sql (Int32.of_string wikinum) in
                  Wiki_page (Some wiki, page, forceproto)
                with
                  Failure _ | Not_found -> Wiki_page (None, page, forceproto)
                end
              else
                Wiki_page (None, page, forceproto)
          |  _ -> Absolute addr
        end

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
(* NOT USED
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


*)

(** **)

type preparser = Wiki_syntax_types.preparser
type desugarer = Wiki_syntax_types.desugarer
type 'a wikicreole_parser = 'a Wiki_syntax_types.wikicreole_parser

type ('a, 'b, 'c) ext_wikicreole_parser =
    ('a, 'b, 'c) Wiki_syntax_types.ExtParser.ext_wikicreole_parser

open Wiki_syntax_types.ExtParser

(* cast ('a, 'b, 'c) ext_wikicreole_parser to 'a wikicreole_parser *)
let cast_wp (type a) (type b) (type c) wp =
  let module P = (val wp : ExtParser with type res = a
                                      and type res_without_interactive = b
                                     and type link_content = c) in
  (module P : Parser with type res = a)

(* cast ('a, 'b, 'c) ext_wikicreole_parser to 'b wikicreole_parser *)
let cast_niwp (type a) (type b) (type c) wp =
  let module P = (val wp : ExtParser with type res = a
                                     and type res_without_interactive = b
                                     and type link_content = c) in
  (module struct
     type res = P.res_without_interactive
     let from_string = P.from_string_without_interactive
     let preparse_string = P.preparse_string
     let desugar_string = P.desugar_string
   end : Parser with type res = b)

let get_plugin_resolver (type a) (type b) (type c) wp =
  let module P = (val wp : ExtParser with type res = a
                                      and type res_without_interactive = b
                                     and type link_content = c) in
  P.plugin_resolver

let preparse_string (type a) (type b) (type c) wp =
  let module P = (val wp : ExtParser with type res = a
                                      and type res_without_interactive = b
                                     and type link_content = c) in
  P.preparse_string

let desugar_string (type a) (type b) (type c) wp =
  let module P = (val wp : ExtParser with type res = a
                                      and type res_without_interactive = b
                                     and type link_content = c) in
  P.desugar_string

(********************************)
(* Default parser functions:    *)

let xml_of_wiki (type t) wp bi content =
  let module Parser = (val wp : Parser with type res = t) in
  let xml = Parser.from_string ~sectioning:bi.bi_sectioning bi content in
  element xml >|= List.flatten

let preprocess_extension (type t) wp =
  let module Parser = (val wp : Parser with type res = t) in
  (module Parser : Wiki_syntax_types.Preprocessor)

(*******)

module type RawParser = sig

  type res
  type res_without_interactive
  type text
  type link_content
  type list_item

  include Wikicreole.RawBuilder
    with type param := Wiki_widgets_interface.box_info
    and type href := href
    and type phrasing = text Html5.F.elt list Lwt.t
    and type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
    and type flow = res Html5.F.elt list Lwt.t
    and type flow_without_interactive = res_without_interactive Html5.F.elt list Lwt.t
    and type uo_list = list_item Html5.F.elt list Lwt.t

  val ignore_a_elem_phrasing :
    Wikicreole.attribs -> href ->
    link_content Html5.F.elt list Lwt.t list ->
    link_content Html5.F.elt list Lwt.t
  val ignore_a_elem_flow :
    Wikicreole.attribs -> href ->
    res_without_interactive Html5.F.elt list Lwt.t list ->
    res_without_interactive Html5.F.elt list Lwt.t

  val default_extension:
    name:string ->
    Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
    string option -> link_content Html5.F.elt list Lwt.t

  val default_ni_extension:
    name:string ->
    Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
    string option -> link_content Html5.F.elt list Lwt.t

end

type (+'flow,
      +'flow_without_interactive,
      +'phrasing_without_interactive) plugin_content =
  [ `Flow5_link
      of (href * Wikicreole.attribs * 'flow_without_interactive Html5.F.elt list Lwt.t)
  | `Phrasing_link
      of (href * Wikicreole.attribs * 'phrasing_without_interactive Html5.F.elt list Lwt.t)
  | `Flow5 of 'flow Html5.F.elt list Lwt.t
  | `Phrasing_without_interactive
      of 'phrasing_without_interactive Html5.F.elt list Lwt.t ]

type (+'flow_without_interactive,
      +'phrasing_without_interactive) ni_plugin_content =
  [ `Flow5 of 'flow_without_interactive Html5.F.elt list Lwt.t
  | `Phrasing_without_interactive
      of 'phrasing_without_interactive Html5.F.elt list Lwt.t ]

type (+'flow_without_interactive,
      +'phrasing_without_interactive) link_plugin_content =
  [ `Flow5_link
      of (href * Wikicreole.attribs * 'flow_without_interactive Html5.F.elt list Lwt.t)
  | `Phrasing_link
      of (href * Wikicreole.attribs * 'phrasing_without_interactive Html5.F.elt list Lwt.t) ]


module MakeParser(B: RawParser) :
  ExtParser with type res = B.res
            and type res_without_interactive = B.res_without_interactive
            and type link_content = B.link_content
  = struct

  type res = B.res
  type res_without_interactive = B.res_without_interactive
  type link_content = B.link_content

  type wikiparser = (res, res_without_interactive, link_content) ext_wikicreole_parser

  type interactive_plugin_content =
      (res, res_without_interactive, link_content) plugin_content

  type non_interactive_plugin_content =
      (res_without_interactive, link_content) ni_plugin_content

  type simple_plugin =
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      string option ->
      (res, res_without_interactive, link_content) plugin_content

  type simple_ni_plugin =
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      string option ->
      (res_without_interactive, link_content) ni_plugin_content

  type 'a wiki_plugin =
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      'a Html5.F.elt list Lwt.t option ->
      (res, res_without_interactive, link_content) plugin_content

  type 'a wiki_ni_plugin =
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      'a Html5.F.elt list Lwt.t option ->
      (res_without_interactive, link_content) ni_plugin_content

  type 'a link_plugin =
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      'a Html5.F.elt list Lwt.t option ->
      (res_without_interactive, link_content) link_plugin_content

    (* Module to encode existential type parameter of the recursive wikiparser.
       Could be replaced by a GADT with Ocaml 3.13. *)
  module type WikiPlugin = sig

    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser:
      (rec_res,
       rec_res_without_interactive,
       rec_link_content) ExtParser.ext_wikicreole_parser
    val update_context:
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      Wiki_widgets_interface.box_info
    val plugin: rec_res wiki_plugin
    val ni_plugin: rec_res_without_interactive wiki_ni_plugin option

  end

  module type LinkPlugin = sig

    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser:
      (rec_res,
       rec_res_without_interactive,
       rec_link_content) ExtParser.ext_wikicreole_parser
    val update_context:
      Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
      Wiki_widgets_interface.box_info
    val plugin: rec_res_without_interactive link_plugin

  end

  module type RawWikiPlugin = sig

    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser:
      (rec_res,
       rec_res_without_interactive,
       rec_link_content) ExtParser.ext_wikicreole_parser
      val plugin: rec_res wikicreole_parser -> simple_plugin
      val ni_plugin:
        (rec_res_without_interactive wikicreole_parser -> simple_ni_plugin) option

  end


  type plugin =
    | SimplePlugin of simple_plugin * simple_ni_plugin option
    | WikiPlugin of (module WikiPlugin)
    | LinkPlugin of (module LinkPlugin)
    | RawWikiPlugin of (module RawWikiPlugin)

  let plugin_assoc : (string, plugin * preparser option) Hashtbl.t =
    Hashtbl.create 17
  let register_extension ~name ?preparser plugin =
    Hashtbl.add plugin_assoc name (plugin, preparser)

  let rec plugin_resolver =
    Wikicreole.Resolver
      (fun name ->
        try match Hashtbl.find plugin_assoc name with
          | SimplePlugin _, _ -> None
          | WikiPlugin p, _ ->
              let module Plugin = (val p: WikiPlugin) in
              Some (get_plugin_resolver Plugin.wikiparser)
          | LinkPlugin p, _ ->
              let module Plugin = (val p: LinkPlugin) in
              Some (get_plugin_resolver Plugin.wikiparser)
          | RawWikiPlugin p, _ ->
              let module Plugin = (val p: RawWikiPlugin) in
              Some (get_plugin_resolver Plugin.wikiparser)
        with Not_found -> Some plugin_resolver)

  module InteractiveBuilder = struct

    include B

    type href' = href
    type href = href'
    type param = Wiki_widgets_interface.box_info

    type plugin_content = interactive_plugin_content

    let plugin_resolver = plugin_resolver
    let plugin name =
      try
        match Hashtbl.find plugin_assoc name with
        | SimplePlugin (plugin, _), _ -> (None, plugin)
        | WikiPlugin p, _ ->
            let module Plugin = (val p: WikiPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             (fun bi attribs content ->
               let bi = Plugin.update_context bi attribs in
               let xml =
                 Option.map
                   (xml_of_wiki (cast_wp Plugin.wikiparser) bi)
                   (Option.map remove_spaces content)
               in
               Plugin.plugin bi attribs xml))
        | LinkPlugin p, _ ->
            let module Plugin = (val p: LinkPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             (fun bi attribs content ->
               let bi = Plugin.update_context bi attribs in
               let xml =
                 Option.map
                   (xml_of_wiki (cast_niwp Plugin.wikiparser) bi)
                   (Option.map remove_spaces content)
               in
               (Plugin.plugin bi attribs xml
                :> (res, res_without_interactive,
                    link_content) Wiki_syntax_types.plugin_content)))
        | RawWikiPlugin p, _ ->
            let module Plugin = (val p: RawWikiPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             (fun bi attribs content ->
               Plugin.plugin (cast_wp Plugin.wikiparser) bi attribs content))
      with Not_found ->
        (Some plugin_resolver,
         (fun bi attribs content ->
           `Phrasing_without_interactive
             (B.default_extension ~name bi attribs content)))

    let plugin = (plugin :> _ -> (_ * (_ -> _ -> _ -> plugin_content)))

    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
    let href_action _ _ _ _ _ = ()

  end

  let interactive_builder =
    (module InteractiveBuilder
        : Wikicreole.Builder with type param = Wiki_widgets_interface.box_info
                             and type flow = B.flow)

  let from_string ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb interactive_builder content

  module NonInteractiveBuilder = struct

    type flow = B.flow_without_interactive
    type flow_without_interactive = B.flow_without_interactive
    type phrasing_without_interactive = B.phrasing_without_interactive
    type phrasing = B.phrasing_without_interactive
    type uo_list = B.uo_list
    type href' = href
    type href = href'
    type param = Wiki_widgets_interface.box_info

    let error = B.error
    let list = B.list
    let hr_elem = B.hr_elem
    let pre_elem = B.pre_elem
    let make_href = B.make_href
    let emdash = B.emdash
    let endash = B.endash
    let nbsp = B.nbsp
    let img_elem = B.img_elem
    let br_elem = B.br_elem
    let chars = B.chars

    let strong_elem att c = B.strong_elem att (List.map B.phrasing c)
    let em_elem att c = B.em_elem att (List.map B.phrasing c)
    let tt_elem att c = B.tt_elem att (List.map B.phrasing c)
    let monospace_elem att c = B.monospace_elem att (List.map B.phrasing c)
    let underlined_elem att c =
      B.underlined_elem att (List.map B.phrasing c)
    let linethrough_elem att c =
      B.linethrough_elem att (List.map B.phrasing c)
    let subscripted_elem att c =
      B.subscripted_elem att (List.map B.phrasing c)
    let superscripted_elem att c =
      B.superscripted_elem att (List.map B.phrasing c)

    let p_elem att c = B.p_elem att (List.map B.phrasing c)
    let h1_elem att c = B.h1_elem att (List.map B.phrasing c)
    let h2_elem att c = B.h2_elem att (List.map B.phrasing c)
    let h3_elem att c = B.h3_elem att (List.map B.phrasing c)
    let h4_elem att c = B.h4_elem att (List.map B.phrasing c)
    let h5_elem att c = B.h5_elem att (List.map B.phrasing c)
    let h6_elem att c = B.h6_elem att (List.map B.phrasing c)
    let section_elem att c = B.section_elem att (List.map B.flow c)

    let map_item (a,b,c) = (List.map B.phrasing a, b, c)
    let ul_elem att l = B.ul_elem att (List.map map_item l)
    let ol_elem att l = B.ol_elem att (List.map map_item l)

    let map_def (a,b,c) = (a, List.map B.phrasing b, c)
    let dl_elem att l = B.dl_elem att (List.map map_def l)

    let map_td (a,b,c) = (a,b,List.map B.phrasing c)
    let map_tr (a, b) = (List.map map_td a, b)
    let table_elem att l = B.table_elem att (List.map map_tr l)

    let phrasing x = x
    let flow x = x
    let a_elem_phrasing = B.ignore_a_elem_phrasing
    let a_elem_flow = B.ignore_a_elem_flow

    type plugin_content =
      [ `Flow5_link of (href * Wikicreole.attribs * flow_without_interactive)
      | `Phrasing_link of (href * Wikicreole.attribs * phrasing_without_interactive)
      | `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

    let default_ni_plugin ~name bi attribs content =
      `Phrasing_without_interactive
        (B.default_ni_extension ~name bi attribs content)

    let plugin_resolver = plugin_resolver
    let plugin name =
      try
        match Hashtbl.find plugin_assoc name with
        | SimplePlugin (_, Some ni_plugin), _ -> (None, ni_plugin)
        | SimplePlugin (_, None), _ -> (None, default_ni_plugin ~name)
        | WikiPlugin p, _ ->
            let module Plugin = (val p: WikiPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             (fun bi attribs content ->
               let bi = Plugin.update_context bi attribs in
               let xml =
                 Option.map
                   (xml_of_wiki (cast_niwp Plugin.wikiparser) bi)
                   (Option.map remove_spaces content)
               in
               match Plugin.ni_plugin with
               | Some f -> f bi attribs xml
               | None -> default_ni_plugin ~name bi attribs content))
        | LinkPlugin p, _ ->
            let module Plugin = (val p: LinkPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             default_ni_plugin ~name)
        | RawWikiPlugin p, _ ->
            let module Plugin = (val p: RawWikiPlugin) in
            (Some (get_plugin_resolver Plugin.wikiparser),
             (fun bi attribs content ->
               match Plugin.ni_plugin with
               | Some f -> f (cast_niwp Plugin.wikiparser) bi attribs content
               | None -> default_ni_plugin ~name bi attribs content))
      with Not_found ->
        (Some plugin_resolver, default_ni_plugin ~name)

    let plugin = (plugin :> _ -> (_ * (_ -> _ -> _ -> plugin_content)))

    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
    let href_action _ _ _ _ _ = ()
  end

  let non_interactive_builder =
    (module NonInteractiveBuilder
       : Wikicreole.Builder with type param = Wiki_widgets_interface.box_info
                            and type flow = B.flow_without_interactive)

  let from_string_without_interactive ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb non_interactive_builder content

  (** Used to build the Preparser and Desugarer. *)
  module UnitBuilder = struct

    type href = string
    type phrasing_without_interactive = unit
    type phrasing = unit
    type flow = unit
    type flow_without_interactive = unit
    type uo_list = unit

    let nothing _ _ = ()
    let nothing1 _ = ()
    let chars = nothing1
    let strong_elem = nothing
    let em_elem = nothing
    let a_elem_phrasing _ _ _ = ()
    let a_elem_flow _ _ _ = ()
    let br_elem = nothing1
    let img_elem _ _ _ = ()
    let tt_elem = nothing
    let monospace_elem = nothing
    let underlined_elem = nothing
    let linethrough_elem = nothing
    let subscripted_elem = nothing
    let superscripted_elem = nothing
    let nbsp = ()
    let endash = ()
    let emdash = ()
    let p_elem = nothing
    let pre_elem = nothing
    let h1_elem = nothing
    let h2_elem = nothing
    let h3_elem = nothing
    let h4_elem = nothing
    let h5_elem = nothing
    let h6_elem = nothing
    let section_elem = nothing
    let ul_elem = nothing
    let ol_elem = nothing
    let dl_elem = nothing
    let list = nothing1
    let flow = nothing1
    let hr_elem = nothing1
    let table_elem = nothing
    let phrasing = nothing1
    let error = nothing1

    let make_href _ a fragment = match fragment with
      | None -> a
      | Some f -> a ^"#"^f

    type plugin_content =
      [ `Flow5_link of (href * Wikicreole.attribs * flow_without_interactive)
      | `Phrasing_link of (href * Wikicreole.attribs * phrasing_without_interactive)
      | `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

    let plugin_resolver = plugin_resolver
    let plugin name =
      let wiki_content =
        try match Hashtbl.find plugin_assoc name with
          | SimplePlugin _,_ -> None
          | RawWikiPlugin _, _ -> Some plugin_resolver
          | WikiPlugin p,_ ->
              let module WikiPlugin = (val p: WikiPlugin) in
              Some (get_plugin_resolver WikiPlugin.wikiparser)
          | LinkPlugin p,_ ->
              let module LinkPlugin = (val p: LinkPlugin) in
              Some (get_plugin_resolver LinkPlugin.wikiparser)
        with Not_found -> Some plugin_resolver
      in
      (wiki_content,
       (fun _ _ _ -> `Phrasing_without_interactive ()))

  end

  (* Type of the substitutions collected by [desugar_string] and [preparse_string]. *)
  type substitutions = (int * int * string option Lwt.t) list ref

  let link_action_ref = ref (fun _ _ _ _ -> Lwt.return None)
  let href_action_ref = ref (fun _ _ _ _ -> Lwt.return None)

  let preparser =
    let module Preparser = struct
      type param = substitutions * Wiki_types.wikibox
      include UnitBuilder

      let plugin_action name start end_ (subst, wb) attribs content =
        try
          let plugin, preparser = Hashtbl.find plugin_assoc name in
          let content' =
            lwt content' = match plugin with
              | SimplePlugin _ -> Lwt.return content
              | WikiPlugin p ->
                ( let module Plugin = (val p: WikiPlugin) in
                  match content with
                  | None -> Lwt.return None
                  | Some content ->
                      lwt content =
                        preparse_string Plugin.wikiparser wb content in
                      Lwt.return (Some content) )
              | LinkPlugin p ->
                ( let module Plugin = (val p: LinkPlugin) in
                  match content with
                  | None -> Lwt.return None
                  | Some content ->
                      lwt content =
                        preparse_string Plugin.wikiparser wb content in
                      Lwt.return (Some content) )
              | RawWikiPlugin p ->
                ( let module Plugin = (val p: RawWikiPlugin) in
                  match content with
                  | None -> Lwt.return None
                  | Some content ->
                      lwt content =
                        preparse_string Plugin.wikiparser wb content in
                      Lwt.return (Some content) )
            in
            match preparser with
            | None ->
              ( match content, content' with
                | None, None -> Lwt.return None
                | Some content, Some content' when content' == content -> Lwt.return None
                | _, _ -> Lwt.return (Some (string_of_extension name attribs content')) )
            | Some preparser -> preparser wb attribs content'
          in
          subst := (start, end_, content') :: !subst
        with _ (* was Not_found *) -> ()

      let link_action addr fragment attribs (start, end_) (subst, params) =
        subst := (start,
                  end_,
                  try !link_action_ref addr fragment attribs params
                  with _ -> Lwt.return None) ::!subst

      let href_action addr fragment attribs (start, end_) (subst, params) =
        subst := (start,
                  end_,
                  try !href_action_ref addr fragment attribs params
                  with _ -> Lwt.return None) ::!subst


    end in
    (module Preparser : Wikicreole.Builder
      with type param = substitutions * Wiki_types.wikibox
      and type flow = unit)

  let normalize_href_ref = ref normalize_link

  let desugarer =
    let module Desugarer = struct
      type param = substitutions * Wiki_syntax_types.desugar_param
      include UnitBuilder

      let plugin_action : string -> int -> int -> (param, unit) Wikicreole.plugin =
        fun name start end_ (subst, wb) attribs content ->
        let desugar_attributes () =
          lwt attribs' =
            let f = function
              | "item", it ->
                  lwt it' =
                    let link, text =
                      try String.sep '|' it
                      with Not_found -> it, it
                    in
                    match_lwt !normalize_href_ref (0,0) link None wb with
                      | Some link' ->
                          Lwt.return (link'^"|"^text)
                      | None -> Lwt.return it
                  in
                  Lwt.return ("item", it')
              | x -> Lwt.return x
            in
            Lwt_list.map_s f attribs
          in
          Lwt.return (
            if attribs' <> attribs then
              Some (string_of_extension name attribs' content)
            else
              None
          )
        in
        let desugar_content desugar_string_with_parser =
          match content with
            | None ->
                Lwt.return None
            | Some content ->
                lwt content' = desugar_string_with_parser wb content in
                Lwt.return (
                  if content' <> content then
                    Some (string_of_extension name attribs (Some content'))
                  else None
                )
        in
        try
          let plugin, preparser = Hashtbl.find plugin_assoc name in
          let content' =
            match plugin with
              | SimplePlugin _ ->
                  desugar_attributes ()
              | WikiPlugin p ->
                  desugar_content (let module Plugin = (val p: WikiPlugin) in desugar_string Plugin.wikiparser)
              | LinkPlugin p ->
                  desugar_content (let module Plugin = (val p: LinkPlugin) in desugar_string Plugin.wikiparser)
              | RawWikiPlugin p ->
                  desugar_content (let module Plugin = (val p: RawWikiPlugin) in desugar_string Plugin.wikiparser)
          in
          subst := (start, end_, content') :: !subst
        with _ (* was Not_found *) -> ()

      let link_action : string -> string option -> _ -> int * int -> param -> unit =
        fun _ _ _ _ _ -> ()

      let href_action : string -> string option -> _ -> int * int -> param -> unit =
        fun addr fragment attribs ((start, end_) as pos) (subst, wikipage) ->
          subst := (start,
                    end_,
                    try !normalize_href_ref pos addr fragment wikipage
                    with _ -> Lwt.return None) ::!subst
    end in
    (module Desugarer : Wikicreole.Builder
      with type param = substitutions * Wiki_syntax_types.desugar_param
      and type flow = unit)

  let apply_subst subst content =
    let buf = Buffer.create 1024 in
    Lwt_list.fold_left_s
      (fun pos (start, end_, replacement) ->
        replacement >>=
          function None -> Lwt.return pos;
            | Some replacement ->
              Buffer.add_substring buf content pos (start - pos);
              Buffer.add_string buf replacement;
              Lwt.return end_)
      0
      subst
    >>= fun pos ->
    if pos < String.length content then
      Buffer.add_substring buf content pos (String.length content - pos);
    Lwt.return (Buffer.contents buf)

  let with_actions ?href_action ?link_action f =
    (* No mutex required: the "lexer" do not cooperate and any access
       to the reference take place before the call to [apply_subst] *)
    let old_link_action = !link_action_ref in
    let old_href_action = !href_action_ref in
    (match link_action with Some f -> link_action_ref := f | None -> ());
    (match href_action with Some f -> href_action_ref := f | None -> ());
    let res = f () in
    link_action_ref := old_link_action;
    href_action_ref := old_href_action;
    res

  let desugar_string ?href_action ?link_action wb content =
    with_actions ?href_action ?link_action
      (fun () ->
         let subst = ref [] in
         ignore (Wikicreole.from_string (subst, wb) desugarer content : unit list);
         apply_subst (List.rev !subst) content)

  let preparse_string ?href_action ?link_action wb content =
    with_actions ?href_action ?link_action
      (fun () ->
         let subst = ref [] in
         ignore (Wikicreole.from_string (subst, wb) preparser content : unit list);
         apply_subst (List.rev !subst) content)

end

let make_href bi addr fragment =
  let wiki_page_aux ~fragment https wiki page =
    match Wiki_self_services.find_servpage wiki with
      | Some servpage ->
        let addr =
          Url.remove_slash_at_beginning
            (Neturl.split_path page)
        in
        Service_href (service_href ?fragment ?https servpage addr)
      (* Eliom_registration.Html5.make_string_uri ?https
         ?fragment ~service:servpage addr *)
      | None -> String_href "malformed link" (*VVV ??? *)
  in
  let fix_csp_page_path path =
    Eliom_uri.reconstruct_relative_url_path
      (Eliom_request_info.get_csp_original_full_path ())
      path
  in
  match addr with
      Wiki_page (None, page, forceproto) ->
        let wiki,_ = bi.Wiki_widgets_interface.bi_page in
        wiki_page_aux ~fragment forceproto wiki page
    | Wiki_page (Some wiki, page, forceproto) ->
        wiki_page_aux ~fragment forceproto wiki page
    | Site (href, forceproto) ->
        String_href (* CCC could we find a service for the site ? *)
          (try
             let url = Neturl.url_of_string site_url_syntax href in
             let path = Neturl.url_path url in
             let path =
               Eliom_request_info.get_site_dir () @ Url.remove_slash_at_beginning path
             in
             match forceproto with
               | None ->
                   let path = fix_csp_page_path path in
                   Neturl.string_of_url (Neturl.modify_url ?fragment ~path url)
               | Some https ->
                   Eliom_content.Html5.F.make_proto_prefix https ^
                     (Neturl.string_of_url (Neturl.modify_url ?fragment ~path url))
           with
             Neturl.Malformed_URL -> "malformed link")
    | Href (href, forceproto) ->
        String_href
          (match forceproto with
               None ->
                 let path =
                   let open Neturl in
                   join_path (match split_path href with
                                 "" :: path -> fix_csp_page_path path
                               | path -> path)
                 in
                 path ^ get_map_option ~default:"" ~f:((^) "#") fragment
             | Some https ->
                 let path =
                   match Neturl.(url_path (url_of_string site_url_syntax href)) with
                       "" :: path -> path
                     | path ->
                         (* A relative href and an enforced protocoll: create the complete target
                            path by combining the current URL-path with the href *)
                         let prefix = List.rev (Eliom_request_info.get_current_full_path ()) in
                         let prefix = List.rev (match prefix with _ :: xs -> xs | [] -> []) in
                         prefix @ path
                 in
                 let url = Neturl.url_of_string site_url_syntax (Eliom_request_info.get_full_url ()) in
                 Eliom_content.Html5.F.make_proto_prefix https ^
                   Neturl.(string_of_url (modify_url ?fragment ~path url)))
    | Absolute addr ->
        String_href (addr ^ get_map_option ~default:"" ~f:((^) "#") fragment)


(********************************)
(* builders. Default functions: *)

let menu_make_href bi c fragment =
  (* Accept only simple page. Ignore fragment and anything else silently... *)
  try
    match link_kind c with
      | Wiki_page (Some wiki,page,None) ->
          String_href ("wiki(" ^ Wiki_types.string_of_wiki wiki ^ "):" ^ page)
      | _ -> String_href ""
  with Failure _ ->
    String_href c




(*******************************************)
(* Type information for predefined parser. *)

module FlowTypes = struct

  type res = Html5_types.flow5
  type res_without_interactive = Html5_types.flow5_without_interactive

  type text = Html5_types.phrasing
  type link_content = Html5_types.phrasing_without_interactive

  type list_item = [ `Ol | `Ul | `Em ]

end

module FlowWithoutHeaderFooterTypes = struct

  type res = Html5_types.flow5_without_header_footer
  type res_without_interactive = Html5_types.flow5_without_interactive_header_footer

  type text = Html5_types.phrasing
  type link_content = Html5_types.phrasing_without_interactive

  type list_item = [ `Ol | `Ul | `Em ]

end

module PhrasingTypes = struct

  type res = Html5_types.phrasing
  type res_without_interactive = Html5_types.phrasing_without_interactive

  type text = Html5_types.phrasing
  type link_content = Html5_types.phrasing_without_interactive

  type list_item = [ `Em ]

end

module MenuTypes = struct

  type res = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
  type res_without_interactive = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]

  type text = Html5_types.phrasing
  type link_content = Html5_types.phrasing_without_interactive

  type list_item = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]

end

module ButtonTypes = struct

  type res = [Html5_types.button_content | `PCDATA]
  type res_without_interactive = [Html5_types.button_content | `PCDATA]

  type text = [Html5_types.button_content | `PCDATA]
  type link_content = [Html5_types.button_content | `PCDATA]

  type list_item = [Html5_types.button_content | `PCDATA]

end

(********************************)
(* Predefined builders.         *)

module FlowBuilder = struct

  let chars s = Lwt.return [Html5.F.pcdata s]

  let strong_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.strong ?a r : [>`Strong] Html5.F.elt)]

  let em_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.em ?a r : [>`Em] Html5.F.elt)]

  let monospace_elem attribs content =
    (* No more tt in HTML5
       (fun attribs content ->
       let a = opt_list (parse_common_attribs attribs) in
       element content >|= List.flatten >|= fun r ->
       [(Html5.F.tt ?a r : [>`Tt] Html5.F.elt)]
       ) *)
    let a = Html5.F.a_class ["monospace"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.span ~a r : [>`Span] Html5.F.elt)]


  let underlined_elem attribs content =
    let a = Html5.F.a_class ["underlined"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.span ~a r : [>`Span] Html5.F.elt)]

  let linethrough_elem attribs content =
    let a = Html5.F.a_class ["linethrough"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.span ~a r : [>`Span] Html5.F.elt)]

  let subscripted_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.sub ?a r : [>`Sub] Html5.F.elt)]

  let superscripted_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.sup ?a r : [>`Sup] Html5.F.elt)]

  let a_elem_phrasing
      attribs addr
      (c : Html5_types.phrasing_without_interactive Html5.F.elt list Lwt.t list) =
    let a = parse_common_attribs ~classes:["ocsimore_phrasing_link"] attribs in
    Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
      match addr with
        | String_href addr ->
          [(Html5.F.Raw.a ~a:(Html5.F.a_href (Html5.F.Raw.uri_of_string addr) :: a) c
            :> Html5_types.phrasing Html5.F.elt)]
        | Service_href href ->
          [(a_link_of_href href ~a c :> Html5_types.phrasing Html5.F.elt)]

  let a_elem_flow attribs addr c =
    let a = parse_common_attribs ~classes:["ocsimore_flow_link"] attribs in
    Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
      match addr with
        | String_href addr ->
          [Html5.F.Raw.a ~a:(Html5.F.a_href (Html5.F.Raw.uri_of_string addr) :: a) c]
        | Service_href href ->
          [a_link_of_href href ~a c]

  let make_href =
    (fun bi c fragment ->
      try
        make_href bi (link_kind c) fragment
      with Failure _ ->
        String_href "???")

  let br_elem attribs =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(Html5.F.br ?a () : [>`Br] Html5.F.elt)]

  let img_elem attribs href alt =
    let a = opt_list (parse_common_attribs attribs) in
    let src = uri_of_href href (* CCC https ? *) in
    Lwt.return
      [(Html5.F.img ~src ~alt:alt ?a ()
          : [>`Img] Html5.F.elt)]

  let tt_elem attribs content =
    (* no more tt in HTML5
       (fun attribs content ->
       let a = opt_list (parse_common_attribs attribs) in
       element content >|= List.flatten >|= fun r ->
       [(Html5.F.tt ?a r : [>`Tt] Html5.F.elt)]) *)
    let a = Html5.F.a_class ["teletype"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.span ~a r : [>`Span] Html5.F.elt)]


  let nbsp = Lwt.return [(Html5.F.pcdata " " : [>`PCDATA] Html5.F.elt)]

  let endash = Lwt.return [(Html5.F.pcdata "–" : [>`PCDATA] Html5.F.elt)]

  let emdash = Lwt.return [(Html5.F.pcdata "—" : [>`PCDATA] Html5.F.elt)]

  let p_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.p ?a r : [>`P] Html5.F.elt)]

  let pre_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return
      [(Html5.F.pre ?a [Html5.F.pcdata (String.concat "" content)]
          : [>`Pre] Html5.F.elt)]

  let add_backref attribs r =
    if !Ocsimore_config.wiki_headings_backref then
      try
        let id = List.assoc "id" attribs in
        let open Html5.F in
        let a' = [a_class ["backref"]; a_href (Raw.uri_of_string ("#"^id))] in
        r @ [ pcdata " "; Raw.a ~a:a' [entity "#182"] ]
      with Not_found -> r
    else r

  let h1_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h1 ?a (add_backref attribs r) : [>`H1] Html5.F.elt)]

  let h2_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h2 ?a (add_backref attribs r) : [>`H2] Html5.F.elt)]

  let h3_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h3 ?a (add_backref attribs r) : [>`H3] Html5.F.elt)]

  let h4_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h4 ?a (add_backref attribs r) : [>`H4] Html5.F.elt)]

  let h5_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h5 ?a (add_backref attribs r) : [>`H5] Html5.F.elt)]

  let h6_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.h6 ?a (add_backref attribs r) : [>`H6] Html5.F.elt)]

  let section_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.section ?a
          (r :> Html5_types.section_content_fun Html5.F.elt list)
          : [>`Section] Html5.F.elt)]

  let ul_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
      [(Html5.F.ul ?a (r::rs) : [>`Ul] Html5.F.elt)]

  let ol_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
      [(Html5.F.ol ?a (r::rs) : [>`Ol] Html5.F.elt)]

  let dl_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    descr_builder content >|= fun r ->
      [(Html5.F.dl ?a r : [>`Dl] Html5.F.elt)]

  let hr_elem attribs =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(Html5.F.hr ?a () : [>`Hr] Html5.F.elt)]

  let tdh_builder (h, attribs, (c: Html5_types.phrasing Html5.F.elt list Lwt.t list)) =
    let a = opt_list (parse_table_cell_attribs attribs) in
    lwt r = element c >|= List.flatten in
    Lwt.return
      (if h
       then Html5.F.th ?a r
       else Html5.F.td ?a (r:>Html5_types.td_content_fun Html5.F.elt list))

  let tdh_builder =
    (tdh_builder (* opening types *)
       : _ * _ * Html5_types.phrasing Html5.F.elt list Lwt.t list -> _
     :> _ * _ * [< Html5_types.phrasing] Html5.F.elt list Lwt.t list -> _)

  let tr_builder (row, attribs) = match row with
    | [] -> Lwt.return (Html5.F.tr [Html5.F.td []])
    | x::xs ->
      let a = opt_list (parse_common_attribs attribs) in
    (*let a = opt_list (parse_table_row_attribs attribs) in*)
      lwt y = tdh_builder x in
      lwt ys = Lwt_list.map_s tdh_builder xs in
      Lwt.return (Html5.F.tr ?a (y::ys))

  let table_elem attribs l =
    let a = opt_list (parse_table_attribs attribs) in
    match l with
      | [] -> Lwt.return [Html5.F.table ?a (Html5.F.tr [Html5.F.td []]) []]
      | row::rows ->
        lwt row = tr_builder row in
        lwt rows = Lwt_list.map_s tr_builder rows in
        Lwt.return [(Html5.F.table ?a row rows : [>`Table] Html5.F.elt)]

  let error =
    (fun (s : string) ->
      Lwt.return [(Html5.F.strong [Html5.F.pcdata s] : [>`Strong] Html5.F.elt)])

  let span_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(Html5.F.span ?a r : [>`Span] Html5.F.elt)]

  let ignore_a_elem_phrasing attribs addr content = span_elem attribs content
  let ignore_a_elem_flow attribs addr content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(Html5.F.div ?a r : [>`Div] Html5.F.elt)]

  let default_extension ~name bi attribs content =
    let s = string_of_extension name attribs content in
     Lwt.return [Html5.F.pcdata s]
  let default_ni_extension = default_extension

end

module ReducedFlowBuilder = struct

  (* without image *)

  include FlowBuilder

  let img_elem _ _ _ =
    Lwt.return
      [Html5.F.em [Html5.F.pcdata "Images not enabled in this syntax"]]

end

module Reduced2FlowBuilder = struct

  (* no images, no titles, no tables, no lists,
     no subwikiboxes, no content, no objects *)

  include ReducedFlowBuilder

  let h1_elem = p_elem
  let h2_elem = p_elem
  let h3_elem = p_elem
  let h4_elem = p_elem
  let h5_elem = p_elem
  let h6_elem = p_elem
  let ul_elem _ _ =
    Lwt.return
      [Html5.F.em [Html5.F.pcdata "Lists not enabled in this syntax"]]
  let ol_elem _ _ =
    Lwt.return
      [Html5.F.em [Html5.F.pcdata "Lists not enabled in this syntax"]]
  let dl_elem _ _ =
    Lwt.return
      [Html5.F.em [Html5.F.pcdata "Lists not enabled in this syntax"]]
  let table_elem  _ _ =
    Lwt.return
          [Html5.F.em [Html5.F.pcdata "Tables not enabled in this syntax"]]
end

module PhrasingBuilder = struct

  (* no images, no titles, no tables, no lists,
     no subwikiboxes, no content, no objects,
     no paragraph, no pre, ... *)

  include Reduced2FlowBuilder

  let p_elem _ (c: PhrasingTypes.text Html5.F.elt list Lwt.t list) : PhrasingTypes.res_without_interactive  Html5.F.elt list Lwt.t =
    lwt l = Lwt_list.map_s (* Don't do this at home kids ! PC *)
      (fun x ->  lwt x = x in Lwt.return (Html5.F.totl (Html5.F.toeltl x))) c in
    Lwt.return (List.flatten l)
  let pre_elem _ _ =
         Lwt.return
          [Html5.F.em
             [Html5.F.pcdata "Blocks of code not enabled in this syntax"]]
  let h1_elem = span_elem
  let h2_elem = span_elem
  let h3_elem = span_elem
  let h4_elem = span_elem
  let h5_elem = span_elem
  let h6_elem = span_elem
  let section_elem = span_elem
  let hr_elem _ =
    Lwt.return
      [Html5.F.em
          [Html5.F.pcdata "Horizontal rules not enabled in this syntax"]]
  let table_elem _ _ =
    Lwt.return
      [Html5.F.em [Html5.F.pcdata "Tables not enabled in this syntax"]]

  let ignore_a_elem_flow = ignore_a_elem_phrasing

end

module MenuBuilder = struct

  include FlowBuilder

  let nothing _ _ = Lwt.return []
  and nothing1 _ = Lwt.return []

  let strong_elem = strong_elem
  let em_elem = em_elem
  let monospace_elem = monospace_elem
  let underlined_elem = underlined_elem
  let linethrough_elem = linethrough_elem
  let subscripted_elem = subscripted_elem
  let superscripted_elem = superscripted_elem
  let a_elem_phrasing = a_elem_phrasing
  let a_elem_flow _ _ _ = Lwt.return []
  let make_href = menu_make_href
  let br_elem = nothing1
  let p_elem = nothing
  let pre_elem = nothing
  let section_elem = nothing
  let ul_elem = nothing
  let ol_elem = nothing
  let dl_elem = nothing
  let hr_elem = nothing1
  let table_elem = nothing

  let ignore_a_elem_flow _ _ _ = Lwt.return []

end

module ButtonBuilder = struct

  include FlowBuilder

  let forbid0 s =
    Lwt.return [(Html5.F.em [Html5.F.pcdata (s ^ " not enabled in buttons")]
                   : [Html5_types.button_content | `PCDATA] Html5.F.elt)]

  let forbid1 s _ = forbid0 s
  let forbid2 s _ _ = forbid0 s
  let forbid3 s _ _ _ = forbid0 s

  let strong_elem = forbid2 "strong"
  let em_elem = forbid2 "em"
  let monospace_elem = forbid2 "monospace"
  let underlined_elem = forbid2 "underlined"
  let linethrough_elem = forbid2 "linethrough"
  let subscripted_elem = forbid2 "subscripted"
  let superscripted_elem = forbid2 "superscripted"
  let a_elem_phrasing = forbid3 "a_elem"
  let a_elem_flow = forbid3 "a_elem"
  let br_elem = forbid1 "br"
  let img_elem = forbid3 "img"
  let tt_elem = forbid2 "tt"
  let nbsp = forbid0 "nbsp"
  let endash = forbid0 "endash"
  let emdash = forbid0 "emdash"
  let p_elem = forbid2 "p_elem"
  let pre_elem = forbid2 "pre"
  let h1_elem = forbid2 "h1"
  let h2_elem = forbid2 "h2"
  let h3_elem = forbid2 "h3"
  let h4_elem = forbid2 "h4"
  let h5_elem = forbid2 "h5"
  let h6_elem = forbid2 "h6"
  let section_elem = forbid2 "section"
  let ul_elem = forbid2 "ul"
  let ol_elem = forbid2 "ol"
  let dl_elem = forbid2 "dl"
  let hr_elem = forbid1 "hr"
  let table_elem = forbid2 "table"
  let phrasing = forbid1 "phrasing"

  let ignore_a_elem_phrasing = forbid3 "a_elem"
  let ignore_a_elem_flow = forbid3 "a_elem"

end


(********************************)
(* Predefined builders.         *)


(* Default parser *)

module WikicreoleParser = MakeParser(struct
  include FlowTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include FlowBuilder
end)

let wikicreole_parser =
  (module WikicreoleParser
      : ExtParser with type res = WikicreoleParser.res
                  and type res_without_interactive =
                         WikicreoleParser.res_without_interactive
                  and type link_content = WikicreoleParser.link_content)


(* Default flow parser but types as flow5_without_header_footer *)

module WikicreoleParserWithoutHeaderFooter = MakeParser(struct
  include FlowWithoutHeaderFooterTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include FlowBuilder
end)

let wikicreole_parser_without_header_footer =
  (module WikicreoleParserWithoutHeaderFooter
      : ExtParser with type res = WikicreoleParserWithoutHeaderFooter.res
                  and type res_without_interactive =
                         WikicreoleParserWithoutHeaderFooter.res_without_interactive
                  and type link_content =
                        WikicreoleParserWithoutHeaderFooter.link_content)


(* Reduced parsers. *)

module ReducedWikicreoleParser0 = MakeParser(struct
  include FlowTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include FlowBuilder
end)

module ReducedWikicreoleParser1 = MakeParser(struct
  include FlowTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include ReducedFlowBuilder
end)

module ReducedWikicreoleParser2 = MakeParser(struct
  include FlowTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include Reduced2FlowBuilder
end)

let reduced_wikicreole_parser0 =
  (module ReducedWikicreoleParser0
      : ExtParser with type res = ReducedWikicreoleParser0.res
                  and type res_without_interactive =
                        ReducedWikicreoleParser0.res_without_interactive
                  and type link_content =
                        ReducedWikicreoleParser0.link_content)

let reduced_wikicreole_parser1 =
  (module ReducedWikicreoleParser1
      : ExtParser with type res = ReducedWikicreoleParser1.res
                  and type res_without_interactive =
                        ReducedWikicreoleParser1.res_without_interactive
                  and type link_content =
                        ReducedWikicreoleParser1.link_content)

let reduced_wikicreole_parser2 =
  (module ReducedWikicreoleParser2
      : ExtParser with type res = ReducedWikicreoleParser2.res
                  and type res_without_interactive =
                        ReducedWikicreoleParser2.res_without_interactive
                  and type link_content =
                        ReducedWikicreoleParser2.link_content)

(* Phrasing parser. *)

module PhrasingWikicreoleParser = MakeParser(struct
  include PhrasingTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include PhrasingBuilder
end)

let phrasing_wikicreole_parser =
  (module PhrasingWikicreoleParser
      : ExtParser with type res = PhrasingWikicreoleParser.res
                  and type res_without_interactive =
                         PhrasingWikicreoleParser.res_without_interactive
                  and type link_content =
                        PhrasingWikicreoleParser.link_content)
(* Menu builder *)

module MenuParser = MakeParser(struct
  include MenuTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include MenuBuilder
end)

let menu_parser =
  (module MenuParser
      : ExtParser with type res = MenuParser.res
                  and type res_without_interactive =
                         MenuParser.res_without_interactive
                  and type link_content = MenuParser.link_content)

(* Button builder *)

module ButtonParser = MakeParser(struct
  include ButtonTypes
  (* *)
  type flow = res Html5.F.elt list Lwt.t
  type flow_without_interactive =
      res_without_interactive Html5.F.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)
  type phrasing = text  Html5.F.elt list Lwt.t
  type phrasing_without_interactive = link_content Html5.F.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)
  type uo_list = list_item Html5.F.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)
  (* *)
  include ButtonBuilder
end)

let reduced_wikicreole_parser_button_content =
  (module ButtonParser
     : ExtParser with type res = [Html5_types.button_content | `PCDATA]
                 and type res_without_interactive =
                        [Html5_types.button_content | `PCDATA]
                 and type link_content =
                       [Html5_types.button_content | `PCDATA])

(********************************)
(* Predefined content types:    *)

let wikicreole_content_type =
  Wiki_models.register_flows_wiki_parser "wikicreole"
    (preprocess_extension (cast_wp wikicreole_parser))
    (xml_of_wiki (cast_wp wikicreole_parser))

let reduced_wikicreole_content_type0 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole0"
    (preprocess_extension (cast_wp reduced_wikicreole_parser0))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser0)
     :> Html5_types.flow5 Html5.F.elt list Wiki_models.wiki_parser)

let reduced_wikicreole_content_type1 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole1"
    (preprocess_extension (cast_wp reduced_wikicreole_parser1))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser1)
     :> Html5_types.flow5 Html5.F.elt list Wiki_models.wiki_parser)

let reduced_wikicreole_content_type2 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole2"
    (preprocess_extension (cast_wp reduced_wikicreole_parser2))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser2)
     :> Html5_types.flow5 Html5.F.elt list Wiki_models.wiki_parser)

let wikicreole_phrasing_content_type =
  Wiki_models.register_phrasings_wiki_parser "phrasing_wikicreole"
    (preprocess_extension (cast_wp phrasing_wikicreole_parser))
    (xml_of_wiki (cast_wp phrasing_wikicreole_parser))

(* For backward compatibility *)
let wikicreole_inline_content_type =
  Wiki_models.register_phrasings_wiki_parser "inline_wikicreole"
    (preprocess_extension (cast_wp phrasing_wikicreole_parser))
    (xml_of_wiki (cast_wp phrasing_wikicreole_parser))

let rawtext_content_type =
  Wiki_models.register_flows_wiki_parser "rawtext"
    Wiki_models.identity_preprocessor
    (fun _bi s -> Lwt.return [Html5.F.p [Html5.F.pcdata s]])



(*************************)
(** Registring extension *)

type (+'flow_without_interactive,
      +'phrasing_without_interactive) non_interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
      ('flow_without_interactive,
       'phrasing_without_interactive) ni_plugin_content) Wikicreole.plugin

type (+'flow,
      +'flow_without_interactive,
      +'phrasing_without_interactive) interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
     ('flow, 'flow_without_interactive,
      'phrasing_without_interactive) plugin_content) Wikicreole.plugin

type (+'without_interactive) link_simple_plugin =
    (Wiki_widgets_interface.box_info,
     href * Wikicreole.attribs * 'without_interactive Html5.F.elt list Lwt.t)
    Wikicreole.plugin

let register_simple_extension
    (type a) (type b) (type c)
    ~(wp: (a,b,c) ext_wikicreole_parser)
    ~name ?preparser ?ni_plugin plugin =
  let module Parser =
    (val wp : ExtParser with type res = a
                                and type res_without_interactive = b
                                and type link_content = c) in
  let open Parser in
  register_extension ~name ?preparser (SimplePlugin (plugin, ni_plugin))

(***** Registering to a group of parser. *)

let register_simple_flow_extension
    ~name ?(reduced = true) ?preparser
    (plugin:
       ([< Html5_types.flow5_without_interactive_header_footer],
        [< Html5_types.phrasing_without_interactive])
       non_interactive_simple_plugin)
    =
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser
    ~ni_plugin:
      (plugin :> WikicreoleParser.simple_ni_plugin)
    (plugin :> WikicreoleParser.simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:
      (plugin :> WikicreoleParserWithoutHeaderFooter.simple_ni_plugin)
    (plugin :> WikicreoleParserWithoutHeaderFooter.simple_plugin);
  if reduced then begin
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser0
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser0.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser0.simple_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser1
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser1.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser1.simple_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser2
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser2.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser2.simple_plugin)
    end

let register_interactive_simple_flow_extension
    ~name ?(reduced = true) ?preparser
    (plugin:
       (Html5_types.flow5_without_header_footer,
        Html5_types.flow5_without_interactive_header_footer,
        Html5_types.phrasing_without_interactive)
       interactive_simple_plugin) =
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser
    (plugin :> WikicreoleParser.simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    (plugin :> WikicreoleParserWithoutHeaderFooter.simple_plugin);
  if reduced then begin
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser0
      (plugin :> ReducedWikicreoleParser0.simple_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser1
      (plugin :> ReducedWikicreoleParser1.simple_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser2
      (plugin :> ReducedWikicreoleParser2.simple_plugin)
  end

let register_interactive_simple_flow_extension =
  (register_interactive_simple_flow_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (Html5_types.flow5_without_header_footer,
     Html5_types.flow5_without_interactive_header_footer,
     Html5_types.phrasing_without_interactive)
      interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< Html5_types.flow5_without_header_footer],
     [< Html5_types.flow5_without_interactive_header_footer],
     [< Html5_types.phrasing_without_interactive])
      interactive_simple_plugin -> unit)

let register_link_simple_flow_extension ~name ?reduced ?preparser plugin =
  let plugin wb attribs c = `Flow5_link (plugin wb attribs c) in
  register_interactive_simple_flow_extension ~name ?reduced ?preparser plugin

let register_simple_phrasing_extension
    ~name ?reduced ?preparser
    (plugin :
       (Html5_types.phrasing_without_interactive,
        Html5_types.phrasing_without_interactive)
       non_interactive_simple_plugin) =
  register_simple_flow_extension ~name ?reduced ?preparser
    (plugin :>
       (Html5_types.flow5_without_interactive_header_footer,
        Html5_types.phrasing_without_interactive)
       non_interactive_simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:phrasing_wikicreole_parser
    ~ni_plugin:
      (plugin :> PhrasingWikicreoleParser.simple_ni_plugin)
    (plugin :> PhrasingWikicreoleParser.simple_plugin)

let register_simple_phrasing_extension =
  (register_simple_phrasing_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (Html5_types.phrasing_without_interactive,
     Html5_types.phrasing_without_interactive)
      non_interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< Html5_types.phrasing_without_interactive],
     [< Html5_types.phrasing_without_interactive])
      non_interactive_simple_plugin -> unit)

let register_interactive_simple_phrasing_extension
    ~name ?reduced ?preparser
    (plugin :
       (Html5_types.phrasing,
        Html5_types.phrasing_without_interactive,
        Html5_types.phrasing_without_interactive)
       interactive_simple_plugin) =
  register_interactive_simple_flow_extension ~name ?reduced ?preparser
    (plugin :>
       (Html5_types.flow5_without_header_footer,
        Html5_types.flow5_without_interactive_header_footer,
        Html5_types.phrasing_without_interactive)
       interactive_simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:phrasing_wikicreole_parser
    (plugin :> PhrasingWikicreoleParser.simple_plugin)

let register_interactive_simple_phrasing_extension =
  (register_interactive_simple_phrasing_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (Html5_types.phrasing,
     Html5_types.phrasing_without_interactive,
     Html5_types.phrasing_without_interactive)
      interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< Html5_types.phrasing],
     [< Html5_types.phrasing_without_interactive],
     [< Html5_types.phrasing_without_interactive])
      interactive_simple_plugin -> unit)

let register_link_simple_phrasing_extension ~name ?reduced ?preparser plugin =
  let plugin wb attribs c = `Phrasing_link (plugin wb attribs c) in
  register_interactive_simple_flow_extension ~name ?reduced ?preparser plugin

(**** *)

type (-'content,
      +'flow_without_interactive,
      +'phrasing_without_interactive)
  wiki_plugin =
    Wiki_widgets_interface.box_info ->
      Wikicreole.attribs ->
      'content Html5.F.elt list Lwt.t option ->
    ('flow_without_interactive, 'phrasing_without_interactive) ni_plugin_content

let register_wiki_extension
    (type a) (type b) (type c) (type a') (type b') (type c')
    ~wp ~name ~wp_rec ?preparser ?(context = fun bi _ -> bi)
    ?(ni_plugin : (_,_,_) wiki_plugin option)
    (plugin : (_,_,_) wiki_plugin) =
  let module Parser = (val wp : ExtParser with type res = a
                                          and type res_without_interactive = b
                                          and type link_content = c) in
  let module Plugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'
    let wikiparser = wp_rec
    let update_context = context
    let plugin = (plugin :> rec_res Parser.wiki_plugin)
    let ni_plugin = (ni_plugin :> rec_res_without_interactive Parser.wiki_ni_plugin option)
  end in
  let open Parser in
  Parser.register_extension ~name ?preparser (WikiPlugin (module Plugin : WikiPlugin))

type (-'content,
      +'flow_without_interactive,
      +'phrasing_without_interactive)
  link_plugin =
    Wiki_widgets_interface.box_info ->
      Wikicreole.attribs ->
      'content Html5.F.elt list Lwt.t option ->
    ('flow_without_interactive, 'phrasing_without_interactive) link_plugin_content

let register_link_extension
    (type a) (type b) (type c) (type a') (type b') (type c')
    ~wp ~name ~wp_rec ?preparser
    ?(context = fun bi _ -> bi)
    (plugin : (_, _, _) link_plugin)  =
  let module Parser = (val wp : ExtParser with type res = a
                                          and type res_without_interactive = b
                                          and type link_content = c) in
  let module Plugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'
    let wikiparser = wp_rec
    let update_context = context
    let plugin = plugin
  end in
  let open Parser in
  register_extension ~name ?preparser (LinkPlugin (module Plugin : LinkPlugin))

let register_raw_wiki_extension
    (type a) (type b) (type c) (type a') (type b') (type c')
    ~wp ~name ~wp_rec
    ?preparser ?ni_plugin plugin =
  let module Parser = (val wp : ExtParser with type res = a
                                          and type res_without_interactive = b
                                          and type link_content = c) in
  let open Parser in
  let module Plugin : RawWikiPlugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'
    let wikiparser = wp_rec
    let plugin = plugin
    let ni_plugin = ni_plugin
  end in
  register_extension ~name ?preparser
    (RawWikiPlugin (module Plugin : RawWikiPlugin))

type wiki_flow_pplugin = {
  fpp: 'flow.
    ('flow Html5_types.between_flow5_and_flow5_without_interactive_header_footer,
     'flow,
     Html5_types.phrasing_without_interactive)
  wiki_plugin
}


let register_wiki_flow_extension
    ~name ?(reduced = true) ?preparser plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      ~ni_plugin:(plugin.fpp :> (FlowTypes.res_without_interactive,
                             FlowTypes.res_without_interactive, _) wiki_plugin)
      (plugin.fpp :> (FlowTypes.res, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name
    ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    ~ni_plugin:(plugin.fpp :> (FlowWithoutHeaderFooterTypes.res_without_interactive,
                           FlowWithoutHeaderFooterTypes.res_without_interactive, _)
                  wiki_plugin)
    (plugin.fpp :> (FlowWithoutHeaderFooterTypes.res,
                FlowWithoutHeaderFooterTypes.res, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
 end

type interactive_wiki_flow_pplugin = {
  ifpp: 'flow 'flow_without_interactive.
    (('flow, 'flow_without_interactive) Html5_types.between_flow5_and_flow5_without_header_footer,
     'flow,
     Html5_types.phrasing_without_interactive)
    wiki_plugin
}

let register_interactive_wiki_flow_extension
    ~name ?(reduced = true) ?preparser plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      (plugin.ifpp :> (FlowTypes.res, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name
    ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    (plugin.ifpp :>
       (FlowWithoutHeaderFooterTypes.res,
        FlowWithoutHeaderFooterTypes.res,
        _)
       wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
 end

type link_wiki_flow_pplugin = {
  lfpp: 'flow_without_interactive.
    Wiki_widgets_interface.box_info ->
      Wikicreole.attribs ->
      ([> Html5_types.flow5_without_interactive_header_footer] as 'flow_without_interactive)
        Html5.F.elt list Lwt.t option ->
      (href * Wikicreole.attribs * 'flow_without_interactive Html5.F.elt list Lwt.t)
}

let register_link_flow_extension ~name ?(reduced = true) ?preparser plugin =
  let plugin wb attribs c = `Flow5_link (plugin.lfpp wb attribs c) in
  let register wp =
    register_link_extension ~name ~wp ~wp_rec:wp ?preparser plugin
  in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
 end


type wiki_phrasing_pplugin = {
  ppp: 'phrasing 'phrasing_without_interactive.
    (('phrasing, 'phrasing_without_interactive)
       Html5_types.between_phrasing_and_phrasing_without_interactive
       ,
     'phrasing,
     Html5_types.phrasing_without_interactive)
    wiki_plugin
}

let register_wiki_phrasing_extension
    ~name ?(reduced = true) ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name ~wp_rec ?preparser ~wp
      ~ni_plugin:
      (plugin.ppp
         : (FlowTypes.link_content,
            FlowTypes.link_content, _) wiki_plugin
         :> (FlowTypes.link_content,
             FlowTypes.res_without_interactive, _) wiki_plugin)
      (plugin.ppp
         : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
         :> (FlowTypes.text, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:
    (plugin.ppp
       : (FlowTypes.link_content,
            FlowTypes.link_content, _) wiki_plugin
     :> (_, FlowWithoutHeaderFooterTypes.res_without_interactive, _) wiki_plugin)
    (plugin.ppp
       : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
     :> (_, FlowWithoutHeaderFooterTypes.res, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
  end;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser
    ~ni_plugin:plugin.ppp
    plugin.ppp

let register_interactive_wiki_phrasing_extension
    ~name ?(reduced = true) ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec ?preparser
      (plugin.ppp
         : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
       :> (_, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
      (plugin.ppp
         : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
       :> (_, FlowWithoutHeaderFooterTypes.res, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
  end;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser
    plugin.ppp

type link_wiki_phrasing_pplugin =
    Wiki_widgets_interface.box_info ->
    Wikicreole.attribs ->
    Html5_types.phrasing_without_interactive Html5.F.elt list Lwt.t option ->
    (href * Wikicreole.attribs * Html5_types.phrasing_without_interactive Html5.F.elt list Lwt.t)

let register_link_phrasing_extension ~name ?(reduced = true) ?preparser
    (plugin: link_wiki_phrasing_pplugin) =
  let plugin wb attribs c = `Phrasing_link (plugin wb attribs c) in
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_link_extension ~name ~wp ~wp_rec ?preparser plugin
  in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
  end;
  register phrasing_wikicreole_parser

(***** *)



(* Extensions: div; aside; article; nav; section; header; footer *)

let f_block make bi args content =
  `Flow5
    ( let a = Some (parse_common_attribs args) in
      match content with
      | None -> Lwt.return [make ?a []]
      | Some content ->
        lwt content = content in
        Lwt.return [make ?a content])

let () =
  let add_divs wp wp_rec =
    List.iter
      (fun (name, make, make') ->
         (* FIXME it won't type without duplicating the 'make'
            argument... *)
         register_wiki_extension ~wp ~name ~wp_rec
           ~ni_plugin:(f_block make')
           (f_block make))
      ["div", Html5.F.div, Html5.F.div;
       "aside", Html5.F.aside, Html5.F.aside;
       "article", Html5.F.article, Html5.F.article;
       "nav", Html5.F.nav, Html5.F.nav;
       "section", Html5.F.section, Html5.F.section;
      ] in
  add_divs wikicreole_parser wikicreole_parser;
  add_divs wikicreole_parser_without_header_footer wikicreole_parser;
  add_divs reduced_wikicreole_parser0 reduced_wikicreole_parser0;
  add_divs reduced_wikicreole_parser1 reduced_wikicreole_parser1;
  add_divs reduced_wikicreole_parser2 reduced_wikicreole_parser2

let () =
  List.iter
    (fun (name, make, make') ->
       (* FIXME it won't type without duplicating the 'make'
          argument... *)
       register_wiki_extension ~name
         ~wp:wikicreole_parser
         ~context:(fun bi _ -> { bi with bi_sectioning = false })
         ~wp_rec: wikicreole_parser_without_header_footer
         ~ni_plugin:(f_block make')
         (f_block make))
    ["header", Html5.F.header, Html5.F.header;
     "footer", Html5.F.footer, Html5.F.footer]

(* pre *)

let f_pre bi args content =
  `Flow5
    (lwt content = match content with
       | None -> Lwt.return []
       | Some c -> (c :> Html5_types.pre_content Html5.F.elt list Lwt.t)
     in
     let a = Some (parse_common_attribs args) in
     Lwt.return [Html5.F.pre ?a content])

let () =
  let register wp =
    register_wiki_extension ~wp ~wp_rec:phrasing_wikicreole_parser
      ~name:"pre" ~ni_plugin:f_pre f_pre in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  register reduced_wikicreole_parser0;
  register reduced_wikicreole_parser1;
  register reduced_wikicreole_parser2


(* span *)

let f_span bi args content =
  `Phrasing_without_interactive
    (lwt content = match content with
       | None -> Lwt.return []
       | Some c -> (c :> Html5_types.phrasing Html5.F.elt list Lwt.t)
     in
     let a = Some (parse_common_attribs args) in
     Lwt.return [(Html5.F.span ?a content : 'a Html5.F.elt)])

let () =
  register_wiki_phrasing_extension ~name:"span" { ppp = f_span }

(* wikiname *)

let f_wikiname bi _args _c =
  `Phrasing_without_interactive
    (let wid = bi.Wiki_widgets_interface.bi_wiki in
     lwt wiki_info = Wiki_sql.get_wiki_info_by_id wid in
     Lwt.return [Html5.F.pcdata wiki_info.wiki_descr])

let () =
  register_simple_phrasing_extension ~name:"wikiname" f_wikiname

(* Raw *)

let f_raw _bi args content =
  `Phrasing_without_interactive
    (let s = string_of_extension "raw" args content in
     Lwt.return [Html5.F.b [Html5.F.pcdata s]])

let () =
  register_simple_phrasing_extension ~name:"raw" f_raw

(* Empty *)

let f_empty _bi _args _c = `Flow5 (Lwt.return [])

let () =
  register_simple_phrasing_extension ~name:"" f_empty

(* Content *)

let f_content bi _args _c =
  `Flow5
    (match_lwt bi.Wiki_widgets_interface.bi_subbox bi.bi_sectioning bi.bi_menu_style with
     | None ->
         Lwt.return
           [Html5.F.div
               [Html5.F.strong [Html5.F.em [Html5.F.pcdata "<<content>>"]]]
           ]
     | Some subbox ->
         Lwt.return subbox)

let () =
  register_simple_extension ~wp:wikicreole_parser ~name:"content" f_content


let f_content_div bi _args _c =
  `Flow5
    (match_lwt bi.Wiki_widgets_interface.bi_subbox bi.bi_sectioning bi.bi_menu_style with
     | None ->
         Lwt.return
           [Html5.F.div
               [Html5.F.strong [Html5.F.em [Html5.F.pcdata "<<content>>"]]]
           ]
     | Some subbox ->
         Lwt.return [Html5.F.div subbox])

let () =
  register_simple_extension ~wp:wikicreole_parser_without_header_footer ~name:"content" f_content_div

(* menu *)

let f_menu bi args _c =
  let wiki_id = bi.Wiki_widgets_interface.bi_wiki in
  `Flow5
    (let classe =
       Html5.F.a_class
         (   "wikimenu"
             :: filter_raw [
               try Some (List.assoc "class" args) with Not_found -> None
             ])
     in
     let id =
       try Some (Html5.F.a_id (List.assoc "id" args))
       with Not_found -> None
     in
     let style =
       try Some (Html5.F.a_style (List.assoc "style" args))
       with Not_found -> None
     in
     let a = Some (classe :: (filter_raw [id; style])) in
     let f ?classe s =
       let link, text =
         try String.sep '|' s
         with Not_found -> s, s
       in
       lwt wiki_info = Wiki_sql.get_wiki_info_by_id wiki_id in
       lwt text2 =
         xml_of_wiki (cast_niwp phrasing_wikicreole_parser) bi text in
       let text2 : Html5_types.flow5 Html5.F.elt list =
         Html5.F.totl (Html5.F.toeltl text2) (* FIXME *) in
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
           | None   -> Html5.F.a_class ["wikimenu_current"]
           | Some c -> Html5.F.a_class ("wikimenu_current" :: c)
         in
         Lwt.return (Html5.F.li ~a:[classe] text2)
       else
         let link =
           let kind = try link_kind link with Failure _ -> Href ("???", None) in
           match make_href bi kind None with
             | String_href addr ->
                 Html5.F.Raw.a ~a:[Html5.F.a_href (Html5.F.Raw.uri_of_string addr)] text2
             | Service_href href -> a_link_of_href href ~a:[] text2
         in
         let link : Html5_types.flow5 Html5.F.elt =
           Html5.F.tot (Html5.F.toelt link) (* FIXME *) in
         let classe = apply_opt Html5.F.a_class classe in
         let a = apply_opt (fun x -> [x]) classe in
         Lwt.return (Html5.F.li ?a [link])
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
           [Html5.F.ul ?a [y]]
           | x::xs ->
             f ~classe:["wikimenu_first"] x >>= fun y ->
             mapf xs                        >|= fun ys ->
               [Html5.F.ul ?a (y::ys)]
    )

let () =
  register_simple_flow_extension ~name:"menu" f_menu

(* cond ; orcond *)

let rec eval_cond bi = function
  | ("error", "autherror") ->
    User_data.get_login_error () >>=
    Lwt_list.exists_s
      (fun e ->
        Lwt.return (e = User.BadPassword || e = User.BadUser))
  | ("ingroup", g) ->
    Lwt.catch
      (fun () ->
        User.get_user_by_name g >>= fun group ->
        User.in_group ~group ())
      (function _ -> Lwt.return false)
  | ("http_code", "404") ->
    Eliom_reference.get Wiki_widgets_interface.page_displayable_eref >|=
      ((=) Wiki_widgets_interface.Page_404)
  | ("http_code", "403") ->
    Eliom_reference.get Wiki_widgets_interface.page_displayable_eref >|=
      ((=) Wiki_widgets_interface.Page_403)
  | ("http_code", "40?") ->
    Eliom_reference.get Wiki_widgets_interface.page_displayable_eref >|=
      ((=) Wiki_widgets_interface.Page_displayable)
  | ("page", page) ->
    Lwt.return
      (match snd (bi.bi_page) with
        | None -> false
        | Some path -> (String.concat "/" path = page))
  | ("wiki", wiki) ->
    Lwt.return (Wiki_types.string_of_wiki (fst (bi.bi_page)) = wiki)
  | (err, value) when String.length err >= 3 &&
      String.sub err 0 3 = "not" ->
    let not_cond =
      (String.sub err 3 (String.length err - 3), value)
    in
    eval_cond bi not_cond >|= not
  | _ -> Lwt.return false

let f_cond bi args content =
  `Flow5
    (lwt cond = Lwt_list.for_all_p (eval_cond bi) args in
     match content, cond with
     | Some content, true -> content
     | _ -> Lwt.return [])

let f_orcond bi args content =
  `Flow5
    (lwt cond = Lwt_list.exists_p (eval_cond bi) args in
     match content, cond with
     | Some content, true -> content
     | _ -> Lwt.return [])

let () =
  register_wiki_flow_extension ~name:"cond" { fpp = f_cond };
  register_wiki_flow_extension ~name:"orcond" { fpp = f_orcond };
  let add_cond wp =
    register_wiki_extension ~wp ~name:"cond" ~wp_rec:wp f_cond;
    register_wiki_extension ~wp ~name:"orcond" ~wp_rec:wp f_orcond in
  add_cond menu_parser;
  add_cond phrasing_wikicreole_parser

(* sectioning *)

let f_sectioning bi attribs content =
  `Flow5
    (match content with
     | None -> Lwt.return []
     | Some c -> c)

let () =
  let add_sectioning wp =
    register_wiki_extension ~name:"sectioning"
      ~wp ~wp_rec:wp
      ~context:(fun bi _ -> { bi with bi_sectioning = true; })
      ~ni_plugin:f_sectioning
      f_sectioning;
    register_wiki_extension ~name:"nosectioning"
      ~wp ~wp_rec:wp
      ~context:(fun bi _ -> { bi with bi_sectioning = false; })
      ~ni_plugin:f_sectioning
      f_sectioning;
  in
  add_sectioning wikicreole_parser;
  add_sectioning wikicreole_parser_without_header_footer;
  add_sectioning reduced_wikicreole_parser0;
  add_sectioning reduced_wikicreole_parser1
  (* no headings in reduced_wikicreole_parser2 *)
