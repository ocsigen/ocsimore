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

open Eliom_pervasives
open Ocsimore_lib
open Wiki_types
open Wiki_syntax_types
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
    try Some (HTML5.M.a_class (String.split ',' (List.assoc "class" attribs) @ unopt ~def:[] classes))
    with Not_found -> map_option HTML5.M.a_class classes
  and at2 =
    try Some (HTML5.M.a_id (List.assoc "id" attribs))
    with Not_found -> None
  and at3 =
    try Some (HTML5.M.a_style (List.assoc "style" attribs))
    with Not_found -> None
  in
  filter_raw [at1; at2; at3]

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

let item_builder
    ((c : HTML5_types.phrasing HTML5.M.elt list Lwt.t list), l, attribs) =
  let a = opt_list (parse_common_attribs attribs) in
  lwt r = element c >|= List.flatten in
  lwt l = unopt ~def:(Lwt.return []) l in
  Lwt.return
    (HTML5.M.li ?a ((r :> HTML5_types.li_content_fun HTML5.M.elt list)
		    @ (l :> HTML5_types.li_content_fun HTML5.M.elt list)))

let item_builder =
  (item_builder (* opening types *)
     : (HTML5_types.phrasing HTML5.M.elt list Lwt.t list * _ * _ -> _)
     :> ([< HTML5_types.phrasing ] HTML5.M.elt list Lwt.t list * _ * _ -> _))

let list_builder xs = match xs with
  | [] -> Lwt.return (HTML5.M.li [], [])
  | x :: xs ->
      lwt y = item_builder x in
      lwt ys = Lwt_list.map_s item_builder xs in
      Lwt.return (y, ys)

let ddt_builder
    (istitle, (d : HTML5_types.phrasing HTML5.M.elt list Lwt.t list), attribs) =
  let a = opt_list (parse_common_attribs attribs) in
  lwt d = element d in
  Lwt.return
    (if istitle
     then `Dt (HTML5.M.dt ?a ((List.flatten d :> HTML5_types.dt_content_fun HTML5.M.elt list)))
     else `Dd (HTML5.M.dd ?a ((List.flatten  d :> HTML5_types.dd_content_fun HTML5.M.elt list))))

let ddt_builder =
  (ddt_builder (* opening types *)
     : (_ * HTML5_types.phrasing HTML5.M.elt list Lwt.t list * _ -> _)
     :> (_ * [< HTML5_types.phrasing ] HTML5.M.elt list Lwt.t list * _ -> _))

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
	  | [] -> HTML5.M.dt [], []
	  | x::xs -> x,xs
	in
	let dd = match dd with
	  | [] -> HTML5.M.dd [], []
	  | x::xs -> x,xs
	in
	combine ((dt,dd)::acc) rest
  in
  lwt l = Lwt_list.map_s ddt_builder l in
  Lwt.return (combine [] l)

let phrasing (x : HTML5_types.phrasing HTML5.M.elt list) : HTML5_types.phrasing_without_interactive HTML5.M.elt list =
  [HTML5.M.span x]

type ('a,'b, 'kind, 'suff, 'reg, 'appl) wiki_service =
    ('a, unit,
     [< Eliom_services.get_service_kind] as 'kind,
     [< Eliom_services.suff] as 'suff,
     'b, unit,
     [< Eliom_services.registrable] as 'reg,
     [< Eliom_output.appl_service] as 'appl) Eliom_services.service

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
    let uri =
      Uri.uri_of_string (Eliom_uri.make_string_uri ?fragment ?https ~service param)
    let a_link ?a c =
      Eliom_output.Html5.a ~service ?a ?fragment ?https c param
  end in
  (module Href : Service_href)

let a_link_of_href href ?a c =
  let module Href = (val href : Service_href) in
  Href.a_link ?a c

let uri_of_href href =
  match href with
    | String_href s -> Uri.uri_of_string s
    | Service_href href ->
      let module Href = (val href : Service_href) in
      Href.uri

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



(** **)

type preparser = Wiki_syntax_types.preparser
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
   end : Parser with type res = b)


module type RawParser = sig

  include Wikicreole.RawBuilder
    with type param := Wiki_widgets_interface.box_info
    and type href := href

  val ignore_a_elem_phrasing :
    Wikicreole.attribs -> href ->
    phrasing_without_interactive list -> phrasing_without_interactive
  val ignore_a_elem_flow :
    Wikicreole.attribs -> href ->
    flow_without_interactive list -> flow_without_interactive

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

  type interactive_syntax_extension =
      (flow,
       (href * Wikicreole.attribs * flow_without_interactive),
       phrasing_without_interactive,
       (href * Wikicreole.attribs * phrasing_without_interactive))
	Wikicreole.ext_kind

  val default_non_interactive_plugin:
    string ->
    bool * (Wiki_widgets_interface.box_info, non_interactive_syntax_extension) Wikicreole.plugin

  val default_interactive_plugin:
    string ->
    bool * (Wiki_widgets_interface.box_info, interactive_syntax_extension) Wikicreole.plugin

end

module MakeParser(B: RawParser) :
  ExtParser with type res = B.flow
	    and type res_without_interactive = B.flow_without_interactive
	    and type link_content = B.phrasing_without_interactive
  = struct

  type res = B.flow
  type res_without_interactive = B.flow_without_interactive
  type link_content = B.phrasing_without_interactive

  type wikiparser = (res, res_without_interactive, link_content) ext_wikicreole_parser

  type interactive_syntax_extension = B.interactive_syntax_extension
  type non_interactive_syntax_extension = B.non_interactive_syntax_extension

  type non_interactive_plugin =
      (Wiki_widgets_interface.box_info,
       non_interactive_syntax_extension) Wikicreole.plugin
  type interactive_plugin =
      (Wiki_widgets_interface.box_info,
       interactive_syntax_extension) Wikicreole.plugin

  let interactive_plugin_assoc = Hashtbl.create 17
  let register_interactive_extension ~name ~wiki_content plugin =
    Hashtbl.add interactive_plugin_assoc name (wiki_content, plugin)

  let non_interactive_plugin_assoc = Hashtbl.create 17
  let register_non_interactive_extension ~name ~wiki_content plugin =
    Hashtbl.add non_interactive_plugin_assoc name (wiki_content, plugin)

  module InteractiveBuilder = struct
    include B
    type syntax_extension = interactive_syntax_extension
    type href' = href
    type href = href'
    type param = Wiki_widgets_interface.box_info

    let plugin name =
      try Hashtbl.find interactive_plugin_assoc name
      with Not_found -> default_interactive_plugin name

    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
  end

  module NonInteractiveBuilder = struct

    type flow = B.flow_without_interactive
    type flow_without_interactive = B.flow_without_interactive
    type phrasing_without_interactive = B.phrasing_without_interactive
    type phrasing = B.phrasing_without_interactive
    type syntax_extension =
        (flow, href * Wikicreole.attribs * flow_without_interactive,
         phrasing_without_interactive,
         href * Wikicreole.attribs * phrasing_without_interactive)
          Wikicreole.ext_kind
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

    let plugin name =
      ((try
	 Hashtbl.find non_interactive_plugin_assoc name
       with Not_found ->
	 B.default_non_interactive_plugin name)
	 : bool * (param, non_interactive_syntax_extension) Wikicreole.plugin
       :> bool * (param, syntax_extension) Wikicreole.plugin)

    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
  end

  let interactive_builder =
    (module InteractiveBuilder
       : Wikicreole.Builder with type param = Wiki_widgets_interface.box_info
			    and type flow = B.flow)

  let non_interactive_builder =
    (module NonInteractiveBuilder
       : Wikicreole.Builder with type param = Wiki_widgets_interface.box_info
			    and type flow = B.flow_without_interactive)

  let from_string ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb interactive_builder content

  let from_string_without_interactive ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb non_interactive_builder content

  let plugin_action_assoc = Hashtbl.create 17
  let register_subst ~name subst =
    Hashtbl.add plugin_action_assoc name subst

  let link_action_ref = ref (fun _ _ _ _ -> Lwt.return None)
  let set_link_subst action = link_action_ref := action

  module Preparser = struct

    type href = string
    type param = (int * int * string option Lwt.t) list ref * Wiki_types.wikibox
    type phrasing_without_interactive = unit
    type phrasing = unit
    type flow = unit
    type flow_without_interactive = unit
    type uo_list = unit

    type syntax_extension =
	(flow, href * Wikicreole.attribs * flow_without_interactive,
	 phrasing_without_interactive,
         href * Wikicreole.attribs * phrasing_without_interactive)
          Wikicreole.ext_kind

    let nothing _ _ = ()
    let nothing1 _ = ()
    let chars = nothing1
    let strong_elem = nothing
    let em_elem = nothing
    let a_elem_phrasing _ _ _ = ()
    let a_elem_flow _ _ _ = ()
    let make_href _ a fragment = match fragment with
      | None -> a
      | Some f -> a ^"#"^f
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
    let plugin name =
      let wiki_content =
        try fst (Hashtbl.find interactive_plugin_assoc name)
        with Not_found -> false
      in (wiki_content, (fun _ _ _ -> `Phrasing_without_interactive ()))
    let plugin_action name start end_ (subst, params) attribs content =
      subst := (start,
		end_,
		(try
                   (Hashtbl.find plugin_action_assoc) name params attribs content
		 with _ (* was Not_found *) -> Lwt.return None))::!subst
    let link_action addr fragment attribs (start, end_) (subst, params) =
      subst := (start,
		end_,
		try !(link_action_ref) addr fragment attribs params
		with _ -> Lwt.return None) ::!subst
    let error = nothing1
  end

  let preparser =
    (module Preparser : Wikicreole.Builder
      with type param = (int * int * string option Lwt.t) list ref *
                        Wiki_types.wikibox
      and type flow = unit)

  let preparse_string wb content =
    let subst = ref [] in
    ignore (Wikicreole.from_string (subst, wb) preparser content : unit list);
    let buf = Buffer.create 1024 in
    lwt pos =
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
	(List.rev !subst) in
    if pos < (String.length content) then
      Buffer.add_substring buf content pos (String.length content - pos);
    Lwt.return (Buffer.contents buf)

end

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
	  Service_href (service_href servpage addr)
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

let menu_make_href bi c fragment =
  (* Accept only simple page. Ignore fragment and anything else silently... *)
  match link_kind c with
  | Page (page, None) -> String_href page
  | Wiki_page (wiki,page,None) ->
      String_href ("wiki(" ^ Wiki_types.string_of_wiki wiki ^ "):" ^ page)
  | _ -> String_href ""




(*******************************************)
(* Type information for predefined parser. *)

module FlowTypes = struct

  type flow =
      HTML5_types.flow5 HTML5.M.elt list Lwt.t
  type flow_without_interactive =
      HTML5_types.flow5_without_interactive HTML5.M.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)

  type phrasing =
      HTML5_types.phrasing HTML5.M.elt list Lwt.t
  type phrasing_without_interactive =
      HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = [ `Ol | `Ul | `Em ] HTML5.M.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)

  type interactive_syntax_extension =
      (flow, href * Wikicreole.attribs * flow_without_interactive,
       phrasing_without_interactive,
       href * Wikicreole.attribs * phrasing_without_interactive)
        Wikicreole.ext_kind

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

end

module FlowWithoutHeaderFooterTypes = struct

  type flow =
      HTML5_types.flow5_without_header_footer HTML5.M.elt list Lwt.t
  type flow_without_interactive =
      HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)

  type phrasing =
      HTML5_types.phrasing HTML5.M.elt list Lwt.t
  type phrasing_without_interactive =
      HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = [ `Ol | `Ul ] HTML5.M.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)

  type interactive_syntax_extension =
      (flow, href * Wikicreole.attribs * flow_without_interactive,
       phrasing_without_interactive,
       href * Wikicreole.attribs * phrasing_without_interactive)
        Wikicreole.ext_kind

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

end

module PhrasingTypes = struct

  type flow =
      HTML5_types.phrasing HTML5.M.elt list Lwt.t
  type flow_without_interactive =
      HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)

  type phrasing =
      HTML5_types.phrasing HTML5.M.elt list Lwt.t
  type phrasing_without_interactive =
      HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)

  type interactive_syntax_extension =
      (flow, href * Wikicreole.attribs * flow_without_interactive,
       phrasing_without_interactive,
       href * Wikicreole.attribs * phrasing_without_interactive)
        Wikicreole.ext_kind

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

end

module MenuTypes = struct

  type flow = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] HTML5.M.elt list Lwt.t
  type flow_without_interactive = flow
  let flow x = (x: flow_without_interactive :> flow)

  type phrasing =
      HTML5_types.phrasing HTML5.M.elt list Lwt.t
  type phrasing_without_interactive =
      HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] HTML5.M.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)

  type interactive_syntax_extension =
      (flow, href * Wikicreole.attribs * flow_without_interactive,
       phrasing_without_interactive,
       href * Wikicreole.attribs * phrasing_without_interactive)
        Wikicreole.ext_kind

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

end

module ButtonTypes = struct

  type flow =
    [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
  type flow_without_interactive =
    [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
  let flow x = (x: flow_without_interactive :> flow)

  type phrasing =
    [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
  type phrasing_without_interactive =
    [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list =
    [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
  let list x = (x : uo_list :> flow_without_interactive)

  type interactive_syntax_extension =
      (flow, href * Wikicreole.attribs * flow_without_interactive,
       phrasing_without_interactive,
       href * Wikicreole.attribs * phrasing_without_interactive)
        Wikicreole.ext_kind

  type non_interactive_syntax_extension =
      [ `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive ]

end

(********************************)
(* Predefined builders.         *)

module FlowBuilder = struct

  let chars s = Lwt.return [HTML5.M.pcdata s]

  let strong_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.strong ?a r : [>`Strong] HTML5.M.elt)]

  let em_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.em ?a r : [>`Em] HTML5.M.elt)]

  let monospace_elem attribs content =
    (* No more tt in HTML5
       (fun attribs content ->
       let a = opt_list (parse_common_attribs attribs) in
       element content >|= List.flatten >|= fun r ->
       [(HTML5.M.tt ?a r : [>`Tt] HTML5.M.elt)]
       ) *)
    let a = HTML5.M.a_class ["monospace"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]


  let underlined_elem attribs content =
    let a = HTML5.M.a_class ["underlined"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]

  let linethrough_elem attribs content =
    let a = HTML5.M.a_class ["linethrough"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]

  let subscripted_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.sub ?a r : [>`Sub] HTML5.M.elt)]

  let superscripted_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.sup ?a r : [>`Sup] HTML5.M.elt)]

  let a_elem_phrasing
      attribs addr
      (c : HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t list) =
    let a = parse_common_attribs ~classes:["ocsimore_phrasing_link"] attribs in
    Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
      match addr with
	| String_href addr ->
	  [(HTML5.M.a ~a:(HTML5.M.a_href (Uri.uri_of_string addr) :: a) c
            :> HTML5_types.phrasing HTML5.M.elt)]
	| Service_href href ->
	  [(a_link_of_href href ~a c :> HTML5_types.phrasing HTML5.M.elt)]

  let a_elem_flow attribs addr c =
    let a = parse_common_attribs ~classes:["ocsimore_flow_link"] attribs in
    Lwt_list.map_s (fun x -> x) c >|= List.flatten >|= fun c ->
      match addr with
	| String_href addr ->
	  [HTML5.M.a ~a:(HTML5.M.a_href (Uri.uri_of_string addr) :: a) c]
	| Service_href href ->
	  [a_link_of_href href ~a c]

  let make_href =
    (fun bi c fragment ->
      make_href bi (link_kind c) fragment )

  let br_elem attribs =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(HTML5.M.br ?a () : [>`Br] HTML5.M.elt)]

  let img_elem attribs href alt =
    let a = opt_list (parse_common_attribs attribs) in
    let src = uri_of_href href (* CCC https ? *) in
    Lwt.return
      [(HTML5.M.img ~src ~alt:alt ?a ()
          : [>`Img] HTML5.M.elt)]

  let tt_elem attribs content =
    (* no more tt in HTML5
       (fun attribs content ->
       let a = opt_list (parse_common_attribs attribs) in
       element content >|= List.flatten >|= fun r ->
       [(HTML5.M.tt ?a r : [>`Tt] HTML5.M.elt)]) *)
    let a = HTML5.M.a_class ["teletype"] :: parse_common_attribs attribs in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.span ~a r : [>`Span] HTML5.M.elt)]


  let nbsp = Lwt.return [(HTML5.M.pcdata " " : [>`PCDATA] HTML5.M.elt)]

  let endash = Lwt.return [(HTML5.M.pcdata "–" : [>`PCDATA] HTML5.M.elt)]

  let emdash = Lwt.return [(HTML5.M.pcdata "—" : [>`PCDATA] HTML5.M.elt)]

  let p_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.p ?a r : [>`P] HTML5.M.elt)]

  let pre_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return
      [(HTML5.M.pre ?a [HTML5.M.pcdata (String.concat "" content)]
	  : [>`Pre] HTML5.M.elt)]

  let h1_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h1 ?a r : [>`H1] HTML5.M.elt)]

  let h2_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h2 ?a r : [>`H2] HTML5.M.elt)]

  let h3_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h3 ?a r : [>`H3] HTML5.M.elt)]

  let h4_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h4 ?a r : [>`H4] HTML5.M.elt)]

  let h5_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h5 ?a r : [>`H5] HTML5.M.elt)]

  let h6_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.h6 ?a r : [>`H6] HTML5.M.elt)]

  let section_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.section ?a
	  (r :> HTML5_types.section_content_fun HTML5.M.elt list)
	  : [>`Section] HTML5.M.elt)]

  let ul_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
      [(HTML5.M.ul ?a (r::rs) : [>`Ul] HTML5.M.elt)]

  let ol_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    list_builder content >|= fun (r,rs) ->
      [(HTML5.M.ol ?a (r::rs) : [>`Ol] HTML5.M.elt)]

  let dl_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    descr_builder content >|= fun r ->
      [(HTML5.M.dl ?a r : [>`Dl] HTML5.M.elt)]

  let hr_elem attribs =
    let a = opt_list (parse_common_attribs attribs) in
    Lwt.return [(HTML5.M.hr ?a () : [>`Hr] HTML5.M.elt)]

  let tdh_builder (h, attribs, (c: HTML5_types.phrasing HTML5.M.elt list Lwt.t list)) =
    let a = opt_list (parse_table_cell_attribs attribs) in
    lwt r = element c >|= List.flatten in
    Lwt.return
      (if h
       then HTML5.M.th ?a r
       else HTML5.M.td ?a (r:>HTML5_types.td_content_fun HTML5.M.elt list))

  let tdh_builder =
    (tdh_builder (* opening types *)
       : _ * _ * HTML5_types.phrasing HTML5.M.elt list Lwt.t list -> _
     :> _ * _ * [< HTML5_types.phrasing] HTML5.M.elt list Lwt.t list -> _)

  let tr_builder (row, attribs) = match row with
    | [] -> Lwt.return (HTML5.M.tr [HTML5.M.td []])
    | x::xs ->
      let a = opt_list (parse_common_attribs attribs) in
    (*let a = opt_list (parse_table_row_attribs attribs) in*)
      lwt y = tdh_builder x in
      lwt ys = Lwt_list.map_s tdh_builder xs in
      Lwt.return (HTML5.M.tr ?a (y::ys))

  let table_elem attribs l =
    let a = opt_list (parse_table_attribs attribs) in
    match l with
      | [] -> Lwt.return [HTML5.M.table ?a (HTML5.M.tr [HTML5.M.td []]) []]
      | row::rows ->
	lwt row = tr_builder row in
	lwt rows = Lwt_list.map_s tr_builder rows in
	Lwt.return [(HTML5.M.table ?a row rows : [>`Table] HTML5.M.elt)]

  let error =
    (fun (s : string) ->
      Lwt.return [(HTML5.M.strong [HTML5.M.pcdata s] : [>`Strong] HTML5.M.elt)])

  let span_elem attribs content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
      [(HTML5.M.span ?a r : [>`Span] HTML5.M.elt)]

  let default_interactive_plugin name =
    (true,
     (fun _ args content ->
       `Phrasing_without_interactive
	 (let s = string_of_extension name args content in
          Lwt.return [HTML5.M.pcdata s])))
  let default_non_interactive_plugin = default_interactive_plugin

  let ignore_a_elem_phrasing attribs addr content = span_elem attribs content
  let ignore_a_elem_flow attribs addr content =
    let a = opt_list (parse_common_attribs attribs) in
    element content >|= List.flatten >|= fun r ->
    [(HTML5.M.div ?a r : [>`Div] HTML5.M.elt)]


end

module ReducedFlowBuilder = struct

  (* without image *)

  include FlowBuilder

  let img_elem _ _ _ =
    Lwt.return
      [HTML5.M.em [HTML5.M.pcdata "Images not enabled in this syntax"]]

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
      [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]
  let ol_elem _ _ =
    Lwt.return
      [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]
  let dl_elem _ _ =
    Lwt.return
      [HTML5.M.em [HTML5.M.pcdata "Lists not enabled in this syntax"]]
  let table_elem  _ _ =
    Lwt.return
          [HTML5.M.em [HTML5.M.pcdata "Tables not enabled in this syntax"]]
end

module PhrasingBuilder = struct

  (* no images, no titles, no tables, no lists,
     no subwikiboxes, no content, no objects,
     no paragraph, no pre, ... *)

  include Reduced2FlowBuilder

  let p_elem _ (c: PhrasingTypes.phrasing list) : PhrasingTypes.flow_without_interactive =
    lwt l = Lwt_list.map_s (* Don't do this at home kids ! PC *)
      (fun x ->  lwt x = x in Lwt.return (HTML5.M.totl (HTML5.M.toeltl x))) c in
    Lwt.return (List.flatten l)
  let pre_elem _ _ =
         Lwt.return
          [HTML5.M.em
             [HTML5.M.pcdata "Blocks of code not enabled in this syntax"]]
  let h1_elem = span_elem
  let h2_elem = span_elem
  let h3_elem = span_elem
  let h4_elem = span_elem
  let h5_elem = span_elem
  let h6_elem = span_elem
  let section_elem = span_elem
  let hr_elem _ =
    Lwt.return
      [HTML5.M.em
          [HTML5.M.pcdata "Horizontal rules not enabled in this syntax"]]
  let table_elem _ _ =
    Lwt.return
      [HTML5.M.em [HTML5.M.pcdata "Tables not enabled in this syntax"]]

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
    Lwt.return [(HTML5.M.em [HTML5.M.pcdata (s ^ " not enabled in buttons")]
                   : [HTML5_types.button_content | `PCDATA] HTML5.M.elt)]

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
  include FlowBuilder
end)

let wikicreole_parser =
  (module WikicreoleParser
      : ExtParser with type res = FlowTypes.flow
	                  and type res_without_interactive =
				FlowTypes.flow_without_interactive
			 and type link_content =
				FlowTypes.phrasing_without_interactive)


(* Default flow parser but types as flow5_without_header_footer *)

module WikicreoleParserWithoutHeaderFooter = MakeParser(struct
  include FlowWithoutHeaderFooterTypes
  include FlowBuilder

  (* let section_elem attribs (content: flow list) = *)
    (* let a = opt_list (parse_common_attribs attribs) in *)
    (* element content >|= List.flatten >|= fun r -> *)
      (* [(HTML5.M.section ?a *)
	  (* (r :> HTML5_types.section_content_fun HTML5.M.elt list) *)
	  (* : [>`Section] HTML5.M.elt)] *)

  (* let ignore_a_elem_flow attribs addr (content : flow list) = *)
    (* let a = opt_list (parse_common_attribs attribs) in *)
    (* element content >|= List.flatten >|= fun r -> *)
    (* [(HTML5.M.div ?a r : [>`Div] HTML5.M.elt)] *)

end)

let wikicreole_parser_without_header_footer =
  (module WikicreoleParserWithoutHeaderFooter
      : ExtParser with type res = FlowWithoutHeaderFooterTypes.flow
	                  and type res_without_interactive =
				FlowWithoutHeaderFooterTypes.flow_without_interactive
			 and type link_content =
				FlowWithoutHeaderFooterTypes.phrasing_without_interactive)


(* Reduced parsers. *)

module ReducedWikicreoleParser0 = MakeParser(struct
  include FlowTypes
  include FlowBuilder
end)

module ReducedWikicreoleParser1 = MakeParser(struct
  include FlowTypes
  include ReducedFlowBuilder
end)

module ReducedWikicreoleParser2 = MakeParser(struct
  include FlowTypes
  include Reduced2FlowBuilder
end)

let reduced_wikicreole_parser0 =
  (module ReducedWikicreoleParser0
      : ExtParser with type res = FlowTypes.flow
	                  and type res_without_interactive =
				FlowTypes.flow_without_interactive
			 and type link_content =
				FlowTypes.phrasing_without_interactive)

let reduced_wikicreole_parser1 =
  (module ReducedWikicreoleParser1
      : ExtParser with type res = FlowTypes.flow
	                  and type res_without_interactive =
				FlowTypes.flow_without_interactive
			 and type link_content =
				FlowTypes.phrasing_without_interactive)

let reduced_wikicreole_parser2 =
  (module ReducedWikicreoleParser2
      : ExtParser with type res = FlowTypes.flow
	                  and type res_without_interactive =
				FlowTypes.flow_without_interactive
			 and type link_content =
				FlowTypes.phrasing_without_interactive)

(* Phrasing parser. *)

module PhrasingWikicreoleParser = MakeParser(struct
  include PhrasingTypes
  include PhrasingBuilder
end)

let phrasing_wikicreole_parser =
  (module PhrasingWikicreoleParser
      : ExtParser with type res = PhrasingTypes.flow
	                  and type res_without_interactive =
				PhrasingTypes.flow_without_interactive
			 and type link_content =
				PhrasingTypes.phrasing_without_interactive)

(* Menu builder *)

let menu_parser =
  (module MakeParser(struct
    include MenuTypes
    include MenuBuilder
  end) : ExtParser with type res = MenuTypes.flow
	                  and type res_without_interactive =
				MenuTypes.flow_without_interactive
			 and type link_content =
				MenuTypes.phrasing_without_interactive)

(* Button builder *)

let reduced_wikicreole_parser_button_content =
  (module MakeParser(struct
    include ButtonTypes
    include ButtonBuilder
  end) : ExtParser with type res =
     [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
		   and type res_without_interactive =
     [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t
		   and type link_content =
     [HTML5_types.button_content | `PCDATA] HTML5.M.elt list Lwt.t)


(********************************)
(* Default parser functions:    *)

let xml_of_wiki (type t) wp bi content =
  let module Parser =
    (val wp : Parser with type res = t HTML5.M.elt list Lwt.t) in
  let xml = Parser.from_string ~sectioning:bi.bi_sectioning bi content in
  element xml >|= List.flatten

let preparse_extension (type t) wp wb content =
  let module Parser = (val wp : Parser with type res = t) in
  Parser.preparse_string wb content

(********************************)
(* Predefined content types:    *)



let wikicreole_content_type =
  Wiki_models.register_flows_wiki_parser "wikicreole"
    (preparse_extension (cast_wp wikicreole_parser))
    (xml_of_wiki (cast_wp wikicreole_parser))

let reduced_wikicreole_content_type0 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole0"
    (preparse_extension (cast_wp reduced_wikicreole_parser0))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser0)
     :> HTML5_types.flow5 HTML5.M.elt list Wiki_models.wiki_parser)

let reduced_wikicreole_content_type1 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole1"
    (preparse_extension (cast_wp reduced_wikicreole_parser1))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser1)
     :> HTML5_types.flow5 HTML5.M.elt list Wiki_models.wiki_parser)

let reduced_wikicreole_content_type2 =
  Wiki_models.register_flows_wiki_parser "reduced_wikicreole2"
    (preparse_extension (cast_wp reduced_wikicreole_parser2))
    (xml_of_wiki (cast_wp reduced_wikicreole_parser2)
     :> HTML5_types.flow5 HTML5.M.elt list Wiki_models.wiki_parser)

let wikicreole_phrasing_content_type =
  Wiki_models.register_phrasings_wiki_parser "phrasing_wikicreole"
    (preparse_extension (cast_wp phrasing_wikicreole_parser))
    (xml_of_wiki (cast_wp phrasing_wikicreole_parser))

(* For backward compatibility *)
let wikicreole_inline_content_type =
  Wiki_models.register_phrasings_wiki_parser "inline_wikicreole"
    (preparse_extension (cast_wp phrasing_wikicreole_parser))
    (xml_of_wiki (cast_wp phrasing_wikicreole_parser))

let rawtext_content_type =
  Wiki_models.register_flows_wiki_parser "rawtext"
    (fun _ s -> Lwt.return s)
    (fun _bi s -> Lwt.return [HTML5.M.p [HTML5.M.pcdata s]])


(*********************************************************************)
(* Adding syntax extensions in predefined parsers:                   *)

type (+'flow_without_interactive,
      +'phrasing_without_interactive) non_interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
      [ `Flow5 of 'flow_without_interactive
      | `Phrasing_without_interactive of 'phrasing_without_interactive])
    Wikicreole.plugin

type (+'flow,
      +'flow_without_interactive,
      +'phrasing_without_interactive) interactive_simple_plugin =
    (Wiki_widgets_interface.box_info,
     ('flow,
      (href * Wikicreole.attribs *' flow_without_interactive),
      'phrasing_without_interactive,
      (href * Wikicreole.attribs * 'phrasing_without_interactive))
       Wikicreole.ext_kind) Wikicreole.plugin

type (+'without_interactive) link_simple_plugin =
    (Wiki_widgets_interface.box_info,
     href * Wikicreole.attribs * 'without_interactive)
    Wikicreole.plugin

let register_simple_extension
    (type a) (type b) (type c)
    ~(wp: (a,b,c) ext_wikicreole_parser)
    ~name ?preparser ?ni_plugin plugin =
  let module Parser =
    (val wp : ExtParser with type res = a
			        and type res_without_interactive = b
			        and type link_content = c) in

  Parser.register_interactive_extension ~name ~wiki_content:false plugin;
  iter_option
    (Parser.register_non_interactive_extension ~name ~wiki_content:false)
    ni_plugin;
  iter_option
    (Parser.register_non_interactive_extension ~name ~wiki_content:false)
    ni_plugin;
  iter_option (Parser.register_subst ~name) preparser

(***** Registering to a group of parser. *)

let register_simple_flow_extension
    ~name ?(reduced = true) ?preparser
    (plugin:
       ([< HTML5_types.flow5_without_interactive_header_footer] HTML5.M.elt list Lwt.t,
	[< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t)
       non_interactive_simple_plugin)
    =
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser
    ~ni_plugin:
      (plugin :> WikicreoleParser.non_interactive_plugin)
    (plugin :> WikicreoleParser.interactive_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:
      (plugin :> WikicreoleParserWithoutHeaderFooter.non_interactive_plugin)
    (plugin :> WikicreoleParserWithoutHeaderFooter.interactive_plugin);
  if reduced then begin
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser0
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser0.non_interactive_plugin)
      (plugin :> ReducedWikicreoleParser0.interactive_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser1
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser1.non_interactive_plugin)
      (plugin :> ReducedWikicreoleParser1.interactive_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser2
      ~ni_plugin:
        (plugin :> ReducedWikicreoleParser2.non_interactive_plugin)
      (plugin :> ReducedWikicreoleParser2.interactive_plugin)
    end

let register_interactive_simple_flow_extension
    ~name ?(reduced = true) ?preparser
    (plugin:
       (HTML5_types.flow5_without_header_footer HTML5.M.elt list Lwt.t,
	HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t )
       interactive_simple_plugin) =
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser
    (plugin :> WikicreoleParser.interactive_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    (plugin :> WikicreoleParserWithoutHeaderFooter.interactive_plugin);
  if reduced then begin
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser0
      (plugin :> ReducedWikicreoleParser0.interactive_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser1
      (plugin :> ReducedWikicreoleParser1.interactive_plugin);
    register_simple_extension ~name ?preparser
      ~wp:reduced_wikicreole_parser2
      (plugin :> ReducedWikicreoleParser2.interactive_plugin)
  end

let register_interactive_simple_flow_extension =
  (register_interactive_simple_flow_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (HTML5_types.flow5_without_header_footer HTML5.M.elt list Lwt.t,
     HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t )
      interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Lwt.t,
     [< HTML5_types.flow5_without_interactive_header_footer] HTML5.M.elt list Lwt.t,
     [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t )
      interactive_simple_plugin -> unit)

let register_link_simple_flow_extension ~name ?reduced ?preparser plugin =
  let plugin wb attribs c = `Flow5_link (plugin wb attribs c) in
  register_interactive_simple_flow_extension ~name ?reduced ?preparser plugin

let register_simple_phrasing_extension
    ~name ?reduced ?preparser
    (plugin :
       (HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
       non_interactive_simple_plugin) =
  register_simple_flow_extension ~name ?reduced ?preparser
    (plugin :>
       (HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
       non_interactive_simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:phrasing_wikicreole_parser
    ~ni_plugin:
      (plugin :> PhrasingWikicreoleParser.non_interactive_plugin)
    (plugin :> PhrasingWikicreoleParser.interactive_plugin)

let register_simple_phrasing_extension =
  (register_simple_phrasing_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
      non_interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t,
     [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t)
      non_interactive_simple_plugin -> unit)

let register_interactive_simple_phrasing_extension
    ~name ?reduced ?preparser
    (plugin :
       (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
       interactive_simple_plugin) =
  register_interactive_simple_flow_extension ~name ?reduced ?preparser
    (plugin :>
       (HTML5_types.flow5_without_header_footer HTML5.M.elt list Lwt.t,
	HTML5_types.flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
	HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
       interactive_simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:phrasing_wikicreole_parser
    (plugin :> PhrasingWikicreoleParser.interactive_plugin)

let register_interactive_simple_phrasing_extension =
  (register_interactive_simple_phrasing_extension
     : name:_ -> ?reduced:_ -> ?preparser:_ ->
    (HTML5_types.phrasing HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
      interactive_simple_plugin -> unit
    :> name:_ -> ?reduced:_ -> ?preparser:_ ->
    ([< HTML5_types.phrasing] HTML5.M.elt list Lwt.t,
     [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t,
     [< HTML5_types.phrasing_without_interactive] HTML5.M.elt list Lwt.t)
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
      'content option ->
      [ `Flow5 of 'flow_without_interactive
      | `Phrasing_without_interactive of 'phrasing_without_interactive]

type (-'content,
      +'flow_without_interactive,
      +'phrasing_without_interactive)
  link_plugin =
    Wiki_widgets_interface.box_info ->
      Wikicreole.attribs ->
      'content option ->
      [ `Flow5_link of (href * Wikicreole.attribs * 'flow_without_interactive)
      | `Phrasing_link of (href * Wikicreole.attribs * 'phrasing_without_interactive)]

let make_action (type a) (type b) (type c) ~name wp preparser =
  (fun wb attribs -> function
   | None -> begin
       match preparser with
       | Some action -> action wb attribs None
       | None -> Lwt.return (Some (string_of_extension name attribs None))
     end
   | Some c ->
       let module Parser =
	 (val wp : ExtParser
	  with type res = a
	  and type res_without_interactive = b
	  and type link_content = c) in
       lwt c = Parser.preparse_string wb c in
       match preparser with
       | Some action -> action wb attribs (Some c)
       | None ->
	   Lwt.return (Some (string_of_extension name attribs (Some c))))

let raw_register_wiki_extension
    (type a) (type b) (type c)
    ~wp ~name ?preparser ?ni_plugin plugin =
  let module Parser = (val wp : ExtParser with type res = a
						 and type res_without_interactive = b
						 and type link_content = c) in
  Parser.register_interactive_extension ~name ~wiki_content:true plugin;
  iter_option
    (Parser.register_non_interactive_extension ~name ~wiki_content:true)
    ni_plugin;
  iter_option (Parser.register_subst ~name) preparser

let register_wiki_extension
    ~wp ~name ~wp_rec ?preparser ?ni_plugin plugin =
  let plugin bi attribs c =
    let c = map_option remove_spaces c in
    plugin bi attribs (map_option (xml_of_wiki (cast_wp wp_rec) bi) c) in
  let ni_plugin =
    map_option
      (fun ni_plugin bi attribs c ->
	 let c = map_option remove_spaces c in
	 ni_plugin bi attribs (map_option (xml_of_wiki (cast_niwp wp_rec) bi) c))
      ni_plugin in
  let preparser =
    map_option (fun p -> make_action ~name wp_rec preparser) preparser in
  raw_register_wiki_extension ~wp ~name ?preparser ?ni_plugin
    (plugin :> (_, _, _) interactive_simple_plugin)

let register_link_extension ~wp ~name ~wp_rec ?preparser
    (plugin : (_, _, _) link_plugin)  =
  let plugin bi attribs c =
    let c = map_option remove_spaces c in
    plugin bi attribs (map_option (xml_of_wiki (cast_niwp wp_rec) bi) c) in
  let preparser =
    map_option (fun p -> make_action ~name wp_rec preparser) preparser in
  raw_register_wiki_extension ~wp ~name ?preparser
    (plugin :> _ -> _ -> _ -> (_, _, _, _) Wikicreole.ext_kind)

type wiki_flow_pplugin = {
  fpp: 'flow.
    ('flow HTML5_types.between_flow5_and_flow5_without_interactive_header_footer HTML5.M.elt list Lwt.t,
     'flow HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
  wiki_plugin
}


let register_wiki_flow_extension
    ~name ?(reduced = true) ?preparser plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      ~ni_plugin:(plugin.fpp :> (FlowTypes.flow_without_interactive,
			     FlowTypes.flow_without_interactive, _) wiki_plugin)
      (plugin.fpp :> (FlowTypes.flow, FlowTypes.flow, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name
    ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    ~ni_plugin:(plugin.fpp :> (FlowWithoutHeaderFooterTypes.flow_without_interactive,
			   FlowWithoutHeaderFooterTypes.flow_without_interactive, _)
		  wiki_plugin)
    (plugin.fpp :> (FlowWithoutHeaderFooterTypes.flow,
		FlowWithoutHeaderFooterTypes.flow, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
 end

type interactive_wiki_flow_pplugin = {
  ifpp: 'flow 'flow_without_interactive.
    (('flow, 'flow_without_interactive) HTML5_types.between_flow5_and_flow5_without_header_footer HTML5.M.elt list Lwt.t,
     'flow HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
    wiki_plugin
}

let register_interactive_wiki_flow_extension
    ~name ?(reduced = true) ?preparser plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      (plugin.ifpp :> (FlowTypes.flow, FlowTypes.flow, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name
    ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    (plugin.ifpp :>
       (FlowWithoutHeaderFooterTypes.flow,
	FlowWithoutHeaderFooterTypes.flow,
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
      ([> HTML5_types.flow5_without_interactive_header_footer] as 'flow_without_interactive)
	HTML5.M.elt list Lwt.t option ->
      (href * Wikicreole.attribs * 'flow_without_interactive HTML5.M.elt list Lwt.t)
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
       HTML5_types.between_phrasing_and_phrasing_without_interactive
       HTML5.M.elt list Lwt.t,
     'phrasing HTML5.M.elt list Lwt.t,
     HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)
    wiki_plugin
}

let register_wiki_phrasing_extension
    ~name ?(reduced = true) ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name~wp_rec ?preparser ~wp
      ~ni_plugin:
      (plugin.ppp
	 : (FlowTypes.phrasing_without_interactive,
	    FlowTypes.phrasing_without_interactive, _) wiki_plugin
         :> (FlowTypes.phrasing_without_interactive,
	     FlowTypes.flow_without_interactive, _) wiki_plugin)
      (plugin.ppp
	 : (FlowTypes.phrasing, FlowTypes.phrasing, _) wiki_plugin
         :> (FlowTypes.phrasing, FlowTypes.flow, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:
    (plugin.ppp
       : (FlowTypes.phrasing_without_interactive,
	    FlowTypes.phrasing_without_interactive, _) wiki_plugin
     :> (_, FlowWithoutHeaderFooterTypes.flow_without_interactive, _) wiki_plugin)
    (plugin.ppp
       : (FlowTypes.phrasing, FlowTypes.phrasing, _) wiki_plugin
     :> (_, FlowWithoutHeaderFooterTypes.flow, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
  end;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser
    ~ni_plugin:plugin.ppp
    (plugin.ppp :> _ -> _ -> _ -> (_, _, _, _) Wikicreole.ext_kind)

let register_interactive_wiki_phrasing_extension
    ~name ?(reduced = true) ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec ?preparser
      (plugin.ppp
	 : (FlowTypes.phrasing, FlowTypes.phrasing, _) wiki_plugin
       :> (_, FlowTypes.flow, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
      (plugin.ppp
	 : (FlowTypes.phrasing, FlowTypes.phrasing, _) wiki_plugin
       :> (_, FlowWithoutHeaderFooterTypes.flow, _) wiki_plugin);
  if reduced then begin
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2
  end;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser
    (plugin.ppp :> _ -> _ -> _ -> (_, _, _, _) Wikicreole.ext_kind)

type link_wiki_phrasing_pplugin =
    Wiki_widgets_interface.box_info ->
    Wikicreole.attribs ->
    HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t option ->
    (href * Wikicreole.attribs * HTML5_types.phrasing_without_interactive HTML5.M.elt list Lwt.t)

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

let f_block make sectioning wp bi args content =
  `Flow5
    ( let content = map_option remove_spaces content in
      let bi = { bi with bi_sectioning = bi.bi_sectioning && sectioning } in
      lwt content = match content with
	| None -> Lwt.return []
	| Some content -> xml_of_wiki wp bi content in
      let a = Some (parse_common_attribs args) in
      Lwt.return [make ?a content])

let () =
  let add_divs wp wp_rec =
    List.iter
      (fun (name, make, make') ->
	 (* FIXME it won't type without duplicating the 'make'
	    argument... *)
	 raw_register_wiki_extension ~wp ~name
	   ~ni_plugin:(f_block make' true (cast_niwp wp_rec))
	   (f_block make true (cast_wp wp_rec)))
      ["div", HTML5.M.div, HTML5.M.div;
       "aside", HTML5.M.aside, HTML5.M.aside;
       "article", HTML5.M.article, HTML5.M.article;
       "nav", HTML5.M.nav, HTML5.M.nav;
       "section", HTML5.M.section, HTML5.M.section;
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
       raw_register_wiki_extension ~name
	 ~wp:wikicreole_parser
	 ~ni_plugin:
	 (f_block make' false (cast_niwp wikicreole_parser_without_header_footer))
	 (f_block make false (cast_wp wikicreole_parser_without_header_footer)))
    ["header", HTML5.M.header, HTML5.M.header;
     "footer", HTML5.M.footer, HTML5.M.footer]

(* pre *)

let f_pre bi args content =
  `Flow5
    (lwt content = match content with
       | None -> Lwt.return []
       | Some c -> (c :> HTML5_types.pre_content HTML5.M.elt list Lwt.t)
     in
     let a = Some (parse_common_attribs args) in
     Lwt.return [HTML5.M.pre ?a content])

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
       | Some c -> (c :> HTML5_types.phrasing HTML5.M.elt list Lwt.t)
     in
     let a = Some (parse_common_attribs args) in
     Lwt.return [(HTML5.M.span ?a content : 'a HTML5.M.elt)])

let () =
  register_wiki_phrasing_extension ~name:"span" { ppp = f_span }

(* wikiname *)

let f_wikiname bi _args _c =
  `Phrasing_without_interactive
    (let wid = bi.Wiki_widgets_interface.bi_wiki in
     lwt wiki_info = Wiki_sql.get_wiki_info_by_id wid in
     Lwt.return [HTML5.M.pcdata wiki_info.wiki_descr])

let () =
  register_simple_phrasing_extension ~name:"wikiname" f_wikiname

(* Raw *)

let f_raw _bi args content =
  `Phrasing_without_interactive
    (let s = string_of_extension "raw" args content in
     Lwt.return [HTML5.M.b [HTML5.M.pcdata s]])

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
	   [HTML5.M.div
               [HTML5.M.strong [HTML5.M.em [HTML5.M.pcdata "<<content>>"]]]
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
	   [HTML5.M.div
               [HTML5.M.strong [HTML5.M.em [HTML5.M.pcdata "<<content>>"]]]
           ]
     | Some subbox ->
         Lwt.return [HTML5.M.div subbox])

let () =
  register_simple_extension ~wp:wikicreole_parser_without_header_footer ~name:"content" f_content_div

(* menu *)

let f_menu bi args _c =
  let wiki_id = bi.Wiki_widgets_interface.bi_wiki in
  `Flow5
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
     let style =
       try Some (HTML5.M.a_style (List.assoc "style" args))
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
       let text2 : HTML5_types.flow5 HTML5.M.elt list =
	 HTML5.M.totl (HTML5.M.toeltl text2) (* FIXME *) in
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
	 let link : HTML5_types.flow5 HTML5.M.elt =
	   HTML5.M.tot (HTML5.M.toelt link) (* FIXME *) in
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

let () =
  register_simple_flow_extension ~name:"menu" f_menu

(* cond ; orcond *)

let rec eval_cond bi = function
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

let f_sectioning bi_sectioning wp bi args content =
  `Flow5
    (match content with
     | None -> Lwt.return []
     | Some c ->
	 let content = remove_spaces c in
	 let bi = { bi with bi_sectioning } in
	 xml_of_wiki wp bi content)

let () =
  let add_sectioning wp =
    raw_register_wiki_extension ~name:"sectioning"
      ~wp
      ~ni_plugin:(f_sectioning true (cast_niwp wp))
      (f_sectioning true (cast_wp wp));
    raw_register_wiki_extension ~name:"nosectioning"
      ~wp
      ~ni_plugin:(f_sectioning false (cast_niwp wp))
      (f_sectioning false (cast_wp wp));
  in
  add_sectioning wikicreole_parser;
  add_sectioning wikicreole_parser_without_header_footer;
  add_sectioning reduced_wikicreole_parser0;
  add_sectioning reduced_wikicreole_parser1
  (* no headings in reduced_wikicreole_parser2 *)
