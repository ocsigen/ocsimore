(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2011
 * Grégoire Henry
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

open Eliom_pervasives

(* let (>>=) = Lwt.bind *)
(* let (>|=) m f = Lwt.map f m *)

exception Error of string
let error (msg:string) =
  Lwt.return [HTML5.M.div ~a:[HTML5.M.a_class ["error"]] [HTML5.M.pcdata msg]]

type menu_item =
   HTML5_types.a_content HTML5.M.elt list *
      (Eliom_services.get_service_kind,
       Eliom_services.registrable,
       Eliom_output.non_caml_service,
       HTML5_types.a_content HTML5.M.elt list)
      Eliom_tools_common.hierarchical_site_item

(** Parse menu in wiki syntax *)

(* Expected XML type *)

type menu =
  [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] HTML5.M.elt list

(* Build hierarchical menu from XML sequence *)

let link_regexp =
  Netstring_pcre.regexp "wiki\\((.*)\\):(.*)"

let is_a node = match XML.content node with
  | XML.Node ("a", _, _) -> true
  | _ -> false

let get_href node = match XML.content node with
  | XML.Node ("a", attribs, _) -> begin
    let a_href = List.find (fun a -> XML.aname a = "href") attribs in
    match XML.acontent a_href with
      | XML.AStr (_ , href) -> href
      | _ -> assert false
  end
  | _ -> assert false

let get_node_contents node = match XML.content node with
  | XML.Node (_, _, contents) -> contents
  | _ -> assert false

let rec get_headline_level node = match XML.content node with
    | XML.Node (name, _, _) when name = "h6" -> 6
    | XML.Node (name, _, _) when name = "h5" -> 5
    | XML.Node (name, _, _) when name = "h4" -> 4
    | XML.Node (name, _, _) when name = "h3" -> 3
    | XML.Node (name, _, _) when name = "h2" -> 2
    | XML.Node (name, _, _) when name = "h1" -> 1
    | _ -> assert false

let build_node ~create_service (contents: XML.elt list) tree =
  match contents with
  | [a] when is_a a -> begin
    let href = get_href a in
    let wiki, href =
      match Netstring_pcre.string_match link_regexp href 0 with
	| None -> None,href
	| Some result ->
	  let wikinum = Netstring_pcre.matched_group result 1 href in
	  Some (Wiki_types.wiki_of_sql (Int32.of_string wikinum)),
	  Netstring_pcre.matched_group result 2 href in
    (HTML5.M.totl (get_node_contents a),
     Eliom_tools_common.Site_tree
       (Eliom_tools_common.Main_page
	  (create_service ?wiki (Neturl.split_path href)), tree))
  end
  | contents ->
    (HTML5.M.totl contents,
     Eliom_tools_common.Site_tree (Eliom_tools_common.Not_clickable, tree))

let rec parse_nodes ~create_service link_stack tree_acc last nodes =
  match nodes with
  | [] -> get_up ~create_service 0 link_stack tree_acc last nodes
  | node :: nodes ->
    let lvl = get_headline_level node in
    if lvl > last then begin
      if lvl <> last + 1 then invalid_arg "parse_nodes";
      parse_nodes ~create_service (get_node_contents node :: link_stack) ([] :: tree_acc) lvl nodes
    end else if lvl = last then begin
      let last_tree = build_node ~create_service (List.hd link_stack) (List.rev (List.hd tree_acc)) in
      let link_stack = get_node_contents node :: List.tl link_stack in
      let tree_acc =
	[] :: (last_tree :: List.hd (List.tl tree_acc)) :: List.tl (List.tl  tree_acc) in
      parse_nodes ~create_service link_stack tree_acc lvl nodes
    end else (* if lvl < last then *) begin
      get_up ~create_service lvl link_stack tree_acc last (node :: nodes)
    end

and get_up ~create_service lvl link_stack tree_acc last nodes =
  if last = 0 then
    List.rev (List.hd tree_acc)
  else if last = lvl then
    parse_nodes ~create_service link_stack tree_acc last nodes
  else
    let last_tree = build_node ~create_service (List.hd link_stack) (List.rev (List.hd tree_acc)) in
    let tree_acc = List.tl tree_acc and link_stack = List.tl link_stack in
    get_up ~create_service
      lvl link_stack
      ((last_tree :: List.hd tree_acc) :: List.tl tree_acc) (pred last) nodes

let build_tree
    ~create_service
    (nodes : [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] Eliom_pervasives.HTML5.M.elt list)
    : menu_item list =
  parse_nodes ~create_service [] [[]] 0 (HTML5.M.toeltl nodes)

(** How to create service for hierarchical menu ? *)

let create_wiki_page_service bi ?(wiki = bi.Wiki_widgets_interface.bi_wiki) page =
  let service =
    match Wiki_self_services.find_servpage wiki with
    | Some s -> s
    | None ->
	let wiki = Wiki_types.string_of_wiki wiki in
	Printf.ksprintf Ocsigen_messages.warning
	  "Wiki_menu: Can't find service for wiki id %s." wiki;
	raise (Error (Printf.sprintf "Oups ! service not found for id %s." wiki))
  in
  (Eliom_services.preapply ~service page :> Eliom_tools_common.get_page)

(** How to resolve the optional "file" attribute.

    File resolver is stored in the request cache. *)

type resolver = string list -> Ocsigen_local_files.resolved
let menu_resolver: resolver Polytables.key = Polytables.make_key ()
let set_menu_resolver r =
  let table = Eliom_request_info.get_request_cache () in
  Polytables.set ~table ~key:menu_resolver ~value:r
let resolve_menu_file  file =
  let table = Eliom_request_info.get_request_cache () in
  (Polytables.get ~table ~key:menu_resolver) file

(** Parse wiki from file (or contents) to xml *)

let get_nodes bi args contents =
  try
    let file = List.assoc "file" args in
    match resolve_menu_file
	(String.split '/' file) with
    | Ocsigen_local_files.RFile file ->
      Lwt_io.with_file ~mode:Lwt_io.input file
	(fun ch ->
          lwt data = Lwt_io.read ch in
          lwt nodes = Wiki_syntax.xml_of_wiki Wiki_syntax.menu_parser bi data in
	  Lwt.return nodes)
    | _ ->  Lwt.fail (Error (Printf.sprintf "Can't find file (%s)" file))
  with
  | Not_found ->
      begin match contents with
      | Some c ->
	  Wiki_syntax.xml_of_wiki Wiki_syntax.menu_parser bi c
      | None -> Lwt.return [] end
  | exc -> Lwt.fail exc (* 404 and so... TODO *)

(* let typecheck_menu bi xml : menu Lwt.t = *)
  (* try Lwt.return {{ ( xml :? menu ) }} *)
  (* with *)
  (* | exc -> *)
      (* let page = *)
	(* match bi.Wiki_widgets_interface.bi_page with *)
	(* | _, Some page -> String.concat "/" page *)
	(* | w, None -> "wiki:" ^ Wiki_types.string_of_wiki w in *)
      (* Printf.ksprintf Ocsigen_messages.warning *)
	(* "Wiki_menu: Can't parse menu, page: %S, exception: %S." *)
	(* page (Printexc.to_string exc); *)
      (* Lwt.fail (Error "Can't parse menu (see warning's log)") *)

(** Process extension *)

let get_kind bi args =
  try
    match List.assoc "kind" args with
    | "depth" -> `DepthFirst
    | "breadth" -> `BreadthFirst
    | _ -> `DepthFirstWhole
  with
  | Not_found -> `DepthFirstWhole

let get_class bi args =
  try
    let a = List.assoc "class" args in
    Some (String.split ~multisep:true ' ' a)
  with Not_found -> None
let get_id bi args =
  try
    Some (List.assoc "id" args)
  with Not_found -> None


let do_wikimenu bi args contents =
  let service = None in
  let contents =
    Lwt.catch
      (fun () ->
	let classe = get_class bi args in
	let id = get_id bi args in
	let kind = get_kind bi args in
	lwt nodes = get_nodes bi args contents in
	let tree = build_tree ~create_service:(create_wiki_page_service bi) nodes in
	let menu =
	  match kind with
	  | `DepthFirstWhole ->
	      Eliom_tools.Html5.hierarchical_menu_depth_first
		?classe ?id
		(Eliom_tools_common.Not_clickable, tree)
		~whole_tree:true
		?service ()
	  | `DepthFirst ->
	      Eliom_tools.Html5.hierarchical_menu_depth_first
		?classe ?id
		(Eliom_tools_common.Not_clickable, tree)
		~whole_tree:false
		?service ()
	  | `BreadthFirst ->
	      Eliom_tools.Html5.hierarchical_menu_breadth_first
		?classe ?id
		(Eliom_tools_common.Not_clickable, tree)
		?service () in
	Lwt.return menu)
      (function
	| (Error msg) -> error (Format.sprintf "Error wikimenu: %s" msg)
	| exc -> error (Format.sprintf "Error wikimenu: exception %s"
			  (Printexc.to_string exc)))
  in
  Wikicreole.Flow5 contents

let _ =
  Wiki_syntax.add_extension Wiki_syntax.wikicreole_parser "wikimenu" do_wikimenu

(** *)

let build_tree_from_string bi ~create_service ~contents =
  lwt xml = Wiki_syntax.xml_of_wiki Wiki_syntax.menu_parser bi contents in
  Lwt.return (build_tree ~create_service xml)

let build_tree_from_file bi ~create_service ~file =
  match file with
  | Ocsigen_local_files.RDir _ -> Lwt.return []
  | Ocsigen_local_files.RFile file ->
    Lwt_io.with_file ~mode:Lwt_io.input file
      (fun ch ->
	lwt contents = Lwt_io.read ch in
	build_tree_from_string bi ~create_service ~contents)
