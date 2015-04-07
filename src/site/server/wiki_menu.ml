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

open Eliom_content
open Eliom_lib
open Lwt_ops
open Ocsimore_lib

(* let (>>=) = Lwt.bind *)
(* let (>|=) m f = Lwt.map f m *)

exception Error of string
let error (msg:string) =
  Lwt.return [Html5.F.div ~a:[Html5.F.a_class ["error"]] [Html5.F.pcdata msg]]

type menu_item =
   Html5_types.a_content Html5.F.elt list *
      (Eliom_service.get_service_kind,
       Eliom_service.registrable,
       Html5_types.a_content Html5.F.elt list)
      Eliom_tools.hierarchical_site_item

(** Parse menu in wiki syntax *)

(* Expected XML type *)

type menu =
  [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] Html5.F.elt list

(* Build hierarchical menu from XML sequence *)

let link_regexp =
  Netstring_pcre.regexp "wiki\\((.*)\\):(.*)"

let is_a node = match Xml.content node with
  | Xml.Node ("a", _, _) -> true
  | Xml.Empty
  | Xml.Comment _
  | Xml.EncodedPCDATA _
  | Xml.PCDATA _
  | Xml.Entity _
  | Xml.Leaf _
  | Xml.Node _ -> false

let get_href node = match Xml.content node with
  | Xml.Node ("a", attribs, _) -> begin
    let a_href = List.find (fun a -> Xml.aname a = "href") attribs in
    match Xml.acontent a_href with
      | Xml.AStr href -> href
      | Xml.AFloat _
      | Xml.AInt _
      | Xml.AStrL _ -> assert false
  end
  | Xml.Empty
  | Xml.Comment _
  | Xml.EncodedPCDATA _
  | Xml.PCDATA _
  | Xml.Entity _
  | Xml.Leaf _
  | Xml.Node _ -> assert false

let get_node_contents node = match Xml.content node with
  | Xml.Node (_, _, contents) -> contents
  | Xml.Empty
  | Xml.Comment _
  | Xml.EncodedPCDATA _
  | Xml.PCDATA _
  | Xml.Entity _
  | Xml.Leaf _ -> assert false

let get_headline_level node = match Xml.content node with
    | Xml.Node (name, _, _) when name = "h6" -> 6
    | Xml.Node (name, _, _) when name = "h5" -> 5
    | Xml.Node (name, _, _) when name = "h4" -> 4
    | Xml.Node (name, _, _) when name = "h3" -> 3
    | Xml.Node (name, _, _) when name = "h2" -> 2
    | Xml.Node (name, _, _) when name = "h1" -> 1
    | Xml.Empty
    | Xml.Comment _
    | Xml.EncodedPCDATA _
    | Xml.PCDATA _
    | Xml.Entity _
    | Xml.Leaf _
    | Xml.Node _ -> assert false

let build_node ~create_service (contents: Xml.elt list) tree =
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
    (Html5.F.totl (get_node_contents a),
     Eliom_tools.Site_tree
       (Eliom_tools.Main_page
          (create_service ?wiki (Neturl.split_path href)), tree))
  end
  | contents ->
    (Html5.F.totl contents,
     Eliom_tools.Site_tree (Eliom_tools.Not_clickable, tree))

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
    (nodes : [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] Html5.F.elt list)
    : menu_item list =
  parse_nodes ~create_service [] [[]] 0 (Html5.F.toeltl nodes)

(** How to create service for hierarchical menu ? *)

let create_wiki_page_service bi ?(wiki = bi.Wiki_widgets_interface.bi_wiki) page =
  let service =
    match Wiki_self_services.find_servpage wiki with
    | Some s -> s
    | None ->
        let wiki = Wiki_types.string_of_wiki wiki in
        Lwt_log.ign_warning_f ~section
          "Wiki_menu: Can't find service for wiki id %s." wiki;
        raise (Error (Printf.sprintf "Oups ! service not found for id %s." wiki))
  in
  (Eliom_service.preapply ~service page :> Eliom_tools.get_page)

(** How to resolve the optional "file" attribute.

    File resolver is stored in the request cache. *)

let menu_resolver_eref = Eliom_reference.eref ~scope:Eliom_common.request_scope None

let set_menu_resolver r =
  Eliom_reference.set menu_resolver_eref (Some r)

let resolve_menu_file file =
  Eliom_reference.get menu_resolver_eref >|= function
    | Some f -> f file
    | None -> raise Not_found (* BB this emulates ancient Polytable's behaviour *)

(** Parse wiki from file (or contents) to xml *)

let get_nodes bi args contents =
  try
    let file = List.assoc "file" args in
    resolve_menu_file (String.split '/' file) >>= function
      | Ocsigen_local_files.RFile file ->
        Lwt_io.with_file ~mode:Lwt_io.input file
          (fun ch ->
            lwt data = Lwt_io.read ch in
            lwt nodes = Wiki_syntax.xml_of_wiki (Wiki_syntax.cast_wp Wiki_syntax.menu_parser) bi data in
            Lwt.return nodes)
      | Ocsigen_local_files.RDir _ ->
        Lwt.fail (Error (Printf.sprintf "Can't find file (%s)" file))
  with
    | Not_found ->
        begin match contents with
      | Some c -> c
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

let get_kind _ args =
  try
    match List.assoc "kind" args with
    | "depth" -> `DepthFirst
    | "breadth" -> `BreadthFirst
    | _ -> `DepthFirstWhole
  with
  | Not_found -> `DepthFirstWhole

let get_class _ args =
  try
    let a = List.assoc "class" args in
    Some (String.split ~multisep:true ' ' a)
  with Not_found -> None
let get_id _ args =
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
              Eliom_tools.D.hierarchical_menu_depth_first
                ?classe ?id
                (Eliom_tools.Not_clickable, tree)
                ~whole_tree:true
                ?service ()
          | `DepthFirst ->
              Eliom_tools.D.hierarchical_menu_depth_first
                ?classe ?id
                (Eliom_tools.Not_clickable, tree)
                ~whole_tree:false
                ?service ()
          | `BreadthFirst ->
              Eliom_tools.D.hierarchical_menu_breadth_first
                ?classe ?id
                (Eliom_tools.Not_clickable, tree)
                ?service () in
        Lwt.return menu)
      (function
        | (Error msg) -> error (Format.sprintf "Error wikimenu: %s" msg)
        | exc -> error (Format.sprintf "Error wikimenu: exception %s"
                          (Printexc.to_string exc)))
  in
  `Flow5 contents

let () =
  Wiki_syntax.register_wiki_extension
    ~wp:Wiki_syntax.wikicreole_parser ~name:"wikimenu"
    ~wp_rec:Wiki_syntax.menu_parser do_wikimenu;
  Wiki_syntax.register_wiki_extension
    ~wp:Wiki_syntax.wikicreole_parser_without_header_footer ~name:"wikimenu"
    ~wp_rec:Wiki_syntax.menu_parser do_wikimenu


(** *)

let build_tree_from_string bi ~create_service ~contents =
  lwt xml = Wiki_syntax.xml_of_wiki (Wiki_syntax.cast_wp Wiki_syntax.menu_parser) bi contents in
  Lwt.return (build_tree ~create_service xml)

let build_tree_from_file bi ~create_service ~file =
  match file with
  | Ocsigen_local_files.RDir _ -> Lwt.return []
  | Ocsigen_local_files.RFile file ->
    Lwt_io.with_file ~mode:Lwt_io.input file
      (fun ch ->
        lwt contents = Lwt_io.read ch in
        build_tree_from_string bi ~create_service ~contents)
