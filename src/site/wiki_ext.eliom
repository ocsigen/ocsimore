(* Ocsimore
 * Copyright (C) 2005
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
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Eliom_pervasives
open User_sql.Types
open Wiki_types
open Wiki_widgets_interface

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)


(* Helper functions for the syntax extensions below *)
let extract args v default =
  try List.assoc v args
  with Not_found -> default

let extract_wiki_id args default =
  try wiki_of_string (List.assoc "wiki" args)
  with Failure _ | Not_found -> default
and extract_https args =
  try match List.assoc "protocol" args with
    | "http" -> Some false
    | "https" -> Some true
    | _ -> None
  with Not_found -> None

let register_wikibox_syntax_extensions
    (error_box : Widget.widget_with_error_box) =

  let add_extension l ~name ~wiki_content f =
    List.iter
      (fun wp -> Wiki_syntax.add_extension ~wp ~name ~wiki_content (f wp))
      l
  in
  let wikicreole_parser = Wiki_syntax.wikicreole_parser in
  let reduced_wikicreole_parser0 = Wiki_syntax.reduced_wikicreole_parser0 in
  let reduced_wikicreole_parser1 = Wiki_syntax.reduced_wikicreole_parser1 in
  let reduced_wikicreole_parser2 = Wiki_syntax.reduced_wikicreole_parser2 in
  let phrasing_wikicreole_parser = Wiki_syntax.phrasing_wikicreole_parser in

  add_extension
    [wikicreole_parser]
    ~name:"wikibox" ~wiki_content:true
    (fun wp bi args c ->
       Wikicreole.Flow5
         (Lwt.catch
           (fun () ->
             let box =
               wikibox_of_sql (Int32.of_string (List.assoc "box" args))
             in
             if Ancestors.in_ancestors box bi.bi_ancestors then
               Lwt.return
                 [(error_box#display_error_box
                     ~message:"Wiki error: loop of wikiboxes" ()
                   : HTML5_types.flow5 HTML5.M.elt
                   :> [>HTML5_types.flow5 | `Code] HTML5.M.elt)]
             else
               let fsubbox menu_style = match c with
                 | None -> Lwt.return None
                 | Some c ->
                     Wiki_syntax.xml_of_wiki wp
                       { bi with bi_menu_style = menu_style } c >|= fun r ->
                     Some (Some bi.bi_box, r)
               in
               Wiki_sql.wikibox_wiki box >>= fun wiki ->
               Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
               let widget = Wiki_models.get_widgets wiki_info.wiki_model in
               let class_box = Wiki_syntax.class_wikibox box in
               widget#display_interactive_wikibox
                 ?rows:(Ocsimore_lib.int_of_string_opt
                          (Ocsimore_lib.list_assoc_opt "rows" args))
                 ?cols:(Ocsimore_lib.int_of_string_opt
                          (Ocsimore_lib.list_assoc_opt "cols" args))
                 ?classes:(try Some [List.assoc "class" args; class_box]
                           with Not_found -> Some [class_box])
                 ~bi:{bi with
                        bi_ancestors = Ancestors.add_ancestor box bi.bi_ancestors;
                        bi_box = box;
                        bi_wiki = wiki;
                        bi_subbox = fsubbox }
                 box >|= fun b ->
                 (b : HTML5_types.flow5 HTML5.M.elt list
                    :> [>HTML5_types.flow5 | `Code] HTML5.M.elt list)
           )
           (function
             | Not_found ->
                 Lwt.return
                   [(HTML5.M.code [HTML5.M.pcdata "<<wikibox>>" ]
                    : [>`Code] HTML5.M.elt
                    :> [>HTML5_types.flow5 | `Code] HTML5.M.elt)]
             | _ ->
                 Lwt.return
                   [(error_box#display_error_box
                       ~message:"Wiki error: error in wikibox extension" ()
                    : HTML5_types.flow5 HTML5.M.elt
                    :> [>HTML5_types.flow5 | `Code] HTML5.M.elt)]
           )
         )
    );

  (* add_extension "wikibox" above has already added a preparser for wikibox,
     which recursively parses the argument of the wikibox. We override it
     below, but do the same thing at the beginning of our preparser *)
  let preparse_wikibox = (fun wp wb args c ->
    (* There are two parts in the extension : we try to create boxes if the
       box='' argument of wikibox is missing. We also recursively parse the
       content c of the extension *)
    (* Parsing of c : *)
    lwt c = match c with
      | None -> Lwt.return None
      | Some c ->
          Wiki_syntax.preparse_extension wp wb c >|= fun c -> Some c
    in
    (* Adding the 'box=' argument *)
    (try
       lwt wid = Wiki_sql.wikibox_wiki wb in
       (* The user can specify the wiki, or we deduce it from the context. *)
       let wid = extract_wiki_id args wid in
       lwt wiki_info = Wiki_sql.get_wiki_info_by_id wid in
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       let content_type =
         Wiki_models.get_default_content_type wiki_info.wiki_model
       in
       try (* If a wikibox is already specified, there is nothing to change *)
         ignore (List.assoc "box" args); Lwt.return (args, c)
       with Not_found ->
         lwt userid = User.get_user_id () in
         rights#can_create_subwikiboxes wid >>= function
           | true ->
             lwt box = Wiki_data.new_wikitextbox
                 ~rights
                 ~wiki:wid
                 ~author:userid
                 ~comment:(Printf.sprintf "Subbox of wikibox %s (from wiki %s)"
                             (string_of_wikibox wb) (string_of_wiki wid))
                 ~content:"**//new wikibox//**"
                 ~content_type
                 ()
           in (Wiki_sql.get_wikibox_info wb >>= function
                    | { wikibox_special_rights = true } ->
                        User.GenericRights.iter_awr_lwt
                        (fun f ->
                           let g = f.User.GenericRights.field
                             Wiki.wikibox_grps in
                           User.add_to_group ~user:(g $ wb) ~group:(g $ box)
                        )
                    | { wikibox_special_rights = false } ->
                        Lwt.return ()
                 ) >|= fun () ->
                 (   ("box", string_of_wikibox box)
                  (*We remove the wiki information, which is no longer useful*)
                  :: List.remove_assoc "wiki" args, None)
           | false -> Lwt.return (args, c)
     with Failure _ -> Lwt.return (args, c))
    >|= fun (args, c) ->
    Some (Wiki_syntax.string_of_extension "wikibox" args c)
    )
  in
  Wiki_syntax.add_preparser_extension
    ~wp:wikicreole_parser ~name:"wikibox"
    (preparse_wikibox wikicreole_parser);

  let f = (fun _wp bi args c ->
       Wikicreole.Link_plugin
         (let page = Ocsimore_lib.list_assoc_default "page" args "" in
          let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
          let https = extract_https args in
          let content =
            match c with
              | Some c -> Wiki_syntax.phrasing_without_interactive_of_wiki bi c
              | None -> Lwt.return [HTML5.M.pcdata page]
          in
          (* class and id attributes will be taken by Wiki_syntax.a_elem *)
          (let wiki = extract_wiki_id args bi.bi_wiki in
           Wiki_syntax.make_href
             bi (Wiki_syntax.Wiki_page (wiki, page, https)) fragment
          ),
          args,
          content)
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"link" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:phrasing_wikicreole_parser
    ~name:"link" ~wiki_content:true (f phrasing_wikicreole_parser);


  let f = (fun _wp bi args c ->
       Wikicreole.Link_plugin
         (let href = Ocsimore_lib.list_assoc_default "page" args ""
          and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
          and https = extract_https args in
          let content =
            match c with
              | Some c -> Wiki_syntax.phrasing_without_interactive_of_wiki bi c
              | None -> Lwt.return [HTML5.M.pcdata href]
          in
          let wiki_id = extract_wiki_id args bi.bi_wiki in
	  (Wiki_syntax.Service_href
	     (Wiki_syntax.service_href ?https ?fragment
                (Wiki_self_services.find_naservpage wiki_id:>('a,'b) Wiki_syntax.wiki_service) href),
           args,
           content)
         )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"nonattachedlink" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:phrasing_wikicreole_parser
    ~name:"nonattachedlink" ~wiki_content:true (f phrasing_wikicreole_parser);


  let f = (fun _wp bi args c ->
    Wikicreole.Link_plugin
      (let content = match c with
         | Some c -> Wiki_syntax.phrasing_without_interactive_of_wiki bi c
         | None -> Lwt.return [HTML5.M.pcdata "Cancel"]
       in
       (Wiki_syntax.Service_href
	  (Wiki_syntax.service_href
	     (Eliom_services.void_coservice':>(unit,unit) Wiki_syntax.wiki_service) ()),
        args,
        content)
      )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"cancellink" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:phrasing_wikicreole_parser
    ~name:"cancellink" ~wiki_content:true (f phrasing_wikicreole_parser);


  add_extension
    [wikicreole_parser]
    ~name:"object" ~wiki_content:true
    (fun _wp bi args _c ->
       Wikicreole.Phrasing_without_interactive
         ((* no more in HTML5 let type_ = Ocsimore_lib.list_assoc_default "type" args "" *)
          let page = Ocsimore_lib.list_assoc_default "data" args ""
          and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
          and atts = Wiki_syntax.parse_common_attribs args in
          let url = Wiki_syntax.make_href
            bi (Wiki_syntax.link_kind page) fragment
          in
          Lwt.return
            [HTML5.M.object_
               ~a:(   (HTML5.M.a_data (Wiki_syntax.uri_of_href url)
                       : [>HTML5_types.common | `Data ] HTML5.M.attrib)
                   (*:: (HTML5.M.a_type type_
                       : [>HTML5_types.common | `Data ] HTML5.M.attrib) *)
                   :: (atts
                       : HTML5_types.common HTML5.M.attrib list
                       :> [>HTML5_types.common | `Data ] HTML5.M.attrib list)
               )
               []
            ]
         )
    );



  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0]
    ~name:"img" ~wiki_content:true
    (fun _wp bi args c ->
       Wikicreole.Phrasing_without_interactive
         (let page = Ocsimore_lib.list_assoc_default "name" args ""
          and https = extract_https args
          and wiki = extract_wiki_id args bi.bi_wiki in
          let alt = match c with Some c -> c | None -> page in
          let atts = Wiki_syntax.parse_common_attribs args in
          let url =
	    Wiki_syntax.uri_of_href
	      (Wiki_syntax.make_href
		 bi (Wiki_syntax.Wiki_page (wiki, page, https)) None)
          in
          Lwt.return
            [HTML5.M.img ~src:url ~alt ~a:atts ()]
         )
    );

  let f = (fun _wp bi args _ ->
       Wikicreole.Phrasing_without_interactive
         (let atts = Wiki_syntax.parse_common_attribs args in
          Lwt.return
            [HTML5.M.span
		[HTML5.M.a ~a:((( HTML5.M.a_onclick
		   {{ ignore (Dom_html.document##body##classList##toggle(Js.string "nomenu"):bool Js.t) }} )
		   :: atts))
                   [HTML5.M.span ~a:[ HTML5.M.a_class ["btmenu"] ] []]
		]
            ]
         )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"switchmenu" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:phrasing_wikicreole_parser
    ~name:"switchmenu" ~wiki_content:true (f phrasing_wikicreole_parser)

