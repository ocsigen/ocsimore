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
  let inline_wikicreole_parser = Wiki_syntax.inline_wikicreole_parser in

  add_extension
    [wikicreole_parser]
    ~name:"wikibox" ~wiki_content:true
    (fun wp bi args c ->
       Wikicreole.Block
         (Lwt.catch
           (fun () ->
             let box =
               wikibox_of_sql (Int32.of_string (List.assoc "box" args))
             in
             if Ancestors.in_ancestors box bi.bi_ancestors then
               Lwt.return
                 [(error_box#display_error_box
                     ~message:"Wiki error: loop of wikiboxes" ()
                   : Xhtmltypes.block XHTML.M.elt
                   :> [>Xhtmltypes.block | `Code] XHTML.M.elt)]
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
                 (b : Xhtmltypes.block XHTML.M.elt list
                    :> [>Xhtmltypes.block | `Code] XHTML.M.elt list)
           )
           (function
             | Not_found ->
                 Lwt.return
                   [(XHTML.M.code [XHTML.M.pcdata "<<wikibox>>" ]
                    : [>`Code] XHTML.M.elt
                    :> [>Xhtmltypes.block | `Code] XHTML.M.elt)]
             | _ ->
                 Lwt.return
                   [(error_box#display_error_box
                       ~message:"Wiki error: error in wikibox extension" ()
                    : Xhtmltypes.block XHTML.M.elt
                    :> [>Xhtmltypes.block | `Code] XHTML.M.elt)]
           )
         )
    );

  (* add_extension "wikibox" above has already added a preparser for wikibox,
     which recursively parses the argument of the wikibox. We override it
     below, but do the same thing at the beginning of our preparser *)
  let preparse_wikibox = (fun wp (sp, wb) args c ->
    (* There are two parts in the extension : we try to create boxes if the
       box='' argument of wikibox is missing. We also recursively parse the
       content c of the extension *)
    (* Parsing of c : *)
    (match c with
      | None -> Lwt.return None
      | Some c ->
          Wiki_syntax.preparse_extension wp (sp, wb) c >|= fun c -> Some c
    ) >>= fun c ->
    (* Adding the 'box=' argument *)
    (try
       Wiki_sql.wikibox_wiki wb >>= fun wid ->
       (* The user can specify the wiki, or we deduce it from the context. *)
       let wid = extract_wiki_id args wid in
       Wiki_sql.get_wiki_info_by_id wid >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       let content_type =
         Wiki_models.get_default_content_type wiki_info.wiki_model
       in
       try (* If a wikibox is already specified, there is nothing to change *)
         ignore (List.assoc "box" args); Lwt.return args
       with Not_found ->
         User.get_user_id ~sp >>= fun userid ->
         rights#can_create_subwikiboxes ~sp wid >>= function
           | true ->
               Wiki_data.new_wikitextbox
                 ~rights ~sp
                 ~wiki:wid
                 ~author:userid
                 ~comment:(Printf.sprintf "Subbox of wikibox %s (from wiki %s)"
                             (string_of_wikibox wb) (string_of_wiki wid))
                 ~content:"**//new wikibox//**"
                 ~content_type
                 ()
                 >>= fun box ->
                 (Wiki_sql.get_wikibox_info wb >>= function
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
                  :: List.remove_assoc "wiki" args)
           | false -> Lwt.return args
     with Failure _ -> Lwt.return args)
    >|= fun args ->
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
              | Some c -> Wiki_syntax.a_content_of_wiki bi c
              | None -> Lwt.return [XHTML.M.pcdata page]
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
  Wiki_syntax.add_extension ~wp:inline_wikicreole_parser
    ~name:"link" ~wiki_content:true (f inline_wikicreole_parser);


  let f = (fun _wp bi args c ->
       Wikicreole.Link_plugin
         (let sp = bi.bi_sp in
          let href = Ocsimore_lib.list_assoc_default "page" args ""
          and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
          and https = extract_https args in
          let content =
            match c with
              | Some c -> Wiki_syntax.a_content_of_wiki bi c
              | None -> Lwt.return [XHTML.M.pcdata href]
          in
          let wiki_id = extract_wiki_id args bi.bi_wiki in
          (XHTML.M.string_of_uri
             (Eliom_predefmod.Xhtml.make_uri ?https ?fragment
                ~service:(Wiki_self_services.find_naservpage wiki_id) ~sp href),
           args,
           content)
         )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"nonattachedlink" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:inline_wikicreole_parser
    ~name:"nonattachedlink" ~wiki_content:true (f inline_wikicreole_parser);


  let f = (fun _wp bi args c ->
    Wikicreole.Link_plugin
      (let content = match c with
         | Some c -> Wiki_syntax.a_content_of_wiki bi c
         | None -> Lwt.return [XHTML.M.pcdata "Cancel"]
       in
       (XHTML.M.string_of_uri
          (Eliom_predefmod.Xhtml.make_uri
             ~service:Eliom_services.void_coservice'
             ~sp:bi.bi_sp ()),
        args,
        content)
      )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"cancellink" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:inline_wikicreole_parser
    ~name:"cancellink" ~wiki_content:true (f inline_wikicreole_parser);


  add_extension
    [wikicreole_parser]
    ~name:"object" ~wiki_content:true
    (fun _wp bi args _c ->
       Wikicreole.A_content
         (let type_ = Ocsimore_lib.list_assoc_default "type" args ""
          and page = Ocsimore_lib.list_assoc_default "data" args ""
          and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
          and atts = Wiki_syntax.parse_common_attribs args in
          let url = Wiki_syntax.make_href
            bi (Wiki_syntax.link_kind page) fragment
          in
          Lwt.return
            [XHTML.M.object_
               ~a:(   (XHTML.M.a_data (XHTML.M.uri_of_string url)
                       : [>Xhtmltypes.core | `Data | `Type] XHTML.M.attrib)
                   :: (XHTML.M.a_type type_
                       : [>Xhtmltypes.core | `Data | `Type] XHTML.M.attrib)
                   :: (atts
                       : Xhtmltypes.core XHTML.M.attrib list
                       :> [>Xhtmltypes.core | `Data | `Type] XHTML.M.attrib list)
               )
               []
            ]
         )
    );



  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0]
    ~name:"img" ~wiki_content:true
    (fun _wp bi args c ->
       Wikicreole.A_content
         (let page = Ocsimore_lib.list_assoc_default "name" args ""
          and https = extract_https args
          and wiki = extract_wiki_id args bi.bi_wiki in
          let alt = match c with Some c -> c | None -> page in
          let atts = Wiki_syntax.parse_common_attribs args in
          let url = Wiki_syntax.make_href
            bi (Wiki_syntax.Wiki_page (wiki, page, https)) None
          in
          Lwt.return
            [XHTML.M.img ~src:(XHTML.M.uri_of_string url) ~alt ~a:atts ()]
         )
    );

  let f = (fun _wp bi args _ ->
       Wikicreole.A_content
         (let atts = Wiki_syntax.parse_common_attribs args
          and on = extract args "msgon" "Show menus"
          and off = extract args "msgoff" "Hide menus"
          in
          Lwt.return
            [XHTML.M.span
               [XHTML.M.a
                  ~a:(   XHTML.M.a_class ["shownmenus"; "jslink"; "btmenu"]
(*Failure "TODO: replace obrowser here" *)
                      :: atts
                  )
                  [XHTML.M.pcdata off]
               ]
            ]
         )
    )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"switchmenu" ~wiki_content:true f;
  Wiki_syntax.add_extension ~wp:inline_wikicreole_parser
    ~name:"switchmenu" ~wiki_content:true (f inline_wikicreole_parser)

