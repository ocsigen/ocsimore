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
   Provides a function to register the wikicreole extensions for wikis.

   @author Vincent Balat
   @author Boris Yakobowski
*)

open Eliom_pervasives
open User_sql.Types
open Wiki_types
open Wiki_widgets_interface

let (>|=) = Lwt.(>|=)

(* Helper functions for the syntax extensions below *)
let extract args v default =
  try List.assoc v args
  with Not_found -> default

let opt_list = function | [] -> None | _::_ as l -> Some l
let rec filter_raw = function (* /!\ NOT TAIL REC /!\ *)
  | [] -> []
  | None :: xs -> filter_raw xs
  | Some x :: xs -> x :: filter_raw xs

let extract_wiki_id args default =
  try wiki_of_string (List.assoc "wiki" args)
  with Failure _ | Not_found -> default
and extract_https args =
  try match List.assoc "protocol" args with
    | "http" -> Some false
    | "https" -> Some true
    | _ -> None
  with Not_found -> None


(** This function registers the following wiki extensions:
      - [<<wikibox>>]
      - [<<link>>], [<<link-inline>>]
      - [<<nonattachedlink>>], [<<nonattachedlink-inline>>]
      - [<<cancellink>>], [<<cancellink-inline>>]
      - [<<object>>]
      - [<<img>>]
      - [<<switchmenu>>]
      - [<<outline>>]
  *)
let register_wikibox_syntax_extensions
    (error_box : Widget.widget_with_error_box) =

  let f_wikibox wp bi args c =
    `Flow5
      (try_lwt
         let box =
           wikibox_of_sql (Int32.of_string (List.assoc "box" args))
         in
         if Ancestors.in_ancestors box bi.bi_ancestors then
           Lwt.return
             [error_box#display_error_box
                 ~message:"Wiki error: loop of wikiboxes" ()]
         else
           if (try ignore (List.assoc "delayed" args); true with | Not_found -> false)
           then
             lwt override = Wiki_services.get_override_wikibox () in
             let page = bi.bi_page in
             let box_replacer_id = HTML5.new_elt_id ~global:false () in
             let box_replacer = HTML5.create_named_elt ~id:box_replacer_id
               (HTML5.M.div
                  ~a:[HTML5.M.a_onload {{
                    ignore (
                      match_lwt Eliom_client.call_caml_service ~keep_get_na_params:true
                        ~service:( %Wiki_services.wikibox_contents )
                        ( %box, %page, %override ) () with
                        | None ->
                          debug "box not allowed for display";
                          Lwt.return ()
                        | Some box_content ->
                          Eliom_dom.Named.replaceAllChild %box_replacer_id box_content;
                          Lwt.return ())
                  }}]
                  [HTML5.M.pcdata "loading"]) in
             Lwt.return [box_replacer] (* CCC if we add a ( :'a) type constraint, it fails type checking... *)
           else
             let fsubbox ~sectioning menu_style = match c with
               | None -> Lwt.return None
               | Some c ->
                 let bi =
                   { bi with bi_menu_style = menu_style;
                     bi_sectioning = sectioning; } in
                 lwt r = Wiki_syntax.xml_of_wiki wp bi c in
                 Lwt.return (Some (r :> HTML5_types.flow5 HTML5.M.elt list))
             in
             lwt wiki = Wiki_sql.wikibox_wiki box in
             lwt wiki_info = Wiki_sql.get_wiki_info_by_id wiki in
             lwt widget = Wiki_models.get_widgets wiki_info.wiki_model in
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
               box
        with
        | Not_found ->
            Lwt.return
              [HTML5.M.code [HTML5.M.pcdata "<<wikibox>>" ]]
        | _ ->
            Lwt.return
              [error_box#display_error_box
                  ~message:"Wiki error: error in wikibox extension" ()])
  in

  let preparse_wikibox wb args c =
    (* There are two parts in the extension : we try to create boxes if the
       box='' argument of wikibox is missing. *)
    (* Adding the 'box=' argument *)
    lwt (args, c) = try
       lwt wid = Wiki_sql.wikibox_wiki wb in
       (* The user can specify the wiki, or we deduce it from the context. *)
       let wid = extract_wiki_id args wid in
       lwt wiki_info = Wiki_sql.get_wiki_info_by_id wid in
       lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
       lwt content_type =
         Wiki_models.get_default_content_type wiki_info.wiki_model
       in
       try (* If a wikibox is already specified, there is nothing to change *)
         ignore (List.assoc "box" args); Lwt.return (args, c)
       with Not_found ->
         lwt userid = User.get_user_id () in
         lwt have_right = rights#can_create_subwikiboxes wid in
         if have_right then
           ( lwt box =
               Wiki_data.new_wikitextbox
                 ~rights
                 ~wiki:wid
                 ~author:userid
                 ~comment:(Printf.sprintf "Subbox of wikibox %s (from wiki %s)"
                             (string_of_wikibox wb) (string_of_wiki wid))
                 ~content:"**//new wikibox//**"
                 ~content_type
                 ()
             in
             lwt special_rights = Wiki_sql.get_wikibox_info box in
             ( if special_rights.wikibox_special_rights then
                 User.GenericRights.iter_awr_lwt
                   (fun f ->
                     let g = f.User.GenericRights.field
                       Wiki.wikibox_grps in
                     User.add_to_group ~user:(g $ wb) ~group:(g $ box))
               else
                 Lwt.return () ) >>
             Lwt.return
               (("box", string_of_wikibox box)
                  (*We remove the wiki information, which is no longer useful*)
                :: List.remove_assoc "wiki" args, None) )
         else
           Lwt.return (args, c)
     with Failure _ -> Lwt.return (args, c)
    in
    Lwt.return (Some (Wiki_syntax.string_of_extension "wikibox" args c))
  in

  let add_wikibox wp =
    Wiki_syntax.register_raw_wiki_extension ~wp ~name:"wikibox"
      ~wp_rec:wp
      ~preparser:preparse_wikibox
      ~ni_plugin:f_wikibox
      f_wikibox in
  add_wikibox Wiki_syntax.wikicreole_parser;
  add_wikibox Wiki_syntax.wikicreole_parser_without_header_footer;

  let f_link bi args c =
    let page = Ocsimore_lib.list_assoc_default "page" args "" in
    let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
    let https = extract_https args in
    let content =
      match c with
        | Some c -> c
        | None -> Lwt.return [HTML5.M.pcdata page]
    in
    (* class and id attributes will be taken by Wiki_syntax.a_elem *)
    let wiki = extract_wiki_id args bi.bi_wiki in
    ( Wiki_syntax.make_href
        bi (Wiki_syntax.Wiki_page (Some wiki, page, https)) fragment,
      args,
      content ) in
  Wiki_syntax.register_link_flow_extension ~name:"link" { Wiki_syntax.lfpp = f_link };
  Wiki_syntax.register_link_phrasing_extension ~name:"link-inline" f_link;

  let f_nonattachedlink bi args c =
    let href = Ocsimore_lib.list_assoc_default "page" args ""
    and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
    and https = extract_https args in
    let content =
      match c with
      | Some c -> c
      | None -> Lwt.return [HTML5.M.pcdata href]
    in
    let wiki_id = extract_wiki_id args bi.bi_wiki in
    (Wiki_syntax.Service_href
       (Wiki_syntax.service_href ?https ?fragment
          (Wiki_self_services.find_naservpage wiki_id) href),
     args,
     content)
  in

  Wiki_syntax.register_link_flow_extension ~name:"nonattachedlink"
    { Wiki_syntax.lfpp = f_nonattachedlink };
  Wiki_syntax.register_link_phrasing_extension ~name:"nonattachedlink-inline"
    f_nonattachedlink;

  let f_cancellink bi args c =
    let content = match c with
      | Some c -> c
      | None -> Lwt.return [HTML5.M.pcdata "Cancel"]
    in
    (Wiki_syntax.Service_href
       (Wiki_syntax.service_href
          Eliom_services.void_coservice' ()),
     args,
     content)
  in

  Wiki_syntax.register_link_flow_extension ~name:"cancellink"
    { Wiki_syntax.lfpp = f_cancellink };
  Wiki_syntax.register_link_phrasing_extension ~name:"cancellink-inline"
    f_cancellink;

  let f_object bi args _c =
    `Phrasing_without_interactive
       (let type_ = Ocsimore_lib.list_assoc_default "type" args ""
        and page = Ocsimore_lib.list_assoc_default "data" args ""
        and fragment = Ocsimore_lib.list_assoc_opt "fragment" args
        and atts = Wiki_syntax.parse_common_attribs args in
        let url = Wiki_syntax.make_href
          bi (try Wiki_syntax.link_kind page with Failure _ -> Wiki_syntax.Href ("???", None)) fragment
        in
        Lwt.return
          [HTML5.M.object_
              ~a:(   (HTML5.M.a_data (Wiki_syntax.uri_of_href url)
                        : [>HTML5_types.common | `Data ] HTML5.M.attrib)
                     :: (HTML5.M.a_mime_type type_
                     : [>HTML5_types.common | `Data ] HTML5.M.attrib)
                     :: (atts
                           : HTML5_types.common HTML5.M.attrib list
                         :> [>HTML5_types.common | `Data ] HTML5.M.attrib list)
              )
              []
          ]
      )
  in
  Wiki_syntax.register_simple_flow_extension ~name:"object" ~reduced:false f_object

  let f_img bi args c =
    `Phrasing_without_interactive
      (let page = Ocsimore_lib.list_assoc_default "name" args ""
      and https = extract_https args
      and wiki = extract_wiki_id args bi.bi_wiki in
       let alt = match c with Some c -> c | None -> page in
       let atts = Wiki_syntax.parse_common_attribs args in
       let url =
         Wiki_syntax.uri_of_href
           (Wiki_syntax.make_href
              bi (Wiki_syntax.Wiki_page (Some wiki, page, https)) None)
       in
       Lwt.return
         [HTML5.M.img ~src:url ~alt ~a:atts ()]
      )
  in
  Wiki_syntax.register_simple_flow_extension ~name:"img" ~reduced:false f_img;

  let f_switchmenu bi args _c =
    `Phrasing_without_interactive
      (let atts = Wiki_syntax.parse_common_attribs args in
       Lwt.return
         [HTML5.M.(span ~a:atts
             [a [span ~a:[
                  a_class ["btmenu"];
                  a_onclick {{
                    ignore (Dom_html.document##body##classList##toggle(Js.string "nomenu"))
                  }}
                 ] [
                   span ~a:[a_class ["show_menus_label"]] [pcdata "Show menus"];
                   span ~a:[a_class ["hide_menus_label"]] [pcdata "Hide menus"];
                 ]]
             ])])
  in
  Wiki_syntax.register_simple_phrasing_extension  ~name:"switchmenu" f_switchmenu;

  let f_outline wp bi (args: (string * string) list) c =
    `Flow5
      (let elem =
         try
           `Id (List.assoc "target" args)
         with Not_found -> `Container in
       let restrict =
         try Some (List.assoc "restrict" args)
         with Not_found -> None in
       let depth =
         try Some (int_of_string (List.assoc "depth" args))
         with _ -> None in
       lwt content = match c with
         | None -> Lwt.return []
         | Some c ->
           (Wiki_syntax.xml_of_wiki wp bi c :> HTML5_types.flow5 HTML5.M.elt list Lwt.t)
       in
       let ignore =
         try List.map String.lowercase (String.split ~multisep:true ' ' (List.assoc "ignore" args))
         with Not_found -> ["nav";"aside"]
       in
       let div =
         (elem = `Container && not bi.bi_sectioning)
         || List.mem_assoc "div" args in

       let a = Wiki_syntax.parse_common_attribs ~classes:["ocsimore_outline"] args in
       let nav = (if div then HTML5.div else HTML5.nav) ~a content in
       Eliom_services.onload {{

         let nav = (Eliom_client.Html5.of_element %nav :> Dom.node Js.t)  in

         let ignore = (fun (n: Dom.node Js.t) ->
           let tag = String.lowercase (Js.to_string n##nodeName) in
           n == nav || List.mem tag %ignore) in
         let elem, restrict = match %elem with
           | `Id id ->
                ( (Dom_html.document##getElementById(Js.string id) :>
                     Dom.node Js.t Js.opt),
                  None )
           | `Container ->
               let fragment =
                 if %div then
                   try let heading = HTML5outliner.find_previous_heading nav in
                       Js.Opt.case
                         (Js.Opt.map heading HTML5outliner.get_fragment)
                         (fun () -> None)
                         id
                   with Not_found -> None
                 else None
               in
               (HTML5outliner.find_container nav, fragment)
         in
         let restrict = match %restrict with
           | None -> restrict
           | Some _ as restrict -> restrict
         in
         match Js.Opt.to_option elem with
         | None -> ()
         | Some elem ->
            let outline =
              HTML5outliner.outline ~ignore (Dom.list_of_nodeList elem##childNodes) in
            let outline =
              match restrict with
              | Some fragment -> HTML5outliner.find_fragment fragment outline
              | None ->
                  match outline with
                  | [ HTML5outliner.Section(_,_,outline) ] -> outline
                  | _ -> outline in
            Dom.appendChild nav (HTML5outliner.build_ol ?depth:%depth outline)

     }};
     Lwt.return [nav] )
  in
  Wiki_syntax.register_raw_wiki_extension ~name:"outline"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    f_outline;
  Wiki_syntax.register_raw_wiki_extension ~name:"outline"
    ~wp:Wiki_syntax.wikicreole_parser_without_header_footer
    ~wp_rec:Wiki_syntax.wikicreole_parser_without_header_footer
    f_outline
