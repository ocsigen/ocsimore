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
exception Undefined

type 'a resolver = 'a -> Ocsigen_local_files.resolved

type 'a wrapper =
    'a
    -> Wiki_widgets_interface.box_info
    -> HTML5_types.flow5 Eliom_pervasives.HTML5.M.elt list
    -> (string * HTML5_types.flow5 Eliom_pervasives.HTML5.M.elt list) Lwt.t


let resolve_file_in_dir ?(default = "") ?(suffix = "") dir =
  fun file ->
    let filename = match file with
      | [] -> Filename.concat dir  default
      | file -> List.fold_left Filename.concat dir file in
    Ocsigen_local_files.resolve
      ~no_check_for:dir
      ~request:(Eliom_request_info.get_request ())
      ~filename:(filename ^ suffix)

let make_page ~wiki_id ?(css = []) contents =

  (* Wiki box info *)
  lwt wiki_info = Wiki_sql.get_wiki_info_by_id wiki_id in
  let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
  let wikibox =
    match wiki_info.Wiki_types.wiki_container with
    | Some wb -> wb | None -> assert false (* FIXME *) in
  let bi = {
    Wiki_widgets_interface.
    bi_ancestors = Wiki_widgets_interface.Ancestors.no_ancestors;
    bi_subbox = (fun _ -> Lwt.return None);
    bi_box = wikibox;
    bi_wiki = wiki_id;
    bi_rights = rights;
    bi_page = wiki_id, Some (Eliom_request_info.get_original_full_path ());
    bi_menu_style = `Linear;
  } in

  (* CSS *)
  let page =
    Url.string_of_url_path ~encode:false
      (Eliom_request_info.get_current_full_path ()) in
  lwt wcss = Wiki_site.wikibox_widget#css_header ~page wiki_id in
  let css = css @ wcss in

  (* Contents *)
  lwt (title, contents) = contents bi in

  (* Container *)
  let bi = { bi with Wiki_widgets_interface.bi_subbox =
	     (fun _ -> Lwt.return (Some (None, contents))) } in
  lwt box =
    Wiki_site.wikibox_widget#display_interactive_wikibox
      ~bi bi.Wiki_widgets_interface.bi_box in

  Page_site.html_page ~title ~css box

let do_wiki_page ?resolve_wiki_menu_file ~wrapper file bi =
  ignore (map_option Wiki_menu.set_menu_resolver resolve_wiki_menu_file);
  Lwt_io.with_file ~mode:Lwt_io.input file
    (fun ch ->
      lwt data = Lwt_io.read ch in
      lwt xml = Wiki_syntax.xml_of_wiki Wiki_syntax.wikicreole_parser bi data in
      wrapper bi xml)

exception Dir

let process ~wiki_id ~resolve_wiki_file ?resolve_wiki_menu_file
    ?(err404 = fun _ _ -> [ HTML5.M.pcdata "404" ])
    ?(err403 = fun _ _ -> [ HTML5.M.pcdata "403" ])
    ?css
    ?wrapper
    () = fun params () ->
  (Wiki_sql.get_wiki_info_by_id wiki_id) >>= fun info ->
  let wrapper = match wrapper with
  | Some wrapper -> wrapper params
  | None -> fun _ contents ->
      Lwt.return (info.Wiki_types.wiki_title, [HTML5.M.div contents]) in
  try
    match resolve_wiki_file params with
    | Ocsigen_local_files.RFile file ->
	make_page ~wiki_id ?css (do_wiki_page ?resolve_wiki_menu_file ~wrapper file)
    | _ -> raise Dir
  with
   | Undefined | Ocsigen_local_files.Failed_404 | Dir ->
       make_page ~wiki_id ?css (fun bi -> (wrapper bi (err404 bi params)))
   | Ocsigen_local_files.NotReadableDirectory
   | Ocsigen_local_files.Failed_403 ->
       make_page ~wiki_id ?css (fun bi -> (wrapper bi (err403 bi params)))

let make_wrapper_of_wikibox ?title ~wb = fun _ bi contents ->
  lwt info = Wiki_sql.get_wiki_info_by_id bi.Wiki_widgets_interface.bi_wiki in
  let title =
    match title with
    | None -> info.Wiki_types.wiki_title
    | Some title -> title in
  let bi = { bi with Wiki_widgets_interface.bi_subbox =
	     (fun _ -> Lwt.return (Some (None, contents))) } in
  lwt box = Wiki_site.wikibox_widget#display_interactive_wikibox ~bi wb in
  Lwt.return (title, box)
