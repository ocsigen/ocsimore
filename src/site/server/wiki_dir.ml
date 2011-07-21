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

let resolve_file_in_dir ?(default = "") ?(suffix = "") dir =
  fun file ->
    let filename = match file with
      | [] | [""] -> Filename.concat dir  default
      | file -> List.fold_left Filename.concat dir file in
    Ocsigen_local_files.resolve
      ~no_check_for:dir
      ~request:(Eliom_request_info.get_request ())
      ~filename:(filename ^ suffix)

exception Dir

let process_wikifile ~wiki ~template ~wb404 ~wb403 resolver file =
  lwt html, _ =
    try
      Wiki_site.wikibox_widget#display_wikifile
        ~wiki ~menu_style:`Linear ~template ~file:(resolver file)
    with
    | Ocsigen_local_files.Failed_404 | Undefined ->
      Wiki_site.wikibox_widget#display_wikibox
	~wiki ~menu_style:`Linear ~template ~wb:wb404
    | Ocsigen_local_files.NotReadableDirectory
    | Ocsigen_local_files.Failed_403 | Dir ->
      Wiki_site.wikibox_widget#display_wikibox
	~wiki ~menu_style:`Linear ~template ~wb:wb403 in
    Lwt.return html

let process_auxfile ~wiki ~template ~wb404 ~wb403 resolver file =
  try
    match resolver file with
    | Ocsigen_local_files.RFile file ->
      Eliom_output.appl_self_redirect Eliom_output.Files.send file
    | _ -> raise Dir
  with
  | Ocsigen_local_files.Failed_404 | Undefined ->
    lwt (html, code) =
      Wiki_site.wikibox_widget#display_wikibox
	~wiki ~menu_style:`Linear ~template ~wb:wb404 in
    Ocsimore_appl.send ~code:404 html
  | Ocsigen_local_files.NotReadableDirectory
  | Ocsigen_local_files.Failed_403 | Dir ->
    lwt (html, code) =
      Wiki_site.wikibox_widget#display_wikibox
	~wiki ~menu_style:`Linear ~template ~wb:wb403 in
    Ocsimore_appl.send ~code:403 html
