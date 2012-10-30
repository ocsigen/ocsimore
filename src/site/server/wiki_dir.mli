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

type 'a resolver = 'a -> Ocsigen_local_files.resolved

exception Undefined

val resolve_file_in_dir:
  ?default:string ->
  ?suffix:string ->
  string ->
  string list ->
  unit ->
  Ocsigen_local_files.resolved

val process_wikifile:
  wiki:Wiki_types.wiki ->
  ?sectioning:bool ->
  ?menu_style:Wiki_widgets_interface.menu_style ->
  template:string ->
  wb404:Wiki_types.wikibox ->
  wb403:Wiki_types.wikibox ->
  ('a -> Ocsigen_local_files.resolved) ->
  'a -> Html5_types.html Html5.F.elt Lwt.t

val process_auxfile:
  wiki:Wiki_types.wiki ->
  ?options:Eliom_registration.File.options ->
  ?sectioning:bool ->
  ?menu_style:Wiki_widgets_interface.menu_style ->
  template:string ->
  wb404:Wiki_types.wikibox ->
  wb403:Wiki_types.wikibox ->
  ('a -> Ocsigen_local_files.resolved) ->
  'a ->
  (Ocsimore_appl.appl Eliom_registration.application_content,
   Eliom_registration.appl_service) Eliom_registration.kind Lwt.t
