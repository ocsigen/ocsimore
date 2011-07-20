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

type 'a resolver = 'a -> Ocsigen_local_files.resolved

exception Undefined

val resolve_file_in_dir:
    ?default:string -> ?suffix:string -> string -> string list resolver

val process_wikifile:
  wiki:Wiki_types.wiki ->
  template:string ->
  wb404:Wiki_types.wikibox ->
  wb403:Wiki_types.wikibox ->
  ('a -> Ocsigen_local_files.resolved) ->
  'a -> HTML5_types.html Eliom_pervasives.HTML5.M.elt Lwt.t

val process_auxfile:
  wiki:Wiki_types.wiki ->
  template:string ->
  wb404:Wiki_types.wikibox ->
  wb403:Wiki_types.wikibox ->
  ('a -> Ocsigen_local_files.resolved) ->
  'a ->
  (Ocsimore_appl.appl Eliom_output.application_content,
   Eliom_output.appl_service) Eliom_output.kind Lwt.t
