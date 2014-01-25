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


type menu =
  [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ] Html5.F.elt list

type menu_item =
   Html5_types.a_content Html5.F.elt list *
      (Eliom_service.get_service_kind,
       Eliom_service.registrable,
       Html5_types.a_content Html5.F.elt list)
      Eliom_tools.hierarchical_site_item

val build_tree :
    create_service:
        (?wiki:Wiki_types.wiki ->
          string list ->
          (Eliom_service.get_service_kind,
           Eliom_service.registrable,
           Eliom_registration.non_ocaml_service)
            Eliom_tools.one_page) ->
    menu ->
    menu_item list

val build_tree_from_string :
    Wiki_widgets_interface.box_info ->
    create_service:
        (?wiki:Wiki_types.wiki ->
          string list ->
          (Eliom_service.get_service_kind,
           Eliom_service.registrable,
           Eliom_registration.non_ocaml_service)
            Eliom_tools.one_page) ->
    contents:string ->
    menu_item list Lwt.t

val build_tree_from_file :
    Wiki_widgets_interface.box_info ->
    create_service:
        (?wiki:Wiki_types.wiki ->
          string list ->
          (Eliom_service.get_service_kind,
           Eliom_service.registrable,
           Eliom_registration.non_ocaml_service)
            Eliom_tools.one_page) ->
    file:Ocsigen_local_files.resolved ->
    menu_item list Lwt.t

val set_menu_resolver: (string list -> Ocsigen_local_files.resolved) -> unit Lwt.t

val create_wiki_page_service:
    Wiki_widgets_interface.box_info ->
      ?wiki:Wiki_types.wiki ->
        string list ->
          (Eliom_service.get_service_kind,
           Eliom_service.registrable,
           Eliom_registration.non_ocaml_service)
            Eliom_tools.one_page
