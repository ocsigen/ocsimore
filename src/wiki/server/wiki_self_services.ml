(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2005-2009
 * Piero Furiesi - Jaap Boender - Vincent Balat - Boris Yakobowski -
 * CNRS - Université Paris Diderot Paris 7
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



open Wiki_types

(** This file define some tables that contain the services that are
    dynamically associated to a given wiki:
    - the service that answers for a page of the wiki (two services:
    attached and non-attached).
    - the service that answers for the css of the wiki

    The other services for wikis take the wiki as argument, and
    are defined in [Wiki_services].

    The tables below are updated in the function [register_wiki] of
    [Wiki_services].
*)
module Servpages =
  Hashtbl.Make(struct
                 type t = wiki
                 let equal = (=)
                 let hash = Hashtbl.hash
               end)

let naservpages :
    (string,
     unit,
     [ `Nonattached of [ `Get ] Eliom_service.na_s ],
     [ `WithoutSuffix ],
     [ `One of string ] Eliom_parameter.param_name,
     unit,
     [`Registrable ],
     Eliom_registration.appl_service
    ) Eliom_service.service Servpages.t = Servpages.create 5
let servpages :
    (string list,
     unit,
     Eliom_service.get_service_kind,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name,
     unit,
     [ `Registrable ],
     Eliom_registration.appl_service
    ) Eliom_service.service Servpages.t = Servpages.create 5
let servwikicss :
    ((Wiki_types.wikibox * int32 option) list,
     unit,
     [ `Attached of
         ([ `Internal of [ `Service | `Coservice ] | `External ],
          [`Get]) Eliom_service.a_s ],
     [ `WithoutSuffix ],
     ([ `One of Wiki_types.wikibox ] Eliom_parameter.param_name *
      [ `One of int32 ] Eliom_parameter.param_name) Eliom_parameter.listnames,
     unit,
     [ `Registrable ],
     Eliom_registration.CssText.return
    ) Eliom_service.service Servpages.t = Servpages.create 5

let add_naservpage = Servpages.add naservpages
let add_servpage = Servpages.add servpages
let add_servwikicss = Servpages.add servwikicss
let find_naservpage k =
  Servpages.find naservpages k
let find_servpage k =
  try Some (Servpages.find servpages k)
  with Not_found -> None
let find_servwikicss k =
  try Some (Servpages.find servwikicss k)
  with Not_found -> None

(** [get_wiki_page_for_path path] finds a wiki which belongs to the longest prefix
    in [path] and the remaining path.
  *)
let get_wiki_page_for_path, insert_into_registered_wikis_tree =
  let module String_map = Map.Make (String) in
  let module Tree = struct
    type tree = T of (wiki option * tree String_map.t)
    let get_map f (T t) = f t
    let map f t = T (get_map f t)
  end in
  let rec find path =
    Tree.get_map
      (fun (opt_root_wiki_id, child_trees) ->
        match path with
            [] ->
              begin match opt_root_wiki_id with
                  Some id -> id, path
                | None -> raise Not_found
              end
          | snippet :: rem_path ->
              try find rem_path (String_map.find snippet child_trees)
              with Not_found ->
                match opt_root_wiki_id with
                    Some wiki_id -> wiki_id, path
                  | None -> raise Not_found)
  in
  let rec insert wiki_id path =
    Tree.map
      (fun (opt_root_wiki_id, child_trees) ->
        match path with
            [] -> Some wiki_id, child_trees
          | snippet :: path ->
              let child_tree =
                try String_map.find snippet child_trees
                with Not_found -> Tree.T (None, String_map.empty)
              in
              let child_tree' = String_map.add snippet (insert wiki_id path child_tree) child_trees in
              opt_root_wiki_id, child_tree')
  in
  let registered_wikis = ref (Tree.T (None, String_map.empty)) in
  (fun path -> find path !registered_wikis),
  (fun wiki_id path -> registered_wikis := insert wiki_id path !registered_wikis)
