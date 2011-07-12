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
     [ `Nonattached of [ `Get ] Eliom_services.na_s ],
     [ `WithoutSuffix ],
     [ `One of string ] Eliom_parameters.param_name,
     unit,
     [`Registrable ],
     Eliom_output.appl_service
    ) Eliom_services.service Servpages.t = Servpages.create 5
let servpages :
    (string list,
     unit,
     Eliom_services.get_service_kind,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameters.param_name,
     unit,
     [ `Registrable ],
     Eliom_output.appl_service
    ) Eliom_services.service Servpages.t = Servpages.create 5
let servwikicss :
    (Wiki_types.wikibox,
     unit,
     [ `Attached of
         ([ `Internal of [ `Service | `Coservice ] | `External ],
          [`Get]) Eliom_services.a_s ],
     [ `WithoutSuffix ],
     [ `One of Wiki_types.wikibox ] Eliom_parameters.param_name,
     unit,
     [ `Registrable ],
     Eliom_output.CssText.return
    ) Eliom_services.service Servpages.t = Servpages.create 5

let add_naservpage = Servpages.add naservpages
let add_servpage = Servpages.add servpages
let add_servwikicss = Servpages.add servwikicss
let find_naservpage = Servpages.find naservpages
let find_servpage k =
  try Some (Servpages.find servpages k)
  with Not_found -> None
let find_servwikicss k =
  try Some (Servpages.find servwikicss k)
  with Not_found -> None
