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

open Opaque
type wiki_arg = [ `Wiki ]
type wikibox_arg = [ `Wikibox ]
type wiki = [`Wiki] int32_t
let wiki_of_sql (i : int32) = (int32_t i : wiki)
let sql_of_wiki (i : wiki) = t_int32 i
let string_of_wiki i = Int32.to_string (sql_of_wiki i)
let wiki_of_string s = (Opaque.int32_t (Int32.of_string s) : wiki)


(* For now. Someday the second int32 will be a properly opacified type *)
type wikibox_id = int32
type wikibox = wiki * wikibox_id
type wikibox_uid = wikibox_arg Opaque.int32_t

let wikibox_uid_of_sql (i : int32) = (int32_t i : wikibox_uid)
let sql_of_wikibox_uid (i : wikibox_uid) = t_int32 i

type wikipage = wiki * string

type wikipage_arg = [ `Wikipage ]
type wikipage_uid = wikipage_arg Opaque.int32_t

type content_type = string
type wiki_model = string
let string_of_wiki_model = Ocsigen_lib.id
let wiki_model_of_string = Ocsigen_lib.id
let string_of_content_type = Ocsigen_lib.id
let content_type_of_string = Ocsigen_lib.id

type wiki_info = {
  wiki_id : wiki;
  wiki_title : string;
  wiki_descr : string;
  wiki_pages : string option;
  wiki_boxrights : bool;
  wiki_container : wikibox_id;
  wiki_staticdir : string option;
  wiki_model : wiki_model;
}

type wikibox_info = {
  wikibox_id : wikibox;
  wikibox_uid: wikibox_uid;
  wikibox_comment: string option;
  wikibox_special_rights: bool;
}

type wikipage_info = {
  wikipage_source_wiki: wiki;
  wikipage_page: string;
  wikipage_dest_wiki: wiki;
  wikipage_wikibox: int32;
  wikipage_title: string option;
  wikipage_uid : wikipage_uid;
(*  wikipage_css_special_rights; *)
}


type 'a rights_aux = sp:Eliom_sessions.server_params -> 'a -> bool Lwt.t

class type wiki_rights =
object
  method can_admin_wiki :          wiki rights_aux
  method can_create_genwikiboxes : wiki rights_aux
  method can_create_subwikiboxes : wiki rights_aux
  method can_create_wikicss :      wiki rights_aux
  method can_create_wikipages :    wiki rights_aux
  method can_delete_wikiboxes :    wiki rights_aux

  method can_admin_wikibox : wikibox rights_aux
  method can_set_wikibox_specific_permissions : wikibox rights_aux
  method can_write_wikibox : wikibox rights_aux
  method can_read_wikibox : wikibox rights_aux

  method can_create_wikipagecss : wikipage rights_aux

  method can_set_wiki_permissions : wiki rights_aux
  method can_view_history : wikibox rights_aux
  method can_view_oldversions : wikibox rights_aux
  method can_view_oldversions_src : wikibox rights_aux
end


type wikibox_content =
    content_type * string option * int32
