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
   @author Boris Yakobowski
*)

open User_sql.Types
open Wiki_sql.Types


let (>>=) = Lwt.bind
let ( ** ) = Eliom_parameters.prod


let aux_grp name descr =
  Lwt_unix.run (User_sql.new_parametrized_group ~prefix:"wiki" ~name
                  ~fullname:descr)

let admin_writer_reader_aux ~name ~descr =
  Users.GenericRights.create_admin_writer_reader ~prefix:"wiki" ~name ~descr


(** All wiki-related groups *)

(* Small hack : we want a special group, but without parameter *)
let wikis_creator =
  let grp = aux_grp "WikisCreators" "Users who can create new wikis" in
  grp $ (Opaque.int32_t 1l)


(** Groups taking a wiki as argument *)

let wiki_admins : wiki_arg parameterized_group = aux_grp
  "WikiAdmin" "All rights on the wiki"

let wiki_wikiboxes_creators : wiki_arg parameterized_group = aux_grp
  "WikiWikiboxesCreator" "Can create wikiboxes in the wiki"
let wiki_wikipages_creators : wiki_arg parameterized_group = aux_grp
  "WikiWikipagesCreator" "Can create wikipages in the wiki"
let wiki_css_creators : wiki_arg parameterized_group = aux_grp
  "WikiCssCreator" "Can create css for the wiki"

let wiki_wikiboxes_grps : wiki_arg admin_writer_reader = admin_writer_reader_aux
  ~name:"WikiWikiboxes" ~descr:"the wikiboxes of the wiki"

let wiki_files_grps : wiki_arg admin_writer_reader = admin_writer_reader_aux
  ~name:"WikiFiles" ~descr:"the files in the wiki"


(** The following groups take a wikibox as argument. They are used to override
    generic wiki permissions. *)
let wikibox_grps : wikibox_arg admin_writer_reader = admin_writer_reader_aux
  ~name:"Wikibox" ~descr:"the wikibox"


(** These groups take a wikipage as argument *)
let wikipage_css_creators : wikipage_arg parameterized_group = aux_grp
  "WikipageCssCreator" "Can create css for the wikipage"


(* Hierarchy of wiki groups :
 w : parameterized by wikis
 wb : parameterized by wikiboxes
 wp : parameterized by wikipages (unused right now)

       -------- WikiAdmin(w)---------------------------------------------
      /                 |                 \             \                \
WikiboxesAdmins(w)   FilesAdmins(w)    Wikiboxes     Wikipages    CssCreators(w)
     |                  |             Creators(w)    Creators(w)
WikiboxesWriters(w)  FilesWriters(w)
     |                  |
WikiboxesReaders(w)  FilesReaders(w)


WikiboxAdmin(wb)
   |
WikiboxWriter(wb)
   |
WikiboxReader(wb)

*)


let () = Lwt_unix.run (
  let add_admin g =
    User_sql.add_generic_inclusion ~superset:g ~subset:wiki_admins
  in
  add_admin wiki_wikiboxes_creators       >>= fun () ->
  add_admin wiki_wikipages_creators       >>= fun () ->
  add_admin wiki_css_creators             >>= fun () ->
  add_admin wiki_wikiboxes_grps.grp_admin >>= fun () ->
  add_admin wiki_files_grps.grp_admin     >>= fun () ->
  Lwt.return ()
)


open Users.GenericRights

let can_sthg_wikibox f ~sp ~wb:(wid, _ as wb) =
  Wiki_sql.get_wikibox_info wb
  >>= fun { wikibox_uid = uid ; wikibox_special_rights = special_rights }->
  let g = if special_rights then
    (f.field wikibox_grps) $ uid
  else
    (f.field wiki_wikiboxes_grps) $ wid
  in
  Users.in_group ~sp ~group:g ()

let aux_group grp ~sp data =
  Users.in_group ~sp ~group:(grp $ data) ()


class wiki_rights =
  let can_adm_wb, can_wr_wb, can_re_wb = can_sthg can_sthg_wikibox in
object
  method can_admin_wikibox = can_adm_wb
  method can_write_wikibox = can_wr_wb
  method can_read_wikibox = can_re_wb

  method can_create_wikipages = aux_group wiki_wikipages_creators

  method can_create_wikiboxes = aux_group wiki_wikiboxes_creators

  method can_create_wikicss = aux_group wiki_css_creators
  method can_create_wikipagecss ~sp (wiki, _page : wikipage) =
  (* XXX add a field to override by wikipage and use wikipage_css_creators *)
  Users.in_group ~sp ~group:(wiki_css_creators $ wiki) ()

end







(** Auxiliary functions and structures to edit rights *)

let helpers_wikibox_permissions =
  Users.GenericRights.helpers_admin_writer_reader
    "edit_wikibox_permissions" wikibox_grps
