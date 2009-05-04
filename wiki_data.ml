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


(** All wiki-related groups *)

(* XXX
let wikis_creator = (* This group has no argument *)
  create_static_group "WikisCreators" "Users who can create new wikis"
*)

let aux_grp name descr =
  Lwt_unix.run (User_sql.new_parametrized_group name descr)


let wiki_admins : wiki_arg parameterized_group =
  aux_grp "WikiAdmin" "All rights on the wiki"



let wiki_wikiboxes_creators : wiki_arg parameterized_group = aux_grp
  "WikiWikiboxesCreator" "Can create wikiboxes in the wiki"
let wiki_wikipages_creators : wiki_arg parameterized_group = aux_grp
  "WikiWikipagesCreator" "Can create wikipages in the wiki"

let wiki_wikiboxes_grps : wiki_arg admin_writer_reader =
  Users.GenericRights.create_admin_writer_reader
    ~name:"WikiWikiboxes" ~descr:"the wikiboxes of the wiki"

(* XXX
let wiki_wikipages_admins, wiki_wikipages_writers, wiki_wikipages_readers =
  create_admin_writer_reader
    ~name:"WikiWikipages"
    ~descr:"the wikipages of the wiki"
*)

let wiki_wikicss_grps : wiki_arg admin_writer_reader =
  Users.GenericRights.create_admin_writer_reader
    ~name:"WikiWikicss" ~descr:"the css of the wiki"

let wiki_wikipagescss_grps : wiki_arg admin_writer_reader =
  Users.GenericRights.create_admin_writer_reader
    ~name:"WikiWikipagescss" ~descr:"the css of the wikipages of the wiki"


(** The following groups take a wikibox as argument. They are used to override
    generic wiki permissions. *)
let wikibox_grps : wikibox_arg admin_writer_reader =
  Users.GenericRights.create_admin_writer_reader
  ~name:"Wikibox" ~descr:"the wikibox"


(*
(** These groups take a wikipage as argument *)
let wikipagescss_grps : wikipage_arg admin_writer_reader =
  create_admin_writer_reader
    ~name:"WikiWikipagescss" ~descr:"the css of the wikipages"
*)


let () = Lwt_unix.run (
  let add_admin g =
    User_sql.add_generic_inclusion ~superset:g ~subset:wiki_admins
  in
  add_admin wiki_wikiboxes_creators >>= fun () ->
  add_admin wiki_wikipages_creators >>= fun () ->
  add_admin wiki_wikiboxes_grps.grp_admin   >>= fun () ->
  add_admin wiki_wikicss_grps.grp_admin     >>= fun () ->
  add_admin wiki_wikipagescss_grps.grp_admin
)

let opaque_wikibox v = (Opaque.int32_t v : wikibox_arg Opaque.int32_t)



open Users.GenericRights

let can_sthg_wikitext f ~sp ~sd ~wb:(wid, _ as wb) =
  Wiki_sql.get_wikibox_info wb
  >>= fun { wikibox_uid = uid ; wikibox_special_rights = special_rights }->
  let g = if special_rights then
    apply_parameterized_group (f.field wikibox_grps) (opaque_wikibox uid)
  else
    apply_parameterized_group (f.field wiki_wikiboxes_grps) wid
  in
  Users.in_group ~sp ~sd ~group:g ()


let can_admin_wikitext, can_write_wikitext, can_read_wikitext =
  can_sthg can_sthg_wikitext


let can_sthg_wikicss f ~sp ~sd ~wiki =
  Wiki_sql.get_css_wikibox_for_wiki wiki
  >>= (function
    | None ->
        Lwt.return (apply_parameterized_group (f.field wiki_wikicss_grps) wiki)
    | Some wb ->
        Wiki_sql.get_wikibox_info (wiki, wb)
        >>= fun { wikibox_uid = uid ;
                  wikibox_special_rights = special_rights } ->
        Lwt.return (
          if special_rights then
            apply_parameterized_group (f.field wikibox_grps)(opaque_wikibox uid)
          else
            apply_parameterized_group (f.field wiki_wikicss_grps) wiki
        )
      )
  >>= fun g ->
  Users.in_group ~sp ~sd ~group:g ()

let can_admin_wikicss, can_write_wikicss, can_read_wikicss =
  can_sthg can_sthg_wikicss


let can_sthg_wikipagecss f ~sp ~sd ~wiki ~page =
  Wiki_sql.get_css_wikibox_for_wikipage wiki page
  >>= (function
    | None -> Lwt.fail Not_found
    | Some wb ->
        Wiki_sql.get_wikibox_info (wiki, wb)
        >>= fun { wikibox_uid = uid ;
                  wikibox_special_rights = special_rights } ->
        Lwt.return (
          if special_rights then
            apply_parameterized_group (f.field wikibox_grps)(opaque_wikibox uid)
          else
            apply_parameterized_group (f.field wiki_wikipagescss_grps) wiki
        )
      )
  >>= fun g ->
  Users.in_group ~sp ~sd ~group:g ()

let can_admin_wikipagecss, can_write_wikipagecss, can_read_wikipagecss =
  can_sthg can_sthg_wikipagecss

