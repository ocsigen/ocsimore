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

open Wiki_sql.Types


let (>>=) = Lwt.bind


let create_static_group name fullname =
  (Lwt_unix.run (
     Users.create_user
       ~name:("#" ^ name)
       ~pwd:User_sql.Connect_forbidden
       ~fullname
       ~groups:[]
       ()
   )).Users.id


type admin_writer_reader = {
  grp_admin: User_sql.userid;
  grp_writer: User_sql.userid;
  grp_reader: User_sql.userid;
}

let grp_admin grp = grp.grp_admin
let grp_write grp = grp.grp_writer
let grp_read  grp = grp.grp_reader

let can_sthg f =
  f grp_admin,
  f grp_write,
  f grp_read


let create_admin_writer_reader ~name ~descr =
  let namea, namew, namer =
    (name ^ "Admin",
     name ^ "Writer",
     name ^ "Reader")
  and descra, descrw, descrr =
    ("All rights on " ^ descr,
     "Can write in " ^ descr,
     "Can read the " ^ descr)
  in
  let ga, gw, gr =
    create_static_group namea descra,
    create_static_group namew descrw,
    create_static_group namer descrr
  in
  Lwt_unix.run (
    Users.add_to_group ~user:ga ~group:gw >>= fun () ->
    Users.add_to_group ~user:gw ~group:gr
  );
  { grp_admin = ga; grp_writer = gw; grp_reader = gr }


(** All wiki-related groups *)

let wikis_creator = (* This group has no argument *)
  create_static_group "WikisCreators" "Users who can create new wikis"


(** The following groups take a wiki id as argument *)
let wiki_admins =
  create_static_group "WikiAdmin" "All rights on the wiki"


let wiki_wikiboxes_creators = create_static_group
  "WikiWikiboxesCreator" "Can create wikiboxes in the wiki"
let wiki_wikipages_creators = create_static_group
  "WikiWikipagesCreator" "Can create wikipages in the wiki"

let wiki_wikiboxes_grps = create_admin_writer_reader
  ~name:"WikiWikiboxes"
  ~descr:"the wikiboxes of the wiki"

(*
let wiki_wikipages_admins, wiki_wikipages_writers, wiki_wikipages_readers =
  create_admin_writer_reader
    ~name:"WikiWikipages"
    ~descr:"the wikipages of the wiki"
*)

let wiki_wikicss_grps = create_admin_writer_reader
  ~name:"WikiWikicss"
  ~descr:"the css of the wiki"

let wiki_wikipagescss_grps = create_admin_writer_reader
  ~name:"WikiWikipagescss"
  ~descr:"the css of the wikipages of the wiki"


(** The following groups take a wikibox as argument. They are used to override
    generic wiki permissions. *)
let wikibox_grps = create_admin_writer_reader
  ~name:"Wikibox"
  ~descr:"the wikibox"

let () = Lwt_unix.run (
  let add_admin g =
    Users.add_to_group ~group:g ~user:wiki_admins
  in
  add_admin wiki_wikiboxes_creators >>= fun () ->
  add_admin wiki_wikipages_creators >>= fun () ->
  add_admin wiki_wikiboxes_grps.grp_admin   >>= fun () ->
  add_admin wiki_wikicss_grps.grp_admin     >>= fun () ->
  add_admin wiki_wikipagescss_grps.grp_admin
)


let parametrized_group (group: User_sql.userid) (_param : int32) = group


let wiki_to_32 (w : wiki) = Opaque.t_int32 w

let can_sthg_wikitext f ~sp ~sd ~wb:(wid, _ as wb) =
  Wiki_sql.get_wikibox_info wb
  >>= fun { wikibox_uid = uid ; wikibox_special_rights = special_rights }->
  let g = if special_rights then
    parametrized_group (f wikibox_grps) uid
  else
    parametrized_group (f wiki_wikiboxes_grps) (wiki_to_32 wid)
  in
  Users.in_group ~sp ~sd ~group:g ()

let can_admin_wikitext, can_write_wikitext, can_read_wikitext =
  can_sthg can_sthg_wikitext


let can_sthg_wikicss f ~sp ~sd ~wiki =
  Wiki_sql.get_css_wikibox_for_wiki wiki
  >>= function
    | None -> Lwt.fail Not_found
    | Some wb ->
        Wiki_sql.get_wikibox_info (wiki, wb)
        >>= fun { wikibox_uid = uid ; wikibox_special_rights = special_rights}->
        let g = if special_rights then
          parametrized_group (f wikibox_grps) uid
        else
          parametrized_group (f wiki_wikicss_grps) (wiki_to_32 wiki)
        in
        Users.in_group ~sp ~sd ~group:g ()


let can_admin_wikicss, can_write_wikicss, can_read_wikicss =
  can_sthg can_sthg_wikicss


(* Semantics wikipage vs. wiki ! *)
let can_sthg_wikipagecss f ~sp ~sd ~wiki ~page=
  Wiki_sql.get_css_wikibox_for_wikipage wiki page
  >>= function
    | None -> Lwt.fail Not_found
    | Some wb ->
        Wiki_sql.get_wikibox_info (wiki, wb)
        >>= fun { wikibox_uid = uid ; wikibox_special_rights = special_rights}->
        (if special_rights then
           Lwt.return (parametrized_group (f wikibox_grps) uid)
         else
           Wiki_sql.get_wikipage_info wiki page >>=
           fun { wikipage_uid = uid } ->
           Lwt.return (parametrized_group (f wiki_wikipagescss_grps) uid)
        ) >>= fun g ->
        Users.in_group ~sp ~sd ~group:g ()

let can_admin_wikipagecss, can_write_wikipagecss, can_read_wikipagecss =
  can_sthg can_sthg_wikipagecss
