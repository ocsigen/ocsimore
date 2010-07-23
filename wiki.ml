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

(** This file contains all the groups for the wiki component of Ocsimore,
    a default implemenetation of a [Wiki_types.Wiki_rights] structure,
    and a few other misc definitions *)

open User_sql.Types
open Wiki_types


let (>>=) = Lwt.bind


(** Name of the administration wiki. This is the name that must
    be used when creating (or searching for) this wiki. If that
    name is changed, the database *must* be upgraded manually to
    reflect the change *)
let wiki_admin_name = "Adminwiki"

exception No_admin_wiki

let get_admin_wiki () =
  Lwt.catch
    (fun () -> Wiki_sql.get_wiki_info_by_name wiki_admin_name)
    (function
       | Not_found -> raise No_admin_wiki
       | e -> Lwt.fail e)


let wiki_admin_servpage () =
  get_admin_wiki () >>= fun wadmin ->
  Lwt.return (match Wiki_self_services.find_servpage wadmin.wiki_id with
                | None -> raise No_admin_wiki
                | Some service -> service)


let wiki_admin_page_link sp page =
  wiki_admin_servpage () >>= fun service ->
  Lwt.return (Eliom_predefmod.Xhtml.make_uri ~service ~sp page)




(** Wiki groups *)

let prefix = "wiki"

let param_wiki = {
  param_description = "name of the wiki";
  param_display = Some (
    (fun wid ->
       Wiki_sql.get_wiki_info_by_id (wiki_of_sql wid) >>= fun wiki ->
       Lwt.return (Printf.sprintf "'%s' (%s)" wiki.wiki_title wiki.wiki_descr)
    ));
  find_param_functions =
    Some ((fun wname ->
             Wiki_sql.get_wiki_info_by_name wname >>= fun wiki ->
             Lwt.return (sql_of_wiki (wiki.wiki_id))),
          (fun wid ->
             Wiki_sql.get_wiki_info_by_id (wiki_of_sql wid) >>= fun wiki ->
             Lwt.return wiki.wiki_title))
}

let param_wikibox = {
  param_description = "id of the wikibox";
  param_display = None;
  find_param_functions = Some
    ((fun s -> Scanf.sscanf s "wikibox %ld" (fun v -> Lwt.return v)),
     (fun v -> Lwt.return (
               Printf.sprintf "wikibox %ld" v)))
}

let param_wikipage = {
  param_description = "id of the wikipage";
  param_display = None; (* XXX Show name and wiki *)
  find_param_functions = None;
}

let aux_grp name descr find_param =
  Lwt_unix.run
    (User_sql.new_parameterized_group ~prefix ~name ~descr ~find_param)

let admin_writer_reader_aux ~name ~descr ~find_param =
  User.GenericRights.create_admin_writer_reader ~prefix:"wiki" ~name ~descr ~find_param


(** All wiki-related groups *)

let wikis_creator =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix ~name:"WikisCreators"
       ~descr:"can create new wikis")


(** Groups taking a wiki as argument *)

let aux_grp_wiki name descr =
  (aux_grp name descr param_wiki : wiki_arg parameterized_group)

let wiki_admins =
  aux_grp_wiki "WikiAdmin" "can change all permissions of the wiki"

let wiki_subwikiboxes_creators = aux_grp_wiki
  "WikiSubwikiboxesCreator" "can create subwikiboxes in the wiki"

let wiki_wikipages_creators = aux_grp_wiki
  "WikiWikipagesCreator" "can create wikipages in the wiki"

let wiki_css_creators = aux_grp_wiki
  "WikiCssCreator" "can create css for the wiki"

let wiki_wikiboxes_creators = aux_grp_wiki
  "WikiGenWikiboxesCreator" "can create wikiboxes in the wiki"

let wiki_wikiboxes_deletors = aux_grp_wiki
  "WikiWikiboxesDeletor" "can delete wikiboxes in the wiki"

let wiki_wikiboxes_grps : wiki_arg admin_writer_reader =
  admin_writer_reader_aux ~name:"WikiWikiboxes"
    ~descr:"the wikiboxes of the wiki" ~find_param:param_wiki

let wiki_files_readers = aux_grp_wiki
  "WikiFilesReader" "can read the static files for this wiki"

let wiki_wikiboxes_src_viewers = aux_grp_wiki
  "WikiWikiboxesSrcViewers" "can view the source of a wikibox"

let wiki_wikiboxes_oldversion_viewers = aux_grp_wiki
  "WikiWikiboxesOldversionViewers" "can view an old version of a wikibox"

let wiki_metadata_editors = aux_grp_wiki
  "WikiMetadataEditors" "can modify the metadata for the wiki"


let rec ok sp g_admin user group = function
  | [] -> Lwt.return false
  | pgroup :: q ->
      match user_is_applied_parameterized_group ~user:group ~pgroup with
        | None -> ok sp g_admin user group q
        | Some v ->
            User.in_group ~sp ~user ~group:(g_admin $ v) () >>= function
              | true -> Lwt.return true
              | false -> ok sp g_admin user group q


let () =
  let g1, g2, g3 = User.GenericRights.map_awr
    (fun v -> v.User.GenericRights.field wiki_wikiboxes_grps) in
  let l1 = [
    wiki_admins;
    wiki_subwikiboxes_creators;
    wiki_wikipages_creators;
    wiki_css_creators;
    wiki_wikiboxes_creators;
    wiki_wikiboxes_deletors;
    wiki_files_readers;
    wiki_wikiboxes_src_viewers;
    wiki_wikiboxes_oldversion_viewers;
    wiki_metadata_editors;
  ] and l2 = [
    g1;
    g2;
    g3;
  ] in
  User_data.add_group_admin_function
    (fun ~sp ~user ~group ->
       ok sp (User.GenericRights.grp_admin.User.GenericRights.field
                wiki_wikiboxes_grps) user group l2 >>= function
         | true -> Lwt.return true
         | false -> ok sp wiki_admins user group (l2 @ l1)
    )



(** The following groups take a wikibox as argument. They are used to override
    generic wiki permissions. *)
let wikibox_grps : wikibox_arg admin_writer_reader = admin_writer_reader_aux
  ~name:"Wikibox" ~descr:"the wikibox" ~find_param:param_wikibox

let () =
  let g1, g2, g3 = User.GenericRights.map_awr
    (fun v -> v.User.GenericRights.field wikibox_grps) in
  let l = [
    g1;
    g2;
    g3;
  ] in
  User_data.add_group_admin_function
    (fun ~sp ~user ~group ->
       ok sp (User.GenericRights.grp_admin.User.GenericRights.field
                wikibox_grps) user group l
    )



(** These groups take a wikipage as argument *)
let (wikipage_css_creators : wikipage_arg parameterized_group)= aux_grp
  "WikipageCssCreator" "can create css for the wikipage" param_wikipage


(* Hierarchy of wiki groups :
 w : parameterized by wikis
 wb : parameterized by wikiboxes
 wp : parameterized by wikipages (unused right now)



       -------- WikiAdmin(w)-----------------------------------------------(1)-<
      /                 |                 |           |             |
WikiboxesAdmins(w)   SubWikiboxes    Wikipages   CssCreators(w) Wikiboxes
     |               Creators(w)    Creators(w)      /          Deletors(w)
WikiboxesWriters(w)          \            |         /
     |                        --    GenWikiboxes  --
WikiboxesReaders(w)                  Creators(w)


>-(1)---------------------------------------------------------------------(2)-<
            |                            |                      |
   WikiboxesOldversionViewers(w)  WikiboxesSrcViewsers(w)  FilesReaders(w)


>-(2)-----
         |
    MetaDataWEditors(w)


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
  add_admin wiki_subwikiboxes_creators    >>= fun () ->
  add_admin wiki_wikiboxes_deletors       >>= fun () ->
  add_admin wiki_wikipages_creators       >>= fun () ->
  add_admin wiki_css_creators             >>= fun () ->
  add_admin wiki_wikiboxes_grps.grp_admin >>= fun () ->
  add_admin wiki_files_readers            >>= fun () ->
  add_admin wiki_wikiboxes_src_viewers    >>= fun () ->
  add_admin wiki_metadata_editors         >>= fun () ->
  add_admin wiki_wikiboxes_oldversion_viewers >>= fun () ->

  User_sql.add_generic_inclusion
    ~superset:wiki_wikiboxes_creators ~subset:wiki_subwikiboxes_creators
  >>= fun () ->
  User_sql.add_generic_inclusion
    ~superset:wiki_wikiboxes_creators ~subset:wiki_wikipages_creators
  >>= fun () ->
  User_sql.add_generic_inclusion
    ~superset:wiki_wikiboxes_creators ~subset:wiki_css_creators
)


open User.GenericRights

let can_sthg_wikibox f ~sp wb =
  Wiki_sql.get_wikibox_info wb
  >>= fun { wikibox_special_rights = special_rights ; wikibox_wiki = wiki}->
  let g = if special_rights then
    (f.field wikibox_grps) $ wb
  else
    (f.field wiki_wikiboxes_grps) $ wiki
  in
  User.in_group ~sp ~group:g ()

let aux_group grp ~sp data =
  User.in_group ~sp ~group:(grp $ data) ()


class wiki_rights : Wiki_types.wiki_rights =
  let can_adm_wb, can_wr_wb, can_re_wb = map_awr can_sthg_wikibox in
object (self)
  method can_create_wiki ~sp () =
    User.in_group ~sp ~group:wikis_creator ()

  method can_admin_wiki = aux_group wiki_admins
  method can_edit_metadata = aux_group wiki_metadata_editors
  method can_set_wiki_permissions = self#can_admin_wiki (* By construction *)

  method can_admin_wikibox = can_adm_wb
  method can_write_wikibox = can_wr_wb
  method can_read_wikibox = can_re_wb

  method can_view_static_files = aux_group wiki_files_readers

  method can_create_wikipages = aux_group wiki_wikipages_creators
  method can_create_subwikiboxes = aux_group wiki_subwikiboxes_creators
  method can_create_wikiboxes = aux_group wiki_wikiboxes_creators
  method can_delete_wikiboxes = aux_group wiki_wikiboxes_deletors

  method can_create_wikicss = aux_group wiki_css_creators
  method can_create_wikipagecss ~sp (wiki, _page : wikipage) =
  (* XXX add a field to override by wikipage and use wikipage_css_creators *)
  User.in_group ~sp ~group:(wiki_css_creators $ wiki) ()

  method can_set_wikibox_specific_permissions ~sp wb =
    Wiki_sql.wikibox_wiki wb >>= fun wiki ->
    Wiki_sql.get_wiki_info_by_id wiki
    >>= fun { wiki_boxrights = boxrights } ->
      if boxrights then
        self#can_admin_wikibox ~sp wb
      else
        Lwt.return false

  (* YYY We might want to introduce overrides at the level of wikiboxes *)
  method can_view_oldversions ~sp wb =
    Wiki_sql.wikibox_wiki wb >>= fun w ->
    aux_group wiki_wikiboxes_oldversion_viewers sp w

  method can_view_history = self#can_view_oldversions

  method can_view_oldversions_src ~sp wb =
    self#can_view_oldversions ~sp wb >>= function
      | false -> Lwt.return false
      | true -> self#can_view_src ~sp wb

  method can_view_src ~sp wb =
    Wiki_sql.wikibox_wiki wb >>= fun w ->
    aux_group wiki_wikiboxes_src_viewers sp w


  method can_admin_wikipage ~sp (wiki,_page) =
    (* XXX create a real group, and some permissions overrides *)
    self#can_admin_wiki sp wiki


end



(** A text suitable as the default text for a container page *)
let default_container_page =
  "= Ocsimore wikipage\r\n\r\n<<loginbox>>\r\n<<cond ingroup='users' |<<switchmenu>> >>\r\n\r\n<<content>>"


(** An exception raised when we register two wikis at the same path.
   The first two strings are the description of the conflicting wikis,
   the third string is the path *)
exception Wiki_already_registered_at_path of (string * string) * string
(** Same thing with two wikis of the same name. The [wiki] returned
    is the id of the existing wiki *)
exception Wiki_with_same_title of wiki


(** Creation of a wiki

    If the optional argument [path] is present, the wiki will be bound to the
    URL represented by [path] when it is registered.

    The argument [container_text] is the wikicreole code for the container
    wikibox of the wiki; [wiki_css] is the css for the wiki.

    If [boxrights] is true (default), it is possible to set the rights on
    each box individually.
*)
let create_wiki ~title ~descr ?path ?staticdir ?(boxrights = true)
    ~author
    ?(admins=[basic_user author]) ?(readers = [basic_user User.anonymous])
    ?container_text
    ~model
    () =
  let path_string = Ocsimore_lib.bind_opt
    path (Ocsigen_lib.string_of_url_path ~encode:true)
  in
  (* We check that no wiki of the same name or already registered at the same
     path exists. There are some race conditions in this code, but
     - when a Postgres error is raised, it is difficult to know the
     cause of the error, as we only get a string. Thus it is perilous
     to try and see if an error is raised
     - nothing can be done for the race on the 'path' field, as wikis
     are not registered in this function
  *)
  Ocsimore_lib.lwt_bind_opt path_string
    (fun path -> Wiki_sql.iter_wikis
       (fun ({ wiki_title = title'; wiki_pages = path' } as w) ->
          if path' = Some path then
            Lwt.fail (Wiki_already_registered_at_path
                        ((descr, w.wiki_descr), path))
          else
            if title = title' then
              Lwt.fail (Wiki_with_same_title w.wiki_id)
            else
              Lwt.return ())
    ) >>= fun _ ->
  (* Notice that there is a theoretical race condition in the code below,
     when the container wikibox receives its rights, in the case this
     container has changed between the creation of the wiki and the moments
     the rights are added *)
  Wiki_sql.new_wiki ~title ~descr ~pages:path_string
     ~boxrights ?staticdir ?container_text ~author ~model ()
   >>= fun (wiki_id, _wikibox_container) ->

   (* Putting users in groups *)
   (* Admins *)
   User.add_list_to_group ~l:admins ~group:(wiki_admins $ wiki_id) >>= fun ()->
   (* Readers *)
   User.add_list_to_group ~l:readers
     ~group:(wiki_wikiboxes_grps.grp_reader $ wiki_id) >>= fun () ->
   User.add_list_to_group ~l:readers
     ~group:(wiki_files_readers $ wiki_id) >>= fun () ->

   Lwt.return wiki_id



(** [modified_wikibox box version] returns [Some curversion] iff the current
    version [curversion] of [box] is greater than [version], [None]
    otherwise *)
let modified_wikibox ~wikibox ~boxversion =
  Wiki_sql.current_wikibox_version wikibox
  >>= function
    | None -> Lwt.return None (* This case is not supposed to happen *)
    | Some curversion ->
        if curversion > boxversion then
          Lwt.return (Some curversion)
        else
          Lwt.return None


let default_bi ~sp ~wikibox ~rights =
  Wiki_sql.wikibox_wiki wikibox >>= fun wiki ->
  Lwt.return {
    Wiki_widgets_interface.bi_sp = sp;
    bi_ancestors = Wiki_widgets_interface.Ancestors.no_ancestors;
    bi_subbox = (fun _ -> Lwt.return None);
    bi_box = wikibox;
    bi_wiki = wiki;
    bi_rights = rights;
    bi_page = wiki, None;
    bi_menu_style = `Linear;
  }
