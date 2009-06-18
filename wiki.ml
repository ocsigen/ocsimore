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
let ( ** ) = Eliom_parameters.prod



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
  Lwt.return (Eliom_duce.Xhtml.make_uri ~service ~sp page)




(** Wiki groups *)

let prefix = "wiki"

let aux_grp name descr =
  let g = Lwt_unix.run
    (User_sql.new_parameterized_group ~prefix ~name ~fullname:descr) in
  g, User.GroupsForms.helpers_group name g

let admin_writer_reader_aux ~name ~descr =
  User.GenericRights.create_admin_writer_reader ~prefix:"wiki" ~name ~descr


(** All wiki-related groups *)

let wikis_creator =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix ~name:"WikisCreators"
       ~fullname:"Users who can create new wikis")


(** Groups taking a wiki as argument *)

let (wiki_admins, h_wiki_admins : wiki_arg parameterized_group * _) = aux_grp
  "WikiAdmin" "All rights on the wiki"

let (wiki_subwikiboxes_creators, h_subwikiboxes_creators :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiSubwikiboxesCreator" "Can create subwikiboxes in the wiki"

let (wiki_wikipages_creators, h_wiki_wikipages_creators :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiWikipagesCreator" "Can create wikipages in the wiki"

let (wiki_genwikiboxes_creators, h_wiki_genwikiboxes_creators :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiGenWikiboxesCreator" "Can create wikiboxes in the wiki"

let (wiki_css_creators, h_wiki_css_creators :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiCssCreator" "Can create css for the wiki"

let (wiki_wikiboxes_deletors, h_wiki_wikiboxes_deletors :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiWikiboxesDeletor" "Can delete wikiboxes in the wiki"

let wiki_wikiboxes_grps : wiki_arg admin_writer_reader =
  admin_writer_reader_aux ~name:"WikiWikiboxes"
    ~descr:"the wikiboxes of the wiki"
let h_wiki_wikiboxes_grps = User.GroupsForms.helpers_admin_writer_reader
  "WikiWikiboxes" wiki_wikiboxes_grps

let (wiki_files_readers, h_wiki_files_readers :
       wiki_arg parameterized_group * _) = aux_grp
  "WikiFilesReader" "can read the staic files for this wiki"


(** The following groups take a wikibox as argument. They are used to override
    generic wiki permissions. *)
let wikibox_grps : wikibox_arg admin_writer_reader = admin_writer_reader_aux
  ~name:"Wikibox" ~descr:"the wikibox"


(** These groups take a wikipage as argument *)
let (wikipage_css_creators, _ : wikipage_arg parameterized_group * _)= aux_grp
  "WikipageCssCreator" "Can create css for the wikipage"


(* Hierarchy of wiki groups :
 w : parameterized by wikis
 wb : parameterized by wikiboxes
 wp : parameterized by wikipages (unused right now)

       -------- WikiAdmin(w)-------------------------------------------------
      /                 |                 |           |             |       |
WikiboxesAdmins(w)   SubWikiboxes    Wikipages   CssCreators(w) Wikiboxes   |
     |               Creators(w)    Creators(w)      /          Deletors(w) |
WikiboxesWriters(w)          \            |         /                       |
     |                        --    GenWikiboxes  --                        |
WikiboxesReaders(w)                  Creators(w)                FilesReaders(w)


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
  User_sql.add_generic_inclusion 
    ~superset:wiki_genwikiboxes_creators ~subset:wiki_subwikiboxes_creators
  >>= fun () ->
  User_sql.add_generic_inclusion 
    ~superset:wiki_genwikiboxes_creators ~subset:wiki_wikipages_creators
  >>= fun () ->
  User_sql.add_generic_inclusion 
    ~superset:wiki_genwikiboxes_creators ~subset:wiki_css_creators
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
  let can_adm_wb, can_wr_wb, can_re_wb = can_sthg can_sthg_wikibox in
object (self)
  method can_create_wiki ~sp () =
    User.in_group ~sp ~group:wikis_creator ()

 method can_admin_wiki = aux_group wiki_admins

  method can_admin_wikibox = can_adm_wb
  method can_write_wikibox = can_wr_wb
  method can_read_wikibox = can_re_wb

  method can_view_static_files = aux_group wiki_files_readers

  method can_create_wikipages = aux_group wiki_wikipages_creators
  method can_create_subwikiboxes = aux_group wiki_subwikiboxes_creators
  method can_create_genwikiboxes = aux_group wiki_genwikiboxes_creators
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

(*VVV Les suivantes à réécrire (créer des groupes pour chaque) *)
  method can_set_wiki_permissions = aux_group wiki_admins (* trop fort *)
(* = can_admin ? *)
  method can_view_history = can_wr_wb (* trop fort *)
  method can_view_oldversions = can_wr_wb (* trop fort *)
  method can_view_oldversions_src = can_wr_wb (* trop fort *)


end







(** Auxiliary functions and structures to edit rights *)

open User.GroupsForms

let helpers_wikibox_permissions =
  User.GroupsForms.helpers_admin_writer_reader
    "edit_wikibox_permissions" wikibox_grps

let helpers_wiki_permissions =
  let params =
    h_wiki_admins.grp_eliom_arg_param **
      (h_wiki_admins.grp_eliom_params **
         (h_subwikiboxes_creators.grp_eliom_params **
            (h_wiki_wikipages_creators.grp_eliom_params **
               (h_wiki_genwikiboxes_creators.grp_eliom_params **
                  (h_wiki_css_creators.grp_eliom_params **
                     (h_wiki_wikiboxes_deletors.grp_eliom_params **
                        (h_wiki_wikiboxes_grps.awr_eliom_params **
                         h_wiki_files_readers.grp_eliom_params)))))))

  and f_save (rights : Wiki_types.wiki_rights) sp
      (wiki, (adm, (subwbcre, (wpcre, (wbcre, (csscre, (wbdel, (wbgrps, wfr)))))))) =
    rights#can_admin_wiki sp wiki >>= function
      | true ->
          h_wiki_admins.grp_save wiki adm >>= fun () ->
          h_subwikiboxes_creators.grp_save wiki subwbcre >>= fun () ->
          h_wiki_wikipages_creators.grp_save wiki wpcre >>= fun () ->
          h_wiki_genwikiboxes_creators.grp_save wiki wbcre >>= fun () ->
          h_wiki_css_creators.grp_save wiki csscre >>= fun () ->
          h_wiki_wikiboxes_deletors.grp_save wiki wbdel >>= fun () ->
          h_wiki_files_readers.grp_save wiki wfr >>= fun () ->
          h_wiki_wikiboxes_grps.awr_save wiki wbgrps
      | false -> Lwt.fail Ocsimore_common.Permission_denied
  and form wiki =
    let aux h = h.grp_form_fun wiki in
    aux h_wiki_admins "Administer the wiki" >>= fun f1 ->
    aux h_subwikiboxes_creators "Create subwikiboxes" >>= fun f2 ->
    aux h_wiki_wikipages_creators "Create wikipages" >>= fun f3 ->
    aux h_wiki_genwikiboxes_creators "Create wikiboxes" >>= fun f4 ->
    aux h_wiki_css_creators "Create CSS" >>= fun f5 ->
    aux h_wiki_wikiboxes_deletors "Delete wikiboxes" >>= fun f6 ->
    h_wiki_wikiboxes_grps.awr_form_fun wiki >>= fun f7 ->
    aux h_wiki_files_readers "Read static files" >>= fun f8 ->

    let msg = Ocamlduce.Utf8.make
      ("Permissions for wiki " ^ string_of_wiki wiki)
    and msg_wikiboxes = Ocamlduce.Utf8.make "Global permissions for wikiboxes:"
    in
    Lwt.return (
      fun (narg, (n1, (n2, (n3, (n4, (n5, (n6, (n7, n8)))))))) ->
        {{ [
             <p>[ !msg ]
             <p>[ {: h_wiki_admins.grp_form_arg wiki narg :}
                  !{: f1 n1 :} <br>[]
                  !{: f2 n2 :} <br>[]
                  !{: f3 n3 :} <br>[]
                  !{: f4 n4 :} <br>[]
                  !{: f5 n5 :} <br>[]
                  !{: f6 n6 :} <br>[]
                  !{: f8 n8 :} <br>[]
                  <b>msg_wikiboxes <br>[]
                  !{: f7 n7 :}]
             <p>[ {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{"Save"}} :} ]
           ] }})

  in
  params, f_save, form




(** A text suitable as the default text for a container page *)
let default_container_page =
  "= Ocsimore wikipage\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"


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
    ?wiki_css ?container_text
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

   (match wiki_css with
      | None -> Lwt.return ()
      | Some css -> Wiki_sql.set_css_for_wiki ~wiki:wiki_id ~author (Some css)
   ) >>= fun () ->

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
    bi_subbox = None;
    bi_box = wikibox;
    bi_wiki = wiki;
    bi_rights = rights;
    bi_page = None;
  }
