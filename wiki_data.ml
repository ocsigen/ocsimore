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
  let g = Lwt_unix.run (User_sql.new_parametrized_group ~prefix:"wiki" ~name
                          ~fullname:descr) in
  g, Users.GroupsForms.helpers_group name g

let admin_writer_reader_aux ~name ~descr =
  Users.GenericRights.create_admin_writer_reader ~prefix:"wiki" ~name ~descr


(** All wiki-related groups *)

(* Small hack : we want a special group, but without parameter *)
let wikis_creator =
  let grp, _h = aux_grp "WikisCreators" "Users who can create new wikis" in
  grp $ (Opaque.int32_t 1l)


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
let h_wiki_wikiboxes_grps = Users.GroupsForms.helpers_admin_writer_reader
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

v v v v v v v
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
object (self)
  method can_admin_wiki = aux_group wiki_admins

  method can_admin_wikibox = can_adm_wb
  method can_write_wikibox = can_wr_wb
  method can_read_wikibox = can_re_wb

  method can_create_wikipages = aux_group wiki_wikipages_creators
  method can_create_subwikiboxes = aux_group wiki_subwikiboxes_creators
  method can_create_genwikiboxes = aux_group wiki_genwikiboxes_creators
  method can_delete_wikiboxes = aux_group wiki_wikiboxes_deletors

  method can_create_wikicss = aux_group wiki_css_creators
  method can_create_wikipagecss ~sp (wiki, _page : wikipage) =
  (* XXX add a field to override by wikipage and use wikipage_css_creators *)
  Users.in_group ~sp ~group:(wiki_css_creators $ wiki) ()

  method can_set_wikibox_specific_permissions ~sp (wiki, _  as wb) =
    Wiki_sql.get_wiki_info_by_id wiki
    >>= fun { wiki_boxrights = boxrights } ->
      if boxrights then
        self#can_admin_wikibox ~sp ~wb
      else
        Lwt.return false

end







(** Auxiliary functions and structures to edit rights *)

open Users.GroupsForms




(*
h_wiki_admins
h_subwikiboxes_creators
h_wiki_wikipages_creators
h_wiki_genwikiboxes_creators
h_wiki_css_creators
h_wiki_wikiboxes_deletors
h_wiki_wikiboxes_grps
h_wiki_files_readers
*)



let helpers_wikibox_permissions =
  Users.GroupsForms.helpers_admin_writer_reader
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

  and f_save (rights : wiki_rights) sp (wiki, (adm, (subwbcre, (wpcre, (wbcre, (csscre, (wbdel, (wbgrps, wfr)))))))) =
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
  (* Uncomment to check to see that the type are correct *)
  (*(fun v sp ->
     let service = Eliom_predefmod.Any.register_new_post_coservice'
       ~name:"foo"  ~post_params:params
       (fun sp () args ->
          f_save (new wiki_rights) sp args >>= fun () ->
          Eliom_predefmod.Redirection.send ~sp
            Eliom_services.void_coservice')
     in
     form v >>= fun form ->
     Lwt.return (Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
       ~service ~sp form () : Eliom_duce.Xhtml.form_elt)
  )*)
  params, f_save, form

