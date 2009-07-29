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


(**
These are all the services related to wikis

*)

open User_sql.Types
open Wiki_widgets_interface
open Wiki_types
let (>>=) = Lwt.bind


(** Polymorphic keys and subsequent functions to govern the display
    of wikiboxes *)

let override_wikibox_key : (wikibox * wikibox_override) Polytables.key = 
  Polytables.make_key ()

(** How to change the display of a wikibox: which wikibox is concerned,
   and what should be displayed instead *)
let get_override_wikibox ~sp =
  try
    Some (Polytables.get
            ~table:(Eliom_sessions.get_request_cache ~sp)
            ~key:override_wikibox_key)
  with Not_found -> None

let set_override_wikibox ~sp v =
  Polytables.set
    ~table:(Eliom_sessions.get_request_cache ~sp)
    ~key:override_wikibox_key
    ~value:v


let wikibox_error_key : (wikibox * exn) Polytables.key = 
  Polytables.make_key ()

(** The error to display in the wikibox *)
let get_wikibox_error ~sp =
  try
    Some (Polytables.get
            ~table:(Eliom_sessions.get_request_cache ~sp)
            ~key:wikibox_error_key)
  with Not_found -> None

let set_wikibox_error ~sp v =
  Polytables.set
    ~table:(Eliom_sessions.get_request_cache ~sp)
    ~key:wikibox_error_key
    ~value:v





let send_wikipage ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ?(menu_style=`Linear) ~page () =
  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
  (* if there is a static page, and should we send it ? *)
  Lwt.catch
    (fun () ->
       match wiki_info.wiki_staticdir with
         | Some dir ->
                Eliom_predefmod.Files.send ~sp (dir ^"/"^ fst page) >>= fun r ->
                  (rights#can_view_static_files sp wiki >>= function
                     | true -> Lwt.return r
                     | false -> Lwt.fail Ocsimore_common.Permission_denied (* XXX We should send a 403. ? *)
             )
         | None -> Lwt.fail Eliom_common.Eliom_404)
    (function
       | Eliom_common.Eliom_404 ->
           Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
           let widgets = Wiki_models.get_widgets wiki_info.wiki_model in
           widgets#display_wikipage ~sp ~wiki ~menu_style ~page
           >>= fun (html, code) ->
           Eliom_duce.Xhtml.send ~sp ~code html
       | e -> Lwt.fail e)


(* Register the services for the wiki [wiki] *)
let register_wiki ~rights ?sp ~path ~wiki () =
  Ocsigen_messages.debug
    (fun () -> Printf.sprintf "Registering wiki %s (at path '%s')"
       (string_of_wiki wiki) (String.concat "/"  path));
  (* Registering the service with suffix for wikipages *)
  (* Note that Eliom will look for the service corresponding to
     the longest prefix. Thus it is possible to register a wiki
     at URL / and another one at URL /wiki and it works,
     whatever be the order of registration *)
  let servpage =
    Eliom_predefmod.Any.register_new_service ~path ?sp
      ~get_params:(Eliom_parameters.suffix (Eliom_parameters.all_suffix "page"))
      (fun sp path () ->
         let page' = Ocsigen_lib.string_of_url_path ~encode:true path in
         send_wikipage ~rights ~sp ~wiki ~page:(page', path) ()
      )
  in
  Wiki_self_services.add_servpage wiki servpage;

  (* the same, but non attached: *)
  let naservpage =
    Eliom_predefmod.Any.register_new_coservice' ?sp
      ~name:("display"^string_of_wiki wiki)
      ~get_params:(Eliom_parameters.string "page")
      (fun sp page () ->
         let path =
           Ocsigen_lib.remove_slash_at_beginning (Neturl.split_path page) in
         let page' = Ocsigen_lib.string_of_url_path ~encode:true path in
         send_wikipage ~rights ~sp ~wiki ~page:(page', path) ()
      )
  in
  Wiki_self_services.add_naservpage wiki naservpage;

  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service ?sp
      ~path:(path@["__ocsiwikicss"])
      ~get_params:Eliom_parameters.unit
      (fun sp () () -> Wiki_data.wiki_css rights sp wiki)
  in
  Wiki_self_services.add_servwikicss wiki wikicss_service



let save_then_redirect overriden_wikibox ~sp redirect_mode f =
  Lwt.catch
    (fun () ->
       f () >>= fun _ ->
       (* We do a redirection to prevent repost *)
       match redirect_mode with
         | `BasePage ->
             Eliom_predefmod.Redirection.send ~sp Eliom_services.void_coservice'
         | `SamePage ->
             Eliom_predefmod.Action.send ~sp ()
    )
    (fun e ->
       set_wikibox_error ~sp (overriden_wikibox, e);
       Eliom_predefmod.Action.send ~sp ())




let ( ** ) = Eliom_parameters.prod

let eliom_wiki : string -> wiki Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32
let eliom_wikibox : string -> wikibox Ocsimore_common.eliom_usertype =
  Ocsimore_common.eliom_opaque_int32

let eliom_wiki_args = eliom_wiki "wid"
let eliom_wikibox_args = eliom_wikibox "wbid"
let eliom_wikipage_args = eliom_wiki_args ** (Eliom_parameters.string "page")
let eliom_css_args =
  (eliom_wiki "widcss" **
   Eliom_parameters.opt (Eliom_parameters.string "pagecss"))
  ** eliom_wikibox "wbcss"



(* Services *)

let path_edit_wiki = [Ocsimore_lib.ocsimore_admin_dir;"edit_wiki"]

open Wiki
open User.GroupsForms

let make_services () =
  let action_edit_css = Eliom_predefmod.Action.register_new_coservice'
    ~name:"css_edit"
    ~get_params:(eliom_wikibox_args **
                   (eliom_css_args **
                      (Eliom_parameters.opt(Eliom_parameters.string "css" **
                                            Eliom_parameters.int32 "version"))))
    (fun sp (wb, args) () ->
       set_override_wikibox ~sp (wb, EditCss args);
       Lwt.return ())

  and action_edit_wikibox = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wiki_edit" ~get_params:eliom_wikibox_args
    (fun sp wb () ->
       set_override_wikibox ~sp (wb, EditWikitext wb);
       Lwt.return ())

  and action_delete_wikibox = Eliom_predefmod.Any.register_new_coservice'
    ~name:"wiki_delete" ~get_params:eliom_wikibox_args
    (fun sp wb () ->
       Wiki_sql.wikibox_wiki wb >>= fun wiki ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       let content_type =
         Wiki_models.get_default_content_type wiki_info.wiki_model in
       save_then_redirect wb ~sp `BasePage
         (fun () -> Wiki_data.save_wikitextbox ~rights ~content_type ~sp ~wb
            ~content:None)
    )

  and action_edit_wikibox_permissions =
    Eliom_predefmod.Action.register_new_coservice'
      ~name:"wikibox_edit_perm" ~get_params:eliom_wikibox_args
      (fun sp wb () ->
         set_override_wikibox ~sp (wb, EditWikiboxPerms wb);
         Lwt.return ())

  and action_edit_wiki_permissions =
    Eliom_predefmod.Action.register_new_coservice'
      ~name:"wiki_edit_perm" ~get_params:(eliom_wikibox_args ** eliom_wiki_args)
      (fun sp (wb, wiki) () ->
         set_override_wikibox ~sp (wb, EditWikiPerms wiki);
         Lwt.return ())

  and action_wikibox_history = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wikibox_history" ~get_params:eliom_wikibox_args
    (fun sp wb () ->
       set_override_wikibox ~sp (wb, History wb);
       Lwt.return ())

  and action_css_history = Eliom_predefmod.Action.register_new_coservice'
    ~name:"css_history" ~get_params:(eliom_wikibox_args ** eliom_css_args)
    (fun sp (wb, css) () ->
       set_override_wikibox ~sp (wb, CssHistory css);
       Lwt.return ())

  and action_css_permissions = Eliom_predefmod.Action.register_new_coservice'
    ~name:"css_edit_perm" ~get_params:(eliom_wikibox_args ** eliom_css_args)
    (fun sp (wb, css) () ->
       set_override_wikibox ~sp (wb, CssPermissions css);
       Lwt.return ())

  and action_old_wikibox = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wiki_old_version"
    ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
    (fun sp (wb, _ver as arg) () ->
       set_override_wikibox ~sp (wb, Oldversion arg);
       Lwt.return ())

  and action_old_wikiboxcss = Eliom_predefmod.Action.register_new_coservice'
    ~name:"css_old_version"
    ~get_params:(eliom_wikibox_args **
                   (eliom_css_args ** (Eliom_parameters.int32 "version")))
    (fun sp (wb, (wbcss, version)) () ->
       set_override_wikibox ~sp (wb, CssOldversion (wbcss, version));
       Lwt.return ())

  and action_src_wikibox = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wiki_src"
    ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
    (fun sp (wb, _ver as arg) () ->
       set_override_wikibox ~sp (wb, Src arg);
       Lwt.return ())

  and action_send_wikiboxtext = Eliom_predefmod.Any.register_new_post_coservice'
    ~keep_get_na_params:false ~name:"wiki_save_wikitext"
    ~post_params:
    (Eliom_parameters.string "actionname" **
       ((eliom_wikibox_args ** Eliom_parameters.int32 "boxversion") **
          Eliom_parameters.string "content"))
    (fun sp () (actionname, ((wb, boxversion), content)) ->
       (* We always show a preview before saving. Moreover, we check that the
          wikibox that the wikibox has not been modified in parallel of our
          modifications. If this is the case, we also show a warning *)
       Wiki.modified_wikibox wb boxversion >>= fun modified ->
       if actionname = "save" then
         match modified with
           | None ->
               Wiki_sql.wikibox_wiki wb >>= fun wiki ->
               Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
               let rights = Wiki_models.get_rights wiki_info.wiki_model in
               let wp = Wiki_models.get_default_wiki_preparser
                 wiki_info.wiki_model in
               Wiki_data.wikibox_content rights sp wb
               >>= fun (content_type, _, _) ->
               wp (sp, wb) content >>= fun content ->
               save_then_redirect wb ~sp `BasePage
                 (fun () -> Wiki_data.save_wikitextbox ~rights
                    ~content_type ~sp ~wb ~content:(Some content))
           | Some _ ->
               set_override_wikibox ~sp
                 (wb, PreviewWikitext (wb, (content, boxversion)));
               Eliom_predefmod.Action.send ~sp ()
         else begin
           set_override_wikibox ~sp
             (wb, PreviewWikitext (wb, (content, boxversion)));
           Eliom_predefmod.Action.send ~sp ()
         end
    )

  and action_send_css = Eliom_predefmod.Any.register_new_post_coservice'
    ~keep_get_na_params:false ~name:"wiki_save_css"
    ~post_params:
    ((eliom_wikibox_args ** (eliom_css_args **
                               Eliom_parameters.int32 "boxversion")) **
       Eliom_parameters.string "content")
    (fun sp () ((wb, (((wikicss, page), wbcss), boxversion)), content) ->
       (* We always show a preview before saving. Moreover, we check that the
          wikibox has not been modified in parallel of our
          modifications. If this is the case, we also show a warning *)
       Wiki.modified_wikibox wbcss boxversion >>= fun modified ->
         match modified with
           | None ->
               Wiki_sql.wikibox_wiki wbcss >>= fun wiki ->
               Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
               let rights = Wiki_models.get_rights wiki_info.wiki_model in
               save_then_redirect wb ~sp `BasePage
                 (fun () -> match page with
                    | None -> Wiki_data.save_wikicssbox ~rights ~sp
                        ~wiki:wikicss ~content:(Some content)
                    | Some page -> Wiki_data.save_wikipagecssbox ~rights ~sp
                        ~wiki:wikicss ~page ~content:(Some content)
                 )
           | Some _ ->
               set_override_wikibox ~sp
                 (wb, EditCss (((wikicss, page), wbcss),
                               Some (content, boxversion)));
               Eliom_predefmod.Action.send ~sp ()
    )

  and action_send_wikibox_permissions =
    let {User.GroupsForms.awr_eliom_params = params; awr_eliom_arg_param= arg} =
      Wiki.helpers_wikibox_permissions in
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"wiki_save_wikibox_permissions"
      ~post_params:(Eliom_parameters.bool "special" ** (arg ** params))
      (fun sp () (special_rights, (wb, perms))->
         Wiki_sql.wikibox_wiki wb >>= fun wiki ->
         Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
         let rights = Wiki_models.get_rights wiki_info.wiki_model in
         save_then_redirect wb ~sp `SamePage
           (fun () -> Wiki_data.set_wikibox_specific_permissions
              ~rights ~sp ~perms ~special_rights ~wb)
      )

  and action_send_wiki_permissions =
    (* !!! must check that the order is the same in wiki_widgets !!! *)
    let params =
    h_wiki_admins.grp_eliom_arg_param **
      (h_wiki_admins.grp_eliom_params **
         (h_subwikiboxes_creators.grp_eliom_params **
            (h_wiki_wikipages_creators.grp_eliom_params **
               (h_wiki_wikiboxes_creators.grp_eliom_params **
                  (h_wiki_css_creators.grp_eliom_params **
                     (h_wiki_wikiboxes_deletors.grp_eliom_params **
                        (h_wiki_wikiboxes_grps.awr_eliom_params **
                           (h_wiki_files_readers.grp_eliom_params **
                              (h_wiki_wikiboxes_src_viewers.grp_eliom_params **
                               h_wiki_wikiboxes_oldversion_viewers.grp_eliom_params)))))))))

  and f_save (rights : Wiki_types.wiki_rights) sp
      (wiki, (adm, (subwbcre, (wpcre, (wbcre, (csscre, (wbdel, (wbgrps, (wfr, (wsrc, wold)))))))))) =
    rights#can_admin_wiki sp wiki >>= function
      | true ->
          h_wiki_admins.grp_save wiki adm >>= fun () ->
          h_subwikiboxes_creators.grp_save wiki subwbcre >>= fun () ->
          h_wiki_wikipages_creators.grp_save wiki wpcre >>= fun () ->
          h_wiki_wikiboxes_creators.grp_save wiki wbcre >>= fun () ->
          h_wiki_css_creators.grp_save wiki csscre >>= fun () ->
          h_wiki_wikiboxes_deletors.grp_save wiki wbdel >>= fun () ->
          h_wiki_files_readers.grp_save wiki wfr >>= fun () ->
          h_wiki_wikiboxes_grps.awr_save wiki wbgrps >>= fun () ->
          h_wiki_wikiboxes_src_viewers.grp_save wiki wsrc >>= fun () ->
          h_wiki_wikiboxes_oldversion_viewers.grp_save wiki wold >>= fun () ->
          Lwt.return ()

      | false -> Lwt.fail Ocsimore_common.Permission_denied
    in
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"wiki_save_wiki_permissions"
      ~post_params:(eliom_wikibox_args ** params)
      (fun sp () (wb, args) ->
         Wiki_sql.get_wiki_info_by_id (fst args) >>= fun wiki_info ->
         let rights = Wiki_models.get_rights wiki_info.wiki_model in
         save_then_redirect wb ~sp `SamePage (fun () -> f_save rights sp args))

  (* Below are the services for the css of wikis and wikipages.  The css
     at the level of wikis are registered in Wiki_data.ml *)

  (* do not use this service, but the one below for css <link>s inside page *)
  and _ = Eliom_predefmod.CssText.register_new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "pagecss"]
    ~get_params:(Eliom_parameters.suffix eliom_wikipage_args)
    (fun sp (wiki, page) () ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Wiki_data.wikipage_css rights sp wiki page)

  (* This is a non attached coservice, so that the css is in the same
     directory as the page. Important for relative links inside the css. *)
  and pagecss_service = Eliom_predefmod.CssText.register_new_coservice'
    ~name:"pagecss" ~get_params:eliom_wikipage_args
    (fun sp (wiki, page) () ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Wiki_data.wikipage_css rights sp wiki page)

  and  _ = Eliom_predefmod.CssText.register_new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "wikicss"]
    ~get_params:(eliom_wiki "wiki")
    (fun sp wiki () ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Wiki_data.wiki_css rights sp wiki)

  and action_create_page = Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"wiki_page_create"
    ~post_params:(Eliom_parameters.opt eliom_wikibox_args **eliom_wikipage_args)
    (fun sp () (wb, (wiki, page)) ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Lwt.catch
         (fun () -> Wiki_data.create_wikipage ~rights ~sp ~wiki ~page)
         (function
            | Wiki_data.Page_already_exists wb ->
                (* The page already exists. If possible, we display an error
                   message in the existing wikibox, which should have
                   contained the button leading to the creation of the page. *)
                set_wikibox_error ~sp  (wb, Wiki_data.Page_already_exists wb);
                Lwt.return ()

            | Ocsimore_common.Permission_denied ->
                (match wb with
                   | None -> ()
                   | Some wb -> set_wikibox_error ~sp
                       (wb, Ocsimore_common.Permission_denied);
                ); Lwt.return ()

            | e -> Lwt.fail e)
    )

  and action_create_css = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wiki_create_css"
    ~get_params:(eliom_wiki_args **
                   (Eliom_parameters.opt (Eliom_parameters.string "pagecss")))
    (fun sp (wiki, page) () ->
       (* YYY add error handler *)
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Wiki_data.create_css ~rights ~sp ~wiki ~page
    )

  and edit_wiki = Eliom_services.new_service
    ~path:path_edit_wiki ~get_params:eliom_wiki_args ()

  and view_wikis = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"view_wikis"]
    ~get_params:Eliom_parameters.unit ()


  in (
    action_edit_css,
    action_edit_wikibox,
    action_delete_wikibox,
    action_edit_wikibox_permissions,
    action_edit_wiki_permissions,
    action_wikibox_history,
    action_css_history,
    action_css_permissions,
    action_old_wikibox,
    action_old_wikiboxcss,
    action_src_wikibox,
    action_send_wikiboxtext,
    action_send_css,
    action_send_wiki_permissions,
    action_send_wikibox_permissions,
    pagecss_service,
    action_create_page,
    action_create_css,
    edit_wiki,
    view_wikis
  )
