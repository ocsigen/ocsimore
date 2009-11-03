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


let wikibox_error_key : (wikibox option * exn) Polytables.key =
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
         let page' = Ocsigen_lib.string_of_url_path ~encode:false path in
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
      ~path:(path@["__wikicss"])
      ~get_params:(Ocsimore_common.eliom_opaque_int32 "wb")
      (fun sp wb () ->
         Wiki_data.wiki_css rights sp wiki >>= fun l ->
         try Lwt.return (let (v, _, _) = List.assoc wb l in v)
         with Not_found -> Lwt.fail Eliom_common.Eliom_404
      )
  in
  Wiki_self_services.add_servwikicss wiki wikicss_service



let save_then_redirect ~sp ?(error=(fun _ _ -> ())) redirect_mode f =
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
       error sp e;
       Eliom_predefmod.Action.send ~sp ())

let error_handler_wb_opt wb sp e =
  set_wikibox_error ~sp (wb, e)

let error_handler_wb wb = error_handler_wb_opt (Some wb)



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


module type WikiServices =
  sig
    open Eliom_parameters
    open Eliom_services

    val action_edit_css :
      ((Wiki_types.wikibox *
        (Wiki_widgets_interface.css_wikibox * ((string * int32) option))),
        unit, [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (((([ | `One of Wiki_types.wiki ] param_name) *
            ([ | `One of string ] param_name)) *
           ([ | `One of Wiki_types.wikibox ] param_name)) *
          (([ | `One of string ] param_name) *
           ([ | `One of int32 ] param_name)))),
        unit, [> | `Registrable]) service

    val action_edit_css_list :
      ((Wiki_types.wikibox * (Wiki_types.wiki * (string option))), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (([ | `One of Wiki_types.wiki ] param_name) *
          ([ | `One of string ] param_name))),
        unit, [> | `Registrable]) service

    val action_edit_wikibox :
      (Wiki_types.wikibox, unit, [> | `Nonattached of [> | `Get] na_s],
        [ | `WithoutSuffix ], [ | `One of Wiki_types.wikibox ] param_name,
        unit, [> | `Registrable]) service

    val action_delete_wikibox :
      (Wiki_types.wikibox, unit, [> | `Nonattached of [> | `Get] na_s],
        [ | `WithoutSuffix ], [ | `One of Wiki_types.wikibox ] param_name,
        unit, [> | `Registrable]) service

    val action_edit_wikibox_permissions :
      (Wiki_types.wikibox, unit, [> | `Nonattached of [> | `Get] na_s],
        [ | `WithoutSuffix ], [ | `One of Wiki_types.wikibox ] param_name,
        unit, [> | `Registrable]) service

    val action_edit_wiki_options :
      ((Wiki_types.wikibox * Wiki_types.wiki), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ([ | `One of Wiki_types.wiki ] param_name)),
        unit, [> | `Registrable]) service

    val action_wikibox_history :
      (Wiki_types.wikibox, unit, [> | `Nonattached of [> | `Get] na_s],
        [ | `WithoutSuffix ], [ | `One of Wiki_types.wikibox ] param_name,
        unit, [> | `Registrable]) service

    val action_css_history :
      ((Wiki_types.wikibox * Wiki_widgets_interface.css_wikibox), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ((([ | `One of Wiki_types.wiki ] param_name) *
           ([ | `One of string ] param_name)) *
          ([ | `One of Wiki_types.wikibox ] param_name))),
        unit, [> | `Registrable]) service

    val action_css_permissions :
      ((Wiki_types.wikibox * Wiki_widgets_interface.css_wikibox), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ((([ | `One of Wiki_types.wiki ] param_name) *
           ([ | `One of string ] param_name)) *
          ([ | `One of Wiki_types.wikibox ] param_name))),
        unit, [> | `Registrable]) service

    val action_old_wikibox :
      ((Wiki_types.wikibox * int32), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ([ | `One of int32 ] param_name)),
        unit, [> | `Registrable]) service

    val action_old_wikiboxcss :
      ((Wiki_types.wikibox * (Wiki_widgets_interface.css_wikibox * int32)),
        unit, [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (((([ | `One of Wiki_types.wiki ] param_name) *
            ([ | `One of string ] param_name)) *
           ([ | `One of Wiki_types.wikibox ] param_name)) *
          ([ | `One of int32 ] param_name))),
        unit, [> | `Registrable]) service

    val action_src_wikibox :
      ((Wiki_types.wikibox * int32), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ([ | `One of int32 ] param_name)),
        unit, [> | `Registrable]) service

    val action_edit_wikipage_properties :
      ((Wiki_types.wikibox * Wiki_types.wikipage), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (([ | `One of Wiki_types.wiki ] param_name) *
          ([ | `One of string ] param_name))),
        unit, [> | `Registrable]) service

    val action_send_wikiboxtext :
      (unit, (string * ((Wiki_types.wikibox * Int32.t) * string)),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of string ] param_name) *
         ((([ | `One of Wiki_types.wikibox ] param_name) *
           ([ | `One of int32 ] param_name)) *
          ([ | `One of string ] param_name))),
        [> | `Registrable]) service

    val action_send_css :
      (unit,
        ((Wiki_types.wikibox *
          (Wiki_widgets_interface.css_wikibox * Int32.t)) *
         string),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        ((([ | `One of Wiki_types.wikibox ] param_name) *
          (((([ | `One of Wiki_types.wiki ] param_name) *
             ([ | `One of string ] param_name)) *
            ([ | `One of Wiki_types.wikibox ] param_name)) *
           ([ | `One of int32 ] param_name))) *
         ([ | `One of string ] param_name)),
        [> | `Registrable]) service

    val action_send_wikibox_permissions :
      (unit, (bool * (Wiki_types.wikibox * User.GroupsForms.six_strings)),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of bool ] param_name) *
         ((Wiki_types.wikibox_arg User.GroupsForms.opaque_int32_eliom_param) *
          User.GroupsForms.six_input_strings)),
        [> | `Registrable]) service

    val action_send_wiki_permissions :
      (unit,
        ((Wiki_types.wikibox option) *
         (Wiki_types.wiki *
          ((string * string) *
           ((string * string) *
            ((string * string) *
             ((string * string) *
              ((string * string) *
               ((string * string) *
                (User.GroupsForms.six_strings *
                 ((string * string) *
                  ((string * string) *
                   ((string * string) * (string * string))))))))))))),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ((Wiki_types.wiki_arg User.GroupsForms.opaque_int32_eliom_param) *
          (User.GroupsForms.two_input_strings *
           (User.GroupsForms.two_input_strings *
            (User.GroupsForms.two_input_strings *
             (User.GroupsForms.two_input_strings *
              (User.GroupsForms.two_input_strings *
               (User.GroupsForms.two_input_strings *
                (User.GroupsForms.six_input_strings *
                 (User.GroupsForms.two_input_strings *
                  (User.GroupsForms.two_input_strings *
                   (User.GroupsForms.two_input_strings * User.GroupsForms.
                    two_input_strings)))))))))))),
        [> | `Registrable]) service

    val pagecss_service :
      ((Wiki_types.wikipage * Wiki_types.wikibox), unit,
        [> | `Nonattached of [> | `Get] na_s], [ | `WithoutSuffix ],
        ((([ | `One of Wiki_types.wiki ] param_name) *
          ([ | `One of string ] param_name)) *
         ([ | `One of Wiki_types.wikibox ] param_name)),
        unit, [> | `Registrable]) service

    val action_create_page :
      (unit, ((Wiki_types.wikibox option) * Wiki_types.wikipage),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (([ | `One of Wiki_types.wiki ] param_name) *
          ([ | `One of string ] param_name))),
        [> | `Registrable]) service

    val action_create_css :
      (unit,
        (Wiki_types.wikibox *
         ((Wiki_types.wiki * (string option)) *
          (Wiki_types.media_type * (Wiki_types.wikibox option)))),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ((([ | `One of Wiki_types.wiki ] param_name) *
           ([ | `One of string ] param_name)) *
          (([ | `Set of string ] param_name) *
           ([ | `One of Wiki_types.wikibox option ] param_name)))),
        [> | `Registrable]) service

    val action_send_css_options :
      (unit,
        (Wiki_types.wikibox *
         (((Wiki_widgets_interface.css_wikibox * (Wiki_types.wikibox option)) *
           Wiki_types.media_type) *
          int32)),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (((((([ | `One of Wiki_types.wiki ] param_name) *
              ([ | `One of string ] param_name)) *
             ([ | `One of Wiki_types.wikibox ] param_name)) *
            ([ | `One of Wiki_types.wikibox ] param_name)) *
           ([ | `Set of string ] param_name)) *
          ([ | `One of int32 ] param_name))),
        [> | `Registrable]) service

    val edit_wiki :
      (Wiki_types.wiki, unit,
        [>
          | `Attached of [> | `Internal of ([> | `Service] * [> | `Get])] a_s
        ], [ | `WithoutSuffix ], [ | `One of Wiki_types.wiki ] param_name,
        unit, [> | `Registrable]) service

    val view_wikis :
      (unit, unit,
        [>
          | `Attached of [> | `Internal of ([> | `Service] * [> | `Get])] a_s
        ], [ | `WithoutSuffix ], unit, unit, [> | `Registrable]) service

    val action_send_wikipage_properties :
      (unit,
        (Wiki_types.wikibox *
         (Wiki_types.wikipage *
          (string * ((Wiki_types.wikibox option) * string)))),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         ((([ | `One of Wiki_types.wiki ] param_name) *
           ([ | `One of string ] param_name)) *
          (([ | `One of string ] param_name) *
           (([ | `One of Wiki_types.wikibox option ] param_name) *
            ([ | `One of string ] param_name))))),
        [> | `Registrable]) service

    val action_send_wiki_metadata :
      (unit,
        ((Wiki_types.wikibox option) *
         (Wiki_types.wiki * (string * (Wiki_types.wikibox option)))),
        [> | `Nonattached of [> | `Post] na_s], [ | `WithoutSuffix ], unit,
        (([ | `One of Wiki_types.wikibox ] param_name) *
         (([ | `One of Wiki_types.wiki ] param_name) *
          (([ | `One of string ] param_name) *
           ([ | `One of Wiki_types.wikibox option ] param_name)))),
        [> | `Registrable]) service

    val edit_wiki_permissions_ocsisite :
      (Wiki_types.wiki, unit,
        [>
          | `Attached of [> | `Internal of ([> | `Service] * [> | `Get])] a_s
        ], [ | `WithoutSuffix ], [ | `One of Wiki_types.wiki ] param_name,
        unit, [> | `Registrable]) service

end


module MakeServices (X: sig end) : WikiServices = struct

  let action_edit_css = Eliom_predefmod.Action.register_new_coservice'
    ~name:"css_edit"
    ~get_params:(eliom_wikibox_args **
                   (eliom_css_args **
                      (Eliom_parameters.opt(Eliom_parameters.string "css" **
                                            Eliom_parameters.int32 "version"))))
    (fun sp (wb, args) () ->
       set_override_wikibox ~sp (wb, EditCss args);
       Lwt.return ())

  and action_edit_css_list = Eliom_predefmod.Action.register_new_coservice'
    ~name:"list_css_edit"
    ~get_params:(eliom_wikibox_args **
                   (eliom_wiki_args **
                      Eliom_parameters.opt (Eliom_parameters.string "pagecss")))
    (fun sp (wb, args) () ->
       set_override_wikibox ~sp (wb, EditCssList args);
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
       save_then_redirect ~sp ~error:(error_handler_wb wb) `BasePage
         (fun () -> Wiki_data.save_wikitextbox ~rights ~content_type ~sp ~wb
            ~content:None)
    )

  and action_edit_wikibox_permissions =
    Eliom_predefmod.Action.register_new_coservice'
      ~name:"wikibox_edit_perm" ~get_params:eliom_wikibox_args
      (fun sp wb () ->
         set_override_wikibox ~sp (wb, EditWikiboxPerms wb);
         Lwt.return ())

  and action_edit_wiki_options =
    Eliom_predefmod.Action.register_new_coservice'
      ~name:"wiki_edit_options"
      ~get_params:(eliom_wikibox_args ** eliom_wiki_args)
      (fun sp (wb, wiki) () ->
         set_override_wikibox ~sp (wb, EditWikiOptions wiki);
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

  and action_edit_wikipage_properties = Eliom_predefmod.Action.register_new_coservice'
    ~name:"wikipage_properties"
    ~get_params:(eliom_wikibox_args ** eliom_wikipage_args)
    (fun sp (wb, wp) () ->
       set_override_wikibox ~sp (wb, EditWikipageProperties wp);
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
               save_then_redirect ~sp ~error:(error_handler_wb wb) `BasePage
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
       (* As above, we check that the wikibox has not been modified in parallel
          of our modifications. If this is the case, we also show a warning *)
       Wiki.modified_wikibox wbcss boxversion >>= fun modified ->
         match modified with
           | None ->
               Wiki_sql.wikibox_wiki wbcss >>= fun wiki ->
               Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
               let rights = Wiki_models.get_rights wiki_info.wiki_model in
               save_then_redirect ~sp ~error:(error_handler_wb wb) `BasePage
                 (fun () -> match page with
                    | None -> Wiki_data.save_wikicssbox ~rights ~sp
                        ~wiki:wikicss ~content:(Some content) ~wb:wbcss
                    | Some page -> Wiki_data.save_wikipagecssbox ~rights ~sp
                        ~wiki:wikicss ~page ~content:(Some content) ~wb:wbcss
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
         save_then_redirect ~sp ~error:(error_handler_wb wb) `SamePage
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
                                 (h_wiki_wikiboxes_oldversion_viewers.grp_eliom_params **
                                  h_wiki_metadata_editors.grp_eliom_params))))))))))

  and f_save (rights : Wiki_types.wiki_rights) sp
      (wiki, (adm, (subwbcre, (wpcre, (wbcre, (csscre, (wbdel, (wbgrps, (wfr, (wsrc, (wold, wmetadata))))))))))) =
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
          h_wiki_metadata_editors.grp_save wiki wmetadata >>= fun () ->
          Lwt.return ()

      | false -> Lwt.fail Ocsimore_common.Permission_denied
    in
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"wiki_save_wiki_permissions"
      ~post_params:(Eliom_parameters.opt eliom_wikibox_args ** params)
      (fun sp () (wb, args) ->
         Wiki_sql.get_wiki_info_by_id (fst args) >>= fun wiki_info ->
         let rights = Wiki_models.get_rights wiki_info.wiki_model in
         save_then_redirect ~sp ~error:(error_handler_wb_opt wb) `SamePage
           (fun () -> f_save rights sp args))

  (* Below are the service for the css of a wikipage.  The css
     at the level of wikis are stored in Wiki_self_services and
     registered in Wiki_data.ml *)

  (* This is a non attached coservice, so that the css is in the same
     directory as the page. Important for relative links inside the css. *)
  and pagecss_service = Eliom_predefmod.CssText.register_new_coservice'
    ~name:"pagecss" ~get_params:(eliom_wikipage_args ** eliom_wikibox_args)
    (fun sp ((wiki, page), wb) () ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       Wiki_data.wikipage_css rights sp wiki page >>= fun l ->
       try Lwt.return (let v, _, _ = List.assoc wb l in v)
       with Not_found -> Lwt.fail Eliom_common.Eliom_404
    )

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
                set_wikibox_error ~sp  (Some wb,
                                        Wiki_data.Page_already_exists wb);
                Lwt.return ()

            | Ocsimore_common.Permission_denied ->
                set_wikibox_error ~sp (wb, Ocsimore_common.Permission_denied);
                Lwt.return ()

            | e -> Lwt.fail e)
    )

  and action_create_css = Eliom_predefmod.Any.register_new_post_coservice'
    ~name:"wiki_create_css" ~keep_get_na_params:true
    ~post_params:(eliom_wikibox_args **
                    ((eliom_wiki_args **
                       Eliom_parameters.opt (Eliom_parameters.string "pagecss"))
                     ** (Eliom_parameters.set Eliom_parameters.string "media"
                         ** Ocsimore_common.eliom_opaque_int32_opt "wbcss")))
    (fun sp () (wb, ((wiki, page), (media, wbcss))) ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       save_then_redirect ~sp ~error:(error_handler_wb wb) `SamePage
         (fun () ->
            Wiki_data.add_css ~rights ~sp ~wiki ~page ~media ?wbcss ())
    )

  and action_send_css_options = Eliom_predefmod.Any.register_new_post_coservice'
    ~name:"wiki_send_css_options" ~keep_get_na_params:true
    ~post_params:(eliom_wikibox_args **
                    (((eliom_css_args ** Eliom_parameters.opt
                        (eliom_wikibox "newwbcss")) **
                       Eliom_parameters.set Eliom_parameters.string "media")
                     ** Eliom_parameters.int32 "rank"))
    (fun sp () (wb, (((((wiki, page), wbcss), newwbcss), media), rank)) ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       save_then_redirect ~sp ~error:(error_handler_wb wb) `SamePage
         (fun () ->
            match newwbcss with
              | None -> Wiki_data.delete_css ~sp ~rights ~wiki ~page ~wb:wbcss
              | Some newwb ->
                  Wiki_data.update_css ~sp ~rights ~wiki ~page ~oldwb:wbcss
                    ~newwb:newwb ~media ~rank)
    )

  and edit_wiki = Eliom_services.new_service
    ~path:path_edit_wiki ~get_params:eliom_wiki_args ()

  and view_wikis = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"view_wikis"]
    ~get_params:Eliom_parameters.unit ()

  and action_send_wikipage_properties =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~keep_get_na_params:false ~name:"wikipage_save_properties"
      ~post_params:
    (eliom_wikibox_args **
       (eliom_wikipage_args **
          (Eliom_parameters.string "title" **
             ((Ocsimore_common.eliom_opaque_int32_opt "wb" **
               Eliom_parameters.string "newpage")))))
    (fun sp () (wb, ((wiki, page), (title, (wbpage, newpage)))) ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       save_then_redirect ~sp ~error:(error_handler_wb wb) `BasePage
         (fun () -> Wiki_data.save_wikipage_properties ~rights ~sp
            ~title ~wb:wbpage ~newpage (wiki, page))
    )

  and action_send_wiki_metadata =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~keep_get_na_params:false ~name:"wiki_save_metadata"
      ~post_params:
    (Eliom_parameters.opt eliom_wikibox_args **
       (eliom_wiki_args **
          (Eliom_parameters.string "descr" **
           Ocsimore_common.eliom_opaque_int32_opt "container")))
    (fun sp () (wb, (wiki, (descr, container))) ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       save_then_redirect ~sp ~error:(error_handler_wb_opt wb) `BasePage
         (fun () -> Wiki_data.update_wiki ~rights ~sp ~container ~descr wiki)
    )

  and edit_wiki_permissions_ocsisite = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"edit_wikis_permissions"]
    ~get_params:eliom_wiki_args ()

end
