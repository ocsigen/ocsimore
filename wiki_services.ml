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

open Wiki_sql.Types
let (>>=) = Lwt.bind



type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

exception Not_css_editor

type wiki_action_info =
  | Edit_box of wikibox
  | Edit_perm of wikibox
  | History of wikibox
  | Oldversion of (wikibox * int32)
  | Src of (wikibox * int32)
  | Error of (wikibox * wiki_errors)
  | Delete_Box of wikibox
(* The second uple is the content of the wikibox, and the version number of the
   wikibox *when the edition started*. This is used to display a warning in case
   of concurrent edits*)
  | Preview of (wikibox * (string * int32))

exception Wiki_action_info of wiki_action_info





(** Name of the administration wiki. This is the name that must
    be used when creating (or searching for) this wiki. If that
    name is changed, the database *must* be upgraded manually to
    reflect the change *)
let wiki_admin_name = "Adminwiki"

let get_admin_wiki () =
  Wiki_sql.get_wiki_info_by_name wiki_admin_name




(** A text suitable as the default text for a container page *)
let default_container_page =
  "= Ocsimore wikipage\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"


let send_static_file sp sd wiki dir page =
  Wiki.readers_group wiki.wiki_id >>= fun g ->
  Users.get_user_id ~sp ~sd >>= fun userid ->
  Users.in_group ~sp ~sd ~user:userid ~group:g () >>= function
    | true -> Eliom_predefmod.Files.send ~sp (dir^"/"^page)
    | false -> Lwt.fail Eliom_common.Eliom_404


let wikicss_service_handler wiki () =
  Wiki_sql.get_css_for_wiki wiki >>= function
    | None -> Lwt.fail Eliom_common.Eliom_404
    | Some css -> Lwt.return css

let wikipagecss_service_handler (wiki, page) () =
  Wiki_sql.get_css_for_wikipage wiki page >>= function
    | Some css -> Lwt.return css
    | None -> Lwt.fail Eliom_common.Eliom_404




(* a table containing the Eliom services generating pages
   for each wiki associated to an URL *)
module Servpages = 
  Hashtbl.Make(struct 
                 type t = wiki
                 let equal = (=) 
                 let hash = Hashtbl.hash 
               end)

let naservpages :
    (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
     [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
     unit, [`Registrable ]) Eliom_services.service Servpages.t =
  Servpages.create 5
let servpages :
    (string list, unit, Eliom_services.get_service_kind,
     [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ]) Eliom_services.service Servpages.t =
  Servpages.create 5
let servwikicss = Servpages.create 5

let add_naservpage = Servpages.add naservpages
let add_servpage = Servpages.add servpages
let add_servwikicss = Servpages.add servwikicss
let find_naservpage = Servpages.find naservpages
let find_servpage k = 
  try Some (Servpages.find servpages k)
  with Not_found -> None
let find_servwikicss k = 
  try Some ((Servpages.find servwikicss k) :
              (unit, unit,
               [ `Attached of
                    [ `Internal of [ `Service ] * [ `Get ] ] Eliom_services.a_s ],
               [ `WithoutSuffix ], unit, unit, [ `Registrable ])
              Eliom_services.service :>
              (unit, unit,
               [ `Attached of
                   [> `Internal of [> `Service ] * [ `Get ] ] Eliom_services.a_s ],
               [ `WithoutSuffix ], unit, unit, [ `Registrable ])
              Eliom_services.service
           )
  with Not_found -> None


(*
(** find services for each wiki *)
val find_naservpage : Wiki_sql.wiki ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service

val find_servpage : Wiki_sql.wiki ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service option

val find_servwikicss : Wiki_sql.wiki ->
  (unit, unit,
   [ `Attached of
       [> `Internal of [> `Service ] * [ `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [ `Registrable ])
    Eliom_services.service option

val add_naservpage : Wiki_sql.wiki ->
  (string, unit, [ `Nonattached of [ `Get ] Eliom_services.na_s ],
   [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name,
   unit, [`Registrable ])
    Eliom_services.service -> unit

val add_servpage : Wiki_sql.wiki ->
  (string list, unit,
   Eliom_services.get_service_kind,
   [ `WithSuffix ], [ `One of string list ] Eliom_parameters.param_name,
     unit, [ `Registrable ])
    Eliom_services.service -> unit

val add_servwikicss : Wiki_sql.wiki ->
  (unit, unit,
   [ `Attached of
      [ `Internal of [ `Service ] * [ `Get ] ] Eliom_services.a_s ],
   [ `WithoutSuffix ], unit, unit, [ `Registrable ])
    Eliom_services.service -> unit
*)




let ( ** ) = Eliom_parameters.prod


let eliom_wiki_args = Wiki_sql.eliom_wiki "wikiid"
let eliom_wikibox_args = eliom_wiki_args ** (Eliom_parameters.int32 "boxid")
let eliom_wikipage_args = eliom_wiki_args ** (Eliom_parameters.string "page")



(* Register the services for the wiki [wiki] *)
let register_wiki ?sp ~path ~wikibox_widget ~wiki () =
  Ocsigen_messages.debug
    (fun () -> Printf.sprintf "Registering wiki %s (at path '%s')"
       (wiki_id_s wiki) (String.concat "/"  path));
  (* Registering the service with suffix for wikipages *)
  (* Note that Eliom will look for the service corresponding to
     the longest prefix. Thus it is possible to register a wiki
     at URL / and another one at URL /wiki and it works,
     whatever be the order of registration *)
  let servpage =
    Eliom_predefmod.Any.register_new_service ~path ?sp
      ~get_params:(Eliom_parameters.suffix (Eliom_parameters.all_suffix "page"))
      (fun sp path () ->
         let sd = Ocsimore_common.get_sd sp in
         let bi = { Wiki_widgets_interface.bi_sp = sp;
                    bi_sd = sd;
                    bi_ancestors = Wiki_widgets_interface.no_ancestors;
                    bi_subbox = None;
                  } in
         wikibox_widget#send_page ~bi ~wiki
           ~page:(Ocsigen_lib.string_of_url_path ~encode:true path)
      )
  in
  add_servpage wiki servpage;

  (* the same, but non attached: *)
  let naservpage =
    Eliom_predefmod.Any.register_new_coservice' ?sp
      ~name:("display"^wiki_id_s wiki)
      ~get_params:(Eliom_parameters.string "page")
      (fun sp page () ->
         let path = Ocsigen_lib.remove_slash_at_beginning (Neturl.split_path page)
         in
         let page = Ocsigen_lib.string_of_url_path ~encode:true path in
         let sd = Ocsimore_common.get_sd sp in
         let bi = { Wiki_widgets_interface.bi_sp = sp;
                    bi_sd = sd;
                    bi_ancestors = Wiki_widgets_interface.no_ancestors;
                    bi_subbox = None;
                  } in
         wikibox_widget#send_page ~bi ~wiki ~page
      )
  in
  add_naservpage wiki naservpage;

  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service ?sp
      ~path:(path@["__ocsiwikicss"])
      ~get_params:Eliom_parameters.unit
      (fun _sp () () -> wikicss_service_handler wiki ())
  in
  add_servwikicss wiki wikicss_service;

  Lwt.return ()


(* Creates and registers a wiki if it does not already exists  *)
let create_and_register_wiki ~title ~descr
    ?sp
    ?path
    ?(readers = [Users.anonymous.Users.id])
    ?(writers = [Users.authenticated_users.Users.id])
    ?(rights_adm = [])
    ?(wikiboxes_creators = [Users.authenticated_users.Users.id])
    ?(container_adm = [])
    ?(page_creators = [Users.authenticated_users.Users.id])
    ?(css_editors = [Users.authenticated_users.Users.id])
    ?(admins = [])
    ?(boxrights = true)
    ?staticdir
    ?wiki_css
    ~container_page
    ~wikibox_widget
    () =
  Lwt.catch
    (fun () -> Wiki_sql.get_wiki_info_by_name title)
    (function
       | Not_found ->
           begin
             Wiki.really_create_wiki ~title ~descr ?path ~readers ~writers
               ~rights_adm ~wikiboxes_creators ~container_adm ~page_creators
               ~css_editors ~admins ~boxrights ?staticdir ?wiki_css
               ~container_page ()
             >>= fun wiki_id ->
             Wiki_sql.get_wiki_info_by_id wiki_id
             >>= fun w ->
             match path with
               | None -> Lwt.return w
               | Some path -> register_wiki ?sp ~path ~wikibox_widget
                   ~wiki:w.wiki_id ()
             >>= fun () ->
             Lwt.return w
           end
       | e -> Lwt.fail e)





let save_wikibox_aux ~sp ~sd ~wikibox ~enough_rights ~content ~content_type =
  Lwt.catch
    (fun () ->
       Wiki.save_wikibox ~enough_rights ~sp ~sd ~wikibox ~content ~content_type
       >>= fun _version ->
       (* We do a redirection to prevent repost *)
       Eliom_predefmod.Redirection.send ~sp Eliom_services.void_coservice')
    (fun e ->
       let e = match e with
         | Ocsimore_common.Permission_denied -> Operation_not_allowed
         | _ -> Action_failed e
       in
       Eliom_predefmod.Action.send ~sp
         [Ocsimore_common.Session_data sd;
          Wiki_action_info (Error (wikibox, e))])



let services () =

let service_edit_wikibox = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "wiki_edit"]
  ~get_params:eliom_wikibox_args ()


and service_edit_wikipage_css = Eliom_services.new_coservice'
    ~name:"css_edit" ~get_params:eliom_wikipage_args ()

and service_edit_wiki_css = Eliom_services.new_coservice'
  ~name:"wiki_css_edit" ~get_params:eliom_wiki_args ()

and action_edit_wikibox = Eliom_predefmod.Actions.register_new_coservice'
  ~name:"wiki_edit" ~get_params:eliom_wikibox_args
  (fun _sp wbox () ->
     Lwt.return [Wiki_action_info (Edit_box wbox)])

and action_delete_wikibox = Eliom_predefmod.Any.register_new_coservice'
  ~name:"wiki_delete" ~get_params:eliom_wikibox_args
  (fun sp wikibox () ->
     let sd = Ocsimore_common.get_sd sp in
     save_wikibox_aux ~enough_rights:Wiki.user_can_save_wikibox
       ~sp ~sd ~wikibox ~content:"" ~content_type:Wiki_sql.Deleted
  )

and action_edit_wikibox_permissions =
  Eliom_predefmod.Actions.register_new_coservice'
    ~name:"wiki_edit_perm" ~get_params:eliom_wikibox_args
    (fun sp wbox () ->
       let sd = Ocsimore_common.get_sd sp in
       Wiki.get_role sp sd wbox
       >>= fun role ->
         if role = Wiki.Admin then
           Lwt.return [Ocsimore_common.Session_data sd;
                       Wiki_action_info (Edit_perm wbox)]
         else Lwt.return [Ocsimore_common.Session_data sd])

and action_wikibox_history = Eliom_predefmod.Actions.register_new_coservice'
  ~name:"wiki_history" ~get_params:eliom_wikibox_args
  (fun _sp g () -> Lwt.return [Wiki_action_info (History g)])

and action_old_wikibox = Eliom_predefmod.Actions.register_new_coservice'
  ~name:"wiki_old_version"
  ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
  (fun _sp g () -> Lwt.return [Wiki_action_info (Oldversion g)])

and action_src_wikibox = Eliom_predefmod.Actions.register_new_coservice'
  ~name:"wiki_src"
  ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
  (fun _sp g () -> Lwt.return [Wiki_action_info (Src g)])

and action_send_wikibox = Eliom_predefmod.Any.register_new_post_coservice'
  ~keep_get_na_params:false ~name:"wiki_send"
  ~post_params:
  (Eliom_parameters.string "actionname" **
     (((Wiki_sql.eliom_wiki "wikiid" **
          (Eliom_parameters.int32 "boxid" **
             Eliom_parameters.int32 "boxversion")
       ) **
         Eliom_parameters.string "content")))
  (fun sp () (actionname, (((wiki_id, (box_id, boxversion)), content))) ->
     let wikibox = (wiki_id, box_id) in
     (* We always show a preview before saving. Moreover, we check that the
        wikibox that the wikibox has not been modified in parallel of our
        modifications. If this is the case, we also show a warning *)
     Wiki.modified_wikibox wikibox boxversion
     >>= fun modified ->
       if actionname = "save" then
         match modified with
           | None ->
               let sd = Ocsimore_common.get_sd sp in
               Wiki_filter.preparse_extension (sp, sd, box_id) wiki_id content
               >>= fun content ->
               save_wikibox_aux ~enough_rights:Wiki.user_can_save_wikibox
                 ~sp ~sd ~wikibox ~content ~content_type:Wiki_sql.Wiki
           | Some _ ->
               Eliom_predefmod.Action.send ~sp
                 [Wiki_action_info (Preview (wikibox, (content, boxversion)))]
       else
         Eliom_predefmod.Action.send ~sp
           [Wiki_action_info (Preview (wikibox, (content, boxversion)))]
      )

and action_send_wikibox_permissions =
  Eliom_predefmod.Any.register_new_post_coservice'
    ~keep_get_na_params:true
    ~name:"wiki_send_permissions"
    ~post_params:
    (eliom_wikibox_args **
       (Eliom_parameters.string "addreaders" **
          (Eliom_parameters.string "addwriters" **
             (Eliom_parameters.string "addrightadm" **
                (Eliom_parameters.string "addwbcr" **
                   (Eliom_parameters.string "delreaders" **
                      (Eliom_parameters.string "delwriters" **
                         (Eliom_parameters.string "delrightadm" **
                            Eliom_parameters.string "delwbcr")
                      )))))))
    (fun sp () p ->
       let sd = Ocsimore_common.get_sd sp in
       Wiki.save_wikibox_permissions sp sd p >>= fun () ->
       Eliom_predefmod.Redirection.send ~sp  Eliom_services.void_hidden_coservice'
    )

and action_send_wikipage_css =
  Eliom_predefmod.Redirection.register_new_post_coservice'
    ~keep_get_na_params:false ~name:"css_send"
    ~post_params:(eliom_wikipage_args ** Eliom_parameters.string "content")
    (fun sp () ((wiki, page), content) ->
       let sd = Ocsimore_common.get_sd sp in
       Users.get_user_data sp sd
       >>= fun user ->
       Wiki_sql.set_css_for_wikipage ~wiki ~page content ~author:user.Users.id
       >>= fun () ->
       Lwt.return Eliom_services.void_coservice'
    )

and action_send_wiki_css =
  Eliom_predefmod.Redirection.register_new_post_coservice'
    ~keep_get_na_params:false
    ~name:"wiki_css_send"
    ~post_params:
    (Wiki_sql.eliom_wiki "wikiid" ** Eliom_parameters.string "content")
    (fun sp () (wiki, content) ->
       let sd = Ocsimore_common.get_sd sp in
       Users.get_user_data sp sd
       >>= fun user ->
       Wiki_sql.set_css_for_wiki ~wiki ~author:user.Users.id content >>= fun () ->
       Lwt.return Eliom_services.void_coservice'
    )

(* Below are the services for the css of wikis and wikipages.  The css
   at the level of wikis are registered in Wiki.ml *)

(* do not use this service, but the one below for css <link>s inside page *)
and _ = Eliom_predefmod.CssText.register_new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "pagecss"]
  ~get_params:(Eliom_parameters.suffix eliom_wikipage_args)
  (fun _sp -> wikipagecss_service_handler)


(* This is a non attached coservice, so that the css is in the same
   directory as the page. Important for relative links inside the css. *)
and pagecss_service = Eliom_predefmod.CssText.register_new_coservice'
  ~name:"pagecss" ~get_params:eliom_wikipage_args
  (fun _sp -> wikipagecss_service_handler)

and  _ = Eliom_predefmod.CssText.register_new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "wikicss"]
  ~get_params:(Wiki_sql.eliom_wiki "wiki")
  (fun _sp -> wikicss_service_handler)

and action_create_page = Eliom_predefmod.Actions.register_new_post_coservice'
  ~name:"wiki_page_create" ~post_params:eliom_wikipage_args
  (fun sp () (wiki, page) ->
     let sd = Ocsimore_common.get_sd sp in
     Users.get_user_id ~sp ~sd
     >>= fun userid ->
     Wiki.page_creators_group wiki
     >>= fun creators ->
     Users.in_group ~sp ~sd ~user:userid ~group:creators ()
     >>= function
       | true ->
           Lwt.catch
             (fun () ->
                Wiki_sql.get_box_for_page wiki page
                >>= fun _ ->
                  (* The page already exists *)
                  Lwt.return [Ocsimore_common.Session_data sd]
                    (*VVV Put an error message *)
             )
             (function
                | Not_found ->
                    Wiki.new_wikibox ~wiki ~author:userid
                      ~comment:(Printf.sprintf "wikipage %s in wiki %s"
                                  page (wiki_id_s wiki))
                      ~content:("== Page "^page^"==")
                      ~content_type:Wiki_sql.Wiki ()
                    >>= fun wbid ->
                    Wiki_sql.set_box_for_page ~sourcewiki:wiki ~wbid ~page ()
                    >>= fun () ->
                    Lwt.return [Ocsimore_common.Session_data sd]
                | e -> Lwt.fail e)
       | false ->  Lwt.fail Ocsimore_common.Permission_denied
  )

in
(service_edit_wikibox,
 service_edit_wikipage_css,
 service_edit_wiki_css,
 action_edit_wikibox,
 action_delete_wikibox,
 action_edit_wikibox_permissions,
 action_wikibox_history,
 action_old_wikibox,
 action_src_wikibox,
 action_send_wikibox,
 action_send_wikibox_permissions,
 action_send_wikipage_css,
 action_send_wiki_css,
 pagecss_service,
 action_create_page)


let register_services (service_edit_wikibox, service_edit_wikipage_css, service_edit_wiki_css, _, _, _, _, _, _, _, _, _, _, _, _) (wikibox_widget : Wiki_widgets_interface.editable_wikibox) =

  Eliom_duce.Xhtml.register service_edit_wikibox
    (fun sp ((w, _b) as g) () ->
       let sd = Ocsimore_common.get_sd sp in
       let bi = { Wiki_widgets_interface.bi_sp = sp;
                  bi_sd = sd;
                  bi_ancestors = Wiki_widgets_interface.no_ancestors;
                  bi_subbox = None;
                }
       in
       wikibox_widget#editable_wikibox ~bi ~data:g ~rows:30 ()
       >>= fun subbox ->
       get_admin_wiki () >>= fun admin_wiki ->
       let bi = { bi with Wiki_widgets_interface.bi_subbox =
           Some {{ [ subbox ] }} } in
       wikibox_widget#editable_wikibox ~bi ?cssmenu:(Some None)
         ~data:(admin_wiki.wiki_id, admin_wiki.wiki_container)  ()
       >>= fun page ->
       wikibox_widget#get_css_header ~admin:true ~bi ~wiki:w ?page:None ()
       >>= fun css ->
         Lwt.return (wikibox_widget#container ~css {{ [ page ] }})
    );

  Eliom_duce.Xhtml.register service_edit_wikipage_css
    (fun sp ((wiki, page) as g) () ->
       let sd = Ocsimore_common.get_sd sp in
       let bi = { Wiki_widgets_interface.bi_sp = sp;
                  bi_sd = sd;
                  bi_ancestors = Wiki_widgets_interface.no_ancestors;
                  bi_subbox = None;
                }
       in
       Wiki_sql.get_wiki_info_by_id wiki
       >>= fun wiki_info ->
       wikibox_widget#edit_css_box ~bi ~rows:30 ~data:g ()
       >>= fun subbox ->
       let bi = { bi with Wiki_widgets_interface.bi_subbox =
           Some {{ [ subbox ] }} } in
       wikibox_widget#editable_wikibox ~bi ?cssmenu:(Some None)
         ~data:(wiki, wiki_info.wiki_container) ()
       >>= fun pagecontent ->
       wikibox_widget#get_css_header ~bi ~wiki ?page:(Some page) ()
       >>= fun css ->
         Lwt.return (wikibox_widget#container ~css {{ [ pagecontent ] }})
    );

  Eliom_duce.Xhtml.register service_edit_wiki_css
    (fun sp wiki () ->
       let sd = Ocsimore_common.get_sd sp in
       let bi = { Wiki_widgets_interface.bi_sp = sp;
                  bi_sd = sd;
                  bi_ancestors = Wiki_widgets_interface.no_ancestors;
                  bi_subbox = None;
                }
       in
       wikibox_widget#edit_wikicss_box ~bi ~rows:30 ~wiki ()
       >>= fun pagecontent ->
       wikibox_widget#get_css_header ~admin:true ~bi ~wiki ?page:None ()
       >>= fun css ->
       Lwt.return (wikibox_widget#container ~css {{ [ pagecontent ] }})
    )

