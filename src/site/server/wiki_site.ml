(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2008-2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**

   @author Vincent Balat
   @author Boris Yakobowski
*)

open Eliom_pervasives
open Ocsimore_lib
open Lwt
open User_sql.Types
open Wiki_types

let wiki_naming wiki =
  Wiki_sql.get_wiki_info_by_id wiki >|= fun wiki_info ->
    Printf.sprintf "%S (ID %s)"
      wiki_info.wiki_title
      (Wiki_types.string_of_wiki wiki_info.wiki_id)

let body_to_div x =
  (x : HTML5_types.body_content HTML5.M.elt list
     :> HTML5_types.flow5 HTML5.M.elt list
  )

let siteid =
  let rec find_wikidata (_siteid as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("siteid", ["id", id], []))::l ->
        find_wikidata (Some id) l

    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Wiki module config"))
  in
  let c = Eliom_config.get_config () in
  Lwt_unix.run (find_wikidata None c)



let error_box = new Wiki_widgets.wikibox_error_box
let wiki_rights = new Wiki.wiki_rights


let wikibox_widget =
 new Wiki_widgets.dynamic_wikibox error_box User_site.user_widget

(** We create the default wiki model, called "wikicreole" *)
let wikicreole_model =
  Wiki_models.register_wiki_model
    ~name:"wikicreole"
    ~content_type:Wiki_syntax.wikicreole_content_type
    ~rights:wiki_rights
    ~widgets:wikibox_widget


let () = Wiki_ext.register_wikibox_syntax_extensions error_box


(** We register auxiliary services for administration boxes *)

let service_edit_wikibox = Eliom_services.service
  ~path:[!Ocsimore_config.admin_dir; "wiki_edit"]
  ~get_params:Wiki_services.eliom_wikibox_args ()

let () =
  Ocsimore_appl.register service_edit_wikibox
    (fun wb () ->
       Wiki_sql.wikibox_wiki wb >>= fun w ->
       Wiki_sql.get_wiki_info_by_id w >>= fun wiki_info ->
       lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
       lwt wikibox_widget = Wiki_models.get_widgets wiki_info.wiki_model in
       lwt bi = Wiki.default_bi ~wikibox:wb ~rights in
       lwt page = wikibox_widget#display_interactive_wikibox ~bi ~rows:30 wb in
       lwt css = wikibox_widget#css_header ?page:None w in
       lwt () = Page_site.add_admin_pages_header () in
       Page_site.html_page ~css (page :> HTML5_types.body_content Eliom_pervasives.HTML5.M.elt list)
    )

(** We register the service that lists all the wikis *)
let () =
  Eliom_output.Html5.register Wiki_services.view_wikis
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun()()->Lwt.return "View wikis")
       ~permissions:(fun () () -> Page_site.userid_permissions (Lwt.return -| (=) User.admin))
       ~display:(fun _ _ -> wikibox_widget#display_all_wikis))


(** (We create the wiki containing the administration boxes *)
let wiki_admin = Lwt_unix.run
  (lwt id =
     try_lwt
       Wiki_sql.get_wiki_info_by_name Wiki.wiki_admin_name
     with Not_found ->
       lwt model = wikicreole_model in
       lwt wid =
         Wiki.create_wiki
           ~title:Wiki.wiki_admin_name
           ~descr:"Administration boxes"
           ~path:[!Ocsimore_config.admin_dir]
           ~boxrights:true
           ~author:User.admin
           ~container_text:"= Ocsimore administration\r\n\r\n\
                            <<loginbox>>\r\n\r\n\
                            <<content>>"
           ~model
           ()
       in
       Wiki_sql.get_wiki_info_by_id wid
   in
(*
   (** We update the fields [staticdir] and [pages] for the admin wiki *)
   (match admin_staticdir with
      | None -> Lwt.return ()
      | Some path ->
          Wiki_sql.update_wiki
            ~staticdir:(Some path) ~path:(Some !Ocsimore_config.admin_dir)
            id.wiki_id
   ) >>=fun () ->
   ((** And give reading rights to the wiki itself. (As usual, this can be
        overridden on a per-page basis) *)
     let groups = [
       Wiki.wiki_wikiboxes_grps.grp_reader;
       Wiki.wiki_files_readers;
     ] in
     Lwt_util.iter
       (fun g -> User_sql.add_to_group ~user:(basic_user User.anonymous)
          ~group:(g $ id.wiki_id))
       groups
   ) >>= fun () ->
*)
   Lwt.return id
  )

let wiki_admin_id = wiki_admin.wiki_id


(** This function registers a page inside the administration wiki if it does not
    exists, and returns a function giving the current value of the
    corresponding wikibox *)
let register_named_wikibox ~page ~content ~content_type ~comment =
  Lwt_unix.run (
    try_lwt
      lwt _ = Wiki_sql.get_wikipage_info ~wiki:wiki_admin_id ~page in
      Lwt.return ()
    with Not_found ->
      Sql.full_transaction_block
        (fun db ->
          lwt box =
            Wiki_sql.new_wikibox ~db
              ~wiki:wiki_admin_id ~comment ~content ~content_type
              ~author:User.admin ()
          in
          Wiki_sql.create_wikipage ~db ~wiki:wiki_admin_id ~page ~wb:box)
  );
  (fun () ->
     lwt { wikipage_wikibox = wb} = Wiki_sql.get_wikipage_info ~wiki:wiki_admin_id ~page in
     Wiki_sql.get_wikibox_content wb >|= function
     | Some (_, _, Some content, _, _, _) ->
         content
     | None | Some (_, _, None, _, _, _) ->
         (* fallback, should not happen if the wikiadmin is not corrupted
         or if the templates are not deleted *)
         content)


(** We create the page for the help on the syntax of the wiki *)
let _ = register_named_wikibox
  ~page:Wiki_widgets_interface.wikisyntax_help_name
  ~content_type:Wiki_syntax.wikicreole_content_type
  ~comment:"Wikisyntax help"
  ~content:"===Wiki syntax===\r\n\r\n\
            This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s \
            syntax, with a few extensions.\r\n\r\n\
            {{site:creole_cheat_sheet.png|Wikicreole's syntax}}"


(** We register the existing wikis of the database, but only those that
    match the [siteid] option *)
let () = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_pages = path; wiki_siteid = h } ->
        (match path with
           | None -> ()
           | Some path ->
               let path = String.split '/' path in
               let siteids = (* we always register the admin wiki *)
                 if wiki = wiki_admin.wiki_id then (h, h) else (siteid, h)
               in
               Wiki_services.register_wiki ~rights:wiki_rights ~path ~wiki
                 ~siteids ()
        );
        Lwt.return ()
     )
  )




open Xform.XformLwt
open Ops


let path_input ?a path =
  string_input ?a
    (match path with
       | None -> ""
       | Some "" -> "/"
       | Some p -> p)
  |> function
       | "" -> None
       | "/" -> Some ""
       | s -> Some s

let staticdir_input ?a staticdir =
  string_input ?a (match staticdir with None -> "" | Some s -> s)
  |> function "" -> None | s -> Some s

let cast_service service =
    (service :> Page_site.menu_link_service)

let model_input ?a model =
  string_input ?a (Wiki_types.string_of_wiki_model model)
  |> Wiki_types.wiki_model_of_string

let create_wiki_form ~serv_path:_ ~service ~arg
    ~title ~descr ~path ~boxrights ~staticdir ~admins ~readers ~container ~model
    ~siteid
    ?err_handler cont =
  let page _arg error frm =
    let ttl = match error with
      | Xform.NoError -> "Wiki creation"
      | _ -> "Error" in
    Page_site.admin_page ~service:(cast_service service) ~title:ttl
      HTML5.M.(
        (match error with
           | Xform.ErrorMsg err ->
               [p ~a:[a_class ["errmsg"]]
                  [pcdata err] ]
           | _ -> []
        ) @
        [frm]
      )
  in
    form ~fallback:service ~get_args:arg ~page ?err_handler
      (table
         (label_input_tr ~label:"Title"
            ~description:"used to identify the wiki in the database"
            (string_input title) @@
          label_input_tr ~label:"Description" (string_input descr) @@
          label_input_tr ~label:"Link this wiki to an url: " (path_input path) @@
          label_input_tr ~label:"Authorize special permissions on wikiboxes" (bool_checkbox boxrights) @@
          label_input_tr ~label:"Serve static files from a local directory" (staticdir_input staticdir) @@
          label_input_tr ~label:"Wiki admin"
            (extensible_list "Add wiki admin" "" admins
               (fun adm ->
                  p (convert (string_input adm) User.user_from_userlogin_xform))) @@
          label_input_tr ~label:"Wiki reader"
            (extensible_list "Add wiki admin" "" admins
               (fun reader ->
                  p (convert (string_input reader) User.user_from_userlogin_xform))) @@
          label_input_tr ~label:"Container text" (text_area ~cols:80 ~rows:20 container) @@
          label_input_tr ~label:"Wiki model" ~description:"For advanced users" (model_input model) @@
          label_input_tr ~label:"Site id" ~description:"Conditional loading of wikis, for advanced users" (string_opt_input siteid) @@
          tr (td (submit_button "Create")))
      |> cont)
  >>= fun form ->
  page arg Xform.NoError form


let create_wiki =
  let err_handler = function
    | Wiki.Wiki_already_registered_at_path ((_, descr), _) ->
        Some (Printf.sprintf
                "The wiki '%s' is already registered at the given url."
                descr)
    | Wiki.Wiki_with_same_title _ ->
        Some "A wiki with this title already exists"
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to create wikis"
    | _ -> Some "An unknown error has occurred"
 in
  let path = [!Ocsimore_config.admin_dir;"create_wiki"] in
  let create_wiki = Eliom_services.service ~path
      ~get_params:Eliom_parameters.unit () in
  Eliom_output.Html5.register create_wiki
    (fun () () ->
       wiki_rights#can_create_wiki () >>= function
         | true ->
             lwt u = User.get_user_name () in
             lwt model = wikicreole_model in
             create_wiki_form ~serv_path:path ~service:create_wiki ~arg:()
               ~title:"" ~descr:"" ~path:(Some "") ~boxrights:true
               ~staticdir:None ~admins:[u] ~readers:[User.anonymous_login]
               ~container:Wiki.default_container_page ~model
               ~siteid ~err_handler
               (fun (title, (descr, (path, (boxrights,
                     (staticdir, (admins, (readers, (container,
                      (model, (hid, (_ : bool))))))))))) () ->
                  let path = match path with
                    | None -> None
                    | Some p -> Some (Neturl.split_path p)
                  in
                  lwt wid =
                    Wiki_data.create_wiki ~title ~descr ?path ~boxrights
                      ?staticdir ~admins ~readers
                      ~container_text:container ~model ~rights:wiki_rights ()
                  in
                  let link = match path with
                    | None -> []
                    | Some path ->
                        Wiki_services.register_wiki ~rights:wiki_rights
                          ~path ~wiki:wid ~siteids:(siteid, hid) ();
                        match Wiki_self_services.find_servpage wid with
                          | None -> (* should never happen, but this is not
                                       really important *) []
                          | Some service ->
                              let link = Eliom_output.Html5.a
                                ~service [HTML5.M.pcdata "root page"] []
                              in
                              [HTML5.M.p
                                 [HTML5.M.pcdata "you can go to the ";
                                  link;
                                  HTML5.M.pcdata " of this wiki.";
                                 ]
                              ]
                  in
                  let msg = (Printf.sprintf "You have created wiki %s"
                                   (string_of_wiki wid))
                  in
                  Page_site.admin_page
                    ~title:"Wiki created"
                    ~service:create_wiki
                    (HTML5.M.p [HTML5.M.pcdata msg] :: link)
               )
         | false ->
             lwt no_permission = Page_site.no_permission () in
             Page_site.admin_page
               ~title:"Wiki creation"
               ~service:create_wiki
               no_permission
    );
  create_wiki

let edit_wiki_form ~service ~arg
      ~wiki ~descr ~path ~boxrights ~staticdir ~container ~model ~siteid
      ?err_handler cont =
  lwt wiki_naming = wiki_naming wiki in
  let page _arg error form =
    let title = match error with
      | Xform.NoError -> "Wiki edition "^wiki_naming
      | _ -> "Error editing "^wiki_naming in
    Page_site.admin_page ~service:(cast_service service) ~title
      ((match error with
             | Xform.ErrorMsg err ->
                 [HTML5.M.p ~a:[HTML5.M.a_class ["errmsg"]]
                    [HTML5.M.pcdata err]
                 ]
             | _ -> [])
       @  [form]
      )
  in
    form ~fallback:service ~get_args:arg ~page ?err_handler
      (table
         (tr (td (Opaque.int32_input_xform ~a:[HTML5.M.a_style "display: none"] wiki)) @@
          label_input_tr ~label:"Description" (string_input descr) @@
          label_input_tr ~label:"Container wikibox" (Opaque.int32_input_opt_xform container) @@
          label_input_tr
            ~label:"Link this wiki to an url"
            ~description:"Changing this option will only take effect after the server \
                          is restarted; use '/' for the root URL, or nothing if you do not \
                          want to bind the wiki"
            (path_input path) @@
          label_input_tr ~label:"Authorize special permissions on wikiboxes" (bool_checkbox boxrights) @@
          label_input_tr ~label:"Serve static files from a local directory" (staticdir_input staticdir) @@
          label_input_tr ~label:"Wiki model (for rights and wiki syntax)" (model_input model) @@
          label_input_tr ~label:"Site id (for conditional loading of wikis)" (string_opt_input siteid) @@
          tr (td (submit_button "Save")))
      |> cont)
  >>= fun form ->
  page arg Xform.NoError form


let edit_wiki =
  let err_handler = function
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to edit wikis"
    | _ -> Some "An unknown error has occurred"
  in
  Eliom_output.Html5.register Wiki_services.edit_wiki
    (fun wiki () ->
       Wiki_sql.get_wiki_info_by_id wiki >>= fun info ->
       lwt rights = Wiki_models.get_rights info.wiki_model in
       rights#can_create_wiki () >>= function
         | true ->
             edit_wiki_form
               ~service:Wiki_services.view_wikis ~arg:()
               ~wiki ~descr:info.wiki_descr ~path:info.wiki_pages
               ~boxrights:info.wiki_boxrights ~staticdir:info.wiki_staticdir
               ~container:info.wiki_container ~model:info.wiki_model
               ~siteid:info.wiki_siteid ~err_handler
               (fun (wiki, (descr, (container, (path, (boxrights, (staticdir,
                     (model, (siteid, (_ : bool))))))))) () ->
                  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
                  lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
                  Wiki_data.update_wiki ~rights ~descr ~path ~boxrights
                    ~staticdir ~container ~model ~siteid wiki
                  >>= fun () ->
                  Page_site.admin_page
                    ~service:Wiki_services.view_wikis
                    ~title:(Printf.sprintf "Edit wiki %S" wiki_info.wiki_title)
                    HTML5.M.([h2 [pcdata "Wiki information sucessfully edited"]])
               )
         | false ->
             Page_site.no_permission () >>= Page_site.admin_page ~title:"Edid wiki"
    )
let _ =
  let open HTML5.M in
  let headers = ["wikibox"; "version"; "author"; "datetime"; "content_type"; "comment"; ""] in
  let render_header_row s = th [pcdata s] in
  (* wikibox * int32 option * userid * CalendarLib.Calendar.t * string * string option * string *)
  let render_wikibox_row (wikibox, version, author, datetime, content_type, content, comment) =
    tr [
      td [pcdata Wiki_sql.(string_of_wikibox wikibox)];
      td [pcdata (Int32.to_string version)];
      td [pcdata author.User_sql.Types.user_fullname];
      td [pcdata (CalendarLib.Printer.Calendar.to_string datetime)];
      td [pcdata (Wiki_types.string_of_content_type content_type)];
      td [pcdata comment];
      td [Eliom_output.Html5.a ~service:Wiki_services.view_box [Page_site.icon ~path:"viewbox.png" ~text:"View box"] (wikibox, Some version)];
    ]
  in
  let wikibox_extension wikibox =
    lwt content = Wiki_sql.get_wikibox_content wikibox in
    match content with
        Some (comment, author, content, datetime, content_type, version) ->
          lwt author = User_sql.get_basicuser_data author in
          Lwt.return (Some (wikibox, version, author, datetime, content_type, content, comment))
     | None -> Lwt.return None
  in
  Eliom_output.Html5.register Wiki_services.view_boxes
    (fun wiki () ->
      lwt wikiboxes = Wiki_sql.get_wikiboxes_by_wiki wiki in
      lwt wikiboxes = Lwt_list.map_s wikibox_extension wikiboxes >|= List.map_filter (fun x -> x) in
      lwt wiki_info = Wiki_sql.get_wiki_info_by_id ~id:wiki in
      lwt title = wiki_naming wiki >|= ((^) "Wiki boxes of wiki ") in
      Page_site.admin_page
        ~title
        [table ~a:[a_class ["table_admin"]]
          (tr (List.map render_header_row headers))
          (List.map render_wikibox_row wikiboxes)])

let _ =
  let render_version_link wikibox version' (version, comment, author, datetime) =
    let version_elt = HTML5.M.pcdata (Int32.to_string version) in
    let version_link =
      if version' <> version then
        Eliom_output.Html5.a ~service:Wiki_services.view_box [version_elt] (wikibox, Some version)
      else
        version_elt 
    in
    HTML5.M.(li [version_link; pcdata (if comment = "" then "" else " ("^comment^")")])
  in
  Eliom_output.Html5.register Wiki_services.view_box
    (fun (wikibox, version) () ->
      Wiki_sql.get_wikibox_content ?version wikibox >>= function
         Some (comment, author, content, datetime, content_type, version) ->
           lwt history = Wiki_sql.get_wikibox_history wikibox in
           lwt { Wiki_types.wikibox_wiki = wiki } = Wiki_sql.get_wikibox_info wikibox in
           lwt wiki_name = wiki_naming wiki in
           let title =
             Printf.sprintf "View wiki box %s version %s in wiki %s"
               (Wiki_types.string_of_wikibox wikibox)
               (Int32.to_string version)
               wiki_name
           in
           Page_site.admin_page
             ~title
             HTML5.M.([
               ul (List.map (render_version_link wikibox version) history);
               table 
                 (tr [td [pcdata "wikibox"];
                      td [pcdata Wiki_sql.(string_of_wikibox wikibox)]])
                 [tr [td [pcdata "version"];
                      td [pcdata (Int32.to_string version)]];
                  tr [td [pcdata "content_type"];
                      td [pcdata (Wiki_types.string_of_content_type content_type)]];
                  tr [td [pcdata "content"];
                  td (match content with Some c -> [pre [pcdata c]] | None -> [])]]
             ])
       | None -> Lwt.fail Eliom_common.Eliom_404)

let replace_links =
  Eliom_services.post_coservice
    ~fallback:Wiki_services.batch_edit_boxes
    ~post_params:Eliom_parameters.unit ()

let normalize_old_page_link wiki wikibox addr fragment attribs params =
  let replacement =
    try
      ignore (Wiki_syntax.link_kind addr);
      None
    with Failure _ ->
      let rep =
        Printf.sprintf "wiki(%s):%s%s"
          (Wiki_types.string_of_wiki wiki)
          addr
          (match fragment with Some f -> "#"^f | None -> "")
      in
      Printf.eprintf ">> Replace in %S by %S in wiki %s / wikibox %s\n%!"
        addr rep (Wiki_types.string_of_wiki wiki) (Wiki_types.string_of_wikibox wikibox);
      Some rep
  in
  Lwt.return replacement

let _ =
  Eliom_output.Html5.register
    Wiki_services.batch_edit_boxes
    (fun () () ->
      Page_site.admin_page
        ~title:"Batch edit boxes"
        HTML5.M.([
          Eliom_output.Html5.post_form
            ~service:replace_links
            (fun () -> [
              Eliom_output.Html5.button
                ~button_type:`Submit
                [HTML5.M.pcdata "Replace links"]
            ]) ()
        ]));
  Eliom_output.Html5.register
    replace_links
    (fun () () ->
      lwt wikis = Wiki_sql.get_wikis () in
      let wikibox_content wpp wiki wikibox =
        lwt c = Wiki_sql.get_wikibox_content wikibox in
        match c with
            Some (_comment, _author, Some old_content, _datetime, content_type, _version) ->
              let href_action = normalize_old_page_link wiki wikibox in
              let desugar_param = Wiki_syntax_types.({
                dc_page_wiki = wiki;
                dc_page_path = None;
                dc_warnings = [];
              }) in
              lwt new_content = Wiki_models.desugar_string ~href_action wpp desugar_param old_content in
              if 0 = String.compare old_content new_content then
                Lwt.return (wikibox, None)
              else
                lwt wikibox' =
                  Wiki_sql.update_wikibox 
                    ~author:User.admin
                    ~comment:"batch_edit_boxes: Replace old relative links"
                    ~content:(Some new_content)
                    ~content_type
                    wikibox
                in
                Lwt.return (wikibox, Some (wikibox', (old_content, new_content)))
         | _ -> Lwt.return (wikibox, None)
      in
      let for_wiki wiki =
        lwt wiki_info = Wiki_sql.get_wiki_info_by_id ~id:wiki in
        lwt wpp = Wiki_models.get_default_wiki_preprocessor wiki_info.wiki_model in
        lwt wikiboxes = Wiki_sql.get_wikiboxes_by_wiki wiki in
        lwt wikiboxes_content = Lwt_list.map_s (wikibox_content wpp wiki) wikiboxes in
        Lwt.return (wiki_info, wikiboxes_content)
      in
      lwt wikiboxes_by_wikis = Lwt_list.map_s for_wiki wikis in
      Page_site.admin_page
        ~title:"Replace links"
        (List.map
          (fun (wiki_info, wikiboxes) ->
            HTML5.M.(
              div [
                h4 [pcdata wiki_info.Wiki_types.wiki_title];
                table
                  (tr [th [pcdata "wikibox"]; th [pcdata "changes"]])
                  (List.map_filter
                     (fun (wikibox, opt_contents) ->
                       match opt_contents with
                           Some (wikibox', (old_content, new_content)) ->
                             let s = if 0 = String.compare old_content new_content then "-" else "some" in
                             Some (tr [
                               td [pcdata (Wiki_types.string_of_wikibox wikibox)];
                               td [pcdata s]
                             ])
                         | None -> None)
                     wikiboxes)
              ]))
          wikiboxes_by_wikis))

let wiki_root =
  Eliom_services.service
    ~path:[!Ocsimore_config.admin_dir;"wikis"]
    ~get_params:Eliom_parameters.unit ()

let () =
  Eliom_output.Html5.register
    wiki_root
    (fun () () ->
       Page_site.admin_page ~service:wiki_root ~title:"Ocsimore - Wiki module"
         [ HTML5.M.p
            [HTML5.M.pcdata "This is the Ocsimore admin page for the wiki \
                             module. The links on the right will help you \
                             configure your installation." ];
         ])


let () = Eliom_output.Html5.register Wiki_services.edit_wiki_permissions_admin
  (fun wiki () ->
     lwt _, form = wikibox_widget#display_edit_wiki_perm_form ~classes:[] wiki in
     lwt wiki_name = Wiki_sql.get_wiki_info_by_id ~id:wiki >|= fun { Wiki_types.wiki_title } -> wiki_title in
     Page_site.admin_page
       ~title:(Printf.sprintf "Edit permissions of wiki %S" wiki_name)
       ~service:Wiki_services.view_wikis
       [HTML5.M.div form]
  )

let () = Page_site.add_to_admin_menu ~root:wiki_root ~name:"Wikis"
  ~links:[
    "View all wikis", Wiki_services.view_wikis, (fun () -> Lwt.return true);
    "Create a new wiki", create_wiki, wiki_rights#can_create_wiki;
  ]

let () =
  Eliom_output.set_exn_handler
    (function
       | Eliom_common.Eliom_404 ->
           Eliom_output.Html5.send
             ~code:404
             HTML5.M.(
               html
                 (head (title (pcdata "Page not found - 404")) [])
                 (body [
                   h1 [pcdata "Page not found - 404"];
                   p [
                     pcdata "Actually, no wiki found. You may create one in the ";
                     Eliom_output.Html5.a ~service:create_wiki [pcdata "Ocsimore administration"] ();
                     pcdata ".";
                   ]
                 ])
             )
       | e -> Lwt.fail e)

