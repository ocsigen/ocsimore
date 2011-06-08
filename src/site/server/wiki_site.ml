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
open Lwt
open User_sql.Types
open Wiki_types

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
 new Wiki_widgets.dynamic_wikibox error_box User_site.user_widgets

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
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "wiki_edit"]
  ~get_params:Wiki_services.eliom_wikibox_args ()

let () =
  Ocsimore_appl.register service_edit_wikibox
    (fun wb () ->
       Wiki_sql.wikibox_wiki wb >>= fun w ->
       Wiki_sql.get_wiki_info_by_id w >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       let wikibox_widget = Wiki_models.get_widgets wiki_info.wiki_model in
       Wiki.default_bi ~wikibox:wb ~rights >>= fun bi ->
       wikibox_widget#display_interactive_wikibox ~bi ~rows:30 wb
       >>= fun page ->
       wikibox_widget#css_header ?page:None w
       >>= fun css ->
       Page_site.add_admin_pages_header ();
       Page_site.html_page ~css page
    )

(** We register the service that lists all the wikis *)
let () =  Eliom_output.Html5.register Wiki_services.view_wikis
    (fun () () ->
       wikibox_widget#display_all_wikis >|= body_to_div >>= fun b ->
       Page_site.admin_page b
    )


(** (We create the wiki containing the administration boxes *)
let wiki_admin = Lwt_unix.run
  (Lwt.catch
     (fun () -> Wiki_sql.get_wiki_info_by_name Wiki.wiki_admin_name)
     (function
        | Not_found ->
            Wiki.create_wiki
              ~title:Wiki.wiki_admin_name
              ~descr:"Administration boxes"
              ~path:[Ocsimore_lib.ocsimore_admin_dir]
              ~boxrights:true
              ~author:User.admin
              ~container_text:"= Ocsimore administration\r\n\r\n\
                               <<loginbox>>\r\n\r\n\
                               <<content>>"
              ~model:wikicreole_model
              ()
            >>= fun wid ->
            Wiki_sql.get_wiki_info_by_id wid
        | e -> Lwt.fail e)
   >>= fun id ->
(*
   (** We update the fields [staticdir] and [pages] for the admin wiki *)
   (match admin_staticdir with
      | None -> Lwt.return ()
      | Some path ->
          Wiki_sql.update_wiki
            ~staticdir:(Some path) ~path:(Some Ocsimore_lib.ocsimore_admin_dir)
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
  Lwt_unix.run(
    Lwt.catch
      (fun () ->
         Wiki_sql.get_wikipage_info ~wiki:wiki_admin_id ~page
         >>= fun _ -> Lwt.return ()
      )
      (function Not_found ->
         Sql.full_transaction_block (fun db ->
           Wiki_sql.new_wikibox ~db
             ~wiki:wiki_admin_id ~comment ~content ~content_type
             ~author:User.admin ()
           >>= fun box ->
           Wiki_sql.create_wikipage ~db ~wiki:wiki_admin_id ~page ~wb:box
           )
       | e -> Lwt.fail e)
  );
  (fun () ->
     Wiki_sql.get_wikipage_info ~wiki:wiki_admin_id ~page
     >>= fun { wikipage_wikibox = wb} ->
     Wiki_sql.get_wikibox_content wb
     >>= function
     | Some (_, _, Some content, _, _, _) ->
         Lwt.return content
     | None | Some (_, _, None, _, _, _) ->
         (* fallback, should not happen if the wikiadmin is not corrupted
         or if the templates are not deleted *)
         Lwt.return content)


(** We create the page for the help on the syntax of the wiki *)
let _ = register_named_wikibox
  ~page:Wiki_widgets_interface.wikisyntax_help_name
  ~content_type:Wiki_syntax.wikicreole_content_type
  ~comment:"Wikisyntax help"
  ~content:(Printf.sprintf
              "===Wiki syntax===\r\n\r\n\
               This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s \
               syntax, with a few extensions.\r\n\r\n\
               {{wiki(%ld):creole_cheat_sheet.png|Wikicreole's syntax}}"
  (Opaque.t_int32 wiki_admin_id))


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
  let page _arg error form =
    let title = match error with
      | Xform.NoError -> "Wiki creation"
      | _ -> "Error" in
    Page_site.admin_page ~service:(cast_service service) ~title
      (HTML5.M.h1 [HTML5.M.pcdata title] ::
       (match error with
          | Xform.ErrorMsg err ->
              [HTML5.M.p
                 ~a:[HTML5.M.a_class ["errmsg"]]
                 [HTML5.M.pcdata err]
              ]
          | _ -> []
       ) @
       [form])
  in
    form ~fallback:service ~get_args:arg ~page ?err_handler
      (p (text "Title: " @+ string_input title +@
          text " (used to identify the wiki in the database) ") @@
       p (text "Description: " @+ string_input descr) @@
       p (text "Link this wiki to an url: " @+ path_input path) @@
       p (text "Authorize special permissions on wikiboxes: " @+
          bool_checkbox boxrights) @@
       p (text "Serve static files from a local directory: " @+
          staticdir_input staticdir) @@
       extensible_list "Add wiki admin" "" admins
         (fun adm ->
            p (text "Admin" @+
               convert (string_input adm) User.user_from_userlogin_xform)) @@
       extensible_list "Add wiki reader" "" readers
         (fun reader ->
            p (text "Reader" @+
               convert (string_input reader) User.user_from_userlogin_xform)) @@
       p (text "Container text :" @+ [HTML5.M.br ()] @+
          text_area ~cols:80 ~rows:20 container) @@
       p (text "Wiki model (for advanced users):" @+ [HTML5.M.br ()] @+
          model_input model) @@
       p (text "Site id (conditional loading of wikis, for advanced users):"
          @+ [HTML5.M.br ()] @+
          string_opt_input siteid) @@
       p (submit_button "Create")
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
  let path = [Ocsimore_lib.ocsimore_admin_dir;"create_wiki"] in
  let create_wiki = Eliom_services.service ~path
      ~get_params:Eliom_parameters.unit () in
  Eliom_output.Html5.register create_wiki
    (fun () () ->
       wiki_rights#can_create_wiki () >>= function
         | true ->
             User.get_user_name () >>= fun u ->
             create_wiki_form ~serv_path:path ~service:create_wiki ~arg:()
               ~title:"" ~descr:"" ~path:(Some "") ~boxrights:true
               ~staticdir:None ~admins:[u] ~readers:[User.anonymous_login]
               ~container:Wiki.default_container_page ~model:wikicreole_model
               ~siteid ~err_handler
               (fun (title, (descr, (path, (boxrights,
                     (staticdir, (admins, (readers, (container,
                      (model, (hid, (_ : bool))))))))))) () ->
                  let path = match path with
                    | None -> None
                    | Some p -> Some (Neturl.split_path p)
                  in
                  Wiki_data.create_wiki ~title ~descr ?path ~boxrights
                    ?staticdir ~admins ~readers
                    ~container_text:container ~model ~rights:wiki_rights ()
                  >>= fun wid ->
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
                  Page_site.admin_page ~service:create_wiki
                    (   HTML5.M.h1 [HTML5.M.pcdata "Wiki sucessfully created"]
                     :: HTML5.M.p [HTML5.M.pcdata msg]
                     :: link
                    )
               )
         | false ->
             Page_site.admin_page ~service:create_wiki
               [HTML5.M.p
                  [HTML5.M.pcdata "You are not allowed to create wikis. \
                                   If you have not already done so, \
                                   try to login."
                  ]
               ]
    );
  create_wiki


let edit_wiki_form ~serv_path:_ ~service ~arg
      ~(wiki:wiki) ~descr ~path ~boxrights ~staticdir ~container ~model ~siteid
      ?err_handler cont =
  let page _arg error form =
    let title = match error with
      | Xform.NoError -> "Wiki edition"
      | _ -> "Error" in
    Page_site.admin_page ~service:(cast_service service) ~title
      (   HTML5.M.h1 [HTML5.M.pcdata title]
       :: (match error with
             | Xform.ErrorMsg err ->
                 [HTML5.M.p ~a:[HTML5.M.a_class ["errmsg"]]
                    [HTML5.M.pcdata err]
                 ]
             | _ -> [])
       @  [form]
      )
  in
    Wiki_sql.get_wiki_info_by_id wiki >>= fun winfo ->
    form ~fallback:service ~get_args:arg ~page ?err_handler
      (p (strong (text (Printf.sprintf "Wiki '%s'" winfo.wiki_title)) ::
          text (Printf.sprintf " (id %s)" (Opaque.int32_t_to_string wiki))  @+
          Opaque.int32_input_xform ~a:[HTML5.M.a_input_type `Hidden] wiki) @@
       p (text "Description: " @+ string_input descr) @@
       p (text "Container wikibox :" @+Opaque.int32_input_opt_xform container)@@
       p (text "Link this wiki to an url: " @+ path_input path +@
          [HTML5.M.br ()] +@
          text "Changing this option will only take effect after the server \
            is restarted; use '/' for the root URL, or nothing if you do not \
            want to bind the wiki") @@
       p (text "Authorize special permissions on wikiboxes: " @+
          bool_checkbox boxrights) @@
       p (text "Serve static files from a local directory: " @+
          staticdir_input staticdir) @@
       p (text "Wiki model (for rights and wiki syntax): " @+
          model_input model) @@
       p (text "Site id (for conditional loading of wikis): " @+
          string_opt_input siteid) @@
       p (submit_button "Save")
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
       let rights = Wiki_models.get_rights info.wiki_model in
       rights#can_create_wiki () >>= function
         | true ->
             edit_wiki_form ~serv_path:Wiki_services.path_edit_wiki
               ~service:Wiki_services.view_wikis ~arg:()
               ~wiki ~descr:info.wiki_descr ~path:info.wiki_pages
               ~boxrights:info.wiki_boxrights ~staticdir:info.wiki_staticdir
               ~container:info.wiki_container ~model:info.wiki_model
               ~siteid:info.wiki_siteid ~err_handler
               (fun (wiki, (descr, (container, (path, (boxrights, (staticdir,
                     (model, (siteid, (_ : bool))))))))) () ->
                  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
                  let rights = Wiki_models.get_rights wiki_info.wiki_model in
                  Wiki_data.update_wiki ~rights ~descr ~path ~boxrights
                    ~staticdir ~container ~model ~siteid wiki
                  >>= fun () ->
                  Page_site.admin_page ~service:Wiki_services.view_wikis
                    [HTML5.M.h1
                       [HTML5.M.pcdata "Wiki information sucessfully edited"]
                    ]
               )
         | false ->
             Page_site.admin_page
               [HTML5.M.h1 [HTML5.M.pcdata "Insufficient permissions"];
                HTML5.M.p [HTML5.M.pcdata "You do not have enough rights to \
                                           edit this wiki"];
               ]
    );
  Wiki_services.edit_wiki



let wiki_root =
  Eliom_services.service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"wikis"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_output.Html5.register wiki_root
  (fun () () ->
     Page_site.admin_page ~service:wiki_root ~title:"Ocsimore - Wiki module"
       [HTML5.M.h1 [HTML5.M.pcdata "Wiki module"];
        HTML5.M.p
          [HTML5.M.pcdata "This is the Ocsimore admin page for the wiki \
                           module. The links on the right will help you \
                           configure your installation." ];
       ]
  )



let () = Eliom_output.Html5.register Wiki_services.edit_wiki_permissions_admin
  (fun wiki () ->
     wikibox_widget#display_edit_wiki_perm_form ~classes:[] wiki
     >>= fun (_, form) ->
     Page_site.admin_page ~service:Wiki_services.view_wikis
       [HTML5.M.div form]
  )

let () = Page_site.add_to_admin_menu ~root:wiki_root ~name:"Wikis"
  ~links:[
    "View all wikis", Wiki_services.view_wikis, (fun () -> Lwt.return true);
    "Create a new wiki", create_wiki, wiki_rights#can_create_wiki;
  ]


