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

open Lwt
open User_sql.Types
open Wiki_types

let ( ** ) = Eliom_parameters.prod

(** Options for Ocsisite *)

let admin_staticdir =
  let rec find_wikidata (_staticadm as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("admin", ["staticdir",path], []))::l ->
        find_wikidata (Some path) l

    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata None c)

exception No_admin_staticdir

let () =
  match admin_staticdir with
    | Some _ -> ()
    | None -> Ocsigen_messages.errlog "Ocsisite: please supply a path for the css and images of the Ocsimore administration wiki.\n  Syntax: <admin staticdir=\"path\" />";
        raise No_admin_staticdir



let error_box = new Wiki_widgets.wikibox_error_box
let wiki_rights = new Wiki.wiki_rights

(** We are at eliom registration time, we can create the services *)
let wiki_services = Wiki_services.make_services ()

let wikibox_widget = new Wiki_widgets.dynamic_wikibox error_box wiki_services

(** We create the default wiki model, called "wikicreole" *)
let wikicreole_model =
  Wiki_models.register_wiki_model
    ~name:"wikicreole"
    ~content_type:Wiki_syntax.wikicreole_content_type
    ~rights:wiki_rights
    ~widgets:wikibox_widget

let () =
  Wiki_ext.register_wikibox_syntax_extensions
    Wiki_syntax.wikicreole_parser error_box


(** We register auxiliary services for administration boxes *)

let service_edit_wikibox = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "wiki_edit"]
  ~get_params:Wiki_services.eliom_wikibox_args ()

let () =
  Eliom_duce.Xhtml.register service_edit_wikibox
    (fun sp ((w, _b) as wb) () ->
       Wiki_sql.get_wiki_info_by_id w >>= fun wiki_info ->
       let rights = Wiki_models.get_rights wiki_info.wiki_model in
       let wikibox_widget = Wiki_models.get_widgets wiki_info.wiki_model in
       let bi = Wiki_widgets_interface.default_bi ~sp ~wikibox:wb ~rights in
       wikibox_widget#display_interactive_wikibox ~bi ~rows:30 wb
       >>= fun subbox ->
       Wiki.get_admin_wiki () >>= fun admin_wiki ->
       let bi = { bi with Wiki_widgets_interface.bi_subbox =
           Some {{ [ subbox ] }} } in
       wikibox_widget#display_interactive_wikibox ~bi
         (admin_wiki.wiki_id, admin_wiki.wiki_container)
       >>= fun page ->
       wikibox_widget#css_header ~admin:true ~bi ?page:None w
       >>= fun css ->
       Lwt.return (wikibox_widget#display_container ~sp ~css {{ [ page ] }})
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
              ~container_text:"= Ocsimore administration\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"
              ~model:wikicreole_model
              ()
            >>= fun wid ->
            Wiki_sql.get_wiki_info_by_id wid
        | e -> Lwt.fail e)
   >>= fun id ->
   (** We update the fields [staticdir] and [pages] for the admin wiki *)
   (match admin_staticdir with
      | None -> Lwt.return ()
      | Some path ->
          Wiki_sql.update_wiki
            ~staticdir:(Some path) ~pages:(Some Ocsimore_lib.ocsimore_admin_dir)
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
         Wiki_sql.new_wikibox
           ~wiki:wiki_admin_id ~comment ~content ~content_type
           ~author:User.admin ()
         >>= fun box ->
         Wiki_sql.set_box_for_page ~sourcewiki:wiki_admin_id ~wbid:box ~page ()

       | e -> Lwt.fail e)
  );
  (fun () ->
     Wiki_sql.get_wikipage_info ~wiki:wiki_admin_id ~page
     >>= fun { wikipage_dest_wiki = wiki'; wikipage_wikibox = box} ->
     Wiki_sql.get_wikibox_data ~wikibox:(wiki', box) ()
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
  ~content:"===Wiki syntax===

This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s syntax, with a few extensions.

{{creole_cheat_sheet.png|Wikicreole's syntax}}"


(** Finally, we register the existing wikis of the database *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_pages = path } ->
        (match path with
           | None -> ()
           | Some path ->
               let path = Ocsigen_lib.split '/' path in
               Wiki_services.register_wiki ~rights:wiki_rights ~path ~wiki ()
        );
        Lwt.return ()
     )
  )


(*
(* Default permissions for the migration to the new system *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_title = name} ->
        User.add_to_group ~user:(basic_user User.anonymous)
          ~group:(Wiki.wiki_wikiboxes_grps.grp_reader $ wiki)
        >>= fun () ->
        User.add_to_group ~user:(basic_user User.anonymous)
          ~group:(Wiki.wiki_files_readers $ wiki)
        >>= fun () ->
        try Scanf.sscanf name "wikiperso for %s"
          (fun user ->
             User.get_user_by_name user
             >>= fun user ->
             User.add_to_group ~user ~group:(Wiki.wiki_admins $ wiki)
          )
        with Scanf.Scan_failure _ -> Lwt.return ()

     ))
*)


let str = Ocamlduce.Utf8.make

open Xform.XformLwt
open Ops

let user_from_userlogin user =
  User.get_user_by_name user >>= fun u ->
  if u = basic_user User.nobody && user <> User.nobody_login then
    Lwt.return (Xform.ConvError ("This user does not exists: " ^ user))
  else
    Lwt.return (Xform.Converted u)


let create_wiki_form ~serv_path:_ ~service ~arg ~sp
      ~title ~descr ~path ~boxrights ~staticdir ~admins ~readers ~container ~css
      ?err_handler cont =
  let page _sp _arg error form =
    let title = match error with
      | Xform.NoError -> "Wiki creation"
      | _ -> "Error" in
    Ocsimore_common.html_page ~title
      {{ [<h1>(str title)
          !{: match error with
              | Xform.ErrorMsg err -> {{[<p>(str err)] }}
              | _ -> {{ [] }}
          :}
          form] }}
  in
    form ~fallback:service ~get_args:arg ~page ~sp ?err_handler
      (p (text "Title: " @+ string_input title +@
          text " (used to identify the wiki in the database) ") @@
       p (text "Description: " @+ string_input descr) @@
       p (text "Link this wiki to an url: " @+ string_opt_input path) @@
       p (text "Authorize special permissions on wikiboxes: " @+
          bool_checkbox boxrights) @@
       p (text "Serve static files from a directory: " @+
          string_opt_input staticdir) @@
       extensible_list "Add wiki admin" "" admins
         (fun adm ->
            p (text "Admin" @+
               convert (string_input adm) user_from_userlogin)) @@
       extensible_list "Add wiki reader" "" readers
         (fun reader ->
            p (text "Reader" @+
               convert (string_input reader) user_from_userlogin)) @@
       p (text "Container text :" @+ [{{<br>[]}}] @+
          text_area ~cols:80 ~rows:20 container)
       @@
       p (text "Css :" @+ [{{<br>[]}}] @+
          text_area ~cols:80 ~rows:20 css)
       @@
       p (submit_button "Create")
      |> (fun (title, (descr, (path, (boxrights, (staticdir, (admins, (readers, (container, (css, _v))))))))) ->
            cont ~title ~descr ~path ~boxrights ~staticdir ~container ~css
              ~admins ~readers)
      )
  >>= fun form ->
  page sp arg Xform.NoError form

let create_wiki =
  let err_handler = function
    | Wiki.Wiki_already_registered_at_path ((_, descr), _) ->
        Some (Printf.sprintf "The wiki '%s' is already registered at this path"
                descr)
    | Wiki.Wiki_with_same_title _ ->
        Some "A wiki with this title already exists"
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to create wikis"
    | _ -> Some "An unknown error has occurred"
 in
  let path = [Ocsimore_lib.ocsimore_admin_dir;"create_wiki"] in
  let create_wiki = Eliom_services.new_service ~path
      ~get_params:Eliom_parameters.unit () in
  Eliom_duce.Xhtml.register create_wiki
    (fun sp () () ->
       User.get_user_name sp >>= fun u ->
       create_wiki_form ~serv_path:path ~service:create_wiki ~arg:() ~sp
         ~title:"" ~descr:"" ~path:(Some "") ~boxrights:true ~staticdir:None
         ~admins:[u] ~readers:[User.anonymous_login]
         ~container:Wiki.default_container_page ~css:""
         ~err_handler
         (fun ~title ~descr ~path ~boxrights ~staticdir ~container ~css ~admins ~readers sp ->
            let path = match path with
              | None -> None
              | Some p -> Some (Neturl.split_path p)
            in
            Wiki_data.create_wiki ~sp ~title ~descr ?path ~boxrights ?staticdir
              ~admins ~readers ~wiki_css:css ~container_text:container
              ~model:wikicreole_model ~rights:wiki_rights ()
            >>= fun wid ->
            let title = str "Wiki sucessfully created"
            and msg = str (Printf.sprintf "You have created wiki %s"
                             (string_of_wiki wid)) in
            Ocsimore_common.html_page
              {{ [<h1>title <p>msg] }}
         ));
  create_wiki
