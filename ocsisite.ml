open Lwt
open Wiki_sql.Types

type user_creation =
  | NoUserCreation
  | BasicUserCreation of User_widgets.basic_user_creation

type external_auth = NoExternalAuth | Nis | Pam of string option

let default_data = (NoExternalAuth, NoUserCreation, None)

let (auth, basicusercreation, admin_staticdir) =
  let rec find_wikidata ((auth, basicusercreation, staticadm) as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("admin", ["staticdir",path], []))::l ->
        find_wikidata (auth, basicusercreation, Some path) l

    | (Simplexmlparser.Element ("nis", [], []))::l ->
        find_wikidata (Nis, basicusercreation, staticadm) l

    | (Simplexmlparser.Element ("pam", ["service", s], []))::l ->
        if Session_manager.pam_loaded ()
        then find_wikidata (Pam (Some s), basicusercreation, staticadm) l
        else
          raise
            (Ocsigen_config.Config_file_error
               "Ocsimore compiled without PAM support");

    | (Simplexmlparser.Element ("pam", [], []))::l ->
        if Session_manager.pam_loaded ()
        then find_wikidata (Pam None, basicusercreation, staticadm) l
        else
          raise
            (Ocsigen_config.Config_file_error
               "Ocsimore compiled without PAM support");

    | (Simplexmlparser.Element ("notsecure", [], []))::l ->
        Session_manager.set_secure false;
        find_wikidata data l

    | (Simplexmlparser.Element ("basicusercreation", atts, []))::l ->
        let registration_mail_from =
          Ocsimore_lib.list_assoc_exn "registration_mail_from" atts
            (Ocsigen_config.Config_file_error
               "Missing registration_mail_from attribute inside <basicusercreation>")
        and registration_mail_addr =
          Ocsimore_lib.list_assoc_exn "registration_mail_addr" atts
            (Ocsigen_config.Config_file_error
               "Missing registration_mail_addr attribute inside <basicusercreation>")
        and registration_mail_subject =
          Ocsimore_lib.list_assoc_default "registration_mail_subject" atts
            "Ocsimore registration"
        in
        (try
          Users.group_list_of_string (List.assoc "groups" atts)
        with Not_found -> Lwt.return [Users.authenticated_users.Users.id])
        >>= fun default_groups ->
        find_wikidata
          (auth,
           BasicUserCreation {
             User_widgets.mail_from = registration_mail_from;
             mail_addr = registration_mail_addr;
             mail_subject = registration_mail_subject;
             new_user_groups = default_groups
           },
           staticadm
          )
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata default_data c)

exception No_admin_staticdir

let () =
  match admin_staticdir with
    | Some _ -> ()
    | None -> Ocsigen_messages.errlog "Ocsisite: please supply a path for the css and images of the Ocsimore administration wiki.\n  Syntax: <admin staticdir=\"path\" />";
        raise No_admin_staticdir


let error_box = new Wiki_widgets.wikibox_error_box

(** We are at eliom registration time, we can create the services *)
let services = Wiki_services.services ()

(** We create the widget that will be used to display the wikiboxes *)
let wikibox_widget =
  Lwt_unix.run
    (let sminfo = {
       Session_manager.url = ["users"];
       administrator = Users.admin;
       login_actions = (fun _sp _sess -> return ());
       logout_actions = (fun _sp -> return ());
     }
     in
     let sm =
       match auth with
         | Pam pam_service ->
             new Session_manager.sessionmanager_pam pam_service sminfo
         | Nis ->
             new Session_manager.sessionmanager_nis sminfo
         | NoExternalAuth ->
             new Session_manager.sessionmanager sminfo
     in

     (* Creation of the login box. This register some extensions at the level
        of the wiki (in the initializer of User_widgets.login_widget)  *)
     (match basicusercreation with
        | BasicUserCreation buc ->
            ignore (new User_widgets.login_widget_basic_user_creation sm buc)
        | NoUserCreation ->
            ignore (new User_widgets.login_widget sm)
     );

     Lwt.return (new Wiki_widgets.dynamic_wikibox error_box services)
    )

let () =
  Wiki_widgets.register_wikibox_syntax_extensions wikibox_widget error_box


(** We register auxiliary services for administration boxes *)

let service_edit_wikibox = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "wiki_edit"]
  ~get_params:Wiki_services.eliom_wikibox_args ()

let () =
  Eliom_duce.Xhtml.register service_edit_wikibox
    (fun sp ((w, _b) as wb) () ->
       let bi = Wiki_widgets_interface.default_bi sp in
       wikibox_widget#display_interactive_wikibox ~bi ~rows:30 wb
       >>= fun subbox ->
       Wiki_services.get_admin_wiki () >>= fun admin_wiki ->
       let bi = { bi with Wiki_widgets_interface.bi_subbox =
           Some {{ [ subbox ] }} } in
       wikibox_widget#display_interactive_wikibox ~bi ?cssmenu:None
         (admin_wiki.wiki_id, admin_wiki.wiki_container)
       >>= fun page ->
       wikibox_widget#css_header ~admin:true ~bi ?page:None w
       >>= fun css ->
       Lwt.return (wikibox_widget#display_container ~css {{ [ page ] }})
    )



(** (We create the wiki containing the administration boxes *)
let wiki_admin = Lwt_unix.run
  (Lwt.catch
     (fun () -> Wiki_sql.get_wiki_info_by_name Wiki_services.wiki_admin_name)
     (function
        | Not_found ->
            Wiki.really_create_wiki
              ~title:Wiki_services.wiki_admin_name
              ~descr:"Administration boxes"
              ~path:[Ocsimore_lib.ocsimore_admin_dir]
              ~boxrights:true
              ~container_page:"= Ocsimore administration\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"
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
         Wiki_sql.new_wikibox ~wiki:wiki_admin_id ~comment ~content ~content_type
           ~author:Users.admin.Users.id ()
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
  ~content_type:Wiki_sql.WikiCreole
  ~comment:"Wikisyntax help"
  ~content:"===Wiki syntax===

This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s syntax, with a few extensions.

{{creole_cheat_sheet.png|Wikicreole's syntax}}"


(** Finally, we registering the existing wikis of the database *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_pages = path } ->
        (match path with
           | None -> ()
           | Some path ->
               let path = Ocsigen_lib.split '/' path in
               Wiki_services.register_wiki ~path ~wikibox_widget ~wiki ()
        );
        Lwt.return ()
     )
  )
