open Lwt
open Wiki_sql.Types

type user_creation =
  | NoUserCreation
  | BasicUserCreation of User_widgets.basic_user_creation

type external_auth = NoExternalAuth | Nis | Pam of string option

let default_data = (NoExternalAuth, NoUserCreation)

let (auth, basicusercreation) =
  let rec find_wikidata ((auth, basicusercreation) as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("nis", [], []))::l ->
        find_wikidata (Nis, basicusercreation) l

    | (Simplexmlparser.Element ("pam", ["service", s], []))::l ->
        if Session_manager.pam_loaded ()
        then find_wikidata (Pam (Some s), basicusercreation) l
        else
          raise
            (Ocsigen_config.Config_file_error
               "Ocsimore compiled without PAM support");

    | (Simplexmlparser.Element ("pam", [], []))::l ->
        if Session_manager.pam_loaded ()
        then find_wikidata (Pam None, basicusercreation) l
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
             new_user_groups = default_groups}
          )
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata default_data c)




let wikiadmin_container_id = 2l
let wiki_help_box = 3l


let services = Wiki_services.services ()

let wikibox =
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

     Lwt.return (new Wiki_widgets.creole_wikibox wiki_help_box services)
    )

let () = Wiki_services.register_services services wikiadmin_container_id wikibox




let wiki_admin =
    Lwt_unix.run
      ((* creating a wiki for the administration boxes: *)
        Wiki_services.create_wiki
          ~title:Wiki_services.wiki_admin_name
          ~descr:"Administration boxes"
          ~wikibox:wikibox
          ~path:[Ocsimore_lib.ocsimore_admin_dir]
          (*       ~readers:[Users.admin]
                   ~writers:[Users.admin]
                   ~rights_adm:[Users.admin]
                   ~wikiboxes_creators:[Users.admin]
                   ~container_adm:[Users.admin]
                   ~page_creators:[Users.admin]
                   ~css_editors:[Users.admin]
                   ~admins:[Users.admin] *)
          ~boxrights:true
          ~container_page:Wiki_services.default_container_page
          ()
      )

let wiki_admin_id = wiki_admin.wiki_id

(** Registers a page inside the administration wiki if it does not
    exists, and returns a function giving the current value of the
    corresponding wikibox *)
let register_named_wikibox ~page ~content ~content_type ~comment =
  Lwt_unix.run(
    Lwt.catch
      (fun () ->
         Wiki_sql.get_box_for_page ~wiki:wiki_admin_id ~page
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
     Wiki_sql.get_box_for_page ~wiki:wiki_admin_id ~page
     >>= fun { Wiki_sql.wikipage_dest_wiki = wiki'; wikipage_wikibox = box} ->
     Wiki_sql.get_wikibox_data ~wikibox:(wiki', box) ()
     >>= function
     | Some (_, _, content, _, _, _) ->
         Lwt.return content
     | None ->
         (* fallback, should not happen if the wikiadmin is not corrupted *)
         Lwt.return content)



let _ =
  Lwt_unix.run

      (* Filling the admin container *)
(*VVV Warning!! Dangerous! How to do this in cleaner way? *)
    (Wiki.new_wikibox
       ~boxid:wikiadmin_container_id
       ~wiki:wiki_admin
       ~author:Users.admin.Users.id
       ~comment:"Admin container"
       ~content:"= Ocsimore administration\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"
       ~content_type:Wiki_sql.Wiki
       ()
     >>= fun _ ->


       (* Filling the wikisyntax help *)
(*VVV Warning!! Dangerous! How to do this in cleaner way? *)
     Wiki.new_wikibox
       ~boxid:wiki_help_box
       ~wiki:wiki_admin
       ~author:Users.admin.Users.id
       ~comment:"Wikisyntax help"
       ~content:"===Wiki syntax===

This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s syntax, with a few extensions.

{{../creole_cheat_sheet.png|Wikicreole's syntax}}"
       ~content_type:Wiki_sql.Wiki
         ()
       >>= fun _ -> Lwt.return ()
      )


(* Registering the existing wikis *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis_path
     (fun wiki path ->
        if wiki <> wiki_admin.wiki_id then
          let path = Ocsigen_lib.split '/' path in
          Wiki_services.register_wiki ~path ~wikibox ~wiki ()
        else
          Lwt.return ()
     )
  )
