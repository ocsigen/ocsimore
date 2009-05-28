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
          Users.user_list_of_string (List.assoc "groups" atts)
        with Not_found -> Lwt.return [basic_user Users.authenticated_users])
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

let _ =
  let sminfo = {
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
  )
  
let () = 
  Wiki_widgets.register_wikibox_syntax_extensions 
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
       Wiki_services.get_admin_wiki () >>= fun admin_wiki ->
       let bi = { bi with Wiki_widgets_interface.bi_subbox =
           Some {{ [ subbox ] }} } in
       wikibox_widget#display_interactive_wikibox ~bi
         (admin_wiki.wiki_id, admin_wiki.wiki_container)
       >>= fun page ->
       wikibox_widget#css_header ~admin:true ~bi ?page:None w
       >>= fun css ->
       Lwt.return (wikibox_widget#display_container ~css {{ [ page ] }})
    )


let () =
  let params = Eliom_parameters.string "group" **
    (Eliom_parameters.string "add" ** Eliom_parameters.string "rem") in
  let action_add_remove_users_from_group =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_users_from_group" ~post_params:params
      (fun sp () (g, (add, rem)) ->
         (** XXX add error catching *)
         Users.get_user_id sp >>= fun user ->
         if user = Users.admin then
           Users.get_user_by_name g
           >>= fun group ->
           Users.GroupsForms.add_remove_users_from_group add rem group
           >>= fun () -> Eliom_predefmod.Action.send ~sp ()
         else
           Lwt.fail Ocsimore_common.Permission_denied
      )
  and action_add_remove_user_from_groups =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_user_from_groups" ~post_params:params
      (fun sp () (g, (add, rem)) ->
         (** XXX add error catching *)
         Users.get_user_id sp >>= fun user ->
         if user = Users.admin then
           Users.get_user_by_name g
           >>= fun group ->
           Users.GroupsForms.user_add_remove_from_groups group add rem
           >>= fun () -> Eliom_predefmod.Action.send ~sp ()
         else
           Lwt.fail Ocsimore_common.Permission_denied
      )
  in
  let service_edit_user = Eliom_predefmod.Any.register_new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_group"]
    ~get_params:(Eliom_parameters.string "group")
    (fun sp g () ->
       Users.get_user_by_name g
       >>= fun group ->
       (if group = basic_user Users.nobody && g <> "nobody" then
          let msg = Ocamlduce.Utf8.make ("Unknown group " ^ g) in
          Lwt.return {{ [<p>msg] }}
        else
          Users.get_user_id sp >>= fun user ->
          let isadmin = (user = Users.admin) in

          (* Adding groups to the group *)
          Users.GroupsForms.form_edit_group ~show_edit:isadmin
            ~group ~text:("Group " ^ g)
          >>= fun form  ->
          let form (n, (n1, n2)) =
            {{ [<p>[{: Eliom_duce.Xhtml.string_input
                       ~input_type:{: "hidden" :} ~name:n ~value:g () :}
                    !{: form (n1, n2) :}
                    !{: if isadmin then
                        {{ [ {: Eliom_duce.Xhtml.button
                                ~button_type:{: "submit" :} {{ "Save" }} :} ] }}
                      else {{ [] }} :}
                   ]] }} in
          let f1 = Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8"} }}
            ~service:action_add_remove_users_from_group ~sp form () in

          (* Adding the group to groups *)
          Users.GroupsForms.form_edit_user ~show_edit:isadmin
            ~user:group ~text:""
          >>= fun form  ->
          let form (n, (n1, n2)) =
            {{ [<p>[{: Eliom_duce.Xhtml.string_input
                       ~input_type:{: "hidden" :} ~name:n ~value:g () :}
                    !{: form (n1, n2) :}
                    !{: if isadmin then
                        {{ [ {: Eliom_duce.Xhtml.button
                                ~button_type:{: "submit" :} {{ "Save" }} :} ] }}
                      else {{ [] }} :}
                   ]] }} in
          let f2 = Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8"} }}
            ~service:action_add_remove_user_from_groups ~sp form ()
          in
          Lwt.return {{ [ f1 f2 ] }}
       )>>= fun body ->
       let html = wikibox_widget#display_container {{ body }} in
       Eliom_duce.Xhtml.send ~sp html
    )
  in
  let _service_choose_group = Eliom_predefmod.Any.register_new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_groups"]
    ~get_params:(Eliom_parameters.unit)
    (fun sp () () ->
       User_sql.all_groups () >>= fun l ->
       let l1, l2 = List.partition (fun {user_kind = u} -> u <> `BasicUser) l in
       let l2 = List.sort
         (fun u1 u2 -> compare u1.user_fullname u2.user_fullname) l2 in

       (* Parameterized users *)
       let hd1, tl1 = List.hd l1, List.tl l1 (* some groups always exist*) in
       let line1 u =
         let g = Ocamlduce.Utf8.make u.user_login
         and p =  (if u.user_kind = `ParameterizedGroup then
                     {{ [<em>['(param)']] }}
                   else {{ [] }})
         and d = Ocamlduce.Utf8.make u.user_fullname in
         {{ <tr>[<td>[<b>g!p ] <td>d ] }}
       in
       let l1 = List.fold_left (fun (s : {{ [Xhtmltypes_duce.tr*] }}) arg ->
                                 {{ [ !s {: line1 arg:} ] }}) {{ [] }} tl1 in
       let t1 = {{ <table>[{: line1 hd1 :}
                              !l1]}} in

       (* Standard users *)
       let hd2, tl2 = List.hd l2, List.tl l2 (* admin always exists*) in
       let line2 u =
         let g = Ocamlduce.Utf8.make u.user_login
         and d = Ocamlduce.Utf8.make u.user_fullname
         and l = {{ [ {: Eliom_duce.Xhtml.a ~service:service_edit_user ~sp
                         {: "Edit" :} u.user_login :}] }}
         in
         {{ <tr>[<td>[<b>g ] <td>d <td>l] }}
       in
       let l2 = List.fold_left (fun (s : {{ [Xhtmltypes_duce.tr*] }}) arg ->
                                 {{ [ !s {: line2 arg:} ] }}) {{ [] }} tl2 in
       let t2 = {{ <table>[{: line2 hd2 :}
                              !l2]}} in
       let form name = {{ [<p>[
           {: Eliom_duce.Xhtml.string_input ~name~input_type:{: "text" :} () :}
           {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
              {{ "Edit this user/group" }} :}
                            ]] }}
       in
       let f = Eliom_duce.Xhtml.get_form ~a:{{ { accept-charset="utf-8" } }}
               ~service:service_edit_user ~sp form
       in

       let title1 = Ocamlduce.Utf8.make "Standard users"
       and title2 = Ocamlduce.Utf8.make "Groups"
       and msg2 = Ocamlduce.Utf8.make "Choose one group, and enter it with \
                    (including its parameter if needed) below" in
       let html = wikibox_widget#display_container
         {{ [ <p>[<b>title1]               t2
              <p>[<b>title2 <br>[] !msg2 ] t1
              f
            ] }} in
       Eliom_duce.Xhtml.send ~sp html
    )
  in ()





(** (We create the wiki containing the administration boxes *)
let wiki_admin = Lwt_unix.run
  (Lwt.catch
     (fun () -> Wiki_sql.get_wiki_info_by_name Wiki_services.wiki_admin_name)
     (function
        | Not_found ->
            Wiki_data.really_create_wiki
              ~title:Wiki_services.wiki_admin_name
              ~descr:"Administration boxes"
              ~path:[Ocsimore_lib.ocsimore_admin_dir]
              ~boxrights:true
              ~author:Users.admin
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
       (fun g -> User_sql.add_to_group ~user:(basic_user Users.anonymous)
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
           ~author:Users.admin ()
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


(* Default permissions for the migration to the new system *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_title = name} ->
        Users.add_to_group ~user:(basic_user Users.anonymous)
          ~group:(Wiki.wiki_wikiboxes_grps.grp_reader $ wiki)
        >>= fun () ->
        try Scanf.sscanf name "wikiperso for %s"
          (fun user ->
             Users.get_user_by_name user
             >>= fun user ->
             Users.add_to_group ~user ~group:(Wiki.wiki_admins $ wiki)
          )
        with Scanf.Scan_failure _ -> Lwt.return ()

     ))

