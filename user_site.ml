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

open User_sql.Types
open Lwt

let ( ** ) = Eliom_parameters.prod


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
           }
          )
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata default_data c)


let user_widget =
  let sminfo = {
    Session_manager.login_actions = (fun _sp _sess -> return ());
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
  (* Creation of the login box. This register some services, in the
     initializers of User_widgets.login_widget  *)
  (match basicusercreation with
     | BasicUserCreation buc ->
         (new User_widgets.login_widget_basic_user_creation sm buc
          :> User_widgets.login_widget)
     | NoUserCreation ->
         new User_widgets.login_widget sm
  )

let () = User_ext.register_user_extensions
  Wiki_syntax.wikicreole_parser user_widget


(*
let () =
  let action_add_remove_users_from_group =
    Eliom_predefmod.Any.register_new_post_coservice'
      ~name:"add_remove_users_from_group" ~post_params:params
      (fun sp () (g, (add, rem)) ->
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
          Users.get_user_data sp >>= fun user ->

          (* Password change *)

          let isadmin = (user.user_id = Users.admin) in

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
       Ocsimore_common.html_page {{ body }} >>= fun html ->
       Eliom_duce.Xhtml.send ~sp html
    )
  in
  let _service_choose_group = Eliom_predefmod.Any.register_new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_groups"]
    ~get_params:(Eliom_parameters.unit)
    (fun sp () () ->
       (* Lists of groups *)
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
       and msg2 = Ocamlduce.Utf8.make "Choose one group, and enter it \
                    (including its parameter if needed) below" in
       Ocsimore_common.html_page
         {{ [ <p>[<b>title1]               t2
              <p>[<b>title2 <br>[] !msg2 ] t1
              f
            ] }} >>= fun html ->
       Eliom_duce.Xhtml.send ~sp html
    )
  in ()
*)
