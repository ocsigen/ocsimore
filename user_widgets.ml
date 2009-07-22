(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
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
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)

open Eliom_parameters
open User_sql.Types

let (>>=) = Lwt.bind


let str_input ?(value="") name =
  Eliom_duce.Xhtml.string_input ~input_type:{:"text":} ~name ~value ()
let passwd_input ?(value="") name =
  Eliom_duce.Xhtml.string_input ~input_type:{:"password":} ~name ~value ()
let submit_input value =
  Eliom_duce.Xhtml.string_input ~input_type:{:"submit":} ~value ()



(** Widget for user login/logout/edition without addition of new users *)
class user_widget ~force_secure
~user_services:(
    action_login,
    action_logout,
    action_logout_get,
    service_edit_user_data,
    action_edit_user_data,
    action_add_remove_users_from_group,
    action_add_remove_user_from_groups,
    service_view_group,
    service_view_groups,
    service_login
  ) =
object (self)

  val xhtml_class = "logbox"

  method private login_box_aux
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ~sp error =
    if (Eliom_sessions.get_ssl sp) || not force_secure
    then begin
      let user_prompt = Ocamlduce.Utf8.make user_prompt in
      let pwd_prompt = Ocamlduce.Utf8.make pwd_prompt in
      let auth_error = Ocamlduce.Utf8.make auth_error in
      self#login_box_extension ~sp >>= fun ext ->
      Lwt.return (fun (usr, pwd) ->
        {{ [<h1>"Login page"
               <table>[
               <tr>[<td>user_prompt
                    <td>[{: str_input usr :}]]
               <tr>[<td>pwd_prompt
                    <td>[{: passwd_input pwd :}]]
               <tr>[<td>[{: submit_input "Login" :}]]
               !{: ext :}
               !{: if error then
                   {{ [<tr>[<td colspan="2">auth_error]] }}
                 else
                   {{ [] }} :}
             ] ] }})
    end
    else
      let switchtohttps = Ocamlduce.Utf8.make switchtohttps in
      Lwt.return (fun _ ->
        {{ [ <p>[{: Eliom_duce.Xhtml.a
                    Eliom_services.https_void_coservice'
                    sp switchtohttps () :} ] ] }})

  method private display_logout_box ~sp u =
    self#logout_box_extension ~sp >>= fun ext ->
    Lwt.return {{ [<table>[
                      <tr>[<td>{: Printf.sprintf "Hi %s!" u.user_fullname :}]
                      <tr>[<td>[{: submit_input "logout" :}]]
                      !ext
                    ]] }}

  method private login_box_extension ~sp:_ = Lwt.return {{ [] }}

  method private logout_box_extension ~sp =
    User.get_user_data sp >>= fun ud ->
    Lwt.return {{ [  <tr>[<td>[{: Eliom_duce.Xhtml.a service_view_group sp
                                  {{ "Manage your account" }} ud.user_login :}]]
                  ] }}

  method display_logout_button ~sp (content : Xhtmltypes_duce.flows) =
    Lwt.return
      (Eliom_duce.Xhtml.post_form ~a:{{ { class="logoutbutton"} }}
         ~sp ~service:action_logout
         (fun () ->
            {{ [<p>[
                   {: Eliom_duce.Xhtml.button ~button_type:{:"submit":}
                      {: [ <div class="ocsimore_button">content ] :}
                      (*VVV How to avoid the <div> here??? *)
                      :}] ] }}) ())

  method logout_uri ~sp =
    Eliom_duce.Xhtml.make_uri ~service:action_logout_get ~sp ()


  method display_login_widget ~sp
    ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps () =
    User.get_user_data sp >>= fun u ->
    User.is_logged_on sp >>= fun logged ->
    (if logged then
       self#display_logout_box sp u >>= fun f ->
       Lwt.return (
         Eliom_duce.Xhtml.post_form ~a:{{ { class="logbox logged"} }}
           ~service:action_logout ~sp (fun _ -> f) ())

     else
       let f_login error ~a =
         self#login_box_aux ?user_prompt ?pwd_prompt ?auth_error
           ?switchtohttps ~sp error
         >>= fun f ->
         Lwt.return
           (Eliom_duce.Xhtml.post_form ~service:action_login ~sp ~a f ())
       in
       if List.exists
         (fun e -> e = User.BadPassword || e = User.BadUser)
         (User_services.get_login_error ~sp)
       then (* unsuccessful attempt *)
         f_login true ~a:{{ {class="logbox error"} }}
       else (* no login attempt yet *)
         f_login false ~a:{{ {class="logbox notlogged"} }}
    ) >>= fun f ->
    Lwt.return {{<div class={: xhtml_class :}>[f]}}

  method display_edit_user_data ?(err="") = fun sp userid () ->
    (match userid with
      | None -> User.get_user_id sp
      | Some user -> Lwt.return user
    ) >>= fun userid ->
    User_data.can_change_user_data_by_userid sp userid >>= function
      | true ->
        User_sql.get_basicuser_data userid >>= fun u ->
        Ocsimore_page.html_page sp
          {{ [<h1>"Your account"
               <p>"Change your personal information:"
               {: Eliom_duce.Xhtml.post_form ~service:action_edit_user_data ~sp
                  (fun (nuserid, (pwd, (pwd2, (desc, email)))) ->
                     {{ [<table>[
                            <tr>[
                              <td>"login name: "
                              <td>[<strong>{: u.user_fullname :}]
                            ]
                            <tr>[
                              <td>"real name: "
                              <td>[{: str_input ~value:u.user_fullname desc :}]
                            ]
                            <tr>[
                              <td>"e-mail address: "
                              <td>[{: str_input
                                      ~value:(match u.user_email with
                                                | None -> ""
                                                | Some e -> e)
                                      email :}] ]
                            <tr>[
                              <td colspan="2">"Enter a new password twice, or
                                                leave blank for no changes:"
                            ]
                            <tr>[
                              <td>[{: passwd_input pwd  :}]
                              <td>[{: passwd_input pwd2 :}]
                            ]
                            <tr>[
                              <td>[{: submit_input "Confirm" :}
                                     {: Eliom_duce.Xhtml.user_type_input
                                        string_from_userid
                                        ~input_type:{: "hidden" :}
                                        ~name:nuserid ~value:userid () :}]
                            ]
                          ]]
                      }}) None :}
               !{: if err = "" then [] else
                   [{{<p>[<strong>{: err :}] }}] :}
             ] }}
        | false ->
            let msg = "You do not have sufficient rights to perform this \
                      operation" in
            Ocsimore_page.html_page sp {{ [<p>[<strong>{: msg :}]] }}

  method display_edit_user_data_done sp (userid, (pw,(pw2,(fullname,email)))) =
    (* The first userid is always ignored, but is needed because we use as
       fallback a service that takes this argument *)
    Lwt.catch
      (fun () ->
         User_data.change_user_data ~sp ~userid ~pwd:(pw, pw2) ~fullname ~email
         >>= fun () ->
         Ocsimore_page.html_page sp {{ [<h1>"Personal information updated"] }}
      )
      (function
       | Failure s ->
           self#display_edit_user_data ~err:s sp (Some userid) ()
       | Ocsimore_common.Permission_denied ->
           self#display_edit_user_data
            ~err:"ERROR: Insufficient rights" sp (Some userid) ()
       | e -> Lwt.fail e
      )


  method display_group ~sp g =
    User.get_user_by_name g  >>= fun group ->
    if group = basic_user User.nobody && g <> User.nobody_login then
       let msg = Ocamlduce.Utf8.make ("Unknown group " ^ g) in
       Lwt.return {{ [<p>msg] }}
    else
       let error = match Ocsimore_common.get_action_failure sp with
         | None -> {{ [] }}
         | Some e -> (* YYY add error handler somewhere *)
             let msg = match e with
               | Ocsimore_common.Permission_denied  ->
                   "Unable to perform operation, insufficient rights"
               | Failure s -> s
               | User.UnknownUser u ->
                   "Unknown user '" ^ u ^ "'"
               | _ -> "Error"
             in
             {{ [<h2>[<em>{:msg:}]] }}
       in

       User.get_user_data sp >>= fun user ->
       let isadmin = (user.user_id = User.admin) in

       (* Group change *)
       (User_data.can_change_user_data_by_user sp group >>= function
          | true ->
              User_sql.get_user_data group >>= fun dgroup ->
              Lwt.return {{ [ <p>[ {: Eliom_duce.Xhtml.a
                                      ~service:service_edit_user_data ~sp
                                      {: "Edit information" :}
                                      (Some dgroup.user_id) :}] ] }}

              | false -> Lwt.return {{[]}}
       ) >>= fun edit ->
       let head = {{ [ <h1>['User/Group \'' !{: Ocamlduce.Utf8.make g :} '\'']
                       !edit ] }} in

       (* Adding groups to the group *)
       User.GroupsForms.form_edit_group ~show_edit:isadmin ~group ~text:""
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
       User.GroupsForms.form_edit_user ~show_edit:isadmin ~user:group ~text:""
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
       Lwt.return {{ [ !error !head f1 f2 ] }}


  method display_all_groups ~sp =
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
      and l = {{ [ {: Eliom_duce.Xhtml.a ~service:service_view_group ~sp
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
      ~service:service_view_group ~sp form
    in

    let title1 = Ocamlduce.Utf8.make "Standard users"
    and title2 = Ocamlduce.Utf8.make "Groups"
    and msg2 = Ocamlduce.Utf8.make "Choose one group, and enter it \
                    (including its parameter if needed) below" in
    Lwt.return
      ({{ [ <p>[<b>title1]               t2
           <p>[<b>title2 <br>[] !msg2 ] t1
           f
       ] }} : Xhtmltypes_duce.blocks)

end



class user_widget_user_creation ~force_secure ~user_services ~user_creation_options ~creation_services:(service_create_new_user, action_create_new_user) =
object (self)

  inherit user_widget ~force_secure ~user_services

  method private login_box_extension ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >>= function
      | true -> Lwt.return
          {{ [ <tr>[<td colspan="2">[
                       {: Eliom_duce.Xhtml.a service_create_new_user
                          sp {{ "New user? Register now!" }} () :}]] ] }}
      | false -> Lwt.return {{ [] }}

  method display_user_creation ?(err="") ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >>= function
      | true ->
          Lwt.return
            ({{ [<h1>"Registration form"
                 <p>['Please fill in the following fields.' <br>[]
                     'Be very careful to enter a valid e-mail address, \
                       as the password for logging in will be sent there.']
                 {: Eliom_duce.Xhtml.post_form ~sp
                    ~service:action_create_new_user
                    (fun (usr,(desc,email)) ->
                       {{ [<table>[
                              <tr>[
                                <td>"login name: (letters & digits only)"
                                <td>[{: str_input usr :}]
                              ]
                              <tr>[
                                <td>"real name:"
                                <td>[{: str_input desc :}]
                              ]
                              <tr>[
                                <td>"e-mail address:"
                                <td>[{: str_input email :}]
                              ]
                              <tr>[
                                <td>[{: submit_input "Register" :}]
                              ]]] }})
                    () :}
                 <p>[<strong>{: err :}]]
             }} : Xhtmltypes_duce.blocks)
      | false ->
          Lwt.return {{ [ <h1>"Error"
                          <p>"Your are not allowed to create new users" ] }}

  method display_user_creation_done sp () (name, (fullname, email)) =
    Lwt.catch
      (fun () ->
         User_data.create_user ~sp ~name ~fullname ~email
           ~options:user_creation_options >>= fun () ->
         Lwt.return
           {{ [<h1>"Registration ok."
                <p>[!"You\'ll soon receive an e-mail message at the \
                       following address:" <br>[]
                     !{: email :} <br>[]
                     !"with your password."] ] }}
      )
      (function
         | Failure err -> self#display_user_creation ~err ~sp
         | Ocsimore_common.Permission_denied ->
             Lwt.return {{ [ <h1>"Error"
                             <p>"Only admin can create new users" ] }}
         | e -> Lwt.fail e)
end
