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


let eliom_user =
  Ocsimore_common.eliom_opaque_int32 "userid"

(** Widget for user login/logout/edition without addition of new users *)
class login_widget ?sp ~(sessman: Session_manager.sessionmanager) =

  (** Edition of an user *)
  let internal_srv_edit =
    Eliom_services.new_service ?sp
      ~path:([Ocsimore_lib.ocsimore_admin_dir; "edit_user"])
      ~get_params:(opt eliom_user) () in
  let srv_edit_done =
    Eliom_services.new_post_coservice
      ~https:sessman#force_secure
      ~fallback:internal_srv_edit
      ~post_params:(eliom_user ** (string "pwd" **
                      (string "pwd2" ** (string "descr" ** string "email")))) ()
  in

  (** Groups-related services *)
  let params_groups = Eliom_parameters.string "group" **
    (Eliom_parameters.string "add" ** Eliom_parameters.string "rem") in

  let action_add_remove_users_from_group =
    Eliom_predefmod.Any.register_new_post_coservice' ?sp
      ~name:"add_remove_users_from_group" ~post_params:params_groups
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
    Eliom_predefmod.Any.register_new_post_coservice' ?sp
      ~name:"add_remove_user_from_groups" ~post_params:params_groups
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
  let service_edit_user = Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_group"]
    ~get_params:(Eliom_parameters.string "group") ()
  in
  let service_choose_group = Eliom_services.new_service ?sp
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "view_groups"]
    ~get_params:(Eliom_parameters.unit) ()
in
object (self)

  val xhtml_class = "logbox"

  method session_manager = sessman

  method private display_login_box
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ~sp error usr pwd =
    if (Eliom_sessions.get_ssl sp) || not sessman#force_secure
    then begin
      let user_prompt = Ocamlduce.Utf8.make user_prompt in
      let pwd_prompt = Ocamlduce.Utf8.make pwd_prompt in
      let auth_error = Ocamlduce.Utf8.make auth_error in
      {{ [<table>([
                  <tr>[<td>user_prompt
                       <td>[{: str_input usr :}]]
                  <tr>[<td>pwd_prompt
                       <td>[{: passwd_input pwd :}]]
                  <tr>[<td>[{: submit_input "Login" :}]]
                  !{: self#login_box_extension ~sp :}
                ] @
                  {: if error then
                     {{ [<tr>[<td colspan="2">auth_error]
                        ] }}
                   else
                     {{ [] }} :})] }}
    end
    else
      let switchtohttps = Ocamlduce.Utf8.make switchtohttps in
      {{ [ <p>[{: Eliom_duce.Xhtml.a
                    Eliom_services.https_void_coservice'
                    sp switchtohttps () :} ] ] }}

  method private display_logout_box ~sp u =
    self#logout_box_extension ~sp >>= fun ext ->
    Lwt.return {{ [<table>[
                      <tr>[<td>{: Printf.sprintf "Hi %s!" u.user_fullname :}]
                      <tr>[<td>[{: submit_input "logout" :}]]
                      !ext
                    ]] }}

  method private login_box_extension ~sp:_ = {{ [] }}

  method private logout_box_extension ~sp =
    Users.get_user_data sp >>= fun ud ->
    Lwt.return {{ [  <tr>[<td>[{: Eliom_duce.Xhtml.a service_edit_user sp
                                  {{ "Manage your account" }} ud.user_login :}]]
                  ] }}

  method display_login_widget ~sp
    ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps () =
    Users.get_user_data sp >>= fun u ->
    Users.is_logged_on sp >>= fun logged ->
    (if logged then
       self#display_logout_box sp u >>= fun f ->
       Lwt.return (
         Eliom_duce.Xhtml.post_form ~a:{{ { class="logbox logged"} }}
           ~service:sessman#action_logout ~sp (fun _ -> f) ())
     else
       let f_login error ~a = Eliom_duce.Xhtml.post_form
         ~service:sessman#action_login ~sp ~a
         (fun (usr, pwd) ->
            (self#display_login_box ?user_prompt ?pwd_prompt
               ?auth_error ?switchtohttps ~sp error usr pwd))
       in
       if List.exists
         (fun e -> e = Users.BadPassword || e = Users.BadUser)
         (Session_manager.get_login_error ~sp)
       then (* unsuccessful attempt *)
         Lwt.return (f_login true ~a:{{ {class="logbox error"} }} ())
       else (* no login attempt yet *)
         Lwt.return (f_login false ~a:{{ {class="logbox notlogged"} }} ())
    ) >>= fun f ->
    Lwt.return {{<div class={: xhtml_class :}>[f]}}

  method private page_edit err = fun sp userid () ->
    (match userid with
      | None -> Users.get_user_id sp
      | Some user -> Lwt.return user
    ) >>= fun userid ->
    User_data.can_change_user_by_userid sp userid >>= function
      | true ->
        User_sql.get_basicuser_data userid >>= fun u ->
        Ocsimore_common.html_page
          {{ [<h1>"Your account"
               <p>"Change your personal information:"
               {: Eliom_duce.Xhtml.post_form ~service:srv_edit_done ~sp
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
               <p>[<strong>{: err :}]] }}
        | false ->
            let msg = "You do not have sufficient rights to perform this \
                      operation" in
            Ocsimore_common.html_page {{ [<p>[<strong>{: msg :}]] }}

  method private page_edit_done = fun sp _ (userid, (pw,(pw2,(fullname,email))))->
    Lwt.catch
      (fun () ->
         User_data.change_user_data ~sp ~userid ~pwd:(pw, pw2) ~fullname ~email
         >>= fun () ->
         Ocsimore_common.html_page {{ [<h1>"Personal information updated"] }}
      )
      (function
       | Failure s ->
           self#page_edit s sp (Some userid) ()
       | Ocsimore_common.Permission_denied ->
           self#page_edit "ERROR: Insufficient rights" sp (Some userid) ()
       | e -> Lwt.fail e
      )


  method service_edit_user_data :
    (userid option,
     unit,
     Eliom_services.get_service_kind,
     [`WithoutSuffix],
     [ `One of User_sql.Types.userid ] Eliom_parameters.param_name,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_srv_edit


  method display_group ~sp g =
    Users.get_user_by_name g  >>= fun group ->
    (if group = basic_user Users.nobody && g <> Users.nobody_login then
       let msg = Ocamlduce.Utf8.make ("Unknown group " ^ g) in
       Lwt.return {{ [<p>msg] }}
     else
       Users.get_user_data sp >>= fun user ->
       let isadmin = (user.user_id = Users.admin) in

       (* Group change *)
       (User_data.can_change_user_by_user sp group >>= function
          | true ->
              User_sql.get_user_data group >>= fun dgroup ->
              Lwt.return {{ [ <p>[ {: Eliom_duce.Xhtml.a
                                      ~service:self#service_edit_user_data ~sp
                                      {: "Edit information" :}
                                      (Some dgroup.user_id) :}] ] }}

              | false -> Lwt.return {{[]}}
       ) >>= fun edit ->
       let head = {{ [ <h1>['User/Group \'' !{: Ocamlduce.Utf8.make g :} '\'']
                       !edit ] }} in

       (* Adding groups to the group *)
       Users.GroupsForms.form_edit_group ~show_edit:isadmin ~group ~text:""
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
       Users.GroupsForms.form_edit_user ~show_edit:isadmin ~user:group ~text:""
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
       Lwt.return {{ [ !head f1 f2 ] }}
    ) >>= fun body ->
    Ocsimore_common.html_page {{ body }}


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
         ] }}


initializer

  Eliom_duce.Xhtml.register ?sp
    ~service:internal_srv_edit (self#page_edit "");
  Eliom_duce.Xhtml.register ?sp
    ~service:srv_edit_done self#page_edit_done;

  ignore
    (Eliom_duce.Xhtml.register_new_service ?sp
       ~https:sessman#force_secure
       ~path:[Ocsimore_lib.ocsimore_admin_dir; "login"]
       ~get_params:Eliom_parameters.unit
       (fun sp () () ->
          self#display_login_widget ~sp () >>= fun lb ->
          Ocsimore_common.html_page {{ [ lb ] }}
       )
    );


  Eliom_duce.Xhtml.register service_edit_user
    (fun sp g () -> self#display_group ~sp g);
  Eliom_duce.Xhtml.register service_choose_group
    (fun sp () () -> self#display_all_groups ~sp);
end




type basic_user_creation = {
  mail_from: string;
  mail_addr: string;
  mail_subject: string;
  new_user_groups: User_sql.Types.user list;
}


class login_widget_basic_user_creation ?sp
  ~(sessman: Session_manager.sessionmanager)
  ~basic_user_creation_options
=
  let internal_srv_register =
    Eliom_services.new_service ?sp
      ~path:([Ocsimore_lib.ocsimore_admin_dir; "register"])
      ~get_params:unit () in
  let srv_register_done =
    Eliom_services.new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
  in
object (self)

  inherit login_widget sessman

  method private login_box_extension ~sp =
    {{ [ <tr>[<td colspan="2">[
               {: Eliom_duce.Xhtml.a self#service_create_new_user
                  sp {{ "New user? Register now!" }} () :}]] ] }}


  method service_create_new_user :
    (unit,
     unit,
     Eliom_services.get_service_kind,
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_srv_register


  method private page_register err = fun sp () ()->
    Ocsimore_common.html_page
      {{ [<h1>"Registration form"
           <p>['Please fill in the following fields.' <br>[]
               'Be very careful to enter a valid e-mail address, \
                 as the password for logging in will be sent there.']
           {: Eliom_duce.Xhtml.post_form ~sp
              ~service:srv_register_done
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
       }}



  method private page_register_done = fun sp () (user, (fullname, email)) ->
    if not (User_data.valid_username user) then
      self#page_register "ERROR: Bad character(s) in login name!" sp () ()
    else if not (User_data.valid_emailaddr email) then
      self#page_register "ERROR: Bad formed e-mail address!" sp () ()
    else
      let pwd = User_data.generate_password () in
      Lwt.catch (fun () ->
        Users.create_fresh_user ~name:user
          ~pwd:(User_sql.Types.Ocsimore_user_crypt pwd) ~fullname ~email ()
        >>= fun userid ->
        Users.add_to_groups (basic_user userid)
          basic_user_creation_options.new_user_groups >>= fun () ->
        User_data.mail_password
          ~name:user ~password:pwd
          ~from_name:basic_user_creation_options.mail_from
          ~from_addr:basic_user_creation_options.mail_addr
          ~subject:basic_user_creation_options.mail_subject
        >>= function
          | true ->
              Ocsimore_common.html_page
                {{ [<h1>"Registration ok."
                    <p>(['You\'ll soon receive an e-mail message at the \
                           following address:'
                          <br>[]] @
                          {: email :} @ [<br>[]
                         'with your password.'])] }}

          | false ->
              User_sql.delete_user ~userid:userid >>= fun () ->
              Ocsimore_common.html_page
                {{ [<h1>"Registration failed."
                    <p>"Please try later."] }}
                )
        (function
           | Users.BadUser ->
               self#page_register "ERROR: This login already exists" sp () ()
           | e -> Lwt.fail e)

  initializer
    begin
      Eliom_duce.Xhtml.register ?sp
        ~service:internal_srv_register (self#page_register "");
      Eliom_duce.Xhtml.register ?sp
        ~service:srv_register_done self#page_register_done;
    end


end
