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


let str_input ?(value="") ?(visible=true) name =
  Eliom_duce.Xhtml.string_input ~name ~value
    ~input_type:(if visible then ({:"text":} : Xhtmltypes_duce.input_type_values)
                 else {:"hidden":}) ()
let passwd_input ?(value="") name =
  Eliom_duce.Xhtml.string_input ~input_type:{:"password":} ~name ~value ()
let submit_input value =
  Eliom_duce.Xhtml.string_input ~input_type:{:"submit":} ~value ()


class type user_widget_class = object
  method display_roles :
    sp:Eliom_sessions.server_params ->
    Eliom_duce.Blocks.page Lwt.t
  method display_groups :
    sp:Eliom_sessions.server_params ->
    Eliom_duce.Blocks.page Lwt.t
  method display_users :
    sp:Eliom_sessions.server_params ->
    Eliom_duce.Blocks.page Lwt.t

  method display_group :
    sp:Eliom_sessions.server_params ->
    Ocamlduce.Utf8.repr -> {{Eliom_duce.Blocks.page}} Lwt.t

  method display_login_widget :
    sp:Eliom_sessions.server_params ->
    ?user_prompt:Ocamlduce.Utf8.repr ->
    ?pwd_prompt:Ocamlduce.Utf8.repr ->
    ?auth_error:Ocamlduce.Utf8.repr ->
    ?switchtohttps:Ocamlduce.Utf8.repr ->
    unit ->
    Xhtmltypes_duce.form_content Lwt.t

  method private display_logout_box :
    sp:Eliom_sessions.server_params ->
    User_sql.Types.userdata ->
    Xhtmltypes_duce.form_contents Lwt.t
  method display_logout_button :
    sp:Eliom_sessions.server_params ->
    Xhtmltypes_duce.flows -> Eliom_duce.Blocks.form_elt Lwt.t
  method logout_uri :
    sp:Eliom_sessions.server_params -> Eliom_duce.Xhtml.uri

  method user_link :
    sp:Eliom_sessions.server_params ->
    Ocamlduce.Utf8.repr -> Eliom_duce.Blocks.a_elt

  method user_list_to_xhtml :
    sp:Eliom_sessions.server_params ->
    ?hook:(sp:Eliom_sessions.server_params ->
          user:string ->
          Xhtmltypes_duce.flows Lwt.t) ->
    User_sql.Types.user list -> Xhtmltypes_duce.blocks Lwt.t


  (** Helper forms to add and remove users from groups. If [show_edit]
      is false, no controls to edit the permissions are shown *)
  (** Form to add users to a group *)
  method form_edit_group:
    sp:Eliom_sessions.server_params ->
    ?show_edit:bool ->
    ?default_add:string ->
    group:user ->
    text:Xhtmltypes_duce.blocks ->
    unit ->
    Xhtmltypes_duce.tr Lwt.t

  (** Form to add an user to a group *)
  method form_edit_user:
    sp:Eliom_sessions.server_params ->
    user:User_sql.Types.user ->
    text:Xhtmltypes_duce.blocks ->
    unit ->
    Xhtmltypes_duce.blocks Lwt.t

  method form_edit_awr: 'a.
    sp:Eliom_sessions.server_params ->
    text_prefix:string ->
    grps:'a User_sql.Types.admin_writer_reader ->
    arg:'a Opaque.int32_t ->
    ?defaults:string * string * string ->
    unit ->
    {{ [Xhtmltypes_duce.tr+] }} Lwt.t

  method status_text:
    sp:Eliom_sessions.server_params ->
    Xhtmltypes_duce.flows Lwt.t

  method display_group_creation :
    ?err:string ->
    sp:Eliom_sessions.server_params ->
    {{Eliom_duce.Blocks.page}} Lwt.t

  method display_group_creation_done :
    Eliom_sessions.server_params ->
    unit ->
    string * string ->
    {{Eliom_duce.Blocks.page}} Lwt.t

end

class type user_widget_user_creation_class = object
  inherit user_widget_class
  method display_user_creation :
    ?err:string ->
    sp:Eliom_sessions.server_params ->
    {{Eliom_duce.Blocks.page}} Lwt.t
  method display_user_creation_done :
    Eliom_sessions.server_params ->
    unit ->
    string * (string * string) ->
    {{Eliom_duce.Blocks.page}} Lwt.t
end



open Xform.XformLwt
open Ops

module MakeWidget(P : Ocsimore_page.PageSig)(S : User_services.Services) = struct

(** Widget for user login/logout/edition without addition of new users *)
class user_widget ~force_secure : user_widget_class =
object (self)

  val xhtml_class = "logbox"

  method form_edit_group ~sp ?(show_edit=false) ?(default_add="") ~group ~text () =
    (if show_edit then
       User.get_user_data sp >>= fun ud ->
       let user = basic_user ud.user_id in
       User_data.can_admin_group ~sp ~user ~group () >>= function
         | true ->
             User_sql.user_to_string group >>= fun group ->
             Lwt.return
               ((fun ~sp ~user ->
                   let r = self#bt_remove_user_from_group ~sp ~group ~user () in
                  Lwt.return {{ [ ' ' r ] }}),
                (let r = self#form_add_user_to_group ~sp ~default_add ~group () in
                 {{ [ r ] }})
               )
         | false ->
             Lwt.return ((fun ~sp:_ ~user:_ -> Lwt.return {{ [] }}), {{ [] }})
     else
       Lwt.return ((fun ~sp:_ ~user:_ -> Lwt.return {{ [] }}), {{ [] }})
    ) >>= fun (hook, add) ->

    User_sql.users_in_group ~generic:false ~group
    >>= fun users ->
    self#user_list_to_xhtml ~sp ~hook users
    >>= fun members ->
    Lwt.return {{ <tr>[<td class="role">text
                       <td class="current_users">[!members !add]] }}

  method form_edit_user ~sp ~user ~text () =
    User_sql.groups_of_user ~user
    >>= fun groups ->
    self#user_list_to_xhtml ~sp groups
    >>= fun members ->
    Lwt.return {{ [ !text !members ] }}

  method form_edit_awr : 'a. sp:_ -> text_prefix:_ -> grps:'a User_sql.Types.admin_writer_reader -> arg:'a Opaque.int32_t -> ?defaults:_ -> unit -> _ =
   fun ~sp ~text_prefix ~grps ~arg ?defaults () ->
    let aux grp text default =
      self#form_edit_group ~sp ~group:(grp $ arg)
        ~text:{{ [ <p class = "eliom_inline">[
                     <b>{: Ocamlduce.Utf8.make text :} <br>[] ] ] }}
        ~show_edit:true ~default_add:default
    and d1, d2, d3 = match defaults with
      | None -> "", "", ""
      | Some (d1, d2, d3) -> d1, d2, d3
    in
    aux grps.grp_admin  (text_prefix ^ " administrators: ") d1 ()
    >>= fun forma ->
    aux grps.grp_writer (text_prefix ^ " writers: ") d2 ()
    >>= fun formw ->
    aux grps.grp_reader (text_prefix ^ " readers: ") d3 ()
    >>= fun formr ->
    Lwt.return  {{ [ formr formw forma ] }}

  method private bt_remove_user_from_group ~sp ~group ~user ?(text="Remove") () =
    let str_input = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      {{ [ <div class = "eliom_inline">[
             {{ str_input ~value:group gname }}
             {{ str_input ~value:user remname }}
             {{ str_input addname }}
             {{ Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                  (Ocamlduce.Utf8.make text) }}
           ] ] }}
    in
    Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8"
                                       class = "eliom_inline"} }}
      ~service:S.action_add_remove_users_from_group ~sp mform ()

  method private form_add_user_to_group ~sp ~group ?(default_add="") ?(text="Add") () =
    let str_input' = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      {{ [ <div class="eliom_inline">[
             {: str_input' ~value:group gname :}
             {: str_input' remname :}
             {: str_input ~value:default_add addname  :}
             {{ Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                  (Ocamlduce.Utf8.make text) }}
           ] ] }}
    in
    Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8"
                                       class = "eliom_inline" } }}
      ~service:S.action_add_remove_users_from_group ~sp mform ()

  method user_list_to_xhtml ~sp ?hook l =
    match l with
      | [] -> Lwt.return {{ [ <p>[<em>"(currently no user)" ' ' ]] }}
      | e :: q ->
          let convert u =
            User_sql.user_to_string ~expand_param:true u >>= fun su ->
            (match hook with
               | None -> Lwt.return {{ [] }}
               | Some hook -> hook ~sp ~user:su
            ) >>= fun hook ->
            Lwt.return ({{ <li>[{{ self#user_link ~sp su }}!hook] }} :
                           Xhtmltypes_duce.li)
          in
          convert e >>= fun e ->
          List.fold_left
            (fun s u ->
               s >>= fun s ->
               convert u >>= fun r ->
               Lwt.return ({{ [ !s  r ] }} : {{ [ Xhtmltypes_duce.li+ ] }}))
            (Lwt.return {{ [ e ] }})
            q
            >>= fun r ->
            Lwt.return {{ [ <ul class="user_list">r ] }}


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
        {{ [   <table>[
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
    Lwt.return {{ [  <tr>[<td>[{: Eliom_duce.Xhtml.a S.service_view_group sp
                                  {{ "Manage your account" }} ud.user_login :}]]
                  ] }}

  method display_logout_button ~sp (content : Xhtmltypes_duce.flows) =
    Lwt.return
      (Eliom_duce.Xhtml.post_form ~a:{{ { class="logoutbutton"} }}
         ~sp ~service:S.action_logout
         (fun () ->
            {{ [<p>[
                   {: Eliom_duce.Xhtml.button ~button_type:{:"submit":}
                      {: [ <div class="ocsimore_button">content ] :}
                      (*VVV How to avoid the <div> here??? *)
                      :}] ] }}) ())

  method logout_uri ~sp =
    Eliom_duce.Xhtml.make_uri ~service:S.action_logout_get ~sp ()


  method display_login_widget ~sp
    ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps () =
    User.get_user_data sp >>= fun u ->
    User.is_logged_on sp >>= fun logged ->
    (if logged then
       self#display_logout_box sp u >>= fun f ->
       Lwt.return (
         Eliom_duce.Xhtml.post_form ~a:{{ { class="logbox logged"} }}
           ~service:S.action_logout ~sp (fun _ -> f) ())

     else
       let f_login error ~a =
         self#login_box_aux ?user_prompt ?pwd_prompt ?auth_error
           ?switchtohttps ~sp error
         >>= fun f ->
         Lwt.return
           (Eliom_duce.Xhtml.post_form ~service:S.action_login ~sp ~a f ())
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


  method user_link ~sp group =
    Eliom_duce.Xhtml.a  ~service:S.service_view_group ~sp
      {: Ocamlduce.Utf8.make group :}  group


  method display_group ~sp g =
    User.get_user_by_name g  >>= fun group ->
    if group = basic_user User.nobody && g <> User.nobody_login then
       let msg = Ocamlduce.Utf8.make ("Unknown group " ^ g) in
       Lwt.return {{ [<p class="errmsg">msg] }}
    else
       let error = match Ocsimore_common.get_action_failure sp with
         | None -> {{ [] }}
         | Some e -> (* YYY add error handler somewhere *)
             let msg = match e with
               | Ocsimore_common.Permission_denied  ->
                   "Unable to perform operation, insufficient rights"
               | Failure s -> s
               | User.UnknownUser u ->
                   "Unknown user/group '" ^ u ^ "'"
               | _ -> "Error"
             in
             {{ [<p class="errmsg">{:msg:}] }}
       in
       let head =
         {{ <h1>['User/Group \'' !{: Ocamlduce.Utf8.make g :} '\''] }} in

       (* Adding groups to the group *)
       self#form_edit_group ~sp ~show_edit:true ~group
         ~text:{{[ <p class = "eliom_inline">[
                     <strong>"Current users in this group: " ] ] }} ()
       >>= fun f1  ->

       (* Adding the group to groups *)
       self#form_edit_user ~sp (* XXX ~show_edit:isadmin *) ~user:group
         ~text:{{[ <p class = "eliom_inline">[
                     <strong>"Current groups in which the user is: " ] ] }} ()
       >>= fun f2  ->

       User_sql.get_user_data group >>= fun g ->
       User_data.can_change_user_data_by_user sp group >>= fun can_change ->
       let edit =
         if can_change &&
           g.user_pwd <> Connect_forbidden &&
           g.user_pwd <> External_Auth
         then
           {{ [ {: Eliom_duce.Xhtml.post_form
                   ~service:S.action_edit_user_data ~sp
                   (fun (nuserid, (pwd, (pwd2, (desc, email)))) ->
                      {{ [<table>[
                             <tr>[
                               <td>[<strong>"Real name: "]
                               <td>[{: str_input ~value:g.user_fullname desc :}]
                             ]
                             <tr>[
                               <td>"e-mail address: "
                               <td>[{: str_input
                                       ~value:(match g.user_email with
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
                                         ~name:nuserid ~value:g.user_id () :}]
                             ]
                           ]]
                       }}) () :}
              ] }}
         else
           {{ [ <p>[ <strong>"Real name: " !{: Ocamlduce.Utf8.make g.user_fullname :} ]
              ] }}
       in
       Lwt.return ({{ [ head
                        !error
                        <div class="user_block">edit
                        <div class="user_block">
                          [<table class="users_in_group">[f1]]
                        <div class="user_block">f2
                      ] }} : Xhtmltypes_duce.blocks)


  method display_users ~sp =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u; user_pwd = a} ->
                           u = `BasicUser && a <> Connect_forbidden ) l in
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    self#display_users_groups ~show_auth:true ~sp ~l >>= fun r ->

    Lwt.return
      ({{ [ <h1>"Existing users" r
       ] }} : Xhtmltypes_duce.blocks)

  method display_groups ~sp =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u; user_pwd = a} ->
                           u = `BasicUser && a = Connect_forbidden ) l in
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    self#display_users_groups ~show_auth:false ~sp ~l >>= fun r ->

    Lwt.return
      ({{ [ <h1>"Existing groups" r
       ] }} : Xhtmltypes_duce.blocks)


  (* Parameterized users *)
  method display_roles ~sp =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u} -> u <> `BasicUser) l in
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    let hd, tl = List.hd l, List.tl l (* some groups always exist*) in
    let line u =
      let g = Ocamlduce.Utf8.make u.user_login
      and p =  (if u.user_kind = `ParameterizedGroup then
                  {{ [<em>['(param)']] }}
                else {{ [] }})
      and d = Ocamlduce.Utf8.make u.user_fullname
      in
      {{ <tr>[<td>[<b>g!p ] <td>d ] }}
    in
    let l1 = List.fold_left (fun (s : {{ [Xhtmltypes_duce.tr*] }}) arg ->
                               {{ [ !s {: line arg:} ] }}) {{ [] }} tl in
    let t1 = {{ <table class="table_admin">[{: line hd :}
                          !l1]}} in

    let form name = {{ [<p>[
           {: Eliom_duce.Xhtml.string_input ~name~input_type:{: "text" :} () :}
           {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
              {{ "Edit this role" }} :}
                         ]] }}
    in
    let f = Eliom_duce.Xhtml.get_form ~a:{{ { accept-charset="utf-8" } }}
      ~service:S.service_view_group ~sp form
    and msg2 = Ocamlduce.Utf8.make "Choose one group, and enter it \
                    (including its parameter if needed) below"
    in
    Lwt.return
      ({{ [ <h1>"Roles" t1
           <p>msg2 f
       ] }} : Xhtmltypes_duce.blocks)


  method private display_users_groups ~show_auth ~sp ~l =
    let line2 u =
      let g = Ocamlduce.Utf8.make u.user_login
      and d = Ocamlduce.Utf8.make u.user_fullname
      and l = Eliom_duce.Xhtml.a ~service:S.service_view_group ~sp
         (P.icon ~sp ~path:"imgedit.png" ~text:"Details")
         u.user_login
      and id = Ocamlduce.Utf8.make (string_from_userid u.user_id)
      and a = if show_auth then
        {{ [ <td>{{Ocamlduce.Utf8.make
                     (match u.user_pwd with
                        | Connect_forbidden -> "group"
                        | Ocsimore_user_plain _ | Ocsimore_user_crypt _ ->
                            "password"
                        | External_Auth -> "external"
                     ) }} ] }}
      else
        {{ [] }}
      in
      {{ <tr>[<td class="userid">id
              <td class="userlogin">[<b>g ]
              <td class="userdescr">d
              !a
              <td>[l]
             ] }}
    in
    let l = List.fold_left (fun (s : {{ [Xhtmltypes_duce.tr*] }}) arg ->
                               {{ [ !s {: line2 arg:} ] }}) {{ [] }} l in
    Lwt.return ({{ <table class="table_admin">[
                     <tr>[<th>"Id"
                           <th>"Login"
                           <th>"Description"
                           !{{ if show_auth then
                                 {{ [ <th>"Authentification" ] }}
                               else {{ [] }} }}
                         ]
                       !l]}} : Xhtmltypes_duce.block )


  method status_text ~sp =
    User.get_user_data sp >>= fun u ->
      if u.user_id <> User.anonymous then
        let u = Ocamlduce.Utf8.make u.user_login in
        self#display_logout_button ~sp {{ ['Logout'] }} >>= fun l ->
        Lwt.return {{ ['You are logged in as ' !u '. ' l ] }}
      else
        let l = Eliom_duce.Xhtml.a S.service_login sp {{ "Login" }} () in
        Lwt.return {{ ['You are not currently logged. ' l]  }}

  method display_group_creation ?(err="") ~sp =
    User_data.can_create_group ~sp >>= function
      | true ->
          Lwt.return
            ({{ [<h1>"Group creation"
                 <p>['You can use the form below to create a new Ocsimore group.
                     (A group is a special form of user that is not \
                     authorized to log in.) Once this is done, you will \
                     be able to add users into your group.'
                    ]
                 <h2>"Create a new group"
                 {: Eliom_duce.Xhtml.post_form ~sp
                    ~service:S.action_create_new_group
                    (fun (usr,desc) ->
                       {{ [<table>[
                              <tr>[
                                <td>"group name: (letters & digits only)"
                                <td>[{: str_input usr :}]
                              ]
                              <tr>[
                                <td>"description:"
                                <td>[{: str_input desc :}]
                              ]
                              <tr>[
                                <td>[{: submit_input "Create" :}]
                              ]]] }})
                    () :}
                 <p>[<strong>{: err :}]]
             }} : Xhtmltypes_duce.blocks)
      | false ->
          Lwt.return {{ [ <h1>"Error"
                          <p>"Your are not allowed to create new groups" ] }}

  method display_group_creation_done sp () (name, descr) =
    Lwt.catch
      (fun () ->
         User_data.create_group ~sp ~name ~descr >>= fun groupid ->
         User_sql.get_basicuser_data groupid >>= fun group ->
         Lwt.return
           {{ [<h1>"Group created"
                <p>[!"You can now "
                    {: Eliom_duce.Xhtml.a ~sp ~service:S.service_view_group
                       {: "edit" :} group.user_login :}
                    !" your new group."
                   ] ] }}
      )
      (function
         | Failure err -> self#display_group_creation ~err ~sp
         | Ocsimore_common.Permission_denied ->
             Lwt.return {{ [ <h1>"Error"
                             <p>"You cannot create new users" ] }}
         | e -> Lwt.fail e)

end


module MakeWidgetCreation(SC : User_services.ServicesCreationUser) = struct

class user_widget_user_creation ~force_secure ~user_creation_options : user_widget_user_creation_class =
object (self)

  inherit user_widget ~force_secure

  method private login_box_extension ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >>= function
      | true -> Lwt.return
          {{ [ <tr>[<td colspan="2">[
                       {: Eliom_duce.Xhtml.a SC.service_create_new_user
                          sp {{ "New user? Register now!" }} () :}]] ] }}
      | false -> Lwt.return {{ [] }}

  method display_user_creation ?(err="") ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >>= function
      | true ->
          Lwt.return
            ({{ [<h1>"User creation"
                 <p>['You can use the form below to create a new Ocsimore user.'
                     <br>[]
                     'Note that users that authenticate through external means \
                     (NIS or PAM) are added automatically the first time they \
                     log in inside Ocsimore, and you do not need to create them'
                    ]
                 <h2>"Create a new user"
                 <p>['Please fill in the following fields.' <br>[]
                     'Be very careful to enter a valid e-mail address, \
                       as the password for logging in will be sent there.']
                 {: Eliom_duce.Xhtml.post_form ~sp
                    ~service:SC.action_create_new_user
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
                             <p>"You cannot create new users" ] }}
         | e -> Lwt.fail e)
end

end

end
