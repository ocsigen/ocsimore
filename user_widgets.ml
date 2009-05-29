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


class login_widget ?sp ~(sessman: Session_manager.sessionmanager) =
object (self)

  val xhtml_class = "logbox"

  method private display_login_box
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ~sp error usr pwd =
    if (Eliom_sessions.get_ssl sp) || not (Session_manager.get_secure ())
    then begin
      let user_prompt = Ocamlduce.Utf8.make user_prompt in
      let pwd_prompt = Ocamlduce.Utf8.make pwd_prompt in
      let auth_error = Ocamlduce.Utf8.make auth_error in
      {{ [<table>([
                  <tr>[<td>user_prompt
                       <td>[{: Eliom_duce.Xhtml.string_input
                                ~input_type:{:"text":} ~name:usr () :}]]
                  <tr>[<td>pwd_prompt
                       <td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"password":} ~name:pwd () :}]]
                  <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"submit":} ~value:"Login" () :}]]
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

  method private display_logout_box ~sp:_ u =
    {{ [<table>[
           <tr>[<td>{: Printf.sprintf "Hi %s!" u.user_fullname :}]
           <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                        ~input_type:{:"submit":} ~value:"logout" () :}]]
         ]] }}

  method display_login_widget
    ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ~sp () =
    Users.get_user_data sp >>= fun u ->
    Users.is_logged_on sp >>= fun logged ->
    Lwt.return
      {{ <div class={: xhtml_class :}>
           [{:
               if logged
               then
                 Eliom_duce.Xhtml.post_form
                   ~a:{{ { class="logbox logged"} }}
                   ~service:sessman#act_logout
                   ~sp
                   (fun _ -> self#display_logout_box sp u) ()
               else
                 let exn = Session_manager.get_login_error ~sp in
                 if List.exists
                   (fun e -> e = Users.BadPassword || e = Users.BadUser)
                   exn
                 then (* unsuccessful attempt *)
                     Eliom_duce.Xhtml.post_form
                       ~a:{{ {class="logbox error"} }}
                       ~service:sessman#act_login
                       ~sp
                       (fun (usr, pwd) ->
                          (self#display_login_box
                             ?user_prompt ?pwd_prompt ?auth_error
                             ?switchtohttps
                             ~sp true usr pwd)) ()
                 else (* no login attempt yet *)
                     Eliom_duce.Xhtml.post_form
                       ~a:{{ {class="logbox notlogged"} }}
                       ~service:sessman#act_login
                       ~sp
                       (fun (usr, pwd) ->
                          (self#display_login_box
                             ?user_prompt ?pwd_prompt ?auth_error
                             ?switchtohttps
                             ~sp false usr pwd)) ()
                       :}]
       }}


  initializer

      Wiki_syntax.add_extension ~wp:Wiki_syntax.wikicreole_parser
        ~name:"loginbox" ~wiki_content:true
        (fun bi args _c ->
           Wikicreole.Block
             (let user_prompt = Ocsimore_lib.list_assoc_opt "user_prompt" args in
              let pwd_prompt = Ocsimore_lib.list_assoc_opt "pwd_prompt" args in
              let auth_error = Ocsimore_lib.list_assoc_opt "auth_error" args in
              let switchtohttps = Ocsimore_lib.list_assoc_opt "switch_to_https" args in
              self#display_login_widget
                ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps
                ~sp:bi.Wiki_widgets_interface.bi_sp
                ()
              >>= fun b ->
              Lwt.return {{ [ b ] }}));

      ignore
        (Eliom_duce.Xhtml.register_new_service ?sp
           ~https:(Session_manager.get_secure ())
           ~path:[Ocsimore_lib.ocsimore_admin_dir; "login"]
           ~get_params:Eliom_parameters.unit
           (fun sp () () ->
              self#display_login_widget ~sp () >>= fun lb ->
              Lwt.return
                {{
                   <html xmlns="http://www.w3.org/1999/xhtml">[
                     <head>[
                       <title>"Ocsimore login"
                         {: Eliom_duce.Xhtml.css_link
                            (Eliom_duce.Xhtml.make_uri
                               (Eliom_services.static_dir sp)
                               sp ["example.css"]) () :}
(*VVV css ??? *)
                     ]
                     <body>[ lb ]
                   ]
                 }}
           )
        );

      let add_extension = Wiki_syntax.add_extension
        ~wp:Wiki_syntax.wikicreole_parser in
      add_extension ~name:"username" ~wiki_content:true
        (fun bi _args _c ->
           Wikicreole.A_content
             (Users.get_user_data
                ~sp:bi.Wiki_widgets_interface.bi_sp
              >>= fun ud ->
             Lwt.return (Ocamlduce.Utf8.make ud.user_fullname))
        );

      add_extension ~name:"logoutbutton" ~wiki_content:true
        (fun bi _args c ->
           Wikicreole.Block
             (let content = match c with
                | Some c -> c
                | None -> "logout"
              in
              Wiki_syntax.xml_of_wiki Wiki_syntax.wikicreole_parser
                bi content >>= fun c ->
              Lwt.return
                {{ [ {:
                        Eliom_duce.Xhtml.post_form
                        ~a:{{ { class="logoutbutton"} }}
                        ~service:sessman#act_logout
                        ~sp:bi.Wiki_widgets_interface.bi_sp
                        (fun () ->
                           {{ [<p>[
                                  {: Eliom_duce.Xhtml.button
                                     ~button_type:{:"submit":}
                                     {: [ <div class="ocsimore_button">c ] :}
                                     (*VVV How to avoid the <div> here??? *)
                                     :}] ] }}) ()
                        :} ] }}
             )
        );

      add_extension ~name:"logoutlink" ~wiki_content:true
        (fun bi args c ->
           Wikicreole.Link_plugin
             (let content = match c with
                | Some c -> Wiki_syntax.a_content_of_wiki
                    Wiki_syntax.wikicreole_parser bi c
                | None -> Lwt.return (Ocamlduce.Utf8.make "logout")
              in
              ((Eliom_duce.Xhtml.make_uri
                  ~service:sessman#act_logout_get
                  ~sp:bi.Wiki_widgets_interface.bi_sp
                  ()
               ),
               args,
               content)
             )
        );

end









(* private: *)
let valid_username usr =
  Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0

let valid_emailaddr email =
  Str.string_match
    (Str.regexp
       ("^[A-Za-z0-9\\._-]+@\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$"))
    email 0




let mail_password ~name ~password ~from_name ~from_addr ~subject =
  Lwt.catch
    (fun () ->
       Users.get_basicuser_by_login name
       >>= fun u ->
       User_sql.get_basicuser_data u
       >>= fun user ->
       Lwt_preemptive.detach
         (fun () ->
            match user.user_email with
              | Some email ->
                  ignore(Netaddress.parse email);
                  Netsendmail.sendmail
                    ~mailer:"/usr/sbin/sendmail"
                    (Netsendmail.compose
                       ~from_addr:(from_name, from_addr)
                       ~to_addrs:[(user.user_fullname, email)]
                       ~subject
                       ("This is an auto-generated message. "
                        ^ "Please do not reply to it.\n"
                        ^ "\n"
                        ^ "Your account is:\n"
                        ^ "\tUsername:\t" ^ name ^ "\n"
                        ^ "\tPassword:\t" ^ password ^ "\n"));
                  true
              | None -> false) ())
    (function _ -> Lwt.return false)


let generate_password () =
  let chars = "0123456789"^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"^
    "abcdefghijklmnopqrstuvwxyz" in
  let pwd = String.create 8 in
    for i = 0 to 7 do
      pwd.[i] <- String.get chars (Random.int (10+2*26))
    done;
    pwd


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
(*VVV URL??? Make it configurable (and all others!!) *)
      ~get_params:unit () in
  let srv_register_done =
    Eliom_services.new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
  and internal_srv_reminder =
    Eliom_services.new_service ?sp
      ~path:[Ocsimore_lib.ocsimore_admin_dir; "reminder"]
      ~get_params:unit ()
  and srv_reminder_done =
    Eliom_services.new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr") ()
(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
  and internal_srv_edit =
    Eliom_services.new_coservice
      ~fallback:internal_srv_register
      ~get_params:unit ()
  and srv_edit_done =
    Eliom_services.new_post_coservice
      ~https:(Session_manager.get_secure ())
      ~fallback:internal_srv_register
      ~post_params:(string "pwd" **
                      (string "pwd2" ** (string "descr" ** string "email"))) ()
*)
  in
object (self)

  inherit login_widget sessman

  method private display_login_box
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ~sp error usr pwd =
    let user_prompt = Ocamlduce.Utf8.make user_prompt in
    let pwd_prompt = Ocamlduce.Utf8.make pwd_prompt in
    let auth_error = Ocamlduce.Utf8.make auth_error in
(*VVV How to personalize every message??? or at least internationalize *)
    if (Eliom_sessions.get_ssl sp) || not (Session_manager.get_secure ())
    then begin
      {{ [<table>([
                  <tr>[<td>user_prompt
                       <td>[{: Eliom_duce.Xhtml.string_input
                                ~input_type:{:"text":} ~name:usr () :}]]
                  <tr>[<td>pwd_prompt
                       <td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"password":} ~name:pwd () :}]]
                  <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"submit":} ~value:"Login" () :}]]
                  <tr>[<td colspan="2">[
                          {: Eliom_duce.Xhtml.a self#srv_register
                             sp {{ "New user? Register now!" }} () :}]]] @
                  {: if error then
                     {{ [<tr>[<td colspan="2">auth_error]
                          <tr>[<td colspan="2">[
                                  {: Eliom_duce.Xhtml.a self#srv_reminder sp
                                     {{ "Forgot your password?" }} () :}]]] }}
                   else
                     {{ [] }} :})] }}
    end
    else
      let switchtohttps = Ocamlduce.Utf8.make switchtohttps in
      {{ [ <p> [ {: Eliom_duce.Xhtml.a
                      Eliom_services.https_void_coservice'
                      sp switchtohttps () :} ] ] }}

  method private display_logout_box ~sp:_sp u =
    {{ [<table>[
           <tr>[<td>{: Printf.sprintf "Hi %s!" u.user_fullname :}]
           <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                        ~input_type:{:"submit":} ~value:"logout" () :}]]
(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
           <tr>[<td>[{: Eliom_duce.Xhtml.a self#srv_edit
                        sp {{ "Manage your account" }} () :}]]
*)
         ]] }}



(*VVV Il faut revoir tout ce qui suit!!!!!!!!! *)


  method srv_register :
    (unit,
     unit,
     Eliom_services.get_service_kind,
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_srv_register

  method srv_reminder :
    (unit,
     unit,
     Eliom_services.get_service_kind,
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_srv_reminder

(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
  method srv_edit :
    (unit,
     unit,
     Eliom_services.get_service_kind,
     [`WithoutSuffix],
     unit,
     unit,
     [`Registrable]) Eliom_services.service
    = internal_srv_edit
*)

  method container
    ~sp:(_ : Eliom_sessions.server_params)
    ~(contents:Xhtmltypes_duce.blocks) : Xhtmltypes_duce.html Lwt.t =
    Lwt.return {{
              <html xmlns="http://www.w3.org/1999/xhtml">[
                <head>[<title>"Ocsimore default login widget"]
                <body>{: contents :}
              ]
            }}


  method private page_register err = fun sp () ()->
    self#container
      ~sp
      ~contents:
      {{ [<h1>"Registration form"
           <p>['Please fill in the following fields.'
               <br>[]
               'You can freely choose your login name: it will be \
               slightly modified automatically if it has already been chosen \
                 by another registered user.'
                 <br>[]
                 'Be very careful to enter a valid e-mail address, \
                   as the password for logging in will be sent there.']
           {: Eliom_duce.Xhtml.post_form
              ~service:srv_register_done
              ~sp
              (fun (usr,(desc,email)) ->
                 {{ [<table>[
                        <tr>[
                          <td>"login name: (letters & digits only)"
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"text":} ~name:usr () :}]
                        ]
                        <tr>[
                          <td>"real name:"
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"text":} ~name:desc () :}]
                        ]
                        <tr>[
                          <td>"e-mail address:"
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"text":} ~name:email () :}]
                        ]
                        <tr>[
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"submit":} ~value:"Register" () :}]
                        ]]] }})
              () :}
           <p>[<strong>{: err :}]]}}



  method private page_register_done = fun sp () (usr, (fullname, email)) ->
    if not (valid_username usr) then
      self#page_register "ERROR: Bad character(s) in login name!" sp () ()
    else if not (valid_emailaddr email) then
      self#page_register "ERROR: Bad formed e-mail address!" sp () ()
    else
      let pwd = generate_password () in
      Users.create_unique_user ~name:usr
        ~pwd:(User_sql.Types.Ocsimore_user_crypt pwd) ~fullname ~email ()
      >>= fun (user, n) ->
      Users.add_to_groups (basic_user user)
        basic_user_creation_options.new_user_groups >>= fun () ->
      mail_password
        ~name:n ~password:pwd
        ~from_name:basic_user_creation_options.mail_from
        ~from_addr:basic_user_creation_options.mail_addr
        ~subject:basic_user_creation_options.mail_subject
      >>= function
        | true ->
            self#container
              ~sp
              ~contents:
              {{ [<h1>"Registration ok."
                  <p>(['You\'ll soon receive an e-mail message at the \
                         following address:'
                        <br>[]] @
                        {: email :} @
                         [<br>[]
                             'reporting your login name and password.'])] }}

        | false ->
            User_sql.delete_user ~userid:user >>= fun () ->
            self#container ~sp
              ~contents:{{ [<h1>"Registration failed."
                            <p>"Please try later."] }}



  method private page_reminder err = fun sp () () ->
    self#container
      ~sp
      ~contents:
      {{ [<h1>"Password reminder"
           <p>['This service allows you to get an e-mail message \
           with your connection password.'
               <br>[]
               'The message will be sent to the address you \
                 entered when you registered your account.']
           {: Eliom_duce.Xhtml.post_form
              ~service:srv_reminder_done
              ~sp
              (fun usr ->
                 {{ [<table>[
                        <tr>[
                          <td>"Enter your login name:"
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"text":} ~name:usr () :}]
                          <td>[{: Eliom_duce.Xhtml.string_input ~input_type:{:"submit":} ~value:"Submit" () :}]
                        ]]]
                  }}) ()        :}
           <p>[<strong>{: err :}]] }}

  method private page_reminder_done = fun sp () _usr ->
    self#page_reminder "Users are being implemented (TODO)" sp () ()
      (* if not (valid_username usr) then
         self#page_reminder "ERROR: Bad character(s) in login name!" sp () ()
         else
         mail_password
         ~name:usr ~from_addr:sessionmanagerinfo.registration_mail_from
         ~subject:sessionmanagerinfo.registration_mail_subject >>= (fun b ->
         if b
         then
         self#container
         ~sp
         ~contents:{{ [<h1>"Password sent"
         <p>"You'll soon receive an e-mail message at \
         the address you entered when you \
         registered your account."] }}
         else
         self#container
         ~sp
         ~contents:{{ [<h1>"Failure"
         <p>"The username you entered doesn't exist, or \
         the service is unavailable at the moment."] }}) *)

(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
  method private page_edit err = fun sp () () ->
    Users.is_logged_on sp >>= fun logged ->
    if logged
    then
      Users.get_user_data sp >>= fun u ->
      self#container
        ~sp
        ~contents:
        {{ [<h1>"Your account"
             <p>"Change your persional information:"
             {: Eliom_duce.Xhtml.post_form
                ~service:srv_edit_done
                ~sp
                (fun (pwd, (pwd2, (desc, email))) ->
                   {{ [<table>[
                          <tr>[
                            <td>"login name: "
                            <td>[<strong>{: u.user_name :}]
                          ]
                          <tr>[
                            <td>"real name: "
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"text":}
                                    ~value:u.user_fullname
                                    ~name:desc () :}]
                          ]
                          <tr>[
                            <td>"e-mail address: "
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"text":}
                                    ~value:(match u.user_email with
                                              | None -> ""
                                              | Some e -> e)
                                    ~name:email () :}]
                          ]
                          <tr>[
                            <td colspan="2">"Enter a new password twice, or
                                                leave blank for no changes:"
                          ]
                          <tr>[
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"password":}
                                    ~value:""
                                    ~name:pwd () :}]
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"password":}
                                    ~value:""
                                    ~name:pwd2 () :}]
                          ]
                          <tr>[
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{: "submit" :}
                                    ~value:"Confirm" () :}]
                          ]
                        ]]
                    }}) () :}
             <p>[<strong>{: err :}]] }}
    else failwith "VVV: SHOULD NOT OCCUR (not implemented)"

  method private page_edit_done = fun sp () (pwd,(pwd2,(fullname,email)))->
    Users.is_logged_on sp >>= fun logged ->
    if logged
    then
      Users.get_user_data sp >>= fun user ->
      if not (valid_emailaddr email) then
        self#page_edit "ERROR: Bad formed e-mail address!" sp () ()
      else if pwd <> pwd2 then
        self#page_edit "ERROR: Passwords don't match!" sp () ()
      else (
        Ocsigen_messages.debug2 (Printf.sprintf "Updating infos for '%s'"
                                   fullname);
        let email = Some email in
        try
          let pwd = match user.user_pwd with
            | User_sql.Connect_forbidden (* Should never happen, the user cannot
                                            be logged *) -> None

            | User_sql.External_Auth (* We cannot change this password,
                                        we should not leave the possibility
                                        to the user  *) ->
                failwith "ERROR: Cannot change NIS or PAM passwords \
                    from Ocsimore!"
            | User_sql.Ocsimore_user_plain _ ->
                if pwd = "" then None else Some (User_sql.Ocsimore_user_plain pwd)
            | User_sql.Ocsimore_user_crypt _ ->
                if pwd = "" then None else Some (User_sql.Ocsimore_user_crypt pwd)
          in
          ignore (Users.update_user_data ~user ~fullname ~email ?pwd ());
          Users.set_session_data sp user >>= fun () ->
            self#container
              ~sp
              ~contents:{{ [<h1>"Personal information updated"] }}

        with Failure s -> self#page_edit s sp () ()
      )
    else failwith "VVV: SHOULD NOT OCCUR (not implemented)"
(*VVV: Must be accessible only to logged users *)
*)

  initializer
    begin
      Eliom_duce.Xhtml.register ?sp
        ~service:internal_srv_register (self#page_register "");
(*VVV For all post services:
  Use redirections instead of pages to prevent re-posting data? *)
      Eliom_duce.Xhtml.register ?sp
        ~service:srv_register_done self#page_register_done;
      Eliom_duce.Xhtml.register ?sp
        ~service:internal_srv_reminder (self#page_reminder "");
      Eliom_duce.Xhtml.register ?sp
        ~service:srv_reminder_done self#page_reminder_done;
(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
      Eliom_duce.Xhtml.register ?sp
        ~service:internal_srv_edit (self#page_edit "");
      Eliom_duce.Xhtml.register ?sp
        ~service:srv_edit_done self#page_edit_done;
*)
    end


end

