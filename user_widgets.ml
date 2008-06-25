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

let (>>=) = Lwt.bind


class login_widget ~(sessman: Session_manager.sessionmanager) =
object (self)
    
  val xhtml_class = "logbox"

  method private display_login_box 
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ~sp error usr pwd =
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
      
  method private display_logout_box ~sp u =
    {{ [<table>[
           <tr>[<td>{: Printf.sprintf "Hi %s!" u.Users.fullname :}]
           <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                        ~input_type:{:"submit":} ~value:"logout" () :}]]
         ]] }}
        
  method display_login_widget ?user_prompt ?pwd_prompt ?auth_error ~sp ~sd () =
    Users.get_user_data sp sd >>= fun u ->
    Users.is_logged_on sp sd >>= fun logged ->
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
                 let exn = Eliom_sessions.get_exn sp in
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
                             ~sp true usr pwd)) ()
                 else (* no login attempt yet *)
                     Eliom_duce.Xhtml.post_form
                       ~a:{{ {class="logbox notlogged"} }}
                       ~service:sessman#act_login
                       ~sp
                       (fun (usr, pwd) ->
                          (self#display_login_box
                             ?user_prompt ?pwd_prompt ?auth_error
                             ~sp false usr pwd)) () 
                       :}] 
       }}


  initializer

      Wiki_syntax.add_block_extension "loginbox"
        (fun _ (sp, sd, subbox) args c -> 
           let user_prompt = Ocsimore_lib.list_assoc_opt "user_prompt" args in
           let pwd_prompt = Ocsimore_lib.list_assoc_opt "pwd_prompt" args in
           let auth_error = Ocsimore_lib.list_assoc_opt "auth_error" args in
           self#display_login_widget
             ?user_prompt ?pwd_prompt ?auth_error 
             ~sp ~sd () >>= fun b ->
           Lwt.return {{ [ b ] }});

      ignore 
        (Eliom_duce.Xhtml.register_new_service
           ~https:(Session_manager.get_secure ())
           ~path:["ocsimore";"login"]
           ~get_params:Eliom_parameters.unit
           (fun sp () () -> 
              let sd = Ocsimore_common.get_sd sp in
              self#display_login_widget ~sp ~sd () >>= fun lb ->
              Lwt.return
                {{
                   <html>[
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


      Wiki_syntax.add_a_content_extension "username"
        (fun w (sp, sd, (subbox, ancestors)) args c -> 
           Users.get_user_data ~sp ~sd >>= fun ud ->
             Lwt.return (Ocamlduce.Utf8.make ud.Users.fullname)
        );
      
      Wiki_syntax.add_block_extension "logoutbutton"
        (fun w (sp, sd, (subbox, ancestors)) args c -> 
           let content = match c with
             | Some c -> c
             | None -> "logout"
           in
           Wiki_syntax.xml_of_wiki
             ?subbox ~ancestors ~sp ~sd w content >>= fun c ->
           Lwt.return
             {{ [ {:
                     Eliom_duce.Xhtml.post_form
                     ~a:{{ { class="logoutbutton"} }} 
                     ~service:sessman#act_logout ~sp
                     (fun () -> 
                        {{ [<p>[ 
                               {: Eliom_duce.Xhtml.button
                                  ~button_type:{:"submit":}
                                  {: [ <div class="ocsimore_button">c ] :}
(*VVV How to avoid the <div> here??? *)
                                    :}] ] }}) ()
                     :} ] }}
                );

      Wiki_filter.add_preparser_extension "logoutbutton"
(*VVV may be done automatically for all extensions with wiki content 
  (with an optional parameter of add_*_extension?) *)
        (fun w param args -> function
           | None -> Lwt.return None
           | Some c ->
               Wiki_filter.preparse_extension param w c >>= fun c ->
                 Lwt.return (Some (Wiki_syntax.string_of_extension
                                     "logoutbutton" args (Some c)))
        )
      ;

      Wiki_syntax.add_link_extension "logoutlink"
        (fun w (sp, sd, (subbox, ancestors)) args c -> 
           let content = match c with
             | Some c -> c
             | None -> "logout"
           in
           ((Eliom_duce.Xhtml.make_uri
               ~service:sessman#act_logout_get ~sp
               ()
            ),
            args,
            Lwt.return (Ocamlduce.Utf8.make content))
        )




end









(* private: *)
let valid_username usr =
  Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0
    
let valid_emailaddr email =
  Str.string_match 
    (Str.regexp
       ("^[A-Za-z0-9\\._-]+@\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$")) 
    email 0




let mail_password ~name ~from_addr ~subject =
  Lwt.catch
    (fun () -> 
       Users.get_user_by_name ~name >>= fun user -> 
       Lwt_preemptive.detach
         (fun () -> 
            match user.Users.email with
              | Some email ->
                  ignore(Netaddress.parse email);
                  Netsendmail.sendmail
                    ~mailer:"/usr/sbin/sendmail"
                    (Netsendmail.compose
                       ~from_addr
                       ~to_addrs:[(user.Users.fullname, email)]
                       ~subject
                       ("This is an auto-generated message. "
                        ^ "Please do not reply to it.\n"
                        ^ "\n"
                        ^ "Your account is:\n"
                        ^ "\tUsername:\t" ^ name ^ "\n"
                        ^ "\tPassword:\t" ^ (match user.Users.pwd with 
                                               | User_sql.Ocsimore_user p -> p
                                               | User_sql.Pam -> "(PAM password)"
                                               | User_sql.Connect_forbidden -> "(connection forbidden)")
                        ^ "\n"));
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
    User_sql.Ocsimore_user pwd



class login_widget_basic_user_creation
  ~(sessman: Session_manager.sessionmanager) 
  (registration_mail_from, registration_mail_subject, default_groups)
  =
  let internal_srv_register = 
    Eliom_services.new_service
      ~path:(["ocsimore";"register"]) 
(*VVV URL??? Make it configurable (and all others!!) *)
      ~get_params:unit () in
  let srv_register_done = 
    Eliom_services.new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
  and internal_srv_reminder = 
    Eliom_services.new_service
      ~path:["ocsimore";"reminder"]
      ~get_params:unit ()
  and srv_reminder_done = 
    Eliom_services.new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr") ()
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
  in
object (self)

  inherit login_widget sessman

  method private display_login_box
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ~sp error usr pwd =
    let user_prompt = Ocamlduce.Utf8.make user_prompt in
    let pwd_prompt = Ocamlduce.Utf8.make pwd_prompt in
    let auth_error = Ocamlduce.Utf8.make auth_error in
(*VVV How to personalize every message??? or at least internationalize *)
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
      
  method private display_logout_box ~sp u =
    {{ [<table>[
           <tr>[<td>{: Printf.sprintf "Hi %s!" u.Users.fullname :}]
           <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                        ~input_type:{:"submit":} ~value:"logout" () :}]]
           <tr>[<td>[{: Eliom_duce.Xhtml.a self#srv_edit
                        sp {{ "Manage your account" }} () :}]]
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
    
  method srv_edit : 
    (unit, 
     unit, 
     Eliom_services.get_service_kind,
     [`WithoutSuffix], 
     unit, 
     unit, 
     [`Registrable]) Eliom_services.service 
    = internal_srv_edit
    
  method container
    ~(sp:Eliom_sessions.server_params)
    ~(sd:Ocsimore_common.session_data)
    ~(contents:Xhtmltypes_duce.blocks) : Xhtmltypes_duce.html Lwt.t =
    Lwt.return {{ 
              <html>[
                <head>[<title>"Ocsimore default login widget"]
                <body>{: contents :}
              ]
            }}
      

  method private page_register err = fun sp () ()-> 
    self#container
      ~sp
      ~sd:Users.anonymous_sd
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
      Users.create_unique_user
        ~name:usr ~pwd ~fullname ~email:(Some email)
        ~groups:default_groups >>= fun (user, n) ->
      mail_password
        ~name:n
        ~from_addr:registration_mail_from 
        ~subject:registration_mail_subject >>= fun b ->
      if b
      then begin
        self#container
          ~sp
          ~sd:Users.anonymous_sd
          ~contents:
          {{ [<h1>"Registration ok."
               <p>(['You\'ll soon receive an e-mail message at the \
                      following address:'
                    <br>[]] @
                     {: email :} @
                      [<br>[]
                          'reporting your login name and password.'])] }}
        
      end
      else 
        Users.delete_user ~userid:user.Users.id >>= fun () ->
        self#container
          ~sp
          ~sd:Users.anonymous_sd
          ~contents:{{ [<h1>"Registration failed."
                         <p>"Please try later."] }}


              
  method private page_reminder err = fun sp () () -> 
    self#container
      ~sp
      ~sd:Users.anonymous_sd
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
      
  method private page_reminder_done = fun sp () usr ->
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
         ~sd:Users.anonymous_sd
         ~contents:{{ [<h1>"Password sent"
         <p>"You'll soon receive an e-mail message at \
         the address you entered when you \
         registered your account."] }}
         else 
         self#container
         ~sp
         ~sd:Users.anonymous_sd
         ~contents:{{ [<h1>"Failure"
         <p>"The username you entered doesn't exist, or \
         the service is unavailable at the moment."] }}) *)
      
  method private page_edit err = fun sp () () ->
    let sd = Ocsimore_common.get_sd sp in
    Users.is_logged_on sp sd >>= fun logged ->
    if logged
    then
      Users.get_user_data sp sd >>= fun u ->
      self#container
        ~sp
        ~sd:Users.anonymous_sd
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
                            <td>[<strong>{: u.Users.name :}]
                          ]
                          <tr>[
                            <td>"real name: "
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"text":} 
                                    ~value:u.Users.fullname
                                    ~name:desc () :}]
                          ]
                          <tr>[
                            <td>"e-mail address: "
                            <td>[{: Eliom_duce.Xhtml.string_input
                                    ~input_type:{:"text":} 
                                    ~value:(match u.Users.email with
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
    let sd = Ocsimore_common.get_sd sp in
    Users.is_logged_on sp sd >>= fun logged ->
    if logged
    then
      Users.get_user_data sp sd >>= fun user ->
      if not (valid_emailaddr email) then  
        self#page_edit "ERROR: Bad formed e-mail address!" sp () ()
      else if pwd <> pwd2 then
        self#page_edit "ERROR: Passwords don't match!" sp () ()
      else
        (Ocsigen_messages.debug2 (Printf.sprintf "fullname: %s" fullname);
         let email = Some email in
         ignore (if pwd = ""
                 then Users.update_user_data ~user ~fullname ~email ()
                 else Users.update_user_data ~user ~fullname ~email
                   ~pwd:(User_sql.Ocsimore_user pwd) ());
         Users.set_session_data sp sd user >>= fun () ->
         self#container
           ~sp
           ~sd (*VVV was: Users.anonymous_sd, but why? *)
           ~contents:{{ [<h1>"Personal information updated"] }})
    else failwith "VVV: SHOULD NOT OCCUR (not implemented)"
(*VVV: Must be accessible only to logged users *)
      
  initializer
    begin
      Eliom_duce.Xhtml.register internal_srv_register (self#page_register "");
      Eliom_duce.Xhtml.register srv_register_done self#page_register_done;
      Eliom_duce.Xhtml.register internal_srv_reminder (self#page_reminder "");
      Eliom_duce.Xhtml.register srv_reminder_done self#page_reminder_done;
      Eliom_duce.Xhtml.register internal_srv_edit (self#page_edit "");
      Eliom_duce.Xhtml.register srv_edit_done self#page_edit_done;
    end


end

