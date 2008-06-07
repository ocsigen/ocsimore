let (>>=) = Lwt.bind


class login_widget ~(sessman: Session_manager.sessionmanager) =
object (self)
    
  val xhtml_class = "logbox"

  method private display_login_box sp error usr pwd =
    {{ [<table>([
                  <tr>[<td>"Username:" 
                       <td>[{: Eliom_duce.Xhtml.string_input
                                ~input_type:{:"text":} ~name:usr () :}]]
                  <tr>[<td>"Password:" 
                       <td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"password":} ~name:pwd () :}]]
                  <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                               ~input_type:{:"submit":} ~value:"Login" () :}]]
                  <tr>[<td colspan="2">[
                          {: Eliom_duce.Xhtml.a sessman#srv_register
                             sp {{ "New user? Register now!" }} () :}]]] @
                  {: if error then
                     {{ [<tr>[<td colspan="2">"Wrong login or password"]
                          <tr>[<td colspan="2">[
                                  {: Eliom_duce.Xhtml.a sessman#srv_reminder sp
                                     {{ "Forgot your password?" }} () :}]]] }}
                   else
                     {{ [] }} :})] }}
      
  method private display_logout_box ~sp u =
    {{ [<table>[
           <tr>[<td>{: Printf.sprintf "Hi %s!" u.Users.fullname :}]
           <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                        ~input_type:{:"submit":} ~value:"logout" () :}]]
           <tr>[<td>[{: Eliom_duce.Xhtml.a sessman#srv_edit
                        sp {{ "Manage your account" }} () :}]]
         ]] }}
        
  method display_login_widget ~sp ~sd =
    Users.get_user_data sp sd >>= fun u ->
    Users.is_logged_on sp sd >>= fun logged ->
    Lwt.return 
      {{ <div class={: xhtml_class :}>
           [{:
               if logged
               then
                 Eliom_duce.Xhtml.post_form
                   ~a:{{ { class="logbox logged"} }} 
                   ~service:sessman#act_logout ~sp
                   (fun _ -> self#display_logout_box sp u) ()
               else
                 let exn = Eliom_sessions.get_exn sp in
                   if List.mem Users.BadPassword exn || 
                     (List.exists 
                        (function Users.NoSuchUser _ -> true | _ -> false) exn)
                   then (* unsuccessful attempt *)
                     Eliom_duce.Xhtml.post_form
                       ~a:{{ {class="logbox error"} }}
                       ~service:sessman#act_login ~sp:sp 
                       (fun (usr, pwd) ->
                          (self#display_login_box sp true usr pwd)) ()
                   else (* no login attempt yet *)
                     Eliom_duce.Xhtml.post_form
                       ~a:{{ {class="logbox notlogged"} }}
                       ~service:sessman#act_login ~sp:sp
                       (fun (usr, pwd) ->
                          (self#display_login_box sp false usr pwd)) () 
                       :}] 
       }}


  initializer

      Wiki_syntax.add_block_extension "loginbox"
        (fun _ (sp, sd, subbox) args c -> 
           self#display_login_widget ~sp ~sd >>= fun b ->
           Lwt.return {{ [ b ] }});

      ignore 
        (Eliom_duce.Xhtml.register_new_service
           ~path:["ocsimore";"login"]
           ~get_params:Eliom_parameters.unit
           (fun sp () () -> 
              let sd = Ocsimore_common.get_sd sp in
              self#display_login_widget ~sp ~sd >>= fun lb ->
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
            Lwt.return (Ocamlduce.Utf8.make content))
        );




end

