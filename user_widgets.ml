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
                     List.mem Users.NoSuchUser exn
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
           Lwt.return {{ [ b ] }})


end

