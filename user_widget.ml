let (>>=) = Lwt.bind


class login_widget ~(parent: SessionManager.sessionmanager) =
object (self)
  inherit [unit] Widget.parametrized_widget parent
    
  val div_class = "logbox"
    
  method private login_box sp error usr pwd =
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
                          {: Eliom_duce.Xhtml.a parent#srv_register
                             sp {{ "New user? Register now!" }} () :}]]] @
                  {: if error then
                     {{ [<tr>[<td colspan="2">"Wrong login or password"]
                          <tr>[<td colspan="2">[
                                  {: Eliom_duce.Xhtml.a parent#srv_reminder sp
                                     {{ "Forgot your password?" }} () :}]]] }}
                   else
                     {{ [] }} :})] }}
      
  method private logout_box sp user =
    let (_,usr,_,descr,email) = parent#get_user_data in
      {{ [<table>[
             <tr>[<td>{: Printf.sprintf "Hi %s!" descr :}]
             <tr>[<td>[{: Eliom_duce.Xhtml.string_input
                          ~input_type:{:"submit":} ~value:"logout" () :}]]
             <tr>[<td>[{: Eliom_duce.Xhtml.a parent#srv_edit
                          sp {{ "Manage your account" }} () :}]]
           ]] }}
        
  method apply ~sp () =
    Ocsigen_messages.debug2 "[forumWidget] login#apply";
    Eliom_sessions.get_persistent_session_data
      SessionManager.user_table sp () >>= fun sess ->
    Lwt.return {{ <div class={: div_class :}>
                [{:
		    match sess with
		      | Eliom_sessions.Data user ->
			  Eliom_duce.Xhtml.post_form
                            ~a:{{ { class="logbox logged"} }} 
                            ~service:parent#act_logout ~sp
			    (fun _ -> self#logout_box sp user) ()
		      | _ ->  let exn = Eliom_sessions.get_exn sp in
                          if List.mem Users.BadPassword exn || 
                            List.mem Users.NoSuchUser exn
                          then (* unsuccessful attempt *)
                            Eliom_duce.Xhtml.post_form
                              ~a:{{ {class="logbox error"} }}
                              ~service:parent#act_login ~sp:sp 
                              (fun (usr, pwd) ->
                                 (self#login_box sp true usr pwd)) ()
                          else (* no login attempt yet *)
                            Eliom_duce.Xhtml.post_form
                              ~a:{{ {class="logbox notlogged"} }}
                              ~service:parent#act_login ~sp:sp
                              (fun (usr, pwd) ->
                                 (self#login_box sp false usr pwd)) () 
                              :}] 
                }}
      
end

