(* open XHTML.M *)
open Eliom
(* open Eliom.Xhtml *)
open Eliomduce.Xhtml
open Lwt
open Users

let user_table : Users.user persistent_table = 
  create_persistent_table "ocsimore_user_table_v1"

type sessionmanager_in = 
    { url: string list;
      default_groups: Users.user list;
      login_actions: Eliom.server_params -> Users.user option -> unit Lwt.t;
      logout_actions: Eliom.server_params -> unit Lwt.t;
      registration_mail_from: string * string;
      registration_mail_subject: string;
  }

class sessionmanager 
    ~(sessionmanagerinfo: sessionmanager_in) =

  let act_login = new_post_coservice'
    ~post_params:(string "usr" ** string "pwd")
    ()
  in

  let act_logout = new_post_coservice'
    ~post_params:unit
    ()
  in

  let srv_register = new_service 
    ~url:(sessionmanagerinfo.url @ ["register"])
    ~get_params:unit
    ()
  in

  let srv_register_done = new_post_coservice
    ~fallback:srv_register
    ~post_params:(string "usr" ** (string "descr" ** string "email"))
    ()
  in

  let srv_reminder = new_service
    ~url:(sessionmanagerinfo.url @ ["reminder"])
    ~get_params:unit
    ()
  in

  let srv_reminder_done = new_post_coservice
    ~fallback:srv_register
    ~post_params:(string "usr")
    ()
  in

  let srv_edit = new_coservice
      ~fallback:srv_register
      ~get_params:unit
      ()
  in

  let srv_edit_done = new_post_coservice
      ~fallback:srv_register
      ~post_params:(string "pwd" ** (string "pwd2" ** (string "descr" ** 
						         string "email")))
      ()
  in


  object (me)

	method container (sp: Eliom.server_params) (user: Users.user option) ~title:(t: string) (contents: {{ Xhtml1_strict.blocks }}): {{ Xhtml1_strict.html }} Lwt.t =
	return {{ 
		<html>[
			<head>[<title>{: t :}]
			<body>{: contents :}
		]
	}}

  method private valid_username usr =
    Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0


  method private valid_emailaddr email =
    Str.string_match 
      (Str.regexp ("^[A-Za-z0-9\\._-]+@" 
		   ^ "\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$")) 
      email 0


  method private login_box sp error usr pwd = 
		{{ [<table>([
				<tr>[<td>"Username:" <td>[{: string_input ~input_type:{:"text":} ~name:usr () :}]]
				<tr>[<td>"Password:" <td>[{: string_input ~input_type:{:"password":} ~name:pwd () :}]]
				<tr>[<td>[{: string_input ~input_type:{:"submit":} ~value:"Login" () :}]]
				<tr>[<td colspan="2">[{: a srv_register sp {{ "New user? Register now!" }} () :}]]] @
				{: if error then
					{{ [<tr>[<td colspan="2">"Wrong login or password"]
					<tr>[<td colspan="2">[{: a srv_reminder sp {{ "Forgot your password?" }} () :}]]] }}
					else
				 {{	[] }} :})] }}

  method private logout_box sp user =
    let (usr,pwd,descr,email) = get_user_data user in
		{{ [<table>[
				<tr>[<td>{: Format.sprintf "Hi %s!" descr :}]
				<tr>[<td>[{: string_input ~input_type:{:"submit":} ~value:"logout" () :}]]
				<tr>[<td>[{: a srv_edit sp {{ "Manage your account" }} () :}]]]] }}
      
  method private page_register err = fun sp () ()-> 
    me#container
      sp
      None
      ~title:("Registration")
			{{ [<h1>"Registration form"
					<p>['Please fill in the following fields.'
					<br>[]
					'You can freely choose your login name: it will be \
					slightly modified automatically if it has already been chosen \
					by another registered user.'
					<br>[]
					'Be very careful to enter a valid e-mail address, \
					as the password for logging in will be sent there.']
					{: post_form srv_register_done sp
          (fun (usr,(desc,email)) -> 
						{{ [<table>[
								<tr>[
									<td>"login name: (letters & digits only)"
									<td>[{: string_input ~input_type:{:"text":} ~name:usr () :}]
								]
								<tr>[
									<td>"real name:"
									<td>[{: string_input ~input_type:{:"text":} ~name:desc () :}]
								]
								<tr>[
									<td>"e-mail address:"
									<td>[{: string_input ~input_type:{:"text":} ~name:email () :}]
								]
								<tr>[
									<td>[{: string_input ~input_type:{:"submit":} ~value:"Register" () :}]
								]]] }})
              () :}
					<p>[<strong>{: err :}]]}} 


  method private page_register_done = fun sp () (usr,(desc,email))-> 
    if not (me#valid_username usr) then 
      me#page_register "ERROR: Bad character(s) in login name!" sp () ()
    else if not (me#valid_emailaddr email) then 
      me#page_register "ERROR: Bad formed e-mail address!" sp () ()
    else 
      let pwd = generate_password() in
        create_unique_user ~name:usr ~pwd ~desc ~email >>= fun (user,n) ->
	  mail_password 
	    ~name:n ~from_addr:sessionmanagerinfo.registration_mail_from 
	    ~subject:sessionmanagerinfo.registration_mail_subject >>=
          (fun b ->
            if b
	    then begin
	      List.fold_left
	        (fun thr g -> thr >>= (fun () -> add_group ~user ~group:g))
                (return ())
	        sessionmanagerinfo.default_groups >>= (fun () ->
                  me#container
                    sp
                    ~title:"Registration"
                    None
										{{ [<h1>"Registration ok."
											<p>(['You\'ll soon receive an e-mail message at the \
											following address:'
											<br>[]] @
											{: email :} @
											[<br>[]
											'reporting your login name and password.'])] }}
	           )
            end
	    else 
	      delete_user ~user >>= (fun () ->
                me#container
                  sp
                  ~title:"Registration"
                  None
									{{ [<h1>"Registration failed."
										<p>"Please try later."] }}
	         )
          )
                

  method private page_reminder err = fun sp () () -> 
    me#container
      sp
      ~title:"Password reminder"
      None
			{{ [<h1>"Password reminder"
			<p>['This service allows you to get an e-mail message \
				with your connection password.'
				<br>[]
				'The message will be sent to the address you \
				entered when you registered your account.']
       {: post_form srv_reminder_done sp
         (fun usr -> 
				 	{{ [<table>[
						<tr>[
							<td>"Enter your login name:"
							<td>[{: string_input ~input_type:{:"text":} ~name:usr () :}]
							<td>[{: string_input ~input_type:{:"submit":} ~value:"Submit" () :}]
					]]]
					}}) ()	:}
			 <p>[<strong>{: err :}]] }}


  method private page_reminder_done = fun sp () usr ->
    if not (me#valid_username usr) then
      me#page_reminder "ERROR: Bad character(s) in login name!" sp () ()
    else 
      mail_password 
	~name:usr ~from_addr:sessionmanagerinfo.registration_mail_from 
	~subject:sessionmanagerinfo.registration_mail_subject >>= (fun b ->
          if b
          then 
            me#container
              sp
              ~title:"Password reminder"
              None
							{{ [<h1>"Password sent"
								<p>"You'll soon receive an e-mail message at \
								the address you entered when you \
								registered your account."] }}
          else 
            me#container
              sp
              ~title:"Password reminder"
              None
							{{ [<h1>"Failure"
							<p>"The username you entered doesn't exist, or \
							the service is unavailable at the moment."] }})


  method private page_edit user err = fun sp () () ->
    let (n,_,d,e) = get_user_data ~user in
      me#container
        sp
        ~title:"Your account"
        None
				{{ [<h1>"Your account"
				<p>"Change your persional information:"
				{: post_form srv_edit_done sp
	   (fun (pwd,(pwd2,(desc,email))) -> 
		  {{ [<table>[
					<tr>[
						<td>"login name: "
						<td>[<strong>{: n :}]
					]
					<tr>[
						<td>"real name: "
						<td>[{: string_input ~input_type:{:"text":} ~value:d ~name:desc () :}]
					]
					<tr>[
						<td>"e-mail address: "
						<td>[{: string_input ~input_type:{:"text":} ~value:e ~name:email () :}]
					]
					<tr>[
						<td colspan="2">"Enter a new password twice, or 
						leave blank for no changes:"
					]
					<tr>[
						<td>[{: string_input ~input_type:{:"password":} ~value:"" ~name:pwd () :}]
						<td>[{: string_input ~input_type:{:"password":} ~value:"" ~name:pwd2 () :}]
					]
					<tr>[
						<td>[{: string_input ~input_type:{: "submit" :} ~value:"Confirm" () :}]
					]
				]]
			}}) () :} 
		  <p>[<strong>{: err :}]] }}


  method private page_edit_done user = fun sp () (pwd,(pwd2,(desc,email)))->
    if not (me#valid_emailaddr email) then 
      me#page_edit user "ERROR: Bad formed e-mail address!" sp () ()
    else if pwd <> pwd2 then
      me#page_edit user "ERROR: Passwords don't match!" sp () ()
    else
      (ignore (if pwd = ""
      then update_user_data ~user ~desc ~email ()
      else update_user_data ~user ~desc ~email ~pwd:(Some pwd) ());
       me#container
         sp
         ~title:"Your account"
         None
				 {{ [<h1>"Personal information updated"] }})


  val mutable all_login_actions = sessionmanagerinfo.login_actions
  val mutable all_logout_actions = sessionmanagerinfo.logout_actions

  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>=
    (fun () -> close_session sp >>= 
      (fun () -> 
        catch
          (fun () ->
            let user = authenticate usr pwd in
            set_persistent_data user_table sp user >>=
            (fun () ->
              all_login_actions sp (Some user) >>=
              (fun () ->
                return (
                register_for_session sp srv_edit (me#page_edit user "");
                register_for_session sp srv_edit_done
                  (me#page_edit_done user);
                [])
              )))
          (fun e -> return [e])))
      
  method add_login_actions f =
    let old_la = all_login_actions in
    all_login_actions <- 
    fun sp u -> 
      old_la sp u >>=
      (fun () -> f sp u)
	
  method private mk_act_logout sp () () = 
    all_logout_actions sp >>=
    (fun () -> close_session sp >>= (fun () -> return []))

  method add_logout_actions f =
    let old_la = all_logout_actions in
    all_logout_actions <- 
    fun sp -> 
      old_la sp >>=
      (fun () -> f sp)
	

    method mk_log_form : Eliom.server_params -> Users.user option -> 
      {{ Xhtml1_strict.form }}
        = fun sp sess -> match sess with
          | Some user -> (* user is logged in *)
              post_form ~a:{{ {class="logbox logged"} }} 
	        act_logout sp (fun _ -> me#logout_box sp user) ()
          | _ ->
              let exn = get_exn sp in
              if List.mem BadPassword exn || List.mem NoSuchUser exn
              then (* unsuccessful attempt *)
	        post_form ~a:{{ {class="logbox error"} }} 
	          act_login sp (fun (usr, pwd) -> 
                    (me#login_box sp true usr pwd)) ()
              else (* no login attempt yet *)
	        post_form ~a:{{ {class="logbox notlogged"} }}
	          act_login sp (fun (usr, pwd) -> 
                    (me#login_box sp false usr pwd)) ()

    method lwtinit = return ()

		method register: unit =
		begin
      Actions.register act_login me#mk_act_login;
      Actions.register act_logout me#mk_act_logout;
      register srv_register (me#page_register "");
      register srv_register_done me#page_register_done;
      register srv_reminder (me#page_reminder "");
      register srv_reminder_done me#page_reminder_done
		end

  end
