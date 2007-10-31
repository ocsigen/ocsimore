open Eliommod
open Eliomparameters
open Eliomservices
open Eliomsessions
open Eliompredefmod
open Eliomduce.Xhtml
open Lwt
open Users

let user_table: Users.user persistent_table = 
  create_persistent_table "ocsimore_user_table_v1"

type sessionmanager_in = 
{
	url: string list;
	default_groups: Users.user list;
	login_actions: server_params -> Users.user session_data -> unit Lwt.t;
	logout_actions: server_params -> unit Lwt.t;
	registration_mail_from: string * string;
	registration_mail_subject: string;
	administrator: Users.user;
}

class sessionmanager ~(db: Sql.db_t) ~(sessionmanagerinfo: sessionmanager_in) =

let act_login = new_post_coservice' ~post_params:(string "usr" ** string "pwd") () 
and act_logout = new_post_coservice' ~post_params:unit ()
and srv_register = new_service ~path:(sessionmanagerinfo.url @ ["register"]) ~get_params:unit () in
let srv_register_done = new_post_coservice ~fallback:srv_register ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
and srv_reminder = new_service ~path:(sessionmanagerinfo.url @ ["reminder"]) ~get_params:unit ()
and srv_reminder_done = new_post_coservice ~fallback:srv_register ~post_params:(string "usr") ()
and srv_edit = new_coservice ~fallback:srv_register ~get_params:unit ()
and srv_edit_done = new_post_coservice ~fallback:srv_register ~post_params:(string "pwd" ** (string "pwd2" ** (string "descr" ** string "email"))) () 
and srv_create_service = new_service ~path:(sessionmanagerinfo.url @ ["create_service"]) ~get_params:unit () in
let srv_create_service_done = new_post_coservice ~fallback:srv_create_service ~post_params:(string "url") () in
let srv_modify_service = new_service ~path:(sessionmanagerinfo.url @ ["modify_service"]) ~get_params:(string "url") () in
let srv_modify_service_done = new_post_coservice ~fallback:srv_modify_service ~post_params:unit () in
let srv_list_services = new_service ~path:(sessionmanagerinfo.url @ ["list_services"]) ~get_params:unit () in

let act_add_parameter = new_post_coservice' ~post_params:(string "service_name" ** string "param_name") () in
let act_add_widget = new_post_coservice' ~post_params:(string "name") () in

object (self)

	val mutable current_user = No_data
	val forums = Hashtbl.create 1
	val widget_types = Hashtbl.create 1

	method db = db

	method set_user u = current_user <- u

	method container ~(sp: server_params) ~(sess: Users.user session_data) ~(contents:Xhtml1_strict.blocks): Xhtml1_strict.html Lwt.t =
	return {{ 
		<html>[
			<head>[<title>"Temporary title"]
			<body>{: contents :}
		]
	}}

	method get_forum (forum_id: int) =
		Hashtbl.find forums forum_id

	method add_forum (f: Forum.forum) =
		Hashtbl.add forums (f#get_forum_id) f

	method is_logged_on =
	match current_user with
	| Data _ -> true
	| _ -> false

	method get_user_name =
	match current_user with
	| Data u -> let (n, _, _, _) = Users.get_user_data ~user:u in n
	| _ -> "Anonymous"

	method get_role (forum_id: int) =
	let f = self#get_forum forum_id in
		if f#can_moderate current_user then Sql.Moderator
		else if f#can_write current_user && self#is_logged_on then Sql.Author self#get_user_name
		else Sql.Unknown

	method private valid_username usr =
	Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0

	method private valid_emailaddr email =
  Str.string_match 
		(Str.regexp ("^[A-Za-z0-9\\._-]+@\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$")) 
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
			<tr>[<td>{: Printf.sprintf "Hi %s!" descr :}]
			<tr>[<td>[{: string_input ~input_type:{:"submit":} ~value:"logout" () :}]]
			<tr>[<td>[{: a srv_edit sp {{ "Manage your account" }} () :}]]
			!{: if Users.in_group user sessionmanagerinfo.administrator then
						{{ [
							<tr>[<td>[{: a srv_create_service sp {{ "Create a new service" }} () :}]]
							<tr>[<td>[{: a srv_list_services sp {{ "List services" }} () :}]]
						] }}
					else
						{{ [] }} :}
	]] }}
      
	method private page_register err = fun sp () ()-> 
   self#container
     ~sp
     ~sess:No_data
		~contents:{{ [<h1>"Registration form"
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
    if not (self#valid_username usr) then 
      self#page_register "ERROR: Bad character(s) in login name!" sp () ()
    else if not (self#valid_emailaddr email) then 
      self#page_register "ERROR: Bad formed e-mail address!" sp () ()
    else 
      let pwd = generate_password() in
        create_unique_user db ~name:usr ~pwd ~desc ~email >>= fun (user,n) ->
	  mail_password 
	    ~name:n ~from_addr:sessionmanagerinfo.registration_mail_from 
	    ~subject:sessionmanagerinfo.registration_mail_subject >>=
          (fun b ->
            if b
	    then begin
	      List.fold_left
	        (fun thr g -> thr >>= (fun () -> add_group db ~user ~group:g))
                (return ())
	        sessionmanagerinfo.default_groups >>= (fun () ->
                  self#container
                    ~sp
										~sess:No_data
										~contents:{{ [<h1>"Registration ok."
											<p>(['You\'ll soon receive an e-mail message at the \
											following address:'
											<br>[]] @
											{: email :} @
											[<br>[]
											'reporting your login name and password.'])] }}
	           )
            end
	    else 
	      delete_user db ~user >>= (fun () ->
                self#container
                  ~sp
									~sess:No_data
									~contents:{{ [<h1>"Registration failed."
										<p>"Please try later."] }}
	         )
          )
                
  method private page_reminder err = fun sp () () -> 
    self#container
      ~sp
			~sess:No_data
			~contents:{{ [<h1>"Password reminder"
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
    if not (self#valid_username usr) then
      self#page_reminder "ERROR: Bad character(s) in login name!" sp () ()
    else 
      mail_password 
	~name:usr ~from_addr:sessionmanagerinfo.registration_mail_from 
	~subject:sessionmanagerinfo.registration_mail_subject >>= (fun b ->
          if b
          then 
            self#container
              ~sp
              ~sess:No_data
							~contents:{{ [<h1>"Password sent"
								<p>"You'll soon receive an e-mail message at \
								the address you entered when you \
								registered your account."] }}
          else 
            self#container
              ~sp
              ~sess:No_data
							~contents:{{ [<h1>"Failure"
							<p>"The username you entered doesn't exist, or \
							the service is unavailable at the moment."] }})

  method private page_edit user err = fun sp () () ->
    let (n,_,d,e) = get_user_data ~user in
      self#container
        ~sp
        ~sess:No_data
				~contents:{{ [<h1>"Your account"
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
    if not (self#valid_emailaddr email) then 
      self#page_edit user "ERROR: Bad formed e-mail address!" sp () ()
    else if pwd <> pwd2 then
      self#page_edit user "ERROR: Passwords don't match!" sp () ()
    else
      (ignore (if pwd = ""
      then update_user_data db ~user ~desc ~email ()
      else update_user_data db ~user ~desc ~email ~pwd:(Some pwd) ());
       self#container
         ~sp
         ~sess:No_data
				 ~contents:{{ [<h1>"Personal information updated"] }})

	method private page_not_allowed = fun sp () () ->
		self#container
			~sp
			~sess:No_data
			~contents:{{ [<h1>"I can\'t do that, Dave."
				<p>"In order to manipulate services, you must be an administrator."] }}	

	method private page_create_service user = fun sp () () ->
		let sess = Data user in
		self#container
			~sp
			~sess
			~contents:(
				if Users.in_group user sessionmanagerinfo.administrator then
					begin
						{{ [
							<h1>"Creation of a new service"
							{: post_form ~service:srv_create_service_done ~sp:sp
								(fun url -> {{ [<table>[
									<tr>[<td>"URL:" <td>[{: string_input ~input_type:{:"text":} ~name:url () :}]]
									<tr>[<td>[{: string_input ~input_type:{:"submit":} ~value:"OK" () :}]]
								]] }}) ()
							:}	
						] }}
					end
					else 
					let (n, _, _, _) = get_user_data user in
						{{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }})

	method private page_create_service_done user = fun sp () url ->
		let sess = Data user in
		Services.create_service db ~url:url >>=
		fun _ -> self#container
			~sp
			~sess
			~contents:(
				if Users.in_group user sessionmanagerinfo.administrator then
					{{ [<h1>"The service has been created."] }}
				else 
				let (n, _, _, _) = get_user_data user in
					{{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }})

	method private page_modify_service user = fun sp url () ->
		let sess = Data user in
	  Messages.debug (Printf.sprintf "[page_modify_service] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
		Services.get_service_parameters db ~url >>=
		fun params -> Services.get_service_widgets ~url >>=
		fun widgets ->
			return {{ [<h1>"Configure your new service"
			{: post_form ~service:srv_modify_service_done ~sp
				(fun () -> {{ [<table>[
						<tr>[
							<td>"Service URL:"
							<td>{: url :}
							<td>[{: string_input ~input_type:{: "submit" :} ~value:"Modify service" () :}]]
					]] }}) url :}
				<div class="service_parameters">
				[
					<h2>"Parameters"
					<table>
					[
						<tr>[<th>"Name" <th>"Type"]
						!{: List.map (fun p -> {{ <tr>[<td>{: p.Services.name :} <td>{: match p.Services.p_type with Services.Int -> "int" | Services.String -> "string" :} ] }}) params :}
					]
					{: post_form act_add_parameter sp 
						(fun (srv_name, param_name) -> {{ [<p>[ {: string_input ~input_type:{: "hidden" :} ~name:srv_name ~value:url () :}  {: string_input ~input_type:{: "text" :} ~name:param_name () :} {: string_input ~input_type:{: "submit" :} ~value:"Add parameter" () :} ]] }}) () :}
				]
				<div class="service_widgets">
				[
					<h2>"Widgets"
				]
			] }} >>=
		fun cts -> self#container
			~sp
			~sess
			~contents:cts

	method private page_modify_service_done user = fun sp url () ->
	  Messages.debug (Printf.sprintf "[page_modify_service] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
		let sess = Data user in
		self#container
			~sp
			~sess
			~contents:{{ [<h1>"Your service has been modified."] }}

	method private page_list_services user = fun sp () () ->
		let sess = Data user in
		Services.get_services db >>=
		fun services -> 
			(if Users.in_group user sessionmanagerinfo.administrator then
				return {{ [<h1>"Existing services"
					<table>[
						<tr>[<th>"Name" <th>""]
						!{: List.map (fun n ->
							{{ <tr>[<td>{: n :} <td>[{: a srv_modify_service sp {{ "Modify" }} n :}]] }})
						services :}
					]
				] }}
				else
				let (n, _, _, _) = get_user_data user in
					return {{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }}) >>=
		fun cts -> self#container
			~sp
			~sess
			~contents:cts

	method private add_parameter_handler user = fun sp () (url, param_name) ->
		if Users.in_group user sessionmanagerinfo.administrator then
		begin
			Messages.debug "[add_parameter_handler] user is an administrator.";
			Services.add_parameter db ~url ~param:{ Services.name=param_name; Services.p_type=Services.String } >>=
			fun _ -> return []
		end
		else
			return [Users.NotAllowed]
		

  val mutable all_login_actions = sessionmanagerinfo.login_actions
  val mutable all_logout_actions = sessionmanagerinfo.logout_actions

  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>=
    fun () -> close_session ~sp () >>= 
    fun () -> catch
    (fun () -> let user = authenticate usr pwd in
			Messages.debug (Printf.sprintf "[mk_act_login] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
     	set_persistent_session_data user_table sp user >>=
      fun () -> all_login_actions sp (Data user) >>=
      fun () -> return (
			  Messages.debug (Printf.sprintf "[mk_act_login] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
        register_for_session sp srv_edit (self#page_edit user "");
        register_for_session sp srv_edit_done (self#page_edit_done user);
				register_for_session sp srv_create_service (self#page_create_service user);
				register_for_session sp srv_create_service_done (self#page_create_service_done user);
				register_for_session sp srv_modify_service (self#page_modify_service user);
				register_for_session sp srv_modify_service_done (self#page_modify_service_done user);
				register_for_session sp srv_list_services (self#page_list_services user);
				Actions.register_for_session sp act_add_parameter (self#add_parameter_handler user);
			  Messages.debug (Printf.sprintf "[mk_act_login] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
        []))
    (fun e -> return [e])
      
  method add_login_actions f =
    let old_la = all_login_actions in
    all_login_actions <- 
    fun sp u -> 
      old_la sp u >>=
      (fun () -> f sp u)
	
  method private mk_act_logout sp () () = 
    all_logout_actions sp >>=
    (fun () -> close_session ~sp () >>= (fun () -> return []))

  method add_logout_actions f =
    let old_la = all_logout_actions in
    all_logout_actions <- 
    fun sp -> 
      old_la sp >>=
      (fun () -> f sp)
	

    method mk_log_form : server_params -> Users.user session_data -> 
      {{ Xhtml1_strict.form }}
        = fun sp sess -> 
					match sess with
          | Data user -> (* user is logged in *)
              post_form ~a:{{ {class="logbox logged"} }}
							~service:act_logout ~sp:sp (fun _ -> self#logout_box sp user) ()
          | _ ->
              let exn = get_exn sp in
              if List.mem BadPassword exn || List.mem NoSuchUser exn
              then (* unsuccessful attempt *)
	        post_form ~a:{{ {class="logbox error"} }} 
	          ~service:act_login ~sp:sp (fun (usr, pwd) -> 
                    (self#login_box sp true usr pwd)) ()
              else (* no login attempt yet *)
	        post_form ~a:{{ {class="logbox notlogged"} }}
	          ~service:act_login ~sp:sp (fun (usr, pwd) -> 
                    (self#login_box sp false usr pwd)) ()

    method lwtinit =
			return ()

		method register: unit =
		begin
      Actions.register act_login self#mk_act_login;
      Actions.register act_logout self#mk_act_logout;
      register srv_register (self#page_register "");
      register srv_register_done self#page_register_done;
      register srv_reminder (self#page_reminder "");
      register srv_reminder_done self#page_reminder_done;
			register srv_list_services self#page_not_allowed;
			register srv_create_service (fun sp _ () -> self#page_not_allowed sp () ());
			register srv_modify_service (fun sp _ () -> self#page_not_allowed sp () ());
		end
	
end;;

let connect sm srv container (fwl: 'get -> 'post -> (sp:server_params -> Xhtml1_strict._div Lwt.t) list) =
begin
	register srv
	(fun sp get_params post_params ->
		get_persistent_session_data ~table:user_table ~sp () >>=
		fun sess -> 
		sm#set_user sess;
		Lwt_util.map (fun w ->
			w ~sp
		) (fwl get_params post_params) >>=
		fun c -> container ~sp ~sess ~contents:{{ {: c :} }}
	)
end;;
