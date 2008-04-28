open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_predefmod
open Eliom_duce.Xhtml
open Lwt
open Users

let user_table: Users.userdata persistent_table = 
  create_persistent_table "ocsimore_user_table_v1"

type sessionmanager_in = 
{
  url: string list;
  default_groups: Users.group list;
  login_actions: server_params -> Users.userdata session_data -> unit Lwt.t;
  logout_actions: server_params -> unit Lwt.t;
  registration_mail_from: string * string;
  registration_mail_subject: string;
  administrator: Users.userdata;
}



(* private: *)
let valid_username usr =
  Str.string_match (Str.regexp "^[A-Za-z0-9]+$") usr 0
    
let valid_emailaddr email =
  Str.string_match 
    (Str.regexp ("^[A-Za-z0-9\\._-]+@\\([A-Za-z0-9][A-Za-z0-9_-]+\\.\\)+\\([a-z]+\\)+$")) 
    email 0
      


class sessionmanager ~(sessionmanagerinfo: sessionmanager_in) =

  let internal_act_login = 
    new_post_coservice' ~post_params:(string "usr" ** string "pwd") () 
  and internal_act_logout = new_post_coservice' ~post_params:unit ()
  and internal_srv_register = 
    new_service
      ~path:(sessionmanagerinfo.url @ ["register"]) 
      ~get_params:unit () in
  let srv_register_done = 
    new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "usr" ** (string "descr" ** string "email")) ()
  and internal_srv_reminder = 
    new_service
      ~path:(sessionmanagerinfo.url @ ["reminder"]) ~get_params:unit ()
  and srv_reminder_done = 
    new_post_coservice
      ~fallback:internal_srv_register ~post_params:(string "usr") ()
  and internal_srv_edit = 
    new_coservice ~fallback:internal_srv_register ~get_params:unit ()
  and srv_edit_done = 
    new_post_coservice
      ~fallback:internal_srv_register
      ~post_params:(string "pwd" ** 
                      (string "pwd2" ** (string "descr" ** string "email"))) () 
      (* and srv_create_service = new_service ~path:(sessionmanagerinfo.url @ ["create_service"]) ~get_params:unit () in
         let srv_create_service_done = new_post_coservice ~fallback:srv_create_service ~post_params:(string "url") () in
         let srv_modify_service = new_service ~path:(sessionmanagerinfo.url @ ["modify_service"]) ~get_params:(string "url") () in
         let srv_modify_service_done = new_post_coservice ~fallback:srv_modify_service ~post_params:unit () in
         let srv_list_services = new_service ~path:(sessionmanagerinfo.url @ ["list_services"]) ~get_params:unit () *)
  in
    
  let act_add_parameter = 
    new_post_coservice'
      ~post_params:(string "service_name" ** string "param_name") () 
  in
  let act_add_widget = new_post_coservice' ~post_params:(string "name") () in


(* Wiki: *)
(*VVV Where to put this? *)
(* The registration must be done during site loading, nor before! *)

  let action_edit_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_edit"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      (fun sp g () -> Lwt.return [Wiki.Editbox g])
  in

  let action_cancel =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~name:"cancel"
      ~post_params:unit
      (fun sp () () -> Lwt.return [])
  in

  let action_send_wikibox =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~name:"wiki_send"
      ~post_params:
      ((((Eliom_parameters.int32 "wikiid") ** 
           (Eliom_parameters.int32 "boxid")) ** 
          Eliom_parameters.string "content") **
         (Eliom_parameters.opt (Eliom_parameters.string "addreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "addwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "addadmin") **
            Eliom_parameters.opt (Eliom_parameters.string "delreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "delwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "deladmin")
         ))
      (fun sp () p -> 
         Eliom_sessions.get_persistent_session_data user_table sp ()
           >>= fun sd -> 
         Wiki.save_wikibox sp sd p)
  in

    
object (self)
  
  val widget_types = Hashtbl.create 1
    
  method act_login: 
    (unit, string * string,
     [`Nonattached of [`Post] Eliom_services.na_s],
     [`WithoutSuffix], unit, 
     [`One of string] Eliom_parameters.param_name * 
       [`One of string] Eliom_parameters.param_name,
     [`Registrable]) Eliom_services.service =
    internal_act_login
  method srv_register: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable]) service = internal_srv_register
  method srv_reminder: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable]) service = internal_srv_reminder
  method srv_edit: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable]) service = internal_srv_edit
    
  method act_logout: (unit, unit, [`Nonattached of [`Post] Eliom_services.na_s], [`WithoutSuffix], unit, unit, [`Registrable]) service =
    internal_act_logout


  method action_edit_wikibox :
    (int32 * int32, 
     unit,
     get_service_kind,
     [ `WithoutSuffix ], 
     [ `One of int32 ] Eliom_parameters.param_name *
     [ `One of int32 ] Eliom_parameters.param_name,
     unit, 
     [ `Registrable ])
    Eliom_services.service = action_edit_wikibox
      
  method action_cancel :
    (unit, unit, post_service_kind,
     [ `WithoutSuffix ], unit, unit, [ `Registrable ])
    Eliom_services.service
    = action_cancel

  method action_send_wikibox :
    (unit,
     ((int32 * int32) * string) *
       (string option *
          (string option *
             (string option * 
                (string option * 
                   (string option * 
                      string option))))),
     post_service_kind,
     [ `WithoutSuffix ], 
     unit,
     (([ `One of int32 ] Eliom_parameters.param_name *
        [ `One of int32 ] Eliom_parameters.param_name) *
        [ `One of string ] Eliom_parameters.param_name) *
       ([ `Opt of string ] Eliom_parameters.param_name *
          ([ `Opt of string ] Eliom_parameters.param_name *
          ([ `Opt of string ] Eliom_parameters.param_name *
          ([ `Opt of string ] Eliom_parameters.param_name *
          ([ `Opt of string ] Eliom_parameters.param_name *
          ([ `Opt of string ] Eliom_parameters.param_name)))))),
     [ `Registrable ])
    Eliom_services.service 
    = action_send_wikibox
      
  method container
    ~(sp: server_params)
    ~(sd: Users.userdata session_data)
    ~(contents:Xhtmltypes_duce.blocks) : Xhtmltypes_duce.html Lwt.t =
    return {{ 
              <html>[
                <head>[<title>"Temporary title"]
                <body>{: contents :}
              ]
            }}
      
  method private page_register err = fun sp () ()-> 
    self#container
      ~sp
      ~sd:No_data
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
      
  method private page_register_done = fun sp () (usr, (fullname, email)) ->
    if not (valid_username usr) then 
      self#page_register "ERROR: Bad character(s) in login name!" sp () ()
    else if not (valid_emailaddr email) then 
      self#page_register "ERROR: Bad formed e-mail address!" sp () ()
    else 
      let pwd = generate_password () in
      Users.create_unique_user
        ~name:usr ~pwd ~fullname ~email 
        ~groups:sessionmanagerinfo.default_groups >>= fun (user, n) ->
      mail_password
        ~name:n
        ~from_addr:sessionmanagerinfo.registration_mail_from 
        ~subject:sessionmanagerinfo.registration_mail_subject >>= fun b ->
      if b
      then begin
        self#container
          ~sp
          ~sd:No_data
          ~contents:{{ [<h1>"Registration ok."
                         <p>(['You\'ll soon receive an e-mail message at the \
                                following address:'
                              <br>[]] @
                               {: email :} @
                                [<br>[]
                                    'reporting your login name and password.'])] }}
        
      end
      else 
        Users.delete_user ~user >>= fun () ->
        self#container
          ~sp
          ~sd:No_data
          ~contents:{{ [<h1>"Registration failed."
                         <p>"Please try later."] }}


              
  method private page_reminder err = fun sp () () -> 
    self#container
      ~sp
      ~sd:No_data
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
         ~sd:No_data
         ~contents:{{ [<h1>"Password sent"
         <p>"You'll soon receive an e-mail message at \
         the address you entered when you \
         registered your account."] }}
         else 
         self#container
         ~sp
         ~sd:No_data
         ~contents:{{ [<h1>"Failure"
         <p>"The username you entered doesn't exist, or \
         the service is unavailable at the moment."] }}) *)
      
  method private page_edit err = fun sp () () ->
    get_persistent_session_data user_table sp () >>= fun sess -> 
    match sess with
      | Data u -> 
          self#container
            ~sp
              ~sd:No_data
              ~contents:{{ [<h1>"Your account"
                             <p>"Change your persional information:"
                             {: post_form srv_edit_done sp
                                (fun (pwd,(pwd2,(desc,email))) -> 
                                   {{ [<table>[
                                          <tr>[
                                                <td>"login name: "
                                                <td>[<strong>{: u.Users.name :}]
                                        ]
                                        <tr>[
                                                <td>"real name: "
                                                <td>[{: string_input ~input_type:{:"text":} ~value:u.Users.fullname ~name:desc () :}]
                                        ]
                                        <tr>[
                                                <td>"e-mail address: "
                                                <td>[{: string_input ~input_type:{:"text":} ~value:u.Users.email ~name:email () :}]
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

  method private page_edit_done = fun sp () (pwd,(pwd2,(fullname,email)))->
    get_persistent_session_data user_table sp () >>= fun sess -> 
      match sess with
        | Data user ->
            if not (valid_emailaddr email) then  
              self#page_edit "ERROR: Bad formed e-mail address!" sp () ()
            else if pwd <> pwd2 then
              self#page_edit "ERROR: Passwords don't match!" sp () ()
            else
              (Ocsigen_messages.debug2 (Printf.sprintf "fullname: %s" fullname);
               ignore (if pwd = ""
                       then update_user_data ~user ~fullname ~email ()
                       else update_user_data ~user ~fullname ~email ~pwd:(Some pwd) ());
               set_persistent_session_data user_table sp user;        
               self#container
                 ~sp
                 ~sd:No_data
                 ~contents:{{ [<h1>"Personal information updated"] }})
                
  method private page_not_allowed = fun sp () () ->
    self#container
      ~sp
      ~sd:No_data
      ~contents:{{ [<h1>"I can\'t do that, Dave."
                     <p>"In order to manipulate services, you must be an administrator."] }}        
      
  (*        method private page_create_service = fun sp () () ->
        get_persistent_session_data user_table sp () >>=
        fun sess -> 
                self#container
                        ~sp
                        ~sd
                        ~contents:({{ [<p>"being implemented"] }}
                                (* if in_group user sessionmanagerinfo.administrator then
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
                                                {{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }} *) )

        method private page_create_service_done = fun sp () url ->
                get_persistent_session_data user_table sp () >>=
                fun sess -> 
                create_service ~url >>=
                fun () -> register_service ~sp ~url >>=
                fun _ -> self#container
                        ~sp
                        ~sd
                        ~contents:({{ [<p>"being implemented"] }}
                                (* if in_group user sessionmanagerinfo.administrator then
                                        {{ [<h1>"The service has been created."] }}
                                else 
                                let (n, _, _, _) = get_user_data user in
                                        {{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }}*) )

        method private page_modify_service = fun sp url () ->
                (* let type_dropdown name value =
                        let l = List.map (fun t ->
                                Option ({{ {} }}, t, None, t = (string_of_type value))
                        ) ["int"; "float"; "string"; "bool"; "file"; "unit"] in
                        Eliom_duce.Xhtml.string_select ~name (List.hd l) (List.tl l) in *)
                get_persistent_session_data user_table sp () >>=
          fun sess -> Ocsigen_messages.debug2 (Printf.sprintf "[page_modify_service] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
                get_service_parameters ~url >>=
                fun params -> get_service_widgets ~url >>=
                fun widgets ->
                        return {{ [<h1>"Configure your service"
                        {: post_form ~service:srv_modify_service_done ~sp
                                (fun () -> {{ [<table>[
                                                <tr>[
                                                        <td>"Service URL:"
                                                        <td>{: url :}
                                                        <td>[{: string_input ~input_type:{: "submit" :} ~value:"Apply" () :}]]
                                        ]] }}) url :}
                                <div class="service_parameters">
                                [
                                        <h2>"Parameters"
                                        <table>
                                        [
                                                <tr>[<th>"Name"]
                                                !{: List.map (fun p -> {{ <tr>[<td>{: p.name :}] }}) params :}
                                        ]
                                        {: post_form act_add_parameter sp 
                                                (fun (srv_name, param_name) -> {{ [<p>[ {: string_input ~input_type:{: "hidden" :} ~name:srv_name ~value:url () :} {: string_input ~input_type:{: "text" :} ~name:param_name () :} (* {: type_dropdown param_type String :} *) {: string_input ~input_type:{: "submit" :} ~value:"Add parameter" () :} ]] }}) () :}
                                ]
                                <div class="service_widgets">
                                [
                                        <h2>"Widgets"
                                ]
                        ] }} >>=
                fun cts -> self#container
                        ~sp
                        ~sd
                        ~contents:cts

        method private page_modify_service_done = fun sp url () ->
                get_persistent_session_data user_table sp () >>=
                fun sess ->
          Ocsigen_messages.debug2 (Printf.sprintf "[page_modify_service] session name: %s" (match get_session_name ~sp with None -> "<NONE>" | Some x -> x));
                self#container
                        ~sp
                        ~sd
                        ~contents:{{ [<h1>"Your service has been modified."] }}

        method private page_list_services = fun sp () () ->
                get_persistent_session_data user_table sp () >>=
                fun sess -> get_services >>=
                fun services -> 
                        (* (if in_group user sessionmanagerinfo.administrator then
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
                                        return {{ [<h1>{: Printf.sprintf "I can't do that, %s." n :}] }}) >>= *)
                                        return {{ [<p>"being implemented"] }} >>=
                fun cts -> self#container
                        ~sp
                        ~sd
                        ~contents:cts

*)

        method private add_parameter_handler user = fun sp () (url, param_name) ->
                (* if in_group user sessionmanagerinfo.administrator then
                begin
                        Ocsigen_messages.debug2 "[add_parameter_handler] user is an administrator.";
                        add_parameter ~url ~param:{ name=param_name } >>=
                        fun _ -> return []
                end
                else *)
                        return [NotAllowed]
                

        val mutable all_login_actions = sessionmanagerinfo.login_actions
        val mutable all_logout_actions = sessionmanagerinfo.logout_actions
          
        method private mk_act_login sp () (usr, pwd) =
          all_logout_actions sp >>= fun () -> 
          close_session ~sp () >>= fun () -> 
          Lwt.catch
            (fun () -> 
               authenticate ~name:usr ~pwd  >>= fun user -> 
               set_persistent_session_data user_table sp user >>= fun () -> 
               all_login_actions sp (Data user) >>= fun () ->
               Lwt.return []) 
            (fun e -> return [e])
      
        method add_login_actions f =
          let old_la = all_login_actions in
          all_login_actions <- 
            fun sp u -> 
              old_la sp u >>= (fun () -> f sp u)
                
        method private mk_act_logout sp () () = 
          all_logout_actions sp >>= fun () ->
          close_session ~sp () >>= fun () -> return []

  method add_logout_actions f =
    let old_la = all_logout_actions in
    all_logout_actions <- fun sp -> 
      old_la sp >>= fun () -> f sp
        

  method lwtinit =
    return ()
      
  method register =
    begin
      Ocsigen_messages.debug2 "[sessionManager] registering I";
      Actions.register internal_act_login self#mk_act_login;
      Ocsigen_messages.debug2 "[sessionManager] registering II";
      Actions.register internal_act_logout self#mk_act_logout;
      Ocsigen_messages.debug2 "[sessionManager] registering III";
      register internal_srv_register (self#page_register "");
      Ocsigen_messages.debug2 "[sessionManager] registering IV";
      register srv_register_done self#page_register_done;
      Ocsigen_messages.debug2 "[sessionManager] registering V";
      register internal_srv_reminder (self#page_reminder "");
      Ocsigen_messages.debug2 "[sessionManager] registering VI";
      register srv_reminder_done self#page_reminder_done;
      Ocsigen_messages.debug2 "[sessionManager] registering VII";
      (*                        register srv_list_services self#page_list_services;
                        Ocsigen_messages.debug2 "[sessionManager] registering VIII";
                        register srv_create_service self#page_create_service;
                        Ocsigen_messages.debug2 "[sessionManager] registering IX";
                        register srv_modify_service self#page_modify_service;
                        Ocsigen_messages.debug2 "[sessionManager] registering X";
      register internal_srv_edit (self#page_edit "");
                        Ocsigen_messages.debug2 "[sessionManager] registering XI";
      register srv_edit_done self#page_edit_done;
                        Ocsigen_messages.debug2 "[sessionManager] registering XII";
                        (* Services.register_services >>=
                        fun () -> *) Ocsigen_messages.debug2 "[sessionManager] registering done";
*)
                                return ()
                end
        
end;;

let connect sm srv container
    (fwl: 'get -> 'post -> 
      (sp:server_params -> Xhtmltypes_duce._div Lwt.t) list) =
  begin
    register srv
      (fun sp get_params post_params ->
         get_persistent_session_data ~table:user_table ~sp () >>= fun sd -> 
         Lwt_util.map_serial
           (fun w -> w ~sp) 
           (fwl get_params post_params) >>= fun c -> 
           container ~sp ~sd ~contents:{{ {: c :} }}
      )
end


