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
*)

open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_predefmod
open Eliom_duce.Xhtml
open Lwt
open Users


type sessionmanager_in = 
{
  url: string list;
  login_actions: server_params -> Users.userdata -> unit Lwt.t;
  logout_actions: server_params -> unit Lwt.t;
  administrator: Users.userdata;
}




class sessionmanager ~(sessionmanagerinfo: sessionmanager_in) =
  let internal_act_login = 
    new_post_coservice' 
      ~keep_get_na_params:false
      ~post_params:(string "usr" ** string "pwd") () 
  and internal_act_logout = 
    new_post_coservice' 
      ~keep_get_na_params:false
      ~post_params:unit ()
  and internal_act_logout_get = new_coservice' ~get_params:unit ()
(*VVV I add this GET service because it is not possible to make a link 
  towards a POST service ... I use a redirection instead of an action *)
  in
    
object (self)
  
  val widget_types = Hashtbl.create 1
    
  method act_login : 
    (unit, string * string,
     [`Nonattached of [`Post] Eliom_services.na_s],
     [`WithoutSuffix], unit, 
     [`One of string] Eliom_parameters.param_name * 
       [`One of string] Eliom_parameters.param_name,
     [`Registrable]) Eliom_services.service
    = internal_act_login

  method act_logout :
    (unit, 
     unit, 
     [`Nonattached of [`Post] Eliom_services.na_s],
     [`WithoutSuffix], 
     unit, 
     unit, 
     [`Registrable]) service
    = internal_act_logout

  method act_logout_get :
    (unit, 
     unit, 
     [`Nonattached of [`Get] Eliom_services.na_s],
     [`WithoutSuffix], 
     unit, 
     unit, 
     [`Registrable]) service
    = internal_act_logout_get



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
         let sd = Ocsimore_common.create_empty_sd () in
         Users.set_session_data sp sd user >>= fun () -> 
         all_login_actions sp user >>= fun () ->
         Lwt.return [Ocsimore_common.Session_data sd]) 
      (fun e -> return [e])
        
  method add_login_actions f =
    let old_la = all_login_actions in
    all_login_actions <- fun sp u -> 
    old_la sp u >>= (fun () -> f sp u)
                
  method private mk_act_logout sp () () = 
    all_logout_actions sp >>= fun () ->
    close_session ~sp () >>= fun () -> 
    return [] (* do not send sd here! *)
      
  method add_logout_actions f =
    let old_la = all_logout_actions in
    all_logout_actions <- fun sp -> 
    old_la sp >>= fun () -> f sp
          
      
  initializer
    begin
      Actions.register internal_act_login self#mk_act_login;
      Actions.register internal_act_logout self#mk_act_logout;
      Redirections.register internal_act_logout_get
        (fun sp () () ->
           ignore (self#mk_act_logout sp () ());
           Eliom_predefmod.Xhtml.make_full_string_uri
             Eliom_services.void_action sp ()
        );
    end

        
end;;






let connect sm srv container
    (fwl: 'get -> 'post -> 
      (sp:server_params -> Xhtmltypes_duce._div Lwt.t) list) =
  begin
    register srv
      (fun sp get_params post_params ->
         let sd = Ocsimore_common.get_sd sp in
         Lwt_util.map_serial
           (fun w -> w ~sp) 
           (fwl get_params post_params) >>= fun c -> 
           container ~sp ~sd ~contents:{{ {: c :} }}
      )
  end

let pam_auth ~name ~pwd =
  Lwt_preemptive.detach
    (fun () ->
       try
         let pam = Pam.pam_start "" ~user:name (fun _ _ -> pwd) in
         Pam.pam_authenticate pam [] ~silent:true;
         ignore (Pam.pam_end pam)
       with Pam.Pam_Error _ -> raise Users.BadPassword
    )
    ()

class sessionmanager_pam ~(sessionmanagerinfo: sessionmanager_in) =
object
  inherit sessionmanager sessionmanagerinfo

  method private mk_act_login sp () (usr, pwd) =
    all_logout_actions sp >>= fun () -> 
    close_session ~sp () >>= fun () -> 
    Lwt.catch
      (fun () -> 
         Lwt.catch
           (fun () -> Users.authenticate ~name:usr ~pwd)
           (function
              | Users.UsePam u -> 
                  (* check PAM pwd *)
                  pam_auth ~name:usr ~pwd >>= fun () ->
                  Lwt.return u
              | Users.BadUser -> 
                  (* check PAM pwd, and create user if ok *)
                  pam_auth ~name:usr ~pwd >>= fun () ->
                  Users.create_user
                    ~name:usr
                    ~pwd:User_sql.Pam
                    ~fullname:usr
                    ~email:None
                    ~groups:[Users.authenticated_users.Users.id]
                    ()
              | e -> Lwt.fail e)
         >>= fun user -> 
         let sd = Ocsimore_common.create_empty_sd () in
         Users.set_session_data sp sd user >>= fun () -> 
         all_login_actions sp user >>= fun () ->
         Lwt.return [Ocsimore_common.Session_data sd]) 
      (fun e -> return [e])

end































(*VVV What is it?????? : *)
(*
  let act_add_parameter = 
    new_post_coservice'
      ~post_params:(string "service_name" ** string "param_name") () 
  in
  let act_add_widget = new_post_coservice' ~post_params:(string "name") () in
    *)
      (* and srv_create_service = new_service ~path:(sessionmanagerinfo.url @ ["create_service"]) ~get_params:unit () in
         let srv_create_service_done = new_post_coservice ~fallback:srv_create_service ~post_params:(string "url") () in
         let srv_modify_service = new_service ~path:(sessionmanagerinfo.url @ ["modify_service"]) ~get_params:(string "url") () in
         let srv_modify_service_done = new_post_coservice ~fallback:srv_modify_service ~post_params:unit () in
         let srv_list_services = new_service ~path:(sessionmanagerinfo.url @ ["list_services"]) ~get_params:unit () *)
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
