val user_table : Users.user Eliom.persistent_table

type sessionmanager_in = 
    { url: string list;
      default_groups: Users.user list;
      login_actions: Eliom.server_params -> Users.user option -> unit Lwt.t;
      logout_actions: Eliom.server_params -> unit Lwt.t;
      registration_mail_from: string * string;
      registration_mail_subject: string;
  }

class sessionmanager: sessionmanagerinfo: sessionmanager_in ->
object
  method container: Eliom.server_params -> Users.user option -> title:string -> 
          {{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
  method mk_log_form: Eliom.server_params -> Users.user option -> 
    {{ Xhtml1_strict.form }}
  method add_login_actions: 
      (Eliom.server_params -> Users.user option -> unit Lwt.t) -> unit
  method add_logout_actions: 
      (Eliom.server_params -> unit Lwt.t) -> unit
	method lwtinit: unit Lwt.t
	method register: unit
end;;
