open Eliommod
open Eliomparameters
open Eliomservices
open Eliomsessions 
open Eliomduce.Xhtml
val user_table : Users.user persistent_table

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

class sessionmanager: db: Sql.db_t -> sessionmanagerinfo: sessionmanager_in ->
object
	method db: Sql.db_t
	method act_login: (unit, string * string, [`Nonattached of [`Post] na_s], [`WithoutSuffix], unit, [`One of string] param_name * [`One of string] param_name, [`Registrable]) service
	method act_logout: (unit, unit, [`Nonattached of [`Post] na_s], [`WithoutSuffix], unit, unit, [`Registrable]) service
	method srv_register: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable ]) service
	method srv_reminder: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable ]) service
	method srv_edit: (unit, unit, get_service_kind, [`WithoutSuffix], unit, unit, [`Registrable ]) service
	method set_user: Users.user session_data -> unit
	method get_forum: int -> Forum.forum
	method add_forum: Forum.forum -> unit
	method is_logged_on: bool
	method get_user: Users.user session_data
	method get_user_data: string * string option * string * string
	method get_user_name: string
	method get_role: int -> Sql.role
  method container: sp:server_params -> sess:Users.user session_data ->
		contents:Xhtml1_strict.blocks -> Xhtml1_strict.html Lwt.t
  (* method mk_log_form: server_params -> Users.user session_data -> 
    {{ Xhtml1_strict.form }} *)
  method add_login_actions: 
      (server_params -> Users.user session_data -> unit Lwt.t) -> unit
  method add_logout_actions: 
      (server_params -> unit Lwt.t) -> unit
	method lwtinit: unit Lwt.t
	method register: unit Lwt.t
end;;

val connect:
	sessionmanager ->
	('get, 'post, internal_service_kind, [`WithoutSuffix], 'gn, 'pn, [`Registrable]) service ->
	(sp:server_params -> sess:Users.user Eliomsessions.session_data -> contents: Xhtml1_strict.blocks -> Eliomduce.Xhtml.page Lwt.t) ->
('get -> 'post -> (sp:server_params -> Xhtml1_strict._div Lwt.t) list)
-> unit
