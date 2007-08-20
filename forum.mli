type forum_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     moderated: bool;
     readable_by: Users.user;
     writable_by: Users.user;
     moderators: Users.user;
     url: string list;
     max_rows: int;
		 arborescent: bool;
   }

class forum:
	foruminfo: forum_in ->
	sessionmanager: SessionManager.sessionmanager ->
object
	method is_logged_on: Users.user option -> bool
	method can_read: Users.user option -> bool
	method can_write: Users.user option -> bool
	method can_moderate: Users.user option -> bool

	method container :
    Eliom.server_params -> Users.user option -> title:string -> 
       {{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
	method private thread_data_box:
		Eliom.server_params -> Users.user option ->
		(Sql.db_int_t * string * string * string option * Calendar.t * bool * int * int) -> {{ Xhtml1_strict.block }}
	method private article_box:
		Eliom.server_params -> Users.user option ->
		(Sql.db_int_t * string * string * string option * Calendar.t * bool * int * int) -> {{ Xhtml1_strict.block }}
	method private forum_threads_list_box:
		Eliom.server_params -> Users.user option ->
		(Sql.db_int_t * string * string * Calendar.t * bool) list ->
		{{ Xhtml1_strict.block }}
	method private message_data_box:
		Eliom.server_params -> Users.user option -> Sql.db_int_t -> 
		Sql.db_int_t * string * string * Calendar.t * bool * bool * Sql.db_int_t option ->
		int -> int -> {{ Xhtml1_strict.block }}
	method private thread_messageswtext_box:
		Eliom.server_params -> Users.user option -> Sql.db_int_t ->
		int -> int ->
		(Sql.db_int_t * string * string * Calendar.t * bool * bool * Sql.db_int_t option) Sql.collection ->
		{{ Xhtml1_strict.block }}

  method srv_forum :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      Eliom.service
	method srv_thread :
			(Sql.db_int_t, unit, Eliom.get_service_kind,
			[ `WithoutSuffix ], [`One of Sql.db_int_t] Eliom.param_name, unit,[`Registrable ])
			Eliom.service
	method srv_newthread :
		(unit, bool * (string * string), Eliom.post_service_kind,
		[ `WithoutSuffix ], unit, [`One of bool] Eliom.param_name * ([`One of string] Eliom.param_name * [`One of string] Eliom.param_name), [ `Registrable ])
		Eliom.service

	method new_thread_form: 
		[`One of bool] Eliom.param_name *
		([`One of string] Eliom.param_name * [`One of string] Eliom.param_name) ->
		{{ [Xhtml1_strict.form_content*] }}
	method new_message_form:
	[ `One of string ] Eliom.param_name * [`One of bool] Eliom.param_name ->
	{{ [Xhtml1_strict.form_content*] }}

  method box_forum :
       Eliom.server_params -> Users.user option ->
			 {{ Xhtml1_strict.blocks }} Lwt.t
  method box_forum' :
        Eliom.server_params -> Users.user option ->
          int * int -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_threads_list :
        Eliom.server_params -> Users.user option ->
				{{ Xhtml1_strict.blocks }} Lwt.t
  method box_threads_list' :
        Eliom.server_params -> Users.user option ->
          int * int -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_newmessage :
      Eliom.server_params -> Users.user option -> Sql.db_int_t * (int * int) ->
			string * bool -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_replymessage :
      Eliom.server_params -> Users.user option -> Sql.db_int_t ->
			Sql.db_int_t * (string * bool) -> 
			{{ Xhtml1_strict.blocks }} Lwt.t
  method box_newthread :
        Eliom.server_params -> Users.user option -> bool * (string * string) ->
				{{ Xhtml1_strict.blocks }} Lwt.t
  method box_thread' :
     Eliom.server_params -> Users.user option -> Sql.db_int_t * (int * int) ->
		 {{ Xhtml1_strict.blocks }} Lwt.t
  method box_reply' :
     Eliom.server_params -> Users.user option ->
		 Sql.db_int_t * (Sql.db_int_t * (int * int)) ->
		 {{ Xhtml1_strict.blocks }} Lwt.t
  method page_forum :
      Eliom.server_params -> unit -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_forum' :
      Eliom.server_params ->
        int * int -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_newmessage :
      Eliom.server_params -> Sql.db_int_t * (int * int) -> string * bool ->
			{{ Xhtml1_strict.html }} Lwt.t
  method page_replymessage :
      Eliom.server_params -> Sql.db_int_t -> Sql.db_int_t * (string * bool) ->
			{{ Xhtml1_strict.html }} Lwt.t
  method page_newthread :
      Eliom.server_params ->
        unit -> bool * (string * string) -> {{ Xhtml1_strict.html }} Lwt.t
  method page_thread :
      Eliom.server_params -> Sql.db_int_t -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_thread':
		Eliom.server_params ->
		Sql.db_int_t * (int * int) -> unit -> {{ Xhtml1_strict.html }} Lwt.t
	method page_reply:
		Eliom.server_params ->
		(Sql.db_int_t * Sql.db_int_t) -> unit -> {{ Xhtml1_strict.html }} Lwt.t
	method page_reply':
		Eliom.server_params ->
		Sql.db_int_t * (Sql.db_int_t * (int * int)) -> unit -> {{ Xhtml1_strict.html }} Lwt.t
	method register: unit
end;;
