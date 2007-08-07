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
   }

class forum:
	foruminfo: forum_in ->
	sessionmanager: SessionManager.sessionmanager ->
object
	method private is_logged_on: Users.user option -> bool
	method private can_read: Users.user option -> bool
	method private can_write: Users.user option -> bool
	method private can_moderate: Users.user option -> bool

	method container :
    Eliom.server_params -> Users.user option -> title:string -> 
       {{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
	method private thread_data_box:
		Eliom.server_params -> Users.user option ->
		(Sql.db_int_t * string * string * string option * Calendar.t * bool * int * int) -> {{ Xhtml1_strict.block }}
	method private article_box:
		Eliom.server_params -> Users.user option ->
		(Sql.db_int_t * string * string * string option * Calendar.t * bool * int * int) -> {{ Xhtml1_strict.block }}
	method private message_data_box:
		Eliom.server_params -> Users.user option -> Sql.db_int_t -> 
		Sql.db_int_t * string * string * Calendar.t * bool * Sql.db_int_t option * string option ->
		int -> int -> {{ Xhtml1_strict.block }}
	method private thread_messageswtext_box:
		Eliom.server_params -> Users.user option -> Sql.db_int_t ->
		int -> int ->
		(Sql.db_int_t * string * string * Calendar.t * bool * Sql.db_int_t option * string option) Sql.tree list ->
		{{ Xhtml1_strict.block }}

  method srv_forum :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      Eliom.service
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
			string -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_replymessage :
      Eliom.server_params -> Users.user option -> Sql.db_int_t ->
			Sql.db_int_t * string -> 
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
      Eliom.server_params -> Sql.db_int_t * (int * int) -> string ->
			{{ Xhtml1_strict.html }} Lwt.t
  method page_replymessage :
      Eliom.server_params -> Sql.db_int_t -> Sql.db_int_t * string -> {{ Xhtml1_strict.html }} Lwt.t
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
