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
     max_rows: int64;
   }

class forum:
	foruminfo: forum_in ->
	sessionmanager: SessionManager.sessionmanager ->
object
	method container :
    Eliom.server_params -> Users.user option -> title:string -> 
       {{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
	method private thread_data_box:
		Eliom.server_params -> Users.user option ->
		int32 * string * string * Calendar.t * bool * int64 * int64 ->
		{{ Xhtml1_strict.block }}
  method srv_forum :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      Eliom.service
  method box_forum :
      Users.user option ->
        Eliom.server_params -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_forum' :
      Users.user option ->
        Eliom.server_params ->
          int64 * int64 -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_threads_list :
      Users.user option ->
        Eliom.server_params -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_threads_list' :
      Users.user option ->
        Eliom.server_params ->
          int64 * int64 -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_newmessage :
      Users.user option ->
        int32 ->
          Eliom.server_params -> string -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_newthread :
      Users.user option ->
        Eliom.server_params ->
          string * string -> {{ Xhtml1_strict.blocks }} Lwt.t
  method box_thread' :
      Users.user option ->
        Eliom.server_params ->
          int32 * (int64 * int64) -> {{ Xhtml1_strict.blocks }} Lwt.t
  method page_forum :
      Eliom.server_params -> unit -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_forum' :
      Eliom.server_params ->
        int64 * int64 -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_newmessage :
      Eliom.server_params -> int32 -> string -> {{ Xhtml1_strict.html }} Lwt.t
  method page_newthread :
      Eliom.server_params ->
        unit -> string * string -> {{ Xhtml1_strict.html }} Lwt.t
  method page_thread :
      Eliom.server_params -> int32 -> unit -> {{ Xhtml1_strict.html }} Lwt.t
  method page_thread':
		Eliom.server_params ->
		int32 * (int64 * int64) -> unit -> {{ Xhtml1_strict.html }} Lwt.t
	method register: unit
end;;
