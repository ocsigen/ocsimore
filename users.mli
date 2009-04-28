(** Module Users.
    
    Users, authentication, protection. 
    
    In this model, users and groups are the same concept. A group can
    belong to another group. We only distinguish, for practical
    matters, between "login enabled" users and "group only" users: the
    former has [Some] (eventually void) password, the latter has
    [None].

*)
open User_sql.Types


type 'a parametrized_user
type users
val apply_parametrized_user : 'a parametrized_user -> 'a Opaque.int32_t -> users




exception NotAllowed
exception BadPassword
exception BadUser
exception UseAuth of userdata

(** Non atuthenticated users *)
val anonymous : userdata

(** A user that belongs to all groups *)
val admin : userdata

(** A user/group that does not belong to any group, 
    and nobody can be put in it.  *)
val nobody : userdata

(** A group containing all authenticated users (not groups) *)
val authenticated_users : userdata

(** Information about a user. Return [nobody] if the user
    does not currently exists *)
val get_user_by_name : name:string -> userdata Lwt.t
val get_user_id_by_name : string -> userid Lwt.t


val get_user_name_by_id : userid -> string Lwt.t
val get_user_by_id : id:userid -> userdata Lwt.t
val get_user_fullname_by_id : userid -> string Lwt.t


(** Creates a new user with given parameters,
    or returns the existing user without modification
    if [name] is already present. *)
val create_user: 
  name:string -> 
  pwd:pwd -> 
  fullname:string -> 
  ?email:string -> 
  groups: userid list ->
  ?test:(sp:Eliom_sessions.server_params ->
          sd:Ocsimore_common.session_data -> bool Lwt.t) ->
  unit ->
  userdata Lwt.t

val create_unique_user: 
  name:string -> 
  pwd:pwd -> 
  fullname:string -> 
  ?email:string -> 
  groups: userid list ->
  (userdata * string) Lwt.t

val delete_user : userid:userid -> unit Lwt.t

(* BY 2009-03-13: deactivated because update_data is deactivated. See this file *)
(*
val update_user_data: 
  user:userdata -> 
  ?pwd:pwd -> 
  ?fullname:string -> 
  ?email:string option -> 
  ?groups: userid list ->
  unit -> 
  unit Lwt.t
*)

val authenticate : name:string -> pwd:string -> userdata Lwt.t

val in_group : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  ?user:userid -> 
  group:userid -> 
  unit -> bool Lwt.t

val add_to_group : user:userid -> group:userid -> unit Lwt.t

val group_list_of_string : string -> userid list Lwt.t

(****)
val get_user_data : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> userdata Lwt.t

val get_user_id : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> userid Lwt.t

val get_user_name : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> string Lwt.t

val is_logged_on : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> bool Lwt.t

val set_session_data : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> userdata -> unit Lwt.t

val anonymous_sd : Ocsimore_common.session_data
