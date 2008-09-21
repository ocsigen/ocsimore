
(** Module Users.
    
    Users, authentication, protection. 
    
    In this model, users and groups are the same concept. A group can
    belong to another group. We only distinguish, for practical
    matters, between "login enabled" users and "group only" users: the
    former has [Some] (eventually void) password, the latter has
    [None].

*)



(** user information *)
type userdata = 
    private
      { id: User_sql.userid;
        name: string;
        mutable pwd: User_sql.pwd;
        mutable fullname: string;
        mutable email: string option;
        dyn: bool;
      }


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

val get_user_by_name : name:string -> userdata Lwt.t
val get_user_id_by_name : string -> int32 Lwt.t
val get_user_name_by_id : int32 -> string Lwt.t
val get_user_by_id : id:int32 -> userdata Lwt.t


(** Creates a new user with given parameters. 
    Raises {!Users.UserExists} if [name] is already present. *)
val create_user: 
  name:string -> 
  pwd:User_sql.pwd -> 
  fullname:string -> 
  email:string option -> 
  groups: User_sql.userid list ->
  ?test:(sp:Eliom_sessions.server_params ->
          sd:Ocsimore_common.session_data -> bool Lwt.t) ->
  unit ->
  userdata Lwt.t

val create_unique_user: 
  name:string -> 
  pwd:User_sql.pwd -> 
  fullname:string -> 
  email:string option -> 
  groups: User_sql.userid list ->
  (userdata * string) Lwt.t

val delete_user : userid:User_sql.userid -> unit Lwt.t

val update_user_data: 
  user:userdata -> 
  ?pwd:User_sql.pwd -> 
  ?fullname:string -> 
  ?email:string option -> 
  ?groups: User_sql.userid list ->
  unit -> 
  unit Lwt.t

val authenticate : name:string -> pwd:string -> userdata Lwt.t

val in_group : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  ?user:User_sql.userid -> 
  group:User_sql.userid -> 
  unit -> bool Lwt.t

val add_to_group : user:User_sql.userid -> group:User_sql.userid -> unit Lwt.t

val group_list_of_string : string -> User_sql.userid list Lwt.t

(****)
val get_user_data : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> userdata Lwt.t

val get_user_id : 
  sp:Eliom_sessions.server_params -> 
  sd:Ocsimore_common.session_data -> int32 Lwt.t

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
