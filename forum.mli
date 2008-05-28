(**
This is the forum component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
*)

type forum_info = {
  id : Forum_sql.forum;
  title : string;
  descr: string;
  moderated: bool;
  arborescent: bool;
  readable_by : User_sql.userid;
  writable_by : User_sql.userid;
  moderated_by : User_sql.userid;
}
    
val get_forum_by_name : string -> forum_info Lwt.t
val get_forum_by_id : Forum_sql.forum -> forum_info Lwt.t
  
(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  title:string -> 
  descr:string -> 
  moderated:bool -> 
  arborescent:bool -> 
  ?reader:User_sql.userid -> 
  ?writer:User_sql.userid -> 
  ?moderator:User_sql.userid -> 
  unit -> 
  forum_info Lwt.t

(** *)
(* remove
val can_read : forum_info -> Users.userdata -> bool
val can_write : forum_info -> Users.userdata -> bool
val can_moderate : forum_info -> Users.userdata -> bool
*)

val get_role : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  Forum_sql.forum -> Forum_sql.role Lwt.t
