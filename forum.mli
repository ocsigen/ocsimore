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
  readable_by : Users.group;
  writable_by : Users.group;
  moderated_by : Users.group;
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
  ?reader:Users.group -> 
  ?writer:Users.group -> 
  ?moderator:Users.group -> 
  unit -> 
  forum_info Lwt.t

(** *)
val can_read : forum_info -> Users.user -> bool
val can_write : forum_info -> Users.user -> bool
val can_moderate : forum_info -> Users.user -> bool

val get_role : 
  Session_manager.sessionmanager -> Forum_sql.forum -> Forum_sql.role Lwt.t
