(**
This is the forum component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
*)

type forum = {
  id : Forum_sql.forum;
  title : string;
  descr: string;
  readable_by : Users.user;
  writable_by : Users.user;
  moderated_by : Users.user;
}
    
val get_forum_by_name : string -> forum Lwt.t
val get_forum_by_id : Forum_sql.forum -> forum Lwt.t
  
val can_read : forum -> Users.user -> bool
val can_write : forum -> Users.user -> bool
val can_moderate : forum -> Users.user -> bool

(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  title:string -> 
  descr:string -> 
  moderated:bool -> 
  arborescent:bool -> 
  ?reader:Users.user -> 
  ?writer:Users.user -> 
  ?moderator:Users.user -> 
  unit -> 
  forum Lwt.t

(** *)
val get_role : 
  Session_manager.sessionmanager -> 
  Forum_sql.forum -> 
  Forum_sql.role Lwt.t
