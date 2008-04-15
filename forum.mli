(**
This is the forum component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
*)

type forum_data = {
  id : int;
  title : string;
  readable_by : Users.user;
  writable_by : Users.user;
  moderated_by : Users.user;
}
    
val get_forum_by_name : Sql.db_t -> string -> forum_data Lwt.t
val get_forum_by_id : Sql.db_t -> int -> forum_data Lwt.t
  
val can_read : forum_data -> Users.user -> bool
val can_write : forum_data -> Users.user -> bool
val can_moderate : forum_data -> Users.user -> bool

val new_forum : 
  Sql.db_t -> 
  title:string -> 
  descr:string -> 
  moderated:bool -> 
  arborescent:bool -> 
  ?reader:Users.user -> 
  ?writer:Users.user -> 
  ?moderator:Users.user -> 
  unit -> 
  Sql.db_int_t Lwt.t
