(** type of [~role] labelled parameter *)
type role = Moderator | Author of Sql.db_int_t | Lurker of string | Unknown;;

val new_user: 
  Sql.db_t -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  Sql.db_int_t Lwt.t

val find_user: 
  Sql.db_t -> 
  ?id:Sql.db_int_t -> 
  ?name:string -> 
  unit -> 
  (Sql.db_int_t * string * string option * string * string * string option) Lwt.t

val update_data: 
  Sql.db_t -> 
  id:Sql.db_int_t -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  unit Lwt.t

val update_permissions: 
  Sql.db_t -> 
  name:string -> 
  perm:string -> 
  unit Lwt.t

