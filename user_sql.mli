val new_user: 
  Sql.db_t -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  int32 Lwt.t

val find_user: 
  Sql.db_t -> 
  ?id:int32 -> 
  ?name:string -> 
  unit -> 
  (int32 * string * string option * string * string * string option) Lwt.t

val update_data: 
  Sql.db_t -> 
  id:int32 -> 
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

