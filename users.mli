
(** Module Users.
    
    Users, authentication, protection. 
    
    In this model, users and groups are the same concept; we only
    distinguish, for practical matters, between "login enabled" users
    and "group only" users: the former has [Some] (eventually void)
    password, the latter has [None]. *)


exception Loop
exception UserExists
exception NotAllowed
exception BadPassword
exception NoSuchUser


(** The abstract type of users. *)
type user

val anonymous : user

val admin : user

val get_user_by_name : name:string -> user Lwt.t

val generate_password: unit -> string option

val mail_password: 
  name:string -> 
  from_addr:string * string -> 
  subject: string -> 
  bool Lwt.t

(** Creates a new user with given parameters. 
    Raises {!Users.UserExists} if [name] is already present. *)
val create_user: 
  name:string -> 
  pwd:string option -> 
  fullname:string -> 
  email:string -> 
  user Lwt.t

val create_unique_user: 
  name:string -> 
  pwd:string option -> 
  fullname:string -> 
  email:string -> 
  (user * string) Lwt.t

val get_user_data: user:user -> int32 * string * string option * string * string

val update_user_data: 
  user:user -> 
  ?pwd:string option -> 
  ?fullname:string -> 
  ?email:string -> 
  unit -> 
  unit Lwt.t

val authenticate : name:string -> pwd:string -> user Lwt.t

val delete_user : user:user -> unit Lwt.t

val in_group: user:user -> group:user -> bool

val add_group : user:user -> group:user -> unit Lwt.t



