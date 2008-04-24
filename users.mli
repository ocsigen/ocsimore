
(** Module Users.
    
    Users, authentication, protection. 
    
    In this model, users and groups are the same concept; we only
    distinguish, for practical matters, between "login enabled" users
    and "group only" users: the former has [Some] (eventually void)
    password, the latter has [None]. *)


exception UserExists
exception NotAllowed
exception BadPassword
exception NoSuchUser


(** The abstract type of users. *)
type user

(** user information *)
type userdata = 
    { id: int32;
      name: string;
      mutable pwd: string option;
      mutable fullname: string;
      mutable email: string;
      mutable groups: int32 list }

(** The abstract type of groups *)
type group

val anonymous : user
val admin : user

val anonymous_group : group
val admin_group : group

val get_user_by_name : name:string -> user Lwt.t

val get_group : name:string -> group
val get_group_name : group -> string

val group_of_id : int32 -> group
val id_of_group : group -> int32

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
  groups:group list ->
  user Lwt.t

val create_unique_user: 
  name:string -> 
  pwd:string option -> 
  fullname:string -> 
  email:string -> 
  groups:group list ->
  (user * string) Lwt.t

val delete_user : user:user -> unit Lwt.t

val get_user_data: user:user -> userdata

val update_user_data: 
  user:user -> 
  ?pwd:string option -> 
  ?fullname:string -> 
  ?email:string -> 
  ?groups:group list ->
  unit -> 
  unit Lwt.t

val authenticate : name:string -> pwd:string -> user Lwt.t

val in_group: user:user -> group:group -> bool

val add_to_group : user:user -> group:group -> unit Lwt.t

(** Creates a group if it does not exist. Returns its value in all cases. *)
val create_group : name:string -> group Lwt.t
