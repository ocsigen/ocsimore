
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

val get_user_by_name: Sql.db_t -> name:string -> user Lwt.t

val generate_password: unit -> string option

val mail_password: Sql.db_t -> name:string -> from_addr:string * string -> subject: string -> bool Lwt.t

(** Creates a new user with given parameters. 
    Raises {!Users.UserExists} if [name] is already present. *)
val create_user: Sql.db_t -> name:string -> pwd:string option -> fullname:string -> email:string -> user Lwt.t

val create_unique_user: Sql.db_t -> name:string -> pwd:string option -> fullname:string -> email:string -> (user * string) Lwt.t

val get_user_data: user:user -> int * string * string option * string * string

val update_user_data: Sql.db_t -> user:user -> ?pwd:string option -> ?fullname:string -> ?email:string -> unit -> unit Lwt.t

val authenticate: Sql.db_t -> name:string -> pwd:string -> user Lwt.t

val delete_user: Sql.db_t -> user:user -> unit Lwt.t

val in_group: user:user -> group:user -> bool

val add_group: Sql.db_t -> user:user -> group:user -> unit Lwt.t


val create_standard_users: Sql.db_t -> unit Lwt.t

