
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

(** Creates a new user with given parameters. 
    Raises {!Users.UserExists} if [name] is already present. *)
val create_user: name:string -> pwd:string option -> desc:string -> email:string -> user

(** Same as [create_user], but may add a random suffix to [name] for
    failure avoidance. Returns the new user and its name. *)
val create_unique_user: name:string -> pwd:string option -> desc:string -> email:string -> user * string

(** Gets user info. *)
val get_user_data : user:user -> string * string option * string * string 
  (** Return value is {i (name, password, description, e-mail address)}.*)

(** Updates user info. *)
val update_user_data : user:user -> ?pwd:string option -> ?desc:string -> ?email:string -> unit -> unit
  (** Raises {!Users.NotAllowed} if [user] is the anonymous user.
      Note: omission of an optional parameter stands for {e "do not
      change current value"}. *)

(** Deletes a [user]. *)
val delete_user : user:user -> unit

(** Returns [true] iif [user] is in [group]. *)
val in_group : user:user -> group:user -> bool

(** Adds [user] to [group].
    Raises {!Users.Loop} when attempting to make cyclic group membership. *)
val add_group : user:user -> group:user -> unit

(** Removes [user] from [group]. *)
val remove_group : user:user -> group:user -> unit

(** The anonymous user *)
val anonymous : unit -> user

  (** {3 Authentication} *)

(** Authentication function. *)
val authenticate : name:string -> pwd:string -> user Lwt.t

(** Automatic password generation. *)
val generate_password : unit -> string option

(** Password reminder via e-mail. *)
val mail_password : name:string -> from_addr:string * string -> subject:string -> bool
  (** If a user named [name] exists and has some e-mail address,
      [mail-password ~name ~from_addr ~subject] sends an e-mail message to
      such address and returns [true]; otherwise, returns [false]. *)



  (** {3 Protection of values} *)

(** Input signature for the functor {!Users.Protect} *)
module type T = 
sig 
  type t (** type of the value to be protected *)
  val value : t (** the value *)
  val group : user (** group of allowed users *)
end

(** Protect a value given its type and a group of allowed users.  Not
    so useful, as you could embed the 'in_group' test in your function
    code, wit no use of this functor... *)
module Protect :  functor (A: T) -> 
sig

  (** [f ~actor] returns [A.value] if [actor] is in [A.group];
      otherwise it raises [NotAllowed]. *)
  val f : actor:user -> A.t 

end
  
(** In your module, you can write something like this:
    {[
    let your_function = ...
    let your_group = create_user ~name:"mygroup"
    let your_protected_function = 
    let module M = Protect(
    struct 
      type t = type_of_your_function
      let value = your_function
      let group = your_group
    end) in M.f
    ]}
    and declare [your_protected_function] in the interface. *)



  (** {7 Some useful protected functions} *)

(** These functions are available only to the Administrator (i.e.,
    [~actor] must be the user with name ["root"]), as they return
    values of type [user].  All other ordinary users can get a value
    of type [user] by calling {!Users.create_user} or
    {!Users.authenticate} only (and {!Users.anonymous}, of course...). *)

(** Gets a user by [name]. *)
val get_user_by_name: actor:user -> name:string -> user option

(** Gets the list of groups a [user] belongs to. *)
val get_user_groups: actor:user -> user:user -> user list
  (** Note: the users structure won't be flattened, so if [u] belongs
      to [g1],[g2] and [g1] belongs to [g3], return value is
      [[g1;g2]], {b not} [[g1;g2;g3]]. *)
