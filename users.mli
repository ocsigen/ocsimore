(** Module Users.

    Users, authentication, protection.

    In this model, users and groups are the same concept. A group can
    belong to another group. We only distinguish, for practical
    matters, between "login enabled" users and "group only" users: the
    former has [Some] (eventually void) password, the latter has
    [None].

*)
open User_sql.Types

exception NotAllowed
exception BadPassword
exception BadUser
exception UseAuth of userid

(** Non authenticated users *)
val anonymous : userid

(** A user that belongs to all groups *)
val admin : userid

(** A user/group that does not belong to any group,  and in which nobody
    can be.  *)
val nobody : userid

(** A group containing all authenticated users (not groups) *)
val authenticated_users : userid


(** Information about a user. Return [nobody] if the user
    does not currently exists, and raises [User_sql.NotBasicUser]
    if the user does not correspond to a basic user. *)
val get_basicuser_by_login : string -> userid Lwt.t

(** Returns the user that corresponds to a given string
    (inverse of the function [User_sql.user_to_string],
    or nobody if the user does not exists *)
val get_user_by_name: string -> user Lwt.t

(** Convert a list of string representation of users into the
    corresponding users, according to [get_user_by_name]. Nobody
    is never returned. Fails with [Failure u] if the user
    [u] is not recognized *)
val user_list_of_string : string -> user list Lwt.t


(** Creates a new user with given parameters,
    or returns the existing user without modification
    if [name] is already present. *)
val create_user:
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  groups:user list ->
  ?test:(sp:Eliom_sessions.server_params ->
          sd:Ocsimore_common.session_data -> bool Lwt.t) ->
  unit ->
  userid Lwt.t


val create_unique_user: (* XXX Buggy, will fail if all name-related logins are used *)
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  groups:user list ->
  (userid * string) Lwt.t


(* BY 2009-03-13: deactivated because update_data is deactivated. See this file
val update_user_data:
  user:userdata ->
  ?pwd:pwd ->
  ?fullname:string ->
  ?email:string option ->
  ?groups: userid list ->
  unit ->
  unit Lwt.t
*)

val authenticate : name:string -> pwd:string -> userdata Lwt.t


val add_to_group : user:user -> group:user -> unit Lwt.t

(****)


val in_group :
  (?user:user -> group:user -> unit -> bool Lwt.t) Ocsimore_common.sd_sp

(** Informations on the loggued user *)

val get_user_data : userdata Lwt.t Ocsimore_common.sd_sp
val get_user_id : userid Lwt.t Ocsimore_common.sd_sp
val get_user_name : string Lwt.t Ocsimore_common.sd_sp

val is_logged_on : bool Lwt.t Ocsimore_common.sd_sp

val set_session_data : (userid -> unit Lwt.t) Ocsimore_common.sd_sp


val anonymous_sd : Ocsimore_common.session_data



module GenericRights : sig
  (** Helper functions and definitions to define
      [User_sql.Types.admin_writer_reader] objects *)

  type admin_writer_reader_access =
      { field : 'a. 'a admin_writer_reader -> 'a parameterized_group }

  val grp_admin: admin_writer_reader_access
  val grp_write: admin_writer_reader_access
  val grp_read:  admin_writer_reader_access

  val can_sthg: (admin_writer_reader_access -> 'a) -> 'a * 'a * 'a

  val create_admin_writer_reader:
    name:string -> descr:string -> 'a admin_writer_reader

  val admin_writer_reader_groups:
    'a admin_writer_reader ->
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user)

end
