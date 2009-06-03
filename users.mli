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
exception UnknownUser of string
exception UseAuth of userid

(** Non authenticated users *)
val anonymous : userid

(** A user that belongs to all groups *)
val admin : userid

(** A user/group that does not belong to any group,  and in which nobody
    can be.  *)
val nobody : userid
val nobody_login: string

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
    is never returned. Fails with [UnknownUser u] if the user
    [u] is not recognized *)
val user_list_of_string : string -> user list Lwt.t


(** Creates a new user or group with given parameters,
    or returns the existing user without modification
    if [name] is already present. *)
val create_user:
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  ?test:(sp:Eliom_sessions.server_params -> bool Lwt.t) ->
  unit ->
  userid Lwt.t


(** Same as above, except that the function will raise [BadUser] if
    the user already exists *)
val create_fresh_user:
  name:string ->
  pwd:pwd ->
  fullname:string ->
  ?email:string ->
  unit ->
  userid Lwt.t


val authenticate : name:string -> pwd:string -> userdata Lwt.t


val add_to_group : user:user -> group:user -> unit Lwt.t
val add_to_groups : user:user -> groups:user list -> unit Lwt.t

val add_list_to_group : l:user list -> group:user -> unit Lwt.t

val remove_list_from_group : l:user list -> group:user -> unit Lwt.t

(****)


val in_group :
  sp:Eliom_sessions.server_params ->
  ?user:user -> group:user -> unit -> bool Lwt.t

(** Informations on the loggued user *)

val get_user_data : sp:Eliom_sessions.server_params -> userdata Lwt.t
val get_user_id : sp:Eliom_sessions.server_params -> userid Lwt.t
val get_user_name : sp:Eliom_sessions.server_params -> string Lwt.t

val is_logged_on : sp:Eliom_sessions.server_params -> bool Lwt.t

val set_session_data : sp:Eliom_sessions.server_params -> userid -> unit Lwt.t





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
    prefix:string -> name:string -> descr:string ->
    'a admin_writer_reader

  val admin_writer_reader_groups:
    'a admin_writer_reader ->
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user) *
    ('a Opaque.int32_t -> user)


end


module GroupsForms : sig

  type input_string = [ `One of string ] Eliom_parameters.param_name
  type two_input_strings = input_string * input_string

  type 'a opaque_int32_eliom_param =
      [ `One of 'a Opaque.int32_t ] Eliom_parameters.param_name

  (** Auxiliary records containing all the needed information
      to define forms and services to edit the permissions of a parameterized
      group *)
  type 'a grp_helper = {
    (** The eliom arguments for the two string corresponding to the
        groups to add and remove into the parameterized group *)
    grp_eliom_params : (string * string, [ `WithoutSuffix ], two_input_strings)
      Eliom_parameters.params_type;

    (** The eliom argument for the 'a parameter *)
    grp_eliom_arg_param:
      ('a Opaque.int32_t, [ `WithoutSuffix ], 'a opaque_int32_eliom_param)
      Eliom_parameters.params_type;

    (** A function creating the controls that permit changing the
        permissions of the group^*)
    grp_form_fun:
      'a Opaque.int32_t ->
       string ->
       (two_input_strings -> Xhtmltypes_duce.inlines) Lwt.t;

    (** The hidden argument to use inside forms to specify which parameter
        is meant *)
    grp_form_arg:
      'a Opaque.int32_t ->
      'a opaque_int32_eliom_param ->
      Xhtmltypes_duce.inline_forms;

    (** The function saving the new permissions in the database *)
    grp_save: 'a Opaque.int32_t -> string * string -> unit Lwt.t
  }

  val helpers_group : string -> 'a parameterized_group -> 'a grp_helper


  type six_strings =(string * string) * ((string * string) * (string * string))
  type six_input_strings =
      two_input_strings * (two_input_strings * two_input_strings)

  (** Same thing with an admin_writer_reader structure *)
  type 'a awr_helper = {
    awr_eliom_arg_param:
      ('a Opaque.int32_t, [ `WithoutSuffix ], 'a opaque_int32_eliom_param)
      Eliom_parameters.params_type;

    awr_eliom_params: (six_strings, [`WithoutSuffix], six_input_strings)
      Eliom_parameters.params_type;

    awr_save: 'a Opaque.int32_t -> six_strings -> unit Lwt.t;

   (** Function creating the form to update the permissions *)
    awr_form_fun:
      'a Opaque.int32_t ->
       (six_input_strings -> Xhtmltypes_duce.inlines) Lwt.t;

    awr_form_arg:
      'a Opaque.int32_t ->
      'a opaque_int32_eliom_param ->
      Xhtmltypes_duce.inline_forms;
  }

  val helpers_admin_writer_reader :
    string -> 'a User_sql.Types.admin_writer_reader -> 'a awr_helper


  (** Helper forms to add and remove users from groups. If [show_edit]
      is false, no controls to edit the permissions are shown *)
  (** Form to add users to a group *)
  val form_edit_group:
    ?show_edit:bool ->
    group:user -> text:string ->
    (input_string * input_string -> Xhtmltypes_duce.inlines) Lwt.t

  (** Form to add an user to a group *)
  val form_edit_user:
    ?show_edit:bool ->
    user:user -> text:string ->
    (input_string * input_string -> Xhtmltypes_duce.inlines) Lwt.t


  (** Add the space separated groups in [add], and remove the
      space-separated groups in [remove] from the given [user] *)
  val add_remove_users_from_group:
    add:string -> remove:string -> group:user -> unit Lwt.t

  (** Add [user] to the space separated groups in [add], and removes
      [user] from the space-separated groups in [remove] *)
  val user_add_remove_from_groups:
    user:user -> add:string -> remove:string -> unit Lwt.t


end

