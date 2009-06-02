(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

module Types : sig

  (** The abstract type of user ids *)
  type userid

  val userid_from_sql : int32 -> userid
  val sql_from_userid : userid -> int32

  type pwd =
    | Connect_forbidden
    | Ocsimore_user_plain of string
    | Ocsimore_user_crypt of string
    | External_Auth

  type userdata = {
    user_id: userid;
    user_login: string;
    mutable user_pwd: pwd;
    mutable user_fullname: string;
    mutable user_email: string option;
    user_dyn: bool;
    user_kind: [`BasicUser | `ParameterizedGroup | `NonParameterizedGroup];
  }


  type 'a parameterized_group

  type user

  val apply_parameterized_group:
    'a parameterized_group ->'a Opaque.int32_t -> user
  val ($) : 'a parameterized_group ->'a Opaque.int32_t -> user
  val basic_user : userid -> user


  type 'a admin_writer_reader = {
    grp_admin: 'a parameterized_group;
    grp_writer: 'a parameterized_group;
    grp_reader: 'a parameterized_group;
  }

end
open Types


(** Creates a user. The password passed as argument must be unencrypted.
    Returns the user id and its password after an eventual encryption. *)
val new_user:
  name:string ->
  password:pwd ->
  fullname:string ->
  email:string option ->
  dyn:bool ->
  (userid * pwd) Lwt.t


val new_parameterized_group:
  prefix:string ->
  name:string ->
  fullname:string ->
  'a parameterized_group Lwt.t

val new_nonparameterized_group:
  prefix:string ->
  name:string ->
  fullname:string ->
  user Lwt.t



exception NotBasicUser of userdata

(* Returns the id of the user whose login is passed as argument. Raises
   [NotBasicUser] if the resulting user is not a basic user. *)
val get_basicuser_by_login: string -> userid Lwt.t

val get_basicuser_data : userid -> userdata Lwt.t
val get_parameterized_user_data: 'a parameterized_group -> userdata Lwt.t
val get_user_data : user -> userdata Lwt.t


(** Returns the groups in which a user is directly included *)
val groups_of_user : user:user -> user list Lwt.t

(** Returns the users or groups inside a group. If [generic] is false,
    inclusions coming from generic edges are not returned *)
val users_in_group : ?generic:bool -> group:user -> user list Lwt.t


val add_to_group: user:user -> group:user -> unit Lwt.t
val remove_from_group: user:user -> group:user -> unit Lwt.t

val add_generic_inclusion :
  subset:'a parameterized_group -> superset:'a parameterized_group -> unit Lwt.t

val delete_user: userid:userid -> unit Lwt.t



val update_data:
  userid:userid ->
  ?password:pwd ->
  ?fullname:string ->
  ?email:string ->
  ?dyn:bool ->
  unit ->
  unit Lwt.t


(** Converts an [userid] to a string, by giving the corresponding
    login field. Raises [Not_found] if the user does not exists. *)
val userid_to_string: userid -> string Lwt.t

(** Converts an user to a string. Basic users are converted as
    per [userid_to_string]. Groups are written  [#group(val)]
    where [group] is the name used at the creation
    of the group, and val is the [int32] parameter of the group *)
val user_to_string: user -> string Lwt.t

(** Returns the user that corresponds to a given string
    (inverse of the function [user_to_string], or raises
    [Not_found] if the user does not exists *)
val get_user_by_name: string -> user Lwt.t


(** Returns a list of all the existing users and groups. *)
val all_groups : unit -> userdata list Lwt.t
