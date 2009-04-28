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
  type userid = [`User ] Opaque.int32_t

  val user_from_sql : int32 -> userid
  val sql_from_user : userid -> int32

  val userid_s: userid -> string
  val s_userid: string -> userid

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
  groups:userid list ->
  dyn:bool ->
  (userid * pwd) Lwt.t


(**/**)
(* DO NOT USE THE FOLLOWING BUT THOSE IN user_cache.ml *)


(** Returns the groups for one user (level 1) *)
val get_groups_ : userid:userid -> userid list Lwt.t

val find_user_by_name_:
  string ->
  userdata Lwt.t

val find_user_by_id_:
  userid ->
  userdata Lwt.t

(* BY 2009-03-13: deactivated. See .ml *)
(*
val update_data_:
  userid:userid ->
  password:pwd ->
  fullname:string ->
  email:string option ->
  ?groups:userid list ->
  ?dyn:bool ->
  unit ->
  unit Lwt.t
*)

val add_to_group_ : userid:userid -> groupid:userid -> unit Lwt.t

val remove_from_group_ : userid:userid -> groupid:userid -> unit Lwt.t

val delete_user_ : userid:userid -> unit Lwt.t

