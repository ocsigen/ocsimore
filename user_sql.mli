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

(** The abstract type of user ids *)
type userid = int32

val new_user: 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string option -> 
  groups:userid list ->
  userid Lwt.t


(**/**)
(* DO NOT USE THE FOLLOWING BUT THOSE IN user_cache.ml *)


(** Returns the groups for one user (level 1) *)
val get_groups_ : userid:userid -> userid list Lwt.t

val find_user_: 
  ?db:Sql.db_t ->
  ?id:userid -> 
  ?name:string -> 
  unit -> 
  ((userid * string * string option * string * string option) * 
     userid list) Lwt.t

val update_data_: 
  userid:userid -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string option -> 
  ?groups:userid list ->
  unit ->
  unit Lwt.t

val add_to_group_ : userid:userid -> groupid:userid -> unit Lwt.t

val remove_from_group_ : userid:userid -> groupid:userid -> unit Lwt.t

val delete_user_ : userid:userid -> unit Lwt.t

