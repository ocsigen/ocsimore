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

(** The abstract type of groups *)
type groupid = int32

val new_user: 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  groups:groupid list ->
  userid Lwt.t

val delete_user : user:userid -> unit Lwt.t

val new_group: 
  name:string -> 
  groupid Lwt.t

val find_user: 
  ?db:Sql.db_t ->
  ?id:userid -> 
  ?name:string -> 
  unit -> 
  ((userid * string * string option * string * string) * groupid list) Lwt.t

val update_data: 
  id:userid -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  ?groups:groupid list ->
  unit ->
  unit Lwt.t

val add_to_group : userid:userid -> groupid:groupid -> unit Lwt.t

val remove_from_group : userid:userid -> groupid:groupid -> unit Lwt.t

(** Returns all groups (names and ids) from the database *)
val get_groups : unit -> (groupid * string) list Lwt.t
