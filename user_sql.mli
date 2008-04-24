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


val new_user: 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  groups:int32 list ->
  int32 Lwt.t

val delete_user : user:int32 -> unit Lwt.t

val new_group: 
  name:string -> 
  int32 Lwt.t

val find_user: 
  ?db:Sql.db_t ->
  ?id:int32 -> 
  ?name:string -> 
  unit -> 
  ((int32 * string * string option * string * string) * int32 list) Lwt.t

val update_data: 
  id:int32 -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string -> 
  ?groups:int32 list ->
  unit ->
  unit Lwt.t

val add_to_group : userid:int32 -> groupid:int32 -> unit Lwt.t

val remove_from_group : userid:int32 -> groupid:int32 -> unit Lwt.t

(** Returns all groups (names and ids) from the database *)
val get_groups : unit -> (int32 * string) list Lwt.t
