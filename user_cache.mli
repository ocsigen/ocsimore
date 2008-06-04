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

val find_user: 
  ?id:User_sql.userid -> 
  ?name:string -> 
  unit -> 
  ((User_sql.userid * string * string option * string * string option * bool) * 
     User_sql.userid list) Lwt.t

(** Returns the groups for one user (level 1) *)
val get_groups : userid:User_sql.userid -> User_sql.userid list Lwt.t

val update_data: 
  userid:User_sql.userid -> 
  name:string -> 
  password:string option -> 
  fullname:string -> 
  email:string option -> 
  ?groups:User_sql.userid list ->
  unit ->
  unit Lwt.t

val add_to_group : userid:User_sql.userid -> groupid:User_sql.userid -> unit Lwt.t

val remove_from_group : userid:User_sql.userid -> groupid:User_sql.userid -> unit Lwt.t

val delete_user : userid:User_sql.userid -> unit Lwt.t

