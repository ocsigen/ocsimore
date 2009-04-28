(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vi32cent Balat
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
open User_sql.Types

val get_user_by_id:   userid -> userdata Lwt.t
val get_user_by_name: string -> userdata Lwt.t


(** Returns the groups for one user (level 1) *)
val get_groups : userid:userid -> userid list Lwt.t


val add_to_group: userid:userid -> groupid:userid -> unit Lwt.t

val remove_from_group: userid:userid -> groupid:userid -> unit Lwt.t

val delete_user: userid:userid -> unit Lwt.t



(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
val update_data:
  userid:User_sql.userid ->
  password:User_sql.pwd ->
  fullname:string ->
  email:string option ->
  ?groups:User_sql.userid list ->
  unit ->
  unit Lwt.t
*)
