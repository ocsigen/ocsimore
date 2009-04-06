(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Université Paris Diderot - CNRS
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
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)





(** {2 Database access with verification of permissions} *)

(** create a new forum. [?arborescent] is true by default. 
    Setting it to false will prevent to comment comments.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val new_forum : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  Forum_sql.forum Lwt.t

(** inserts a message in a forum. 
    [?moderated] and [?sticky] are false by default.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val new_message :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  forum_id:Forum_sql.forum ->
  author_id:int32 ->
  ?subject:string ->
  ?parent_id:int32 ->
  ?sticky:bool ->
  text:string ->
  unit ->
  int32 Lwt.t

(** delete or undelete a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val set_deleted :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  message_id:int32 -> deleted:bool -> unit Lwt.t
  
(** set ou unset sticky flag on a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val set_sticky :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  message_id:int32 -> sticky:bool -> unit Lwt.t
  
(** set or unset moderated flag on a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val set_moderated :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  message_id:int32 -> moderated:bool -> unit Lwt.t
  
(** Get forum information, given its id or title.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val get_forum: 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  ?forum_id:Forum_sql.forum -> 
  ?title:string -> 
  unit -> 
  (Forum_sql.forum * string * string * bool * bool) Lwt.t

(** returns the list of forums visible to the user. *)
val get_forums_list : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  unit ->
  (Forum_sql.forum * string * string * bool * bool) list Lwt.t
  
(** returns id, subject, author, datetime, parent id, root id, forum id, text,
    and moderated, deleted, sticky status of a message.
    May fail with exception [Ocsimore_common.Permission_denied].
 *)
val get_message : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  message_id:int32 -> 
 (int32 * string option * int32 * CalendarLib.Calendar.t * int32 option * 
    int32 * int32 * string * bool * bool * bool) Lwt.t
  
(** returns a list of messages containing the message of id [~message_id]
    and all its children, ordered according depth first traversal of the tree.
    For each message, the information retrieved is:
    [(id, subject, author, datetime, parent_id, root_id, forum_id, text, 
    moderated, deleted, sticky)]. 
    If the user can read the first message but not comments, only
    the first message is returned.
    May fail with exception [Ocsimore_common.Permission_denied].
*)
val get_thread : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  message_id:int32 -> 
 (int32 * string option * int32 * CalendarLib.Calendar.t * int32 option *
    int32 * int32 * string * bool * bool * bool) list Lwt.t
  
