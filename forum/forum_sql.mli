(* Ocsimore
 * Copyright (C) 2005
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
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
   @author Boris Yakobowski
*)

open User_sql.Types

type forum = int32

(** returns forum id *)
val get_id : forum -> int32

(** returns forum from id *)
val of_id : int32 -> forum

(** returns forum id as a string *)
val forum_id_s : forum -> string

(** create a new forum. [?arborescent] is true by default. 
    Setting it to false will prevent to comment comments. *)
val new_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  forum Lwt.t

(** inserts a message in a forum. 
    [?moderated] and [?sticky] are false by default. *)
val new_message :
  forum_id:forum ->
  author_id:userid ->
  ?subject:string ->
  ?parent_id:int32 ->
  ?moderated:bool ->
  ?sticky:bool ->
  text:string ->
  int32 Lwt.t

(** delete or undelete a message *)
val set_deleted :
  message_id:int32 -> deleted:bool -> unit Lwt.t
  
(** set ou unset sticky flag on a message *)
val set_sticky :
  message_id:int32 -> sticky:bool -> unit Lwt.t
  
(** set or unset moderated flag on a message *)
val set_moderated :
  message_id:int32 -> moderated:bool -> unit Lwt.t
  
(** Get forum information, given its id or title.
    Information is: (forum id, title, description, arborescent, deleted)
*)
val get_forum: 
  ?not_deleted_only:bool ->
  ?forum_id:forum -> 
  ?title:string -> 
  unit -> 
  (forum * string * string * bool * bool) Lwt.t

(** returns the list of forums *)
val get_forums_list : ?not_deleted_only:bool -> unit ->
  (forum * string * string * bool * bool) list Lwt.t
  
(** returns id, subject, author, datetime, parent id, root id, forum id, text,
    and moderated, deleted, sticky status of a message *)
val get_message : 
  ?not_deleted_only:bool ->
  message_id:int32 -> 
  unit ->
 (int32 * string option * userid * CalendarLib.Calendar.t * int32 option * 
    int32 * int32 * string * bool * bool * bool * int32 * int32) Lwt.t
  
(** returns a list of messages containing the message of id [~message_id]
    and all its children, ordered according depth first traversal of the tree.
    For each message, the information retrieved is:
    [(id, subject, author, datetime, parent_id, root_id, forum_id, text, 
    moderated, deleted, sticky, tree_min, tree_max)]. 
    The list is not filtered and also contains deleted messages.
    The result is ordered according to tree_min.
*)
val get_thread : 
  message_id:int32 -> 
  unit ->
 (int32 * string option * userid * CalendarLib.Calendar.t * int32 option *
    int32 * int32 * string * bool * bool * bool * int32 * int32) list Lwt.t
  

