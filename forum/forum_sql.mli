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

module Types : sig

  (** Semi-abstract type for a forum *)
  type forum_arg = [ `Forum ]
  type forum = forum_arg Opaque.int32_t

  (** Semi-abstract type for a message or comment *)
  type message_arg = [ `Message ]
  type message = message_arg Opaque.int32_t

  val sql_of_message : message -> int32
  val message_of_sql : int32 -> message
  val sql_of_forum : forum -> int32
  val forum_of_sql : int32 -> forum
  val string_of_forum : forum -> string
  val forum_of_string : string -> forum
  val string_of_message : message -> string
  val message_of_string : string -> message

  type forum_info = {
    f_id: forum;
    f_title: string;
    f_descr: string;
    f_arborescent: bool;
    f_deleted: bool;
    f_messages_wiki: Wiki_types.wiki;
    f_comments_wiki: Wiki_types.wiki;
  }

  type message_info = {
    m_id: message;
    m_subject: string option;
    m_creator_id: User_sql.Types.userid;
    m_datetime: CalendarLib.Calendar.t;
    m_parent_id: message option;
    m_root_id: message;
    m_forum: forum;
    m_wikibox: Wiki_types.wikibox_uid;
    m_moderated: bool;
    m_sticky: bool;
    m_tree_min: int32;
    m_tree_max: int32;
  }

  type raw_forum_info
  type raw_message_info

  val get_forum_info : raw_forum_info -> forum_info
  val get_message_info : raw_message_info -> message_info

end
open Types

(** create a new forum. [?arborescent] is true by default. 
    Setting it to false will prevent to comment comments. *)
val new_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  messages_wiki:Wiki_types.wiki ->
  comments_wiki:Wiki_types.wiki ->
  unit ->
  forum Lwt.t

(** inserts a message in a forum. 
    [?moderated] and [?sticky] are false by default. *)
val new_message :
  sp:Eliom_sessions.server_params ->
  forum:forum ->
  wiki:Wiki_types.wiki ->
  creator_id:userid ->
  ?subject:string ->
  ?parent_id:message ->
  ?moderated:bool ->
  ?sticky:bool ->
  text:string ->
  message Lwt.t

(** set ou unset sticky flag on a message *)
val set_sticky :
  message_id:message -> sticky:bool -> unit Lwt.t
  
(** set or unset moderated flag on a message *)
val set_moderated :
  message_id:message -> moderated:bool -> unit Lwt.t
  
(** Get forum information, given its id or title.
    Information is: (forum id, title, description, arborescent, deleted)
*)
val get_forum: 
  ?not_deleted_only:bool ->
  ?forum:forum -> 
  ?title:string -> 
  unit -> 
  forum_info Lwt.t

(** returns the list of forums *)
val get_forums_list : ?not_deleted_only:bool -> unit ->
  raw_forum_info list Lwt.t
  
(** returns a message *)
val get_message : 
  message_id:message -> 
  unit ->
  message_info Lwt.t
  
(** returns a list of messages containing the message of id [~message_id]
    and all its children, ordered according depth first traversal of the tree.
    For each message, the information retrieved is:
    [(id, subject, author, datetime, parent_id, root_id, forum_id, wikibox, 
    moderated, sticky, tree_min, tree_max)]. 
    The list is not filtered and also contains deleted messages.
    The result is ordered according to tree_min.
*)
val get_thread : 
  message_id:message -> 
  unit ->
  raw_message_info list Lwt.t
  

(** returns the list of messages (without comments) in a forum. *)
val get_message_list : 
  forum:Types.forum ->
  first:int64 ->
  number:int64 ->
  moderated_only:bool ->
  unit ->
  raw_message_info list Lwt.t
  

