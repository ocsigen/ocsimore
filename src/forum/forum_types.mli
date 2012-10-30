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

open Eliom_content
open User_sql.Types

  (** Semi-abstract type for a forum *)
  type forum_arg = [ `Forum ]
  type forum = forum_arg Opaque.int32_t

  (** Semi-abstract type for a message or comment *)
  type message_arg = [ `Message ]
  type message = message_arg Opaque.int32_t

  val sql_of_message_option : message option -> int32 option
  val message_of_sql_option : int32 option -> message option
  val sql_of_message : message -> int32
  val message_of_sql : int32 -> message
  val sql_of_forum : forum -> int32
  val forum_of_sql : int32 -> forum
  val sql_of_forum_option : forum option -> int32 option
  val forum_of_sql_option : int32 option -> forum option
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
    f_title_syntax: Html5_types.phrasing Html5.F.elt list Wiki_types.content_type;
    f_messages_wiki: Wiki_types.wiki;
    f_comments_wiki: Wiki_types.wiki;
  }

  type message_info = {
    m_id: message;
    m_creator_id: User_sql.Types.userid;
    m_datetime: CalendarLib.Calendar.t;
    m_parent_id: message option;
    m_root_id: message;
    m_forum: forum;
    m_subject: Wiki_types.wikibox option;
    m_wikibox: Wiki_types.wikibox;
    m_moderated: bool;
    m_has_special_rights: bool Lwt.t Lazy.t;
    m_tree_min: int32;
    m_tree_max: int32;
  }

  val get_forum_info :
    < arborescent : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t > Sql.t;
    comments_wiki : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    deleted : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t > Sql.t;
    descr : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t;
    id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    messages_wiki : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    title : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t;
    title_syntax : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t >
      -> forum_info
  val get_message_info :
    < creator_id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    datetime : < get : unit; nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t;
    forum_id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    moderated : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t > Sql.t;
    parent_id : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t;
    root_id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    special_rights : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t > Sql.t;
    subject : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t;
    tree_max : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    tree_min : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
    wikibox : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t >
      -> message_info

  type ('a,'b,'c,'d) forum_services = {
    add_message_service : 'a;
    moderate_message_service : 'b;
(* See the comment in forum_widgets.eliom ligne 100 *)
(*    thread_feed_service : 'c;
    forum_feed_service : 'd;*)
  }
