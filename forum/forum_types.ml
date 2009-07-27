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
open Forum_sql0

let (>>=) = Lwt.bind



  (** Semi-abstract type for a forum *)
  type forum_arg = [ `Forum ]
  type forum = forum_arg Opaque.int32_t

  (** Semi-abstract type for a message or comment *)
  type message_arg = [ `Message ]
  type message = message_arg Opaque.int32_t

  type forum_info = {
    f_id: forum;
    f_title: string;
    f_descr: string;
    f_arborescent: bool;
    f_deleted: bool;
    f_title_syntax: Xhtmltypes_duce.inlines Wiki_types.content_type;
    f_messages_wiki: Wiki_types.wiki;
    f_comments_wiki: Wiki_types.wiki;
  }

  let forum_of_sql (u : int32) = (Opaque.int32_t u : forum)
  let sql_of_forum (u : forum) = Opaque.t_int32 u
  let message_of_sql (u : int32) = (Opaque.int32_t u : message)
  let sql_of_message (u : message) = Opaque.t_int32 u

  let forum_of_sql_option (u : int32 option) = 
    (Opaque.int32_t_option u : forum option)
  let sql_of_forum_option (u : forum option) = Opaque.t_int32_option u
  let message_of_sql_option (u : int32 option) = 
    (Opaque.int32_t_option u : message option)
  let sql_of_message_option (u : message option) = Opaque.t_int32_option u

  let string_of_forum i = Int32.to_string (sql_of_forum i)
  let forum_of_string s = (Opaque.int32_t (Int32.of_string s) : forum)
  let string_of_message i = Int32.to_string (sql_of_message i)
  let message_of_string s = (Opaque.int32_t (Int32.of_string s) : message)

  type raw_forum_info = (int32 * string * string * bool * bool * string * int32 * int32)

  let get_forum_info
      (id,
       title,
       descr,
       arborescent,
       deleted,
       title_syntax,
       messages_wiki,
       comments_wiki)
      = 
      {
        f_id = forum_of_sql id;
        f_title = title;
        f_descr = descr;
        f_arborescent = arborescent;
        f_deleted = deleted;
        f_title_syntax = Wiki_types.content_type_of_string title_syntax;
        f_messages_wiki = Wiki_types.wiki_of_sql messages_wiki;
        f_comments_wiki = Wiki_types.wiki_of_sql comments_wiki;
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
    m_sticky: bool;
    m_has_special_rights: bool Lwt.t Lazy.t;
    m_tree_min: int32;
    m_tree_max: int32;
  }

  type raw_message_info =
      (int32 * int32 * CalendarLib.Calendar.t * int32 option *
         int32 * int32 * int32 option * int32 * bool * bool * bool
       * int32 * int32)

  let get_message_info
      (id,
       creator_id,
       datetime,
       parent_id,
       root_id,
       forum_id,
       subject,
       wikibox,
       moderated,
       sticky,
       has_special_rights,
       tree_min,
       tree_max) =
    {
      m_id = message_of_sql id;
      m_creator_id = User_sql.Types.userid_from_sql creator_id;
      m_datetime = datetime;
      m_parent_id = message_of_sql_option parent_id;
      m_root_id = message_of_sql root_id;
      m_forum = forum_of_sql forum_id;
      m_subject = (match subject with
                     | None -> None 
                     | Some s -> Some (Wiki_types.wikibox_of_sql s));
      m_wikibox = Wiki_types.wikibox_of_sql wikibox;
      m_moderated = moderated;
      m_sticky = sticky;
      m_has_special_rights = 
        lazy (if root_id = id (* root *)
              then Lwt.return has_special_rights
              else begin
                get_message_raw ~message_id:root_id ()
                >>= fun (_, _, _, _, _, _, _, _, _, _,
                         has_special_rights, _, _) ->
                Lwt.return has_special_rights
              end);
      m_tree_min = tree_min;
      m_tree_max = tree_max;
    }


