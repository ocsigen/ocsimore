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


type forum_info = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  arborescent: bool;
  deleted: bool;
}

(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  forum_info Lwt.t

val forum_visible_group : int32 -> User_sql.userid Lwt.t

val forum_creators : Users.userdata

(** {2 Session data} *)

type role = 
    {
      message_writers : bool Lwt.t Lazy.t;
      message_writers_notmod : bool Lwt.t Lazy.t;
      message_moderators : bool Lwt.t Lazy.t;
      message_deletors : bool Lwt.t Lazy.t;
      message_deletors_if_author : bool Lwt.t Lazy.t;
      message_sticky_setters : bool Lwt.t Lazy.t;
      message_readers : bool Lwt.t Lazy.t;

      comment_writers : bool Lwt.t Lazy.t;
      comment_writers_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_author : bool Lwt.t Lazy.t;
      comment_sticky_setters : bool Lwt.t Lazy.t;
      comment_readers : bool Lwt.t Lazy.t;

      writers : bool Lwt.t Lazy.t;
      writers_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_author : bool Lwt.t Lazy.t;
      sticky_setters : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

val get_role : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  Forum_sql.forum -> role Lwt.t



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_sql.forum * int32 option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_sql.forum * int32 option (* parent *))

exception Forum_action_info of forum_action_info
