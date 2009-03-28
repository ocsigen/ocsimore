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
  readonly: bool;
}

(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  forum_info Lwt.t



(** {2 Session data} *)

type role = 
    {
      messages_writers : bool;
      messages_writers_notmod : bool;
      messages_moderators : bool;
      messages_deletors : bool;
      messages_sticky_setters : bool;
      messages_readers : bool;

      comments_writers : bool;
      comments_writers_notmod : bool;
      comments_moderators : bool;
      comments_deletors : bool;
      comments_sticky_setters : bool;
      comments_readers : bool;

      writers : bool;
      writers_notmod : bool;
      moderators : bool;
      deletors : bool;
      sticky_setters : bool;
      readers : bool;

      forum_admin : bool;
    }

val get_role : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  Forum_sql.forum -> role Lwt.t



(*

    
val get_forum_by_name : string -> forum_info Lwt.t
val get_forum_by_id : Forum_sql.forum -> forum_info Lwt.t
  

(** *)
(* remove
val can_read : forum_info -> Users.userdata -> bool
val can_write : forum_info -> Users.userdata -> bool
val can_moderate : forum_info -> Users.userdata -> bool
*)

*)
