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


type forum

(** Role of user in the forum *)
type role = Moderator | Author of int32 | Lurker of string | Unknown;;

(** returns forum id *)
val get_id : forum -> int32

(** returns forum from id *)
val of_id : int32 -> forum

(** inserts a new forum *)
val new_forum : 
  title:string -> 
  descr:string -> 
  moderated:bool ->
  arborescent:bool -> 
  reader:int32 -> 
  writer:int32 ->
  moderator:int32 ->  
  forum Lwt.t

(** inserts a message starting a new thread; both thread and message
    will be hidden if forum is moderated *)
val new_thread_and_message :
  frm_id:forum ->
  author_id:int32 -> 
  subject:string -> 
  txt:string -> 
  (int32 * int32) Lwt.t

(** inserts a thread with an article; the thread will be hidden if the forum
    is moderated *)
val new_thread_and_article:
  frm_id:forum -> 
  author_id:int32 -> 
  subject:string -> txt:string ->
  (int32 * int32) Lwt.t

(** inserts a message for an existing thread; message will be hidden
    if forum is moderated *)
val new_message :
  thr_id:int32 ->
  ?parent_id:int32 -> 
  author_id:int32 ->
  txt:string -> 
  sticky:bool -> 
  unit -> 
  int32 Lwt.t

(** toggle moderation status of a forum *)
val forum_toggle_moderated : frm_id:forum -> unit Lwt.t
  
(** hides/shows a thread *)
val thread_toggle_hidden : 
  frm_id:forum -> thr_id:int32 -> unit Lwt.t
  
(** hides/shows a message *)
val message_toggle_hidden :
  frm_id:forum -> msg_id:int32 -> unit Lwt.t

(** makes a message sticky (or not) *)
val message_toggle_sticky: 
  frm_id:forum -> msg_id:int32 -> unit Lwt.t

(** Find forum information for a wiki, given its id or title *)
val find_forum: 
  ?id:forum -> 
  ?title:string -> 
  unit -> 
  (forum * string * string * bool * bool * 
     Users.group * Users.group * Users.group) Lwt.t

(** returns the list of available forums *)
val get_forums_list : unit ->
  (forum * string * string * bool * bool) list Lwt.t

(** returns id, title, description, moderation status, number of shown/hidden
    threads and messages of a forum.  
    NB: a message is counted as hidden if: 
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val forum_get_data: 
  frm_id:forum -> 
  role:role -> 
  (int32 * string * string * bool * int64 * int64 * int64 * int64) Lwt.t
 
(** returns the number of visible messages in a thread *)
val thread_get_nr_messages : 
  thr_id:int32 -> role:role -> int64 Lwt.t

(** returns id, subject, author, datetime, hidden status, number of shown/hidden
    messages of a thread.  
    NB: a message is counted as hidden if:
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val thread_get_data : 
  (* frm_id:forum -> *) 
  thr_id:int32 -> 
  role:role -> 
  (int32 * 
     string * 
     string * 
     string option * 
     CalendarLib.Calendar.t * 
     bool * 
     int64 * 
     int64) Lwt.t
  
(** returns id, text, author, datetime, hidden status of a message *)
val message_get_data : 
  frm_id:forum -> 
  msg_id:int32 -> 
 (int32 * string * string * CalendarLib.Calendar.t * bool) Lwt.t
  
(** returns None|Some id of prev & next thread in the same forum *)
val thread_get_neighbours :
  frm_id:forum ->  
  thr_id:int32 -> 
  role:role -> 
  (int32 option * int32 option) Lwt.t

(** returns None|Some id of prev & next message in the same thread *)
val message_get_neighbours :
  frm_id:forum ->  
  msg_id:int32 -> 
  role:role -> 
  (int32 option * int32 option) Lwt.t

(** returns the threads list of a forum, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (thr_id, subject, author, datetime,
    hidden status). *)
val forum_get_threads_list :
  frm_id:forum -> 
  ?offset:int64 -> 
  ?limit:int64 -> 
  role:role -> 
  unit ->
  (int32 * string * string * CalendarLib.Calendar.t * bool) list Lwt.t

val thread_get_messages_with_text :
  thr_id:int32 -> 
  ?offset:int64 -> 
  ?limit:int64 -> 
  role:role ->
  ?bottom:int32 -> 
  unit ->
  (forum * string * string * CalendarLib.Calendar.t * bool * bool) list Lwt.t
(** as above, but in tree form *)

val thread_get_messages_with_text_forest :
  thr_id:int32 -> 
  ?offset:int64 -> 
  ?limit:int64 ->
  ?top:int32 -> 
  ?bottom:int32 -> 
  role:role -> 
  unit ->
  (forum * 
     string * 
     string * 
     CalendarLib.Calendar.t * 
     bool * 
     bool * 
     int32 * 
     int32) Ocsimorelib.tree list Lwt.t

val get_latest_messages:
  frm_ids:forum list -> 
  limit:int64 -> 
  unit ->
  (forum * string * string) list Lwt.t

