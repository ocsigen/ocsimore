(** Database interface. 
    All SQL commands go here. *)

open Ocsimorelib
open CalendarLib

type db_t = (string, bool) Hashtbl.t Lwt_PGOCaml.t

val uuid_of_conn: db_t Lwt.t -> string

val connect: unit -> db_t Lwt.t

(** type for database integers (and IDs) *)
type db_int_t;;

(** type for database offsets, and limits *)
type db_size_t;;

(** type for counts *)
type db_count_t;;

val db_int_of_int: int -> db_int_t
val int_of_db_int: db_int_t -> int
val db_int_of_string: string -> db_int_t
val string_of_db_int: db_int_t -> string
val db_size_of_int: int -> db_size_t
val db_count_of_int: int -> db_count_t

(** type of [~role] labelled parameter *)
type role = Moderator | Author of db_int_t | Lurker of string | Unknown;;

(** {4  Users} *)

(** {4 Forums} *)
val new_user: db_t -> name:string -> password:string option -> fullname:string -> email:string -> db_int_t Lwt.t

val find_user: db_t -> ?id:db_int_t -> ?name:string -> unit -> (db_int_t * string * string option * string * string * string option) Lwt.t

val update_data: db_t -> id:db_int_t -> name:string -> password:string option -> fullname:string -> email:string -> unit Lwt.t

val update_permissions: db_t -> name:string -> perm:string -> unit Lwt.t

(** inserts a new forum *)
val new_forum : db_t -> title:string -> descr:string -> moderated:bool ->
   arborescent:bool -> reader:db_int_t -> writer:db_int_t ->
   moderator:db_int_t ->  db_int_t Lwt.t

(** inserts a message starting a new thread; both thread and message
    will be hidden if forum is moderated *)
val new_thread_and_message :
  db_t -> frm_id:db_int_t ->
  author_id:db_int_t -> subject:string -> txt:string -> (db_int_t * db_int_t) Lwt.t

(** inserts a thread with an article; the thread will be hidden if the forum
    is moderated *)
val new_thread_and_article:
	db_t -> frm_id:db_int_t -> author_id:db_int_t -> subject:string -> txt:string ->
	(db_int_t * db_int_t) Lwt.t

(** inserts a message for an existing thread; message will be hidden
    if forum is moderated *)
val new_message :
  db_t -> thr_id:db_int_t -> ?parent_id:db_int_t -> author_id:db_int_t ->
	txt:string -> sticky:bool -> unit -> db_int_t Lwt.t

(** toggle moderation status of a forum *)
val forum_toggle_moderated : db_t -> frm_id:db_int_t -> unit Lwt.t
  
(** hides/shows a thread *)
val thread_toggle_hidden : db_t -> frm_id:db_int_t -> thr_id:db_int_t -> unit Lwt.t
  
(** hides/shows a message *)
val message_toggle_hidden : db_t -> frm_id:db_int_t -> msg_id:db_int_t -> unit Lwt.t

(** makes a message sticky (or not) *)
val message_toggle_sticky: db_t -> frm_id:db_int_t -> msg_id:db_int_t -> unit Lwt.t

val find_forum: db_t -> ?id:db_int_t -> ?title:string -> unit -> (db_int_t * string * string * string * string) Lwt.t

(** returns the list of available forums *)
val get_forums_list: db_t -> (db_int_t * string * string * bool * bool) list Lwt.t

(** returns id, title, description, moderation status, number of shown/hidden
    threads and messages of a forum.  
    NB: a message is counted as hidden if: 
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val forum_get_data: 
  db_t -> frm_id:db_int_t -> role:role -> 
  (db_int_t * string * string * bool * int * int * int * int) Lwt.t
 
(** returns the number of visible messages in a thread *)
val thread_get_nr_messages : 
  db_t -> thr_id:db_int_t -> role:role -> int Lwt.t

(** returns id, subject, author, datetime, hidden status, number of shown/hidden
    messages of a thread.  
    NB: a message is counted as hidden if:
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val thread_get_data : 
  (* frm_id:db_int_t -> *) db_t -> thr_id:db_int_t -> role:role -> 
  (db_int_t * string * string * string option * Calendar.t * bool * int * int) Lwt.t
  
(** returns id, text, author, datetime, hidden status of a message *)
val message_get_data : db_t -> frm_id:db_int_t -> msg_id:db_int_t -> 
 (db_int_t * string * string * Calendar.t * bool) Lwt.t
  
(** returns None|Some id of prev & next thread in the same forum *)
val thread_get_neighbours :
  db_t -> frm_id:db_int_t ->  thr_id:db_int_t -> role:role -> 
    (db_int_t option * db_int_t option) Lwt.t

(** returns None|Some id of prev & next message in the same thread *)
val message_get_neighbours :
  db_t -> frm_id:db_int_t ->  msg_id:db_int_t -> role:role -> 
    (db_int_t option * db_int_t option) Lwt.t

(** returns the threads list of a forum, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (thr_id, subject, author, datetime,
    hidden status). *)
val forum_get_threads_list :
  db_t -> frm_id:db_int_t -> ?offset:int -> ?limit:int -> role:role -> unit ->
	(db_int_t * string * string * Calendar.t * bool) list Lwt.t

val thread_get_messages_with_text :
	db_t -> thr_id:db_int_t -> ?offset:int -> ?limit:int -> role:role ->
	?bottom:db_int_t -> unit ->
	(db_int_t * string * string * Calendar.t * bool * bool) list Lwt.t
(** as above, but in tree form *)
val thread_get_messages_with_text_forest :
	db_t -> thr_id:db_int_t -> ?offset:int -> ?limit:int ->
	?top:db_int_t -> ?bottom:db_int_t -> role:role -> unit ->
	(db_int_t * string * string * Calendar.t * bool * bool * db_int_t * db_int_t) tree list Lwt.t

val get_latest_messages:
	db_t -> frm_ids:db_int_t list -> limit:int -> unit ->
	(db_int_t * string * string) list Lwt.t

(** {4 Wikis} *)

(** inserts a new wiki container *)
val new_wiki : db_t -> title:string -> descr:string -> db_int_t Lwt.t

(*
(** inserts a new wikipage in an existing wiki; returns [None] if
    insertion failed due to [~suffix] already in use; [Some id] otherwise. *) 
val new_wikipage : wik_id:db_int_t -> suffix:string -> author:string ->
  subject:string -> txt:string -> db_int_t option Lwt.t

(** updates or inserts a wikipage. *) 
val add_or_change_wikipage : wik_id:db_int_t -> suffix:string -> author:string ->
  subject:string -> txt:string -> unit Lwt.t

(** returns title, description, number of wikipages of a wiki. *)
val wiki_get_data : wik_id:db_int_t -> (string * string * int) Lwt.t

(** returns the list of subject, suffix, author, datetime of wikipages, sorted by subject *)
val wiki_get_pages_list : wik_id:db_int_t -> 
  (string * string * string * Calendar.t) list Lwt.t

(** look for a wikipage and returns [Some (subject, text, author,
    datetime)], or [None] if the page doesn't exist. *)
val wikipage_get_data : wik_id:db_int_t -> suffix:string ->
  (string * string * string * Calendar.t) option Lwt.t
 *)

(*
(** create a new service *)
val new_service: db_t -> url:string -> db_int_t Lwt.t

(** list services *)
val list_services: db_t -> string list Lwt.t

val get_service_parameters: db_t -> url:string -> (db_int_t * string) list Lwt.t

val add_parameter_to_service: db_t -> url:string -> param_name:string -> db_int_t Lwt.t
*)
