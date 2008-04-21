(** inserts a new forum *)
val new_forum : 
  Sql.db_t -> 
  title:string -> 
  descr:string -> 
  moderated:bool ->
  arborescent:bool -> 
  reader:Sql.db_int_t -> 
  writer:Sql.db_int_t ->
  moderator:Sql.db_int_t ->  
  Sql.db_int_t Lwt.t

(** inserts a message starting a new thread; both thread and message
    will be hidden if forum is moderated *)
val new_thread_and_message :
  Sql.db_t -> 
  frm_id:Sql.db_int_t ->
  author_id:Sql.db_int_t -> 
  subject:string -> 
  txt:string -> 
  (Sql.db_int_t * Sql.db_int_t) Lwt.t

(** inserts a thread with an article; the thread will be hidden if the forum
    is moderated *)
val new_thread_and_article:
  Sql.db_t -> 
  frm_id:Sql.db_int_t -> 
  author_id:Sql.db_int_t -> 
  subject:string -> txt:string ->
  (Sql.db_int_t * Sql.db_int_t) Lwt.t

(** inserts a message for an existing thread; message will be hidden
    if forum is moderated *)
val new_message :
  Sql.db_t -> 
  thr_id:Sql.db_int_t ->
  ?parent_id:Sql.db_int_t -> 
  author_id:Sql.db_int_t ->
  txt:string -> 
  sticky:bool -> 
  unit -> 
  Sql.db_int_t Lwt.t

(** toggle moderation status of a forum *)
val forum_toggle_moderated : Sql.db_t -> frm_id:Sql.db_int_t -> unit Lwt.t
  
(** hides/shows a thread *)
val thread_toggle_hidden : 
  Sql.db_t -> frm_id:Sql.db_int_t -> thr_id:Sql.db_int_t -> unit Lwt.t
  
(** hides/shows a message *)
val message_toggle_hidden :
  Sql.db_t -> frm_id:Sql.db_int_t -> msg_id:Sql.db_int_t -> unit Lwt.t

(** makes a message sticky (or not) *)
val message_toggle_sticky: 
  Sql.db_t -> frm_id:Sql.db_int_t -> msg_id:Sql.db_int_t -> unit Lwt.t

val find_forum: 
  Sql.db_t -> 
  ?id:Sql.db_int_t -> 
  ?title:string -> 
  unit -> 
  (Sql.db_int_t * string * string * string * string) Lwt.t

(** returns the list of available forums *)
val get_forums_list : 
  Sql.db_t -> (Sql.db_int_t * string * string * bool * bool) list Lwt.t

(** returns id, title, description, moderation status, number of shown/hidden
    threads and messages of a forum.  
    NB: a message is counted as hidden if: 
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val forum_get_data: 
  Sql.db_t -> 
  frm_id:Sql.db_int_t -> 
  role:User_sql.role -> 
  (Sql.db_int_t * string * string * bool * int * int * int * int) Lwt.t
 
(** returns the number of visible messages in a thread *)
val thread_get_nr_messages : 
  Sql.db_t -> thr_id:Sql.db_int_t -> role:User_sql.role -> int Lwt.t

(** returns id, subject, author, datetime, hidden status, number of shown/hidden
    messages of a thread.  
    NB: a message is counted as hidden if:
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val thread_get_data : 
  (* frm_id:Sql.db_int_t -> *) 
  Sql.db_t -> 
  thr_id:Sql.db_int_t -> 
  role:User_sql.role -> 
  (Sql.db_int_t * 
     string * 
     string * 
     string option * 
     CalendarLib.Calendar.t * 
     bool * 
     int * 
     int) Lwt.t
  
(** returns id, text, author, datetime, hidden status of a message *)
val message_get_data : 
  Sql.db_t -> 
  frm_id:Sql.db_int_t -> 
  msg_id:Sql.db_int_t -> 
 (Sql.db_int_t * string * string * CalendarLib.Calendar.t * bool) Lwt.t
  
(** returns None|Some id of prev & next thread in the same forum *)
val thread_get_neighbours :
  Sql.db_t -> 
  frm_id:Sql.db_int_t ->  
  thr_id:Sql.db_int_t -> 
  role:User_sql.role -> 
  (Sql.db_int_t option * Sql.db_int_t option) Lwt.t

(** returns None|Some id of prev & next message in the same thread *)
val message_get_neighbours :
  Sql.db_t -> 
  frm_id:Sql.db_int_t ->  
  msg_id:Sql.db_int_t -> 
  role:User_sql.role -> 
  (Sql.db_int_t option * Sql.db_int_t option) Lwt.t

(** returns the threads list of a forum, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (thr_id, subject, author, datetime,
    hidden status). *)
val forum_get_threads_list :
  Sql.db_t -> 
  frm_id:Sql.db_int_t -> 
  ?offset:int -> 
  ?limit:int -> 
  role:User_sql.role -> 
  unit ->
  (Sql.db_int_t * string * string * CalendarLib.Calendar.t * bool) list Lwt.t

val thread_get_messages_with_text :
  Sql.db_t -> 
  thr_id:Sql.db_int_t -> 
  ?offset:int -> 
  ?limit:int -> 
  role:User_sql.role ->
  ?bottom:Sql.db_int_t -> 
  unit ->
  (Sql.db_int_t * string * string * CalendarLib.Calendar.t * bool * bool) list Lwt.t
(** as above, but in tree form *)

val thread_get_messages_with_text_forest :
  Sql.db_t -> 
  thr_id:Sql.db_int_t -> 
  ?offset:int -> 
  ?limit:int ->
  ?top:Sql.db_int_t -> 
  ?bottom:Sql.db_int_t -> 
  role:User_sql.role -> 
  unit ->
  (Sql.db_int_t * 
     string * 
     string * 
     CalendarLib.Calendar.t * 
     bool * 
     bool * 
     Sql.db_int_t * 
     Sql.db_int_t) Ocsimorelib.tree list Lwt.t

val get_latest_messages:
  Sql.db_t -> 
  frm_ids:Sql.db_int_t list -> 
  limit:int -> 
  unit ->
  (Sql.db_int_t * string * string) list Lwt.t

