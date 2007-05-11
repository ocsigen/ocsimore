(** Database interface. 

    All SQL commands go here. *)



(** {4 Persistent data} *)

(** Persistent data -- this is, mutatis mutandis, Vincent Balat's
    Ocsipersist module; thanks to him.

    Data are kept in memory but all modifications
    are stored in the database; when launching the program, if the
    value exists in the database, it is loaded, otherwise it is
    initialised to the default value. *)
module Persist : sig

  (** Type of persistent data *)
  type 'a t

  (** [create name default] returns a persistent value named
      [name] from database or create it with the [default] value if it
      does not exist. [default] will be evaluated (i.e., applied to
      [()]), only in the latter case.*)
  val create : string -> (unit -> 'a) -> 'a t Lwt.t

  val lwtcreate : string -> (unit -> 'a Lwt.t) -> 'a t Lwt.t

  (** [get pv] gives the value of [pv] *)
  val get : 'a t -> 'a

  (** [set pv value] sets a persistent value [pv] to [value] and store it in the database. *)
  val set : 'a t -> 'a -> unit Lwt.t
    
  (** [write_back pv] forces a database write of [pv]. *)
  val write_back : 'a t -> unit Lwt.t

end


(** {4 Forums} *)

(** type of [~role] labelled parameter *)
type role = Moderator | Author of string | Unknown

(** inserts a new forum *)
val new_forum : title:string -> descr:string -> moderated:bool -> int32 Lwt.t

(** inserts a message starting a new thread; both thread and message
    will be hidden if forum is moderated *)
val new_thread_and_message :
  frm_id:int32 ->
  author:string -> subject:string -> txt:string -> (int32 * int32) Lwt.t

(** inserts a message for an existing thread; message will be hidden
    if forum is moderated *)
val new_message :
  frm_id:int32 ->  thr_id:int32 -> author:string -> txt:string -> int32 Lwt.t

(** toggle moderation status of a forum *)
val forum_toggle_moderated : frm_id:int32 -> unit Lwt.t
  
(** hides/shows a thread *)
val thread_toggle_hidden : frm_id:int32 -> thr_id:int32 -> unit Lwt.t
  
(** hides/shows a message *)
val message_toggle_hidden : frm_id:int32 -> msg_id:int32 -> unit Lwt.t

(** returns id, title, description, moderation status, number of shown/hidden
    threads and messages of a forum.  
    NB: a message is counted as hidden if: 
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val forum_get_data: 
  frm_id:int32 -> role:role -> 
  (int32 * string * string * bool * int64 * int64 * int64 * int64) Lwt.t
  
(** returns id, subject, author, datetime, hidden status, number of shown/hidden
    messages of a thread.  
    NB: a message is counted as hidden if:
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val thread_get_data : 
  frm_id:int32 -> thr_id:int32 -> role:role -> 
  (int32 * string * string * Calendar.t * bool * int64 * int64) Lwt.t
  
(** returns id, text, author, datetime, hidden status of a message *)
val message_get_data : frm_id:int32 -> msg_id:int32 -> 
 (int32 * string * string * Calendar.t * bool) Lwt.t
  
(** returns None|Some id of prev & next thread in the same forum *)
val thread_get_neighbours :
  frm_id:int32 ->  thr_id:int32 -> role:role -> 
    (int32 option * int32 option) Lwt.t

(** returns None|Some id of prev & next message in the same thread *)
val message_get_neighbours :
  frm_id:int32 ->  msg_id:int32 -> role:role -> 
    (int32 option * int32 option) Lwt.t

(** returns the threads list of a forum, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (thr_id, subject, author, datetime,
    hidden status). *)
val forum_get_threads_list :
  frm_id:int32 ->
  offset:int32 ->
  limit:int32 ->
  role:role -> (int32 * string * string * Calendar.t * bool) list Lwt.t

(** returns the messages list of a thread, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (msg_id, author, datetime, hidden
    status). *)
val thread_get_messages_list :
  frm_id:int32 ->  thr_id:int32 ->
  offset:int32 ->
  limit:int32 -> role:role -> (int32 * string * Calendar.t * bool)
  list Lwt.t

(** as above, but returns the text of each message too.
    A list elt is (msg_id, text, author, datetime, hidden status). *)
val thread_get_messages_with_text_list :
  frm_id:int32 ->  thr_id:int32 ->
  offset:int32 ->
  limit:int32 -> role:role -> (int32 * string * string * Calendar.t * bool)
  list Lwt.t


(** {4 Wikis} *)


(** inserts a new wiki container *)
val new_wiki : title:string -> descr:string -> int32  Lwt.t

(** inserts a new wikipage in an existing wiki; returns [None] if
    insertion failed due to [~suffix] already in use; [Some id] otherwise. *) 
val new_wikipage : wik_id:int32 -> suffix:string -> author:string ->
  subject:string -> txt:string -> int32 option Lwt.t

(** updates or inserts a wikipage. *) 
val add_or_change_wikipage : wik_id:int32 -> suffix:string -> author:string ->
  subject:string -> txt:string -> unit Lwt.t

(** returns title, description, number of wikipages of a wiki. *)
val wiki_get_data : wik_id:int32 -> (string * string * int64) Lwt.t

(** returns the list of subject, suffix, author, datetime of wikipages, sorted by subject *)
val wiki_get_pages_list : wik_id:int32 -> 
  (string * string * string * Calendar.t) list Lwt.t

(** look for a wikipage and returns [Some (subject, text, author,
    datetime)], or [None] if the page doesn't exist. *)
val wikipage_get_data : wik_id:int32 -> suffix:string ->
  (string * string * string * Calendar.t) option Lwt.t
