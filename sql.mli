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

(** type for database integers (and IDs) *)
type db_int_t;;

(** type for database offsets, counts and limits *)
type db_size_t;;

val db_int_of_int: int -> db_int_t
val db_int_of_string: string -> db_int_t
val string_of_db_int: db_int_t -> string
val db_size_of_int: int -> db_size_t

(** type of [~role] labelled parameter *)
type role = Moderator | Author of string | Unknown;;

(** type of [message] list *)
type 'a tree = Node of 'a * ('a tree list);;


(** inserts a new forum *)
val new_forum : title:string -> descr:string -> moderated:bool -> db_int_t Lwt.t

(** inserts a message starting a new thread; both thread and message
    will be hidden if forum is moderated *)
val new_thread_and_message :
  frm_id:db_int_t ->
  author:string -> subject:string -> txt:string -> (db_int_t * db_int_t) Lwt.t

(** inserts a message for an existing thread; message will be hidden
    if forum is moderated *)
val new_message :
  frm_id:db_int_t -> thr_id:db_int_t -> ?parent_id:db_int_t -> author:string ->
	txt:string -> unit -> db_int_t Lwt.t

(** toggle moderation status of a forum *)
val forum_toggle_moderated : frm_id:db_int_t -> unit Lwt.t
  
(** hides/shows a thread *)
val thread_toggle_hidden : frm_id:db_int_t -> thr_id:db_int_t -> unit Lwt.t
  
(** hides/shows a message *)
val message_toggle_hidden : frm_id:db_int_t -> msg_id:db_int_t -> unit Lwt.t

(** returns id, title, description, moderation status, number of shown/hidden
    threads and messages of a forum.  
    NB: a message is counted as hidden if: 
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val forum_get_data: 
  frm_id:db_int_t -> role:role -> 
  (db_int_t * string * string * bool * int * int * int * int) Lwt.t
  
(** returns id, subject, author, datetime, hidden status, number of shown/hidden
    messages of a thread.  
    NB: a message is counted as hidden if:
    - its hidden status is true, or 
    - it's in a hidden thread. *)
val thread_get_data : 
  frm_id:db_int_t -> thr_id:db_int_t -> role:role -> 
  (db_int_t * string * string * Calendar.t * bool * int * int) Lwt.t
  
(** returns id, text, author, datetime, hidden status of a message *)
val message_get_data : frm_id:db_int_t -> msg_id:db_int_t -> 
 (db_int_t * string * string * Calendar.t * bool) Lwt.t
  
(** returns None|Some id of prev & next thread in the same forum *)
val thread_get_neighbours :
  frm_id:db_int_t ->  thr_id:db_int_t -> role:role -> 
    (db_int_t option * db_int_t option) Lwt.t

(** returns None|Some id of prev & next message in the same thread *)
val message_get_neighbours :
  frm_id:db_int_t ->  msg_id:db_int_t -> role:role -> 
    (db_int_t option * db_int_t option) Lwt.t

(** returns the threads list of a forum, ordered cronologycally
    (latest first), with max [~limit] items and skipping first
    [~offset] rows.  A list elt is (thr_id, subject, author, datetime,
    hidden status). *)
val forum_get_threads_list :
  frm_id:db_int_t ->
  offset:int ->
  limit:int ->
  role:role -> (db_int_t * string * string * Calendar.t * bool) list Lwt.t

(** as above, but in tree form *)
val thread_get_messages_tree :
	frm_id:db_int_t -> thr_id:db_int_t ->
	offset:int ->
	limit:int -> ?top:db_int_t -> max_depth:int -> role:role -> unit ->
	(db_int_t * string * Calendar.t * bool * db_int_t option * string option) tree list Lwt.t

val thread_get_messages_with_text_tree :
	frm_id:db_int_t -> thr_id:db_int_t ->
	offset:int ->
	limit:int -> ?top:db_int_t -> ?bottom:db_int_t ->
	max_depth:int -> role:role -> unit ->
	(db_int_t * string * string * Calendar.t * bool * db_int_t option * string option) tree list Lwt.t

(** {4 Wikis} *)


(** inserts a new wiki container *)
val new_wiki : title:string -> descr:string -> db_int_t  Lwt.t

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
