
(** inserts a new wiki container *)
val new_wiki : Sql.db_t -> title:string -> descr:string -> Sql.db_int_t Lwt.t

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
*)

(** looks for a wikibox and returns [Some (subject, text, author,
    datetime)], or [None] if the page doesn't exist. *)
val wikibox_get_data : 
  Sql.db_t -> 
  wiki:Sql.db_int_t -> 
  id:Sql.db_int_t ->
  (string * string * string * CalendarLib.Calendar.t) option Lwt.t

