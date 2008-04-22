type wiki

(** inserts a new wiki container *)
val new_wiki : 
  Sql.db_t -> 
  title:string -> 
  descr:string -> 
  reader:int32 -> 
  writer:int32 ->
  acl:bool ->
  wiki Lwt.t

(** Find wiki information for a wiki, given its id or title *)
val find_wiki: 
  Sql.db_t -> 
  ?id:wiki -> 
  ?title:string -> 
  unit -> 
  (wiki * string * string * string * string * bool) Lwt.t

(** Inserts a new wikipage in an existing wiki and return the id of the 
    wikibox. *)
val new_wikibox :
  Sql.db_t -> 
  wiki:wiki ->
  author:string ->
  comment:string ->
  content:string ->
  int32 Lwt.t

(** looks for a wikibox and returns [Some (subject, text, author,
    datetime)], or [None] if the page doesn't exist. *)
val get_wikibox_data : 
  Sql.db_t -> 
  wiki:wiki -> 
  id:int32 ->
  (string * string * string * CalendarLib.Calendar.t) option Lwt.t


(*
(** inserts a new wikipage in an existing wiki; returns [None] if
    insertion failed due to [~suffix] already in use; [Some id] otherwise. *) 
val new_wikipage : wik_id:int32 -> suffix:string -> author:string ->
  subject:string -> txt:string -> int32 option Lwt.t

(** updates or inserts a wikipage. *) 
val add_or_change_wikipage : wik_id:int32 -> suffix:string -> author:string ->
  subject:string -> txt:string -> unit Lwt.t

(** returns title, description, number of wikipages of a wiki. *)
val wiki_get_data : wik_id:int32 -> (string * string * int) Lwt.t

(** returns the list of subject, suffix, author, datetime of wikipages, sorted by subject *)
val wiki_get_pages_list : wik_id:int32 -> 
  (string * string * string * Calendar.t) list Lwt.t
*)

