type wiki = int32

(** Role of user in the wiki *)
type role = Admin of int32 | Author of int32 | Lurker of string | Unknown;;
(** Admin can changes the permissions on boxes (if the wiki allows this) *)


(** inserts a new wiki container *)
val new_wiki : 
  title:string -> 
  descr:string -> 
  reader:Users.group -> 
  writer:Users.group ->
  ?admin:Users.group ->
  unit ->
  wiki Lwt.t

(** Find wiki information for a wiki, given its id or title *)
val find_wiki: 
  ?id:wiki -> 
  ?title:string -> 
  unit -> 
  (wiki * string * string * Users.group * Users.group * Users.group option)
    Lwt.t

(** Inserts a new wikipage in an existing wiki and return the id of the 
    wikibox. *)
val new_wikibox :
  wiki:wiki ->
  author:string ->
  comment:string ->
  content:string ->
  ?rights:Users.group list * Users.group list * Users.group list ->
  unit ->
  int32 Lwt.t

(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
val update_wikibox :
  wiki:wiki ->
  wikibox:int32 ->
  author:string ->
  comment:string ->
  content:string ->
  ?readers:Users.group list ->
  ?writers:Users.group list ->
  ?admins:Users.group list ->
  unit ->
  int32 Lwt.t

(** looks for a wikibox and returns [Some (subject, text, author,
    datetime)], or [None] if the page doesn't exist. *)
val get_wikibox_data : 
  wiki:wiki -> 
  id:int32 ->
  (string * string * string * CalendarLib.Calendar.t) option Lwt.t

val get_readers : wiki:wiki -> id:int32 -> Users.group list Lwt.t
val get_writers : wiki:wiki -> id:int32 -> Users.group list Lwt.t
val get_admins : wiki:wiki -> id:int32 -> Users.group list Lwt.t

val populate_readers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val populate_writers : 
  int32 -> int32 -> int32 list -> unit Lwt.t
val populate_wbadmins : 
  int32 -> int32 -> int32 list -> unit Lwt.t


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

