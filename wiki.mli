(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

type wiki_info = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  default_reader: Users.group;
  default_writer: Users.group;
  acl_enabled: bool;
}


(** Creates a new wiki or returns its id without modification
    if it already exists. *)
val create_wiki :
  title:string ->
  descr:string ->
  ?acl_enabled:bool ->
  ?reader:Users.group -> ?writer:Users.group -> unit -> wiki_info Lwt.t

val new_wikibox :
  wiki:wiki_info ->
  author:string ->
  comment:string ->
  content:string ->
  ?readers:Users.group list ->
  ?writers:Users.group list -> unit -> int32 Lwt.t


(** *)
val can_read : wiki_info -> int32 -> Users.user -> bool Lwt.t
val can_write : wiki_info -> int32 -> Users.user -> bool Lwt.t

val get_role : 
  Session_manager.sessionmanager -> Wiki_sql.wiki -> 
  int32 -> Wiki_sql.role Lwt.t
