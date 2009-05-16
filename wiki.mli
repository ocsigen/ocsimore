open User_sql.Types
open Wiki_sql.Types

(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)


(*
(** Creates a new wiki or returns its id without modification if a wiki of the
    same name already exists.

    If the optional argument [path] is present, the wiki will be bound to the
    URL represented by [path].

    The argument [container_page] is the code for the container wikibox
    of the wiki. A suitable default page is given below.

    If [boxrights] is true (default), it is possible to set the rights on
    each box individually.
*)


val really_create_wiki :
  title:string ->
  descr:string ->
  ?path: string list ->
  ?staticdir:string ->
  ?boxrights:bool ->
  author: userid ->
  ?admins:user list ->
  ?readers:user list ->
  ?wiki_css:string ->
  container_text:string ->
  unit ->
  wiki Lwt.t



val new_wikitextbox :
  sp:Eliom_sessions.server_params ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  unit -> int32 Lwt.t


(** Saves a wikibox and returns the new version id of this wikibox. *)
val save_wikitextbox :
  sp:Eliom_sessions.server_params ->
  wb:wikibox ->
  content:string option ->
  int32 Lwt.t

val save_wikicssbox :
  sp:Eliom_sessions.server_params ->
  wb:wikibox ->
  wiki:wiki ->
  content:string option ->
  int32 Lwt.t

val save_wikipagecssbox :
  sp:Eliom_sessions.server_params ->
  wb:wikibox ->
  wiki:wiki ->
  page:string ->
  content:string option ->
  int32 Lwt.t



(** [modified_wikibox box version] returns [Some curversion] iff the current
    version [curversion] of [box] is greater than [version], [None]
    otherwise *)
val modified_wikibox:
  wikibox:wikibox -> boxversion:Int32.t -> Int32.t option Lwt.t


val wikibox_content:
  sp:Eliom_sessions.server_params ->
  ?version:int32 -> 
  wikibox ->
  Wiki_sql.wikibox_content Lwt.t

val wikibox_content':
  sp:Eliom_sessions.server_params ->
  ?version:int32 -> 
  wikibox ->
  (string option * int32) Lwt.t


(** Raised in case of a non-existing wikibox. The optional [int32]
   argument is the version number *)
exception Unknown_box of wikibox * int32 option
