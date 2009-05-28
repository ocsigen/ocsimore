open User_sql.Types
open Wiki_types

(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)


(** Creates the specified wiki if the user has enough permissions.
    Raise [Permission_denied] if it is not the case, and or fails with
    the same errors as [Wiki.create_wiki] if the wiki cannot be created.
    The options are the same as for the this function, except for
    the field [admin], which is used as the author of the container page,
    and becomes admin of the wiki. By default, no one can read
    the wiki.
*)
val create_wiki :
  rights:Wiki_types.wiki_rights ->
  sp: Eliom_sessions.server_params ->
  title:string ->
  descr:string ->
  ?path: string list ->
  ?staticdir:string ->
  ?boxrights:bool ->
  admin: userid ->
  ?wiki_css:string ->
  ?container_text:string ->
  model:Wiki_types.wiki_model ->
  unit ->
  wiki Lwt.t



val new_wikitextbox :
  ?db:Sql.db_t ->
  rights:Wiki_types.wiki_rights ->
  content_type:Wiki_types.content_type ->
  sp:Eliom_sessions.server_params ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  unit -> int32 Lwt.t


(** The next three functiosn save a wikibox and returns the new version id of
    this wikibox. *)
val save_wikitextbox :
  rights:Wiki_types.wiki_rights ->
  content_type:Wiki_types.content_type ->
  sp:Eliom_sessions.server_params ->
  wb:wikibox ->
  content:string option ->
  int32 Lwt.t

val save_wikicssbox :
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  wiki:wiki ->
  content:string option ->
  int32 Lwt.t

val save_wikipagecssbox :
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  wiki:wiki ->
  page:string ->
  content:string option ->
  int32 Lwt.t



(** Raised in case of a non-existing wikibox. The optional [int32]
   argument is the version number *)
exception Unknown_box of wikibox * int32 option


(** Returns the content of the wikibox if the user has enough rights,
    possibly for the given revision *)
val wikibox_content:
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  ?version:int32 ->
  wikibox ->
  Wiki_types.wikibox_content Lwt.t

val wikibox_content':
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  ?version:int32 ->
  wikibox ->
  (string option * int32) Lwt.t


val wikibox_history :
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  wb:wikibox ->
  (int32 * string * (* userid *) int32 * CalendarLib.Calendar.t) list Lwt.t


(** Returns the css for the specified wiki. Fail with [Permission_denied]
    if the user cannot view the css, and [Eliom_common.Eliom_404] if there
    is no css for this wiki *)
val wiki_css :
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  wiki:wiki ->
  string Lwt.t

(** Same thing for a wikipage *)
val wikipage_css :
  rights:Wiki_types.wiki_rights ->
  sp:Eliom_sessions.server_params ->
  wiki:wiki -> page:string ->
  string Lwt.t
