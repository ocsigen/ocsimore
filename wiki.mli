(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

(** Role of user in the wiki *)
type role = Admin | Author | Lurker | Nonauthorized;;
(** Admin can changes the permissions on boxes (if the wiki allows this) *)


type wiki_info = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  path : string list option;
  boxrights : bool;
}


(** Creates a new wiki or returns its id without modification
    if it already exists.
    If the optional argument [path] is present, 
    the wiki will be bound to an URL.
    If [boxrights] is true (default), it is possible to set the rights on
    each box individually.
    For each wiki, four groups of users are created:
    [wiki]i[_readers] (users who can read the boxes by default),
    [wiki]i[_writers] (users who can write the boxes by default),
    [wiki]i[_admins] (users who can changes rights for boxes, if [boxrights] is true),
    [wiki]i[_page_creators] (users who can create new wiki pages, if [path] is present).
    Put users or other groups inside these groups to change permissions on
    the wiki.
    The optional parameters [readers], [writers], [admins] and [page_creators]
    allow to put some users inside these groups.
    Default, respectively: [anonymous], [users], [admin], [users].
*)
val create_wiki :
  title:string ->
  descr:string ->
  ?sp: Eliom_sessions.server_params ->
  ?path: string list ->
  ?page_creators:User_sql.userid list ->
  ?readers:User_sql.userid list -> 
  ?writers:User_sql.userid list -> 
  ?admins:User_sql.userid list ->
  ?boxrights:bool ->
  wikibox: Wiki_widgets.editable_wikibox ->
  unit -> 
  wiki_info Lwt.t

(** [readers_group i] returns the id of the group of users
    who can read wiki [i] by default. *)
val readers_group : int32 -> int32 Lwt.t

(** [writers_group i] returns the id of the group of users
    who can write in wiki [i] by default. *)
val writers_group : int32 -> int32 Lwt.t

(** [admins_group i] returns the id of the group of users
    who can change permissions of boxes in wiki [i] by default 
    (if boxrights activated). *)
val admins_group : int32 -> int32 Lwt.t

(** [page_creators_group i] returns the id of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val page_creators_group : int32 -> int32 Lwt.t

(** [readers_group_name i] returns the name of the group of users
    who can read wiki [i] by default. *)
val readers_group_name : int32 -> string

(** [writers_group_name i] returns the name of the group of users
    who can write in wiki [i] by default. *)
val writers_group_name : int32 -> string

(** [admins_group_name i] returns the name of the group of users
    who can change permissions of boxes in wiki [i] by default 
    (if boxrights activated). *)
val admins_group_name : int32 -> string

(** [page_creators_group_name i] returns the name of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val page_creators_group_name : int32 -> string

(** Returns wiki information from an id. 
    Wiki information is kept in memory (and savec in the database)
*)
val get_wiki_by_id : int32 -> wiki_info Lwt.t

(** Returns wiki information from a name. *)
val get_wiki_by_name : string -> wiki_info Lwt.t

val new_wikibox :
  wiki:wiki_info ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  ?readers:User_sql.userid list ->
  ?writers:User_sql.userid list -> 
  ?admins: User_sql.userid list ->
  unit -> int32 Lwt.t

val save_wikibox :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  (((int32 * int32) * string) *
     (string option *
        (string option *
           (string option * 
              (string option * (string option * string option)))))) ->
  exn list Lwt.t

(*
val can_read : wiki_info -> int32 -> Users.userdata -> bool Lwt.t
val can_write : wiki_info -> int32 -> Users.userdata -> bool Lwt.t
*)

val get_role : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  (Wiki_sql.wiki * int32) ->
  role Lwt.t

val get_readers : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list Lwt.t

val get_writers : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list Lwt.t

val get_admins : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list Lwt.t

(** *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (int32 * int32)
  | Preview of (((int32 * int32) * string) * 
                  (string option * 
                     (string option * 
                        (string option * 
                           (string option * (string option * string option))))))
  | History of ((int32 * int32) * (int option * int option))
  | Oldversion of ((int32 * int32) * int32)
  | Src of ((int32 * int32) * int32)
  | Error of ((int32 * int32) * wiki_errors)

exception Wiki_action_info of wiki_action_info

