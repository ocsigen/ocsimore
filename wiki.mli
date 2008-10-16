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
  boxrights : bool;
  pages : string option;
  container_id: int32;
  staticdir : string option; (* if static dir is given, 
                                ocsimore will serve static pages if present,
                                instead of wiki pages *)
}




(** Creates a new wiki or returns its id without modification
    if it already exists.
    If the optional argument [path] is present, 
    the wiki will be bound to an URL.
    If [boxrights] is true (default), it is possible to set the rights on
    each box individually.
    For each wiki, some groups of users are created:
    [wiki]i[_readers] (users who can read the boxes by default),
    [wiki]i[_writers] (users who can write the boxes by default),
    [wiki]i[_rights_givers] (users who can changes rights for boxes, if [boxrights] is true),
    [wiki]i[_page_creators] (users who can create new wiki pages, if [path] is present).
    [wiki]i[_css_editors] (users who can modify css of wiki pages, if [path] is present),
    [wiki]i[_wikiboxes_creators] (users who can create new wikiboxes),
    [wiki]i[_container_adm] (users who can modify the layout of pages (containers)),
    [wiki]i[_admins] (all rights for wiki [i]),
    Put users or other groups inside these groups to change permissions on
    the wiki.
    The optional parameters [readers], [writers], etc.
    allow to put some users inside these groups.
    Default, respectively: [anonymous], [users], [admin], [users], [admin], 
    [users], [users], [admin].
*)
val create_wiki :
  title:string ->
  descr:string ->
  ?sp:Eliom_sessions.server_params ->
  ?path: string list ->
  ?readers:User_sql.userid list -> 
  ?writers:User_sql.userid list -> 
  ?rights_adm:User_sql.userid list ->
  ?wikiboxes_creators:User_sql.userid list ->
  ?container_adm:User_sql.userid list ->
  ?page_creators:User_sql.userid list ->
  ?css_editors:User_sql.userid list ->
  ?admins:User_sql.userid list ->
  ?boxrights:bool ->
  ?staticdir:string ->
  wikibox: Wiki_widgets.editable_wikibox ->
  unit -> 
  wiki_info Lwt.t


(** Register a pre-existing wiki at the given path *)
val register_wiki :
  ?sp:Eliom_sessions.server_params ->
  path:Ocsigen_extensions.url_path ->
  wikibox: Wiki_widgets.editable_wikibox ->
  wiki:Wiki_sql.wiki ->
  ?wiki_info:wiki_info ->
  unit ->
  unit Lwt.t

(** {2 Groups } *)

(**  
     [wiki1_admin] belongs to
     [wiki1_css_editors], 
     [wiki1_page_creators], [wiki1_wikiboxes_creators], 
     [wiki1_right_givers] and [wiki1_container_adm],

     [wiki1_css_editors], 
     [wiki1_page_creators], [wiki1_wikiboxes_creators], 
     [wiki1_right_givers] and [wiki1_container_adm]
     belong to [wiki1_writers],

     [wiki1_writers] belongs to [wiki1_readers].
*)

(** [readers_group i] returns the id of the group of users
    who can read wiki [i] by default. *)
val readers_group : Wiki_sql.wiki -> int32 Lwt.t

(** [writers_group i] returns the id of the group of users
    who can write in wiki [i] by default. *)
val writers_group : Wiki_sql.wiki -> int32 Lwt.t

(** [rights_adm_group i] returns the id of the group of users
    who can change permissions of boxes in wiki [i] by default 
    (if boxrights activated). *)
val rights_adm_group : Wiki_sql.wiki -> int32 Lwt.t

(** [wikiboxes_creators_group i] returns the id of the group of users
    who can create wikiboxes in wiki [i] by default. *)
val wikiboxes_creators_group : Wiki_sql.wiki -> int32 Lwt.t

(** [page_creators_group i] returns the id of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val page_creators_group : Wiki_sql.wiki -> int32 Lwt.t

(** [css_editors_group i] returns the id of the group of users
    who can modify css of pages in wiki [i]. *)
val css_editors_group : Wiki_sql.wiki -> int32 Lwt.t

(** [container_adm_group i] returns the id of the group of users
    who can change the layout (container) of wikipages in wiki [i]. *)
val container_adm_group : Wiki_sql.wiki -> int32 Lwt.t

(** [admin_group i] returns the id of the group of users
    who have all rights on wiki [i]. *)
val admin_group : Wiki_sql.wiki -> int32 Lwt.t

(** [readers_group_name i] returns the name of the group of users
    who can read wiki [i] by default. *)
val readers_group_name : Wiki_sql.wiki -> string

(** [writers_group_name i] returns the name of the group of users
    who can write in wiki [i] by default. *)
val writers_group_name : Wiki_sql.wiki -> string

(** [rights_adm_group_name i] returns the name of the group of users
    who can change permissions of boxes in wiki [i] by default 
    (if boxrights activated). *)
val rights_adm_group_name : Wiki_sql.wiki -> string

(** [wikiboxes_creators_group_name i] returns the name of the group of users
    who can create wikiboxes in wiki [i] by default. *)
val wikiboxes_creators_group_name : Wiki_sql.wiki -> string

(** [page_creators_group_name i] returns the name of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val page_creators_group_name : Wiki_sql.wiki -> string

(** [css_editors_group_name i] returns the name of the group of users
    who can modify css of pages in wiki [i]. *)
val css_editors_group_name : Wiki_sql.wiki -> string

(** [container_adm_group_name i] returns the name of the group of users
    who can change the layout (container) of wikipages in wiki [i]. *)
val container_adm_group_name : Wiki_sql.wiki -> string

(** [admin_group_name i] returns the name of the group of users
    who have all rights on wiki [i]. *)
val admin_group_name : Wiki_sql.wiki -> string




(** Returns wiki information from an id. 
    Wiki information is kept in memory (and savec in the database)
*)
val get_wiki_by_id : Wiki_sql.wiki -> wiki_info Lwt.t

(** Returns wiki information from a name. *)
val get_wiki_by_name : string -> wiki_info Lwt.t

(** If [?boxid] specified, creates the box only if the box 
    does not already exist. It it exists, returns the existing
    box without modification. *)
val new_wikibox :
  ?boxid:int32 ->
  wiki:wiki_info ->
  author:User_sql.userid ->
  comment:string ->
  content:string ->
  ?readers:User_sql.userid list ->
  ?writers:User_sql.userid list -> 
  ?rights_adm: User_sql.userid list ->
  ?wikiboxes_creators: User_sql.userid list ->
  unit -> int32 Lwt.t

val save_wikibox :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  ((Wiki_sql.wiki * int32) * string) ->
  exn list Lwt.t

val save_wikibox_permissions :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data -> 
  ((Wiki_sql.wiki * int32) *
     (string *
        (string *
           (string * 
              (string * (string * (string * (string * string)))))))) ->
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
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list option Lwt.t

val get_writers : 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list option Lwt.t

val get_rights_adm : 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list option Lwt.t

val get_wikiboxes_creators : 
  (Wiki_sql.wiki * int32) ->
  User_sql.userid list option Lwt.t

val can_create_wikibox : 
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  wiki_info -> int32 -> User_sql.userid -> bool Lwt.t

(** *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (Wiki_sql.wiki * int32)
  | Edit_perm of (Wiki_sql.wiki * int32)
  | Preview of ((Wiki_sql.wiki * int32) * string)
  | History of ((Wiki_sql.wiki * int32) * (int option * int option))
  | Oldversion of ((Wiki_sql.wiki * int32) * int32)
  | Src of ((Wiki_sql.wiki * int32) * int32)
  | Error of ((Wiki_sql.wiki * int32) * wiki_errors)

exception Wiki_action_info of wiki_action_info

(** Administration wiki *)

(** Name of the administration wiki. This is the name that must
    be used when creating (or searching for) this wiki. If that
    name is changed, the database *must* be upgraded manually to
    reflect the change *)
val wiki_admin_name : string
val get_admin_wiki : unit -> Wiki_sql.wiki Lwt.t
