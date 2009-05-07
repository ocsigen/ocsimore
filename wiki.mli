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
  ?readers:userid list ->
  ?writers:userid list ->
  ?rights_adm:userid list ->
  ?wikiboxes_creators:userid list ->
  ?container_adm:userid list ->
  ?page_creators:userid list ->
  ?css_editors:userid list ->
  ?admins:userid list ->
  ?boxrights:bool ->
  ?staticdir:string ->
  wikibox: Wiki_widgets.editable_wikibox ->
  container_page:string ->
  unit ->
  Wiki_sql.wiki_info Lwt.t


(** Register a pre-existing wiki at the given path *)
val register_wiki :
  ?sp:Eliom_sessions.server_params ->
  path:Ocsigen_extensions.url_path ->
  wikibox: Wiki_widgets.editable_wikibox ->
  wiki:Wiki_sql.wiki ->
  ?wiki_info:Wiki_sql.wiki_info ->
  unit ->
  unit Lwt.t
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

(*
(** [readers_group i] returns the id of the group of users
    who can read wiki [i] by default. *)
val readers_wikiboxes_group : wiki -> user

(** [writers_group i] returns the id of the group of users
    who can write in wiki [i] by default. *)
val writers_group : wiki -> user

(** [rights_adm_group i] returns the id of the group of users
    who can change permissions of boxes in wiki [i] by default
    (if boxrights activated). *)
val rights_adm_group : wiki -> user
*)

(*
(** [wikiboxes_creators_group i] returns the id of the group of users
    who can create wikiboxes in wiki [i] by default. *)
val wikiboxes_creators_group : wiki -> user

(** [page_creators_group i] returns the id of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val wikipages_creators_group : wiki -> user
*)

(*
(** [css_editors_group i] returns the id of the group of users
    who can modify css of pages in wiki [i]. *)
val css_editors_group : wiki -> userid Lwt.t

(** [container_adm_group i] returns the id of the group of users
    who can change the layout (container) of wikipages in wiki [i]. *)
val container_adm_group : wiki -> userid Lwt.t

(** [admin_group i] returns the id of the group of users
    who have all rights on wiki [i]. *)
val admin_group : wiki -> userid Lwt.t

(** [readers_group_name i] returns the name of the group of users
    who can read wiki [i] by default. *)
val readers_group_name : wiki -> string

(** [writers_group_name i] returns the name of the group of users
    who can write in wiki [i] by default. *)
val writers_group_name : wiki -> string

(** [rights_adm_group_name i] returns the name of the group of users
    who can change permissions of boxes in wiki [i] by default
    (if boxrights activated). *)
val rights_adm_group_name : wiki -> string

(** [wikiboxes_creators_group_name i] returns the name of the group of users
    who can create wikiboxes in wiki [i] by default. *)
val wikiboxes_creators_group_name : wiki -> string

(** [page_creators_group_name i] returns the name of the group of users
    who can create page in wiki [i] by default
    (if activated). *)
val page_creators_group_name : wiki -> string

(** [css_editors_group_name i] returns the name of the group of users
    who can modify css of pages in wiki [i]. *)
val css_editors_group_name : wiki -> string

(** [container_adm_group_name i] returns the name of the group of users
    who can change the layout (container) of wikipages in wiki [i]. *)
val container_adm_group_name : wiki -> string

(** [admin_group_name i] returns the name of the group of users
    who have all rights on wiki [i]. *)
val admin_group_name : wiki -> string
*)


val new_wikitextbox :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  unit -> int32 Lwt.t


(** Saves a wikibox and returns the new version id of this wikibox. *)
val save_wikibox :
  enough_rights:(sp:Eliom_sessions.server_params -> sd:Ocsimore_common.session_data -> wikibox -> bool Lwt.t) ->
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  wikibox:wikibox ->
  content:string option ->
  content_type:Wiki_sql.wikibox_content_type ->
  int32 Lwt.t

val save_wikitextbox_permissions :
  sp:Eliom_sessions.server_params ->
  sd:Ocsimore_common.session_data ->
  wikibox *
     (string *
        (string *
           (string *
              (string * (string * (string * (string * string))))))) ->
  unit Lwt.t



(** [modified_wikibox box version] returns [Some curversion] iff the current
    version [curversion] of [box] is greater than [version], [None]
    otherwise *)
val modified_wikibox:
  wikibox:wikibox -> boxversion:Int32.t -> Int32.t option Lwt.t


val wikibox_content:
  (?version:int32 -> wikibox ->
    Wiki_sql.wikibox_content Lwt.t) Ocsimore_common.sp_sd

val wikibox_content':
  (?version:int32 -> wikibox ->
    (string option * int32) Lwt.t) Ocsimore_common.sp_sd


(** Raised in case of a non-existing wikibox. The optional [int32]
   argument is the version number *)
exception Unknown_box of wikibox * int32 option
