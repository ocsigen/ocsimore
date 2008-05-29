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
  page_creators: User_sql.userid;
  default_reader: User_sql.userid;
  default_writer: User_sql.userid;
  default_admin: User_sql.userid option; (** the (default) group of users
                                         who can change rights for boxes
                                         if acl enabled *)
}


(** Creates a new wiki or returns its id without modification
    if it already exists. 
    If the optional argument [path] is present, 
    the wiki will be bound to an URL.
    In that case, only users in group [page_creators] can create a new page
    (default: [Users.users_group])
*)
val create_wiki :
  title:string ->
  descr:string ->
  ?sp: Eliom_sessions.server_params ->
  ?path: string list ->
  ?page_creators:User_sql.userid ->
  ?reader:User_sql.userid -> 
  ?writer:User_sql.userid -> 
  ?admin:User_sql.userid ->
  wikibox: Wiki_widgets.editable_wikibox ->
  unit -> 
  wiki_info Lwt.t

(** Returns wiki information from an id. 
    Wiki information is kept in memory (and savec in the database)
*)
val get_wiki_by_id : int32 -> wiki_info Lwt.t

(** Returns wiki information from a name. *)
val get_wiki_by_name : string -> wiki_info Lwt.t

val new_wikibox :
  wiki:wiki_info ->
  author:string ->
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

