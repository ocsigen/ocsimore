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
  default_admin: Users.group option; (** the (default) group of users
                                         who can change rights for boxes
                                         if acl enabled *)
}


(** Creates a new wiki or returns its id without modification
    if it already exists. *)
val create_wiki :
  title:string ->
  descr:string ->
  ?reader:Users.group -> 
  ?writer:Users.group -> 
  ?admin:Users.group ->
  unit -> 
  wiki_info Lwt.t

val new_wikibox :
  wiki:wiki_info ->
  author:string ->
  comment:string ->
  content:string ->
  ?readers:Users.group list ->
  ?writers:Users.group list -> 
  ?admins: Users.group list ->
  unit -> int32 Lwt.t

val save_wikibox :
  sp:Eliom_sessions.server_params ->
  sd:Users.userdata Eliom_sessions.session_data -> 
  (((int32 * int32) * string) *
     (string option *
        (string option *
           (string option * 
              (string option * (string option * string option)))))) ->
  exn list Lwt.t

(** *)
val can_read : wiki_info -> int32 -> Users.userdata -> bool Lwt.t
val can_write : wiki_info -> int32 -> Users.userdata -> bool Lwt.t

val get_role : 
  sd:Users.userdata Eliom_sessions.session_data -> 
  Wiki_sql.wiki -> 
  int32 -> Wiki_sql.role Lwt.t

(** *)
exception Editbox of (int32 * int32)
exception Action_failed of (int32 * int32 * exn)
exception Operation_not_allowed of (int32 * int32)

val edit_in_progress : sp:Eliom_sessions.server_params -> int32 * int32 -> bool
