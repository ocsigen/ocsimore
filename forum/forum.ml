(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
   @author Boris Yakobowski
*)

open User_sql.Types
open Forum_sql.Types

let (>>=) = Lwt.bind
let ($) = User_sql.Types.apply_parameterized_group


(** {2 Forum related groups} *)

let aux_grp name descr =
  Lwt_unix.run (User_sql.new_parametrized_group "forum" name descr)

let message_writers : forum_arg parameterized_group =
  aux_grp "messagewriters" "Can write new messages in the forum"

let message_writers_notmod : forum_arg parameterized_group =
  aux_grp 
    "messagewritersnotmoderated"
    "Can write new messages in the forum without moderation"

let message_moderators : forum_arg parameterized_group =
  aux_grp "messagemoderators" "Can moderate messages in the forum"

let message_deletors : forum_arg parameterized_group =
  aux_grp "messagedeletors" "Can delete messages in the forum"

let message_deletors_if_author : forum_arg parameterized_group =
  aux_grp "messagedeletorsifauthor" 
    "Can delete messages in the forum if author"

let message_sticky_makers : forum_arg parameterized_group =
  aux_grp "messagestickymakers" "Can make messages sticky in the forum"

let message_readers  : forum_arg parameterized_group =
  aux_grp "messagereaders" "Can read messages in the forum"

let comment_writers : forum_arg parameterized_group =
  aux_grp "commentwriters" "Can write new comments in the forum"

let comment_writers_notmod : forum_arg parameterized_group =
  aux_grp 
    "commentwritersnotmoderated"
    "Can write new comments in the forum without moderation"

let comment_moderators : forum_arg parameterized_group =
  aux_grp "commentmoderators" "Can moderate comments in the forum"

let comment_deletors : forum_arg parameterized_group =
  aux_grp "commentdeletors" "Can delete comments in the forum"

let comment_deletors_if_author : forum_arg parameterized_group =
  aux_grp "commentdeletorsifauthor" 
    "Can delete comments in the forum if author"

let comment_sticky_makers : forum_arg parameterized_group =
  aux_grp "commentstickymakers" "Can make comments sticky in the forum"

let comment_readers  : forum_arg parameterized_group =
  aux_grp "commentreaders" "Can read comments in the forum"

let writers : forum_arg parameterized_group =
  aux_grp "writers" "Can write new comments in the forum"

let writers_notmod : forum_arg parameterized_group =
  aux_grp 
    "writersnotmoderated"
    "Can write new messages or comments in the forum without moderation"

let moderators : forum_arg parameterized_group =
  aux_grp "moderators" "Can moderate messages or comments in the forum"

let deletors : forum_arg parameterized_group =
  aux_grp "deletors" "Can delete messages or comments in the forum"

let deletors_if_author : forum_arg parameterized_group =
  aux_grp "deletorsifauthor" 
    "Can delete messages or comments in the forum if author"

let sticky_makers : forum_arg parameterized_group =
  aux_grp "stickymakers" "Can make messages or comments sticky in the forum"

let readers  : forum_arg parameterized_group =
  aux_grp "readers" "Can read messages or comments in the forum"


let forum_admin : forum_arg parameterized_group =
  aux_grp "admin" "All rights on the forum"

let forum_visible : forum_arg parameterized_group =
  aux_grp "visible" "Can see the forum"

let forum_creators =
  User_sql.Types.basic_user
    (Lwt_unix.run
       (Users.create_user 
          ~name:"forum_creators"
          ~pwd:User_sql.Types.Connect_forbidden
          ~fullname:"Can create new forums"
          ()
       ))

(* Generic relations between groups *)
let () = Lwt_unix.run (
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:moderators >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:deletors >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:sticky_makers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:writers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:moderators ~superset:message_moderators >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:moderators ~superset:comment_moderators >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:deletors ~superset:message_deletors >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:deletors ~superset:comment_deletors >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:deletors ~superset:deletors_if_author >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:deletors_if_author ~superset:message_deletors_if_author >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:deletors_if_author ~superset:comment_deletors_if_author >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_deletors ~superset:message_deletors_if_author >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_deletors ~superset:comment_deletors_if_author >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:sticky_makers ~superset:message_sticky_makers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:sticky_makers ~superset:comment_sticky_makers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:writers_notmod ~superset:message_writers_notmod >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:writers_notmod ~superset:comment_writers_notmod >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:writers_notmod ~superset:writers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:writers ~superset:readers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:message_deletors ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_moderators ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_sticky_makers ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_writers ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:readers ~superset:message_readers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:comment_deletors ~superset:comment_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_moderators ~superset:comment_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_sticky_makers ~superset:comment_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_writers ~superset:comment_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:readers ~superset:comment_readers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:message_writers_notmod ~superset:message_writers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:writers ~superset:message_writers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_writers_notmod ~superset:comment_writers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:writers ~superset:comment_writers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:message_readers ~superset:forum_visible >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:comment_readers ~superset:forum_visible
)






(** {2 } *)

let really_create_forum ~title ~descr ~arborescent () =
  Forum_sql.new_forum ~title ~descr ~arborescent ()

let create_forum
    ~title
    ~descr
    ?(arborescent=true)
    () =
  Lwt.catch
    (fun () -> Forum_sql.get_forum ~title ())
    (function
       | Not_found ->
           really_create_forum ~title ~descr ~arborescent () >>= fun id -> 
           Lwt.return { f_id = id; 
                        f_title = title; 
                        f_descr = descr;
                        f_arborescent = arborescent;
                        f_deleted = false
                      }
       | e -> Lwt.fail e)



(** {2 Session data} *)

type role = 
    {
      message_writers : bool Lwt.t Lazy.t;
      message_writers_notmod : bool Lwt.t Lazy.t;
      message_moderators : bool Lwt.t Lazy.t;
      message_deletors : bool Lwt.t Lazy.t;
      message_deletors_if_author : bool Lwt.t Lazy.t;
      message_sticky_makers : bool Lwt.t Lazy.t;
      message_readers : bool Lwt.t Lazy.t;

      comment_writers : bool Lwt.t Lazy.t;
      comment_writers_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_author : bool Lwt.t Lazy.t;
      comment_sticky_makers : bool Lwt.t Lazy.t;
      comment_readers : bool Lwt.t Lazy.t;

      writers : bool Lwt.t Lazy.t;
      writers_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_author : bool Lwt.t Lazy.t;
      sticky_makers : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

let get_role ~sp ~forum_id =
  Users.get_user_id sp >>= fun u ->
  let aux g = Users.in_group ~sp
    ~user:(User_sql.Types.basic_user u) ~group:(g $ forum_id) () 
  in

  Lwt.return
    {
      message_writers = lazy (aux message_writers);
      message_writers_notmod = lazy (aux message_writers_notmod);
      message_moderators = lazy (aux message_moderators);
      message_deletors = lazy (aux message_deletors);
      message_deletors_if_author = lazy (aux message_deletors_if_author);
      message_sticky_makers = lazy (aux message_sticky_makers);
      message_readers = lazy (aux message_readers);
      
      comment_writers = lazy (aux comment_writers);
      comment_writers_notmod = lazy (aux comment_writers_notmod);
      comment_moderators = lazy (aux comment_moderators);
      comment_deletors = lazy (aux comment_deletors);
      comment_deletors_if_author = lazy (aux comment_deletors_if_author);
      comment_sticky_makers = lazy (aux comment_sticky_makers);
      comment_readers = lazy (aux comment_readers);
      
      writers = lazy (aux writers);
      writers_notmod = lazy (aux writers_notmod);
      moderators = lazy (aux moderators);
      deletors = lazy (aux deletors);
      deletors_if_author = lazy (aux deletors_if_author);
      sticky_makers = lazy (aux sticky_makers);
      readers = lazy (aux readers);
      
      forum_admin = lazy (aux forum_admin);
    }

module Roles = Map.Make(struct
                          type t = Forum_sql.Types.forum
                          let compare = compare
                        end)

type forum_sd = Forum_sql.Types.forum -> role Lwt.t

let default_forum_sd ~sp =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  fun k -> 
    try 
      Lwt.return (Roles.find k !cache)
    with Not_found -> 
      get_role ~sp ~forum_id:k >>= fun v ->
      cache := Roles.add k v !cache;
      Lwt.return v

(** The polytable key for retrieving forum data inside session data *)
let forum_key : forum_sd Polytables.key = Polytables.make_key ()

let get_forum_sd ~sp =
  let rc = Eliom_sessions.get_request_cache sp in
  try
    Polytables.get ~table:rc ~key:forum_key
  with Not_found -> 
    let fsd = default_forum_sd ~sp in
    Polytables.set rc forum_key fsd;
    fsd

let get_role ~sp k =
  let forum_sd = get_forum_sd ~sp in
  forum_sd k



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_sql.Types.forum * 
                   Forum_sql.Types.message option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_sql.Types.forum * 
                                   Forum_sql.Types.message option (* parent *))

exception Forum_action_info of forum_action_info

(** {2 Eliom related values} *)

let eliom_forum = Eliom_parameters.user_type forum_of_string string_of_forum 
let eliom_message = Eliom_parameters.user_type message_of_string string_of_message 
let eliom_message_input ?a ~input_type ?name ?value () = 
  Eliom_duce.Xhtml.user_type_input string_of_message ?a ~input_type ?name ?value ()
let eliom_message_button ?a ~name ~value v =
  Eliom_duce.Xhtml.user_type_button string_of_message ?a ~name ~value v
