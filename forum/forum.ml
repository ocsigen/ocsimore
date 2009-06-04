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
  Lwt_unix.run (User_sql.new_parameterized_group "forum" name descr)

let message_creators : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagecreators" "Can create new messages in the forum wiki"

let message_creators_notmod : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_genwikiboxes_creators

let message_moderators : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagemoderators" "Can moderate messages in the forum wiki"

let message_deletors : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_deletors

let message_deletors_if_creator : 
    Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagedeletorsifauthor" 
    "Can delete messages in the forum wiki if author"

let message_sticky_makers : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagestickymakers" "Can make messages sticky in the forum wiki"

let message_readers  : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_grps.grp_reader


let creators : forum_arg parameterized_group =
  aux_grp "creators" "Can create new messages or comments in the forum"

let creators_notmod : forum_arg parameterized_group =
  aux_grp 
    "creatorsnotmoderated"
    "Can create new messages or comments in the forum without moderation"

let moderators : forum_arg parameterized_group =
  aux_grp "moderators" "Can moderate messages or comments in the forum"

let deletors : forum_arg parameterized_group =
  aux_grp "deletors" "Can delete messages or comments in the forum"

let deletors_if_creator : forum_arg parameterized_group =
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
  Lwt_unix.run (User_sql.new_nonparameterized_group ~name:"forum_creators"
                  ~prefix:"forum" ~fullname:"Can create new forums")


(* Generic relations between groups *)
let () = Lwt_unix.run (
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:moderators >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:deletors >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:sticky_makers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:forum_admin ~superset:creators >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:deletors ~superset:deletors_if_creator >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_deletors ~superset:message_deletors_if_creator >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:creators_notmod ~superset:creators >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:creators ~superset:readers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:message_deletors ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_moderators ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_sticky_makers ~superset:message_readers >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:message_creators ~superset:message_readers >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:message_creators_notmod ~superset:message_creators
)






(** {2 } *)
let really_create_forum ~wiki_model ~title ~descr ~arborescent () =
  Wiki.create_wiki
    ~title:(title^" (messages)")
    ~descr:(descr^" (messages)")
    ~boxrights:false
    ~author:User.admin
    ~model:wiki_model
    () >>= fun mw ->
  Wiki.create_wiki
    ~title:(title^" (comments)")
    ~descr:(descr^" (comments)")
    ~boxrights:false
    ~author:User.admin
    ~model:wiki_model
    () >>= fun cw ->
  Forum_sql.new_forum
    ~title ~descr ~arborescent ~messages_wiki:mw ~comments_wiki:cw ()
  >>= fun id ->


  (* Group inclusions that are not generic: *)
  User_sql.add_to_group
    ~user:(moderators $ id) ~group:(message_moderators $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(moderators $ id) ~group:(message_moderators $ cw) >>= fun () ->
  User_sql.add_to_group
    ~user:(deletors $ id) ~group:(message_deletors $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(deletors $ id) ~group:(message_deletors $ cw) >>= fun () ->
  User_sql.add_to_group
    ~user:(deletors_if_creator $ id) ~group:(message_deletors_if_creator $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(deletors_if_creator $ id) ~group:(message_deletors_if_creator $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(sticky_makers $ id) ~group:(message_sticky_makers $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(sticky_makers $ id) ~group:(message_sticky_makers $ cw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators_notmod $ id) ~group:(message_creators_notmod $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators_notmod $ id) ~group:(message_creators_notmod $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(readers $ id) ~group:(message_readers $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(readers $ id) ~group:(message_readers $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(creators $ id) ~group:(message_creators $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators $ id) ~group:(message_creators $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(message_readers $ mw) ~group:(forum_visible $ id) >>= fun () ->
  User_sql.add_to_group
    ~user:(message_readers $ cw) ~group:(forum_visible $ id) >>= fun () ->

  Lwt.return (id, mw, cw)


let create_forum
    ~wiki_model
    ~title
    ~descr
    ?(arborescent=true)
    () =
  Lwt.catch
    (fun () -> Forum_sql.get_forum ~title ())
    (function
       | Not_found ->
           really_create_forum ~wiki_model ~title ~descr ~arborescent () 
           >>= fun (id, mw, cw) -> 
           Lwt.return { f_id = id; 
                        f_title = title; 
                        f_descr = descr;
                        f_arborescent = arborescent;
                        f_deleted = false;
                        f_messages_wiki = mw;
                        f_comments_wiki = cw;
                      }
       | e -> Lwt.fail e)



(** {2 Session data} *)

type role = 
    {
      message_creators : bool Lwt.t Lazy.t;
      message_creators_notmod : bool Lwt.t Lazy.t;
      message_moderators : bool Lwt.t Lazy.t;
      message_deletors : bool Lwt.t Lazy.t;
      message_deletors_if_creator : bool Lwt.t Lazy.t;
      message_sticky_makers : bool Lwt.t Lazy.t;
      message_readers : bool Lwt.t Lazy.t;

      comment_creators : bool Lwt.t Lazy.t;
      comment_creators_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_creator : bool Lwt.t Lazy.t;
      comment_sticky_makers : bool Lwt.t Lazy.t;
      comment_readers : bool Lwt.t Lazy.t;

      creators : bool Lwt.t Lazy.t;
      creators_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_creator : bool Lwt.t Lazy.t;
      sticky_makers : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

let get_role ~sp ~forum =
  User.get_user_id sp >>= fun u ->
  let aux g id = User.in_group ~sp
    ~user:(User_sql.Types.basic_user u) ~group:(g $ id) () 
  in
  let noright = lazy (Lwt.return false) in

  Forum_sql.get_forum ~forum () >>= fun forum_info ->
  User.in_group ~sp ~group:(forum_visible $ forum_info.f_id) ()
  >>= fun b ->
  if b
  then Lwt.return
    {
      message_creators = 
        lazy (aux message_creators forum_info.f_messages_wiki);
      message_creators_notmod = 
        lazy (aux message_creators_notmod forum_info.f_messages_wiki);
      message_moderators = 
        lazy (aux message_moderators forum_info.f_messages_wiki);
      message_deletors = 
        lazy (aux message_deletors forum_info.f_messages_wiki);
      message_deletors_if_creator = 
        lazy (aux message_deletors_if_creator forum_info.f_messages_wiki);
      message_sticky_makers = 
        lazy (aux message_sticky_makers forum_info.f_messages_wiki);
      message_readers = 
        lazy (aux message_readers forum_info.f_messages_wiki);
      
      comment_creators = 
        lazy (aux message_creators forum_info.f_comments_wiki);
      comment_creators_notmod = 
        lazy (aux message_creators_notmod forum_info.f_comments_wiki);
      comment_moderators = 
        lazy (aux message_moderators forum_info.f_comments_wiki);
      comment_deletors = 
        lazy (aux message_deletors forum_info.f_comments_wiki);
      comment_deletors_if_creator = 
        lazy (aux message_deletors_if_creator forum_info.f_comments_wiki);
      comment_sticky_makers = 
        lazy (aux message_sticky_makers forum_info.f_comments_wiki);
      comment_readers = 
        lazy (aux message_readers forum_info.f_comments_wiki);
      
      creators = lazy (aux creators forum);
      creators_notmod = lazy (aux creators_notmod forum);
      moderators = lazy (aux moderators forum);
      deletors = lazy (aux deletors forum);
      deletors_if_creator = lazy (aux deletors_if_creator forum);
      sticky_makers = lazy (aux sticky_makers forum);
      readers = lazy (aux readers forum);
      
      forum_admin = lazy (aux forum_admin forum);
    }
  else Lwt.return
    {
      message_creators = noright;
      message_creators_notmod = noright;
      message_moderators = noright;
      message_deletors = noright;
      message_deletors_if_creator = noright;
      message_sticky_makers = noright;
      message_readers = noright;
      
      comment_creators = noright;
      comment_creators_notmod = noright;
      comment_moderators = noright;
      comment_deletors = noright;
      comment_deletors_if_creator = noright;
      comment_sticky_makers = noright;
      comment_readers = noright;
      
      creators = noright;
      creators_notmod = noright;
      moderators = noright;
      deletors = noright;
      deletors_if_creator = noright;
      sticky_makers = noright;
      readers = noright;
      
      forum_admin = noright;
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
      get_role ~sp ~forum:k >>= fun v ->
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


(** {2 Eliom related values} *)

let eliom_forum = Eliom_parameters.user_type forum_of_string string_of_forum 
let eliom_message = Eliom_parameters.user_type message_of_string string_of_message 
let eliom_forum_input ?a ~input_type ?name ?value () = 
  Eliom_duce.Xhtml.user_type_input string_of_forum ?a ~input_type ?name ?value ()
let eliom_message_input ?a ~input_type ?name ?value () = 
  Eliom_duce.Xhtml.user_type_input string_of_message ?a ~input_type ?name ?value ()
let eliom_message_button ?a ~name ~value v =
  Eliom_duce.Xhtml.user_type_button string_of_message ?a ~name ~value v
