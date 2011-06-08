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

open Eliom_pervasives
open User_sql.Types
open Forum_types

let (>>=) = Lwt.bind
let ($) = User_sql.Types.apply_parameterized_group



let forum_param = {
  param_description = "id of the forum";
  param_display = None;
  find_param_functions = None;
}

let message_param = {
  param_description = "id of the message";
  param_display = None;
  find_param_functions = None;
}



(** {2 Forum related groups} *)

let aux_grp name descr param =
  Lwt_unix.run (User_sql.new_parameterized_group "forum" name descr param)

let message_creators : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagecreators" "Can create new messages in the forum wiki"
    Wiki.param_wiki

let message_creators_notmod : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_creators

let message_moderators : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagemoderators" "Can moderate messages in the forum wiki"
    Wiki.param_wiki

let message_deletors : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_deletors

let message_deletors_if_creator : 
    Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagedeletorsifcreator" 
    "Can delete messages in the forum wiki if author"
    Wiki.param_wiki

let message_sticky_makers : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagestickymakers" "Can make messages sticky in the forum wiki"
    Wiki.param_wiki

let message_readers_evennotmoderated : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_grps.grp_reader

let moderated_message_readers : Wiki_types.wiki_arg parameterized_group =
  aux_grp "moderatedmessagereaders" "Can read moderated messages in the forum wiki"
    Wiki.param_wiki

let message_modifiers : Wiki_types.wiki_arg parameterized_group =
  Wiki.wiki_wikiboxes_grps.grp_writer

let message_modifiers_if_creator : Wiki_types.wiki_arg parameterized_group =
  aux_grp "messagemodifierifcreator" "Can modify their own messages in the forum wiki"
    Wiki.param_wiki


let creators : forum_arg parameterized_group =
  aux_grp "creators" "Can create new messages or comments in the forum"
    forum_param

let creators_notmod : forum_arg parameterized_group =
  aux_grp 
    "creatorsnotmoderated"
    "Can create new messages or comments in the forum without moderation"
    forum_param

let moderators : forum_arg parameterized_group =
  aux_grp "moderators" "Can moderate messages or comments in the forum"
    forum_param

let deletors : forum_arg parameterized_group =
  aux_grp "deletors" "Can delete messages or comments in the forum"
    forum_param

let deletors_if_creator : forum_arg parameterized_group =
  aux_grp "deletorsifauthor" 
    "Can delete messages or comments in the forum if author"  forum_param

let modifiers : forum_arg parameterized_group =
  aux_grp "modifiers" "Can modify messages or comments in the forum (without moderation)"  forum_param

let modifiers_if_creator : forum_arg parameterized_group =
  aux_grp "modifiersifauthor" 
    "Can modify messages or comments in the forum if author (without moderation)" forum_param

let sticky_makers : forum_arg parameterized_group =
  aux_grp "stickymakers" "Can make messages or comments sticky in the forum"
    forum_param

let readers  : forum_arg parameterized_group =
  aux_grp "readers" "Can read messages or comments in the forum (even not moderated)" forum_param

let moderated_readers : forum_arg parameterized_group =
  aux_grp "moderatedreaders" "Can read moderated messages or comments in the forum"  forum_param


let forum_admin : forum_arg parameterized_group =
  aux_grp "admin" "All rights on the forum"  forum_param

let forum_visible : forum_arg parameterized_group =
  aux_grp "visible" "Can see the forum" forum_param

let forum_creators =
  Lwt_unix.run (User_sql.new_nonparameterized_group ~name:"forum_creators"
                  ~prefix:"forum" ~descr:"Can create new forums")



(* special rights for some threads - 
   will override forum rights if present.
   There is a boolean information on the root of the thread to tell that 
   it has special rights. The group parameter is the root identifier.
*)

let thread_readers_evennotmoderated : message_arg parameterized_group =
  aux_grp "threadreadersnotmod"
    "Can read all messages or comments in the thread (even not moderated)"
    message_param
    (* replaces readers *)

let thread_moderated_readers : message_arg parameterized_group =
  aux_grp "threadreaders"
    "Can read moderated messages or comments in the thread"
    message_param
    (* replaces moderated_readers *)

let thread_comments_creators : message_arg parameterized_group =
  aux_grp "threadcommentcreators" "Can add comments in the thread"
    message_param
    (* replaces creators *)

let thread_comments_creators_notmod : message_arg parameterized_group =
  aux_grp "threadcommentcreatorsnotmod" 
    "Can add comments in the thread without moderation"
    message_param
    (* replaces creators_notmod *)



(* Generic relations between groups *)
let () = Lwt_unix.run (

  User_sql.add_to_group_generic
    ~user:forum_admin ~group:moderators >>= fun () ->
  User_sql.add_to_group_generic
    ~user:forum_admin ~group:deletors >>= fun () ->
  User_sql.add_to_group_generic
    ~user:forum_admin ~group:modifiers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:forum_admin ~group:sticky_makers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:forum_admin ~group:creators_notmod >>= fun () ->

  User_sql.add_to_group_generic
    ~user:deletors ~group:deletors_if_creator >>= fun () ->
  User_sql.add_to_group_generic
    ~user:modifiers ~group:modifiers_if_creator >>= fun () ->
  User_sql.add_to_group_generic
    ~user:creators_notmod ~group:creators >>= fun () ->
  User_sql.add_to_group_generic
    ~user:creators ~group:moderated_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:readers ~group:moderated_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:moderators ~group:readers >>= fun () ->

  User_sql.add_to_group_generic
    ~user:message_deletors ~group:message_deletors_if_creator >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_modifiers ~group:message_modifiers_if_creator >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_creators_notmod ~group:message_creators >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_creators_notmod ~group:message_readers_evennotmoderated >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_moderators ~group:message_readers_evennotmoderated >>= fun () ->

  User_sql.add_to_group_generic
    ~user:message_deletors ~group:moderated_message_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_readers_evennotmoderated ~group:moderated_message_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_sticky_makers ~group:moderated_message_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_creators ~group:moderated_message_readers >>= fun () ->
  User_sql.add_to_group_generic
    ~user:message_modifiers ~group:moderated_message_readers >>= fun () ->

  User_sql.add_to_group_generic
    ~user:thread_readers_evennotmoderated ~group:thread_moderated_readers
  >>= fun () ->
  User_sql.add_to_group_generic
    ~user:thread_comments_creators_notmod ~group:thread_comments_creators
  >>= fun () ->
  User_sql.add_to_group_generic
    ~user:thread_comments_creators ~group:thread_moderated_readers

)






(** {2 } *)
let really_create_forum
    ~wiki_model ~title_syntax ~title ~descr ~arborescent () =
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
    ~title ~descr ~arborescent ~title_syntax
    ~messages_wiki:mw ~comments_wiki:cw ()
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
    ~user:(modifiers $ id) ~group:(message_modifiers $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(modifiers $ id) ~group:(message_modifiers $ cw) >>= fun () ->
  User_sql.add_to_group
    ~user:(modifiers_if_creator $ id) ~group:(message_modifiers_if_creator $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(modifiers_if_creator $ id) ~group:(message_modifiers_if_creator $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(sticky_makers $ id) ~group:(message_sticky_makers $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(sticky_makers $ id) ~group:(message_sticky_makers $ cw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators_notmod $ id) ~group:(message_creators_notmod $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators_notmod $ id) ~group:(message_creators_notmod $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(moderated_readers $ id) ~group:(moderated_message_readers $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(moderated_readers $ id) ~group:(moderated_message_readers $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(readers $ id) ~group:(message_readers_evennotmoderated $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(readers $ id) ~group:(message_readers_evennotmoderated $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(creators $ id) ~group:(message_creators $ mw) >>= fun () ->
  User_sql.add_to_group
    ~user:(creators $ id) ~group:(message_creators $ cw) >>= fun () ->

  User_sql.add_to_group
    ~user:(moderated_message_readers $ mw) ~group:(forum_visible $ id) >>= fun () ->
  User_sql.add_to_group
    ~user:(moderated_message_readers $ cw) ~group:(forum_visible $ id) >>= fun () ->

  Lwt.return (id, mw, cw)


let create_forum
    ~wiki_model
    ~title_syntax
    ~title
    ~descr
    ?(arborescent=true)
    () =
  Lwt.catch
    (fun () -> Forum_sql.get_forum ~title ())
    (function
       | Not_found ->
           really_create_forum
             ~wiki_model ~title ~descr ~arborescent ~title_syntax () 
           >>= fun (id, mw, cw) -> 
           Lwt.return { f_id = id; 
                        f_title = title; 
                        f_descr = descr;
                        f_arborescent = arborescent;
                        f_deleted = false;
                        f_title_syntax = title_syntax;
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
      message_modifiers : bool Lwt.t Lazy.t;
      message_modifiers_if_creator : bool Lwt.t Lazy.t;
      message_sticky_makers : bool Lwt.t Lazy.t;
      moderated_message_readers : bool Lwt.t Lazy.t;
      message_readers_evennotmoderated : bool Lwt.t Lazy.t;

      comment_creators : bool Lwt.t Lazy.t;
      comment_creators_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_creator : bool Lwt.t Lazy.t;
      comment_modifiers : bool Lwt.t Lazy.t;
      comment_modifiers_if_creator : bool Lwt.t Lazy.t;
      comment_sticky_makers : bool Lwt.t Lazy.t;
      moderated_comment_readers : bool Lwt.t Lazy.t;
      comment_readers_evennotmoderated : bool Lwt.t Lazy.t;

      creators : bool Lwt.t Lazy.t;
      creators_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_creator : bool Lwt.t Lazy.t;
      modifiers : bool Lwt.t Lazy.t;
      modifiers_if_creator : bool Lwt.t Lazy.t;
      sticky_makers : bool Lwt.t Lazy.t;
      moderated_readers : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

let get_role ~forum =
  User.get_user_id () >>= fun u ->
  let aux g id = User.in_group
    ~user:(User_sql.Types.basic_user u) ~group:(g $ id) () 
  in
  let noright = lazy (Lwt.return false) in

  Forum_sql.get_forum ~forum () >>= fun forum_info ->
  User.in_group ~group:(forum_visible $ forum_info.f_id) () >>= fun b ->
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
      message_modifiers = 
        lazy (aux message_modifiers forum_info.f_messages_wiki);
      message_modifiers_if_creator = 
        lazy (aux message_modifiers_if_creator forum_info.f_messages_wiki);
      message_sticky_makers = 
        lazy (aux message_sticky_makers forum_info.f_messages_wiki);
      moderated_message_readers = 
        lazy (aux moderated_message_readers forum_info.f_messages_wiki);
      message_readers_evennotmoderated = 
        lazy (aux message_readers_evennotmoderated forum_info.f_messages_wiki);
      
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
      comment_modifiers = 
        lazy (aux message_modifiers forum_info.f_comments_wiki);
      comment_modifiers_if_creator = 
        lazy (aux message_modifiers_if_creator forum_info.f_comments_wiki);
      comment_sticky_makers = 
        lazy (aux message_sticky_makers forum_info.f_comments_wiki);
      moderated_comment_readers = 
        lazy (aux moderated_message_readers forum_info.f_comments_wiki);
      comment_readers_evennotmoderated = 
        lazy (aux message_readers_evennotmoderated forum_info.f_comments_wiki);
      
      creators = lazy (aux creators forum);
      creators_notmod = lazy (aux creators_notmod forum);
      moderators = lazy (aux moderators forum);
      deletors = lazy (aux deletors forum);
      deletors_if_creator = lazy (aux deletors_if_creator forum);
      modifiers = lazy (aux modifiers forum);
      modifiers_if_creator = lazy (aux modifiers_if_creator forum);
      sticky_makers = lazy (aux sticky_makers forum);
      moderated_readers = lazy (aux moderated_readers forum);
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
      message_modifiers = noright;
      message_modifiers_if_creator = noright;
      message_sticky_makers = noright;
      moderated_message_readers = noright;
      message_readers_evennotmoderated = noright;
      
      comment_creators = noright;
      comment_creators_notmod = noright;
      comment_moderators = noright;
      comment_deletors = noright;
      comment_deletors_if_creator = noright;
      comment_modifiers = noright;
      comment_modifiers_if_creator = noright;
      comment_sticky_makers = noright;
      moderated_comment_readers = noright;
      comment_readers_evennotmoderated = noright;
      
      creators = noright;
      creators_notmod = noright;
      moderators = noright;
      deletors = noright;
      deletors_if_creator = noright;
      modifiers = noright;
      modifiers_if_creator = noright;
      sticky_makers = noright;
      moderated_readers = noright;
      readers = noright;
      
      forum_admin = noright;
    }


module Roles = Map.Make(struct
                          type t = Forum_types.forum
                          let compare = compare
                        end)

type forum_sd = Forum_types.forum -> role Lwt.t

let default_forum_sd () =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  fun k -> 
    try 
      Lwt.return (Roles.find k !cache)
    with Not_found -> 
      get_role ~forum:k >>= fun v ->
      cache := Roles.add k v !cache;
      Lwt.return v

(** The polytable key for retrieving forum data inside session data *)
let forum_key : forum_sd Polytables.key = Polytables.make_key ()

let get_forum_sd () =
  let rc = Eliom_request_info.get_request_cache () in
  try
    Polytables.get ~table:rc ~key:forum_key
  with Not_found -> 
    let fsd = default_forum_sd () in
    Polytables.set rc forum_key fsd;
    fsd

let get_role k =
  let forum_sd = get_forum_sd () in
  forum_sd k



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_types.forum * 
                   Forum_types.message option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_types.forum * 
                                   Forum_types.message option (* parent *))


(** {2 Eliom related values} *)

let eliom_forum =
  Eliom_parameters.user_type
    ~of_string:forum_of_string ~to_string:string_of_forum 

let eliom_message = 
  Eliom_parameters.user_type
    ~of_string:message_of_string ~to_string:string_of_message 

let eliom_forum_input ?a ~input_type ?name ?value () = 
  Eliom_output.Html5.user_type_input string_of_forum ?a ~input_type ?name ?value ()
let eliom_message_input ?a ~input_type ?name ?value () = 
  Eliom_output.Html5.user_type_input string_of_message ?a ~input_type ?name ?value ()
let eliom_message_button ?a ~name ~value v =
  Eliom_output.Html5.user_type_button string_of_message ?a ~name ~value v


(** {2 Right model for forum's wikis} *)

let is_creator wb =
  User.get_user_id () >>= fun u ->
  Forum_sql.get_wikibox_creator ~wb >>= function
    | None -> Lwt.return false
    | Some v -> Lwt.return (u = v)

class wiki_rights : Wiki_types.wiki_rights =
object (_self)

  inherit Wiki.wiki_rights as papa

  method can_write_wikibox wb = 
    papa#can_write_wikibox wb >>= fun b ->
    if b 
    then Lwt.return true
    else begin
      Wiki_sql.get_wikibox_info wb >>= fun { Wiki_types.wikibox_wiki = wiki} ->
      User.in_group ~group:(message_modifiers_if_creator $ wiki) ()
      >>= fun b ->
      if b
      then is_creator wb
      else Lwt.return false
    end

  method can_read_wikibox wb = 
    papa#can_read_wikibox wb >>= fun b ->
    if b 
    then Lwt.return true
    else begin
      Wiki_sql.get_wikibox_info wb >>= fun { Wiki_types.wikibox_wiki = wiki} ->
      User.in_group ~group:(moderated_message_readers $ wiki) ()
      >>= fun b ->
      if b
      then Forum_sql.wikibox_is_moderated ~wb
      else Lwt.return false
    end


end
