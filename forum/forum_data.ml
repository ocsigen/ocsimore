(* Ocsimore
 * Copyright (C) 2009
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
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Forum
open Forum_sql.Types

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group


(** {2 Database access with verification of permissions} *)

let new_forum ~sp ~title ~descr ?arborescent ~messages_wiki ~comments_wiki () =
  Users.in_group ~sp ~group:forum_creators () >>= fun b ->
  if b
  then Forum_sql.new_forum
    ~title ~descr ?arborescent ~messages_wiki ~comments_wiki ()
  else Lwt.fail Ocsimore_common.Permission_denied

let new_message ~sp ~forum ~creator_id ?subject ?parent_id ?sticky ~text () = 
  Forum.get_role sp forum >>= fun role ->
  Forum_sql.get_forum ~forum () >>= fun f ->
  if f.f_deleted
  then Lwt.fail Ocsimore_common.Permission_denied
  else
    let first_msg = parent_id = None in
    !!(role.message_creators) >>= fun message_creators ->
    !!(role.comment_creators) >>= fun comment_creators ->
    if (first_msg && message_creators)
      || (not first_msg && comment_creators)
    then begin
      !!(role.message_moderators) >>= fun message_moderators ->
      !!(role.message_creators_notmod) >>= fun message_creators_notmod ->
      !!(role.comment_moderators) >>= fun comment_moderators ->
      !!(role.comment_creators_notmod) >>= fun comment_creators_notmod ->
      let moderated =
        (first_msg && (message_moderators || message_creators_notmod))
        || (not first_msg && (comment_moderators || comment_creators_notmod))
      in
      ((* If the forum is not arborescent, we must not comment comments *)
        match parent_id with
         | None -> (* it is not a comment *) Lwt.return (true, f.f_messages_wiki)
         | Some parent_id -> (* it is a comment *)
             if f.f_arborescent
             then Lwt.return (true, f.f_comments_wiki)
             else 
               Forum_sql.get_message ~message_id:parent_id ()
               >>= fun m ->
               Lwt.return ((m.m_parent_id = None), f.f_comments_wiki)
      ) >>= fun (ok, wiki) ->
      if ok
      then
        Forum_sql.new_message ~sp ~forum ~wiki ~creator_id
          ?subject ?parent_id ~moderated ?sticky ~text
      else Lwt.fail Ocsimore_common.Permission_denied
    end
    else Lwt.fail Ocsimore_common.Permission_denied

(* AEFF
let set_deleted ~sp ~message_id ~deleted =
  Forum_sql.get_message ~message_id () >>= fun m ->
  Forum.get_role sp m.m_forum >>= fun role ->
  Users.get_user_data sp >>= fun u ->
  let uid = u.User_sql.Types.user_id in
  let first_msg = m.m_parent_id = None in
  !!(role.message_deletors) >>= fun message_deletors ->
  !!(role.message_deletors_if_creator) >>= fun message_deletors_if_creator ->
  !!(role.comment_deletors) >>= fun comment_deletors ->
  !!(role.comment_deletors_if_creator) >>= fun comment_deletors_if_creator ->
  if ((first_msg && (message_deletors ||
                       (m.m_creator_id = uid && message_deletors_if_creator)))
      || (not first_msg &&
            (comment_deletors ||
               (m.m_creator_id = uid && comment_deletors_if_creator))))
  then Forum_sql.set_deleted ~message_id ~deleted
  else Lwt.fail Ocsimore_common.Permission_denied
*)

let set_moderated ~sp ~message_id ~moderated =
  Forum_sql.get_message ~message_id () >>= fun m ->
  Forum.get_role sp m.m_forum >>= fun role ->
  let first_msg = m.m_parent_id = None in
  !!(role.message_moderators) >>= fun message_moderators ->
  !!(role.comment_moderators) >>= fun comment_moderators ->
  if ((first_msg && message_moderators)
      || (not first_msg && comment_moderators))
  then Forum_sql.set_moderated ~message_id ~moderated
  else Lwt.fail Ocsimore_common.Permission_denied

let set_sticky ~sp ~message_id ~sticky =
  Forum_sql.get_message ~message_id () >>= fun m ->
  Forum.get_role sp m.m_forum >>= fun role ->
  let first_msg = m.m_parent_id = None in
  !!(role.message_sticky_makers) >>= fun message_sticky_makers ->
  !!(role.comment_sticky_makers) >>= fun comment_sticky_makers ->
  if ((first_msg && message_sticky_makers)
      || (not first_msg && comment_sticky_makers))
  then Forum_sql.set_sticky ~message_id ~sticky
  else Lwt.fail Ocsimore_common.Permission_denied

let get_forum ~sp ?forum ?title () =
  Forum_sql.get_forum ?forum ?title () >>= fun f ->
  Users.in_group ~sp ~group:(forum_visible $ f.f_id) () >>= fun b ->
  if b
  then Lwt.return f
  else Lwt.fail Ocsimore_common.Permission_denied

let get_forums_list ~sp () =
  Forum_sql.get_forums_list () >>= fun l ->
  List.fold_right
    (fun f e -> 
       e >>= fun e ->
       let f = get_forum_info f in
       Users.in_group ~sp ~group:(forum_visible $ f.f_id) () >>= fun b ->
       if b
       then Lwt.return (f::e)
       else Lwt.return e)
    l
    (Lwt.return [])

let get_message ~sp ~message_id =
  Forum_sql.get_message ~message_id () >>= fun m ->
  Forum_sql.get_forum ~forum:m.m_forum () >>= fun _ ->
  (* get_forum only to verify that the forum is not deleted? *)
  Forum.get_role sp m.m_forum >>= fun role ->
  let first_msg = m.m_parent_id = None in
  !!(role.message_readers) >>= fun message_readers ->
  !!(role.comment_readers) >>= fun comment_readers ->
  if not ((first_msg && message_readers)
          || (not first_msg && comment_readers))
  then Lwt.fail Ocsimore_common.Permission_denied
  else 
    !!(role.message_moderators) >>= fun message_moderators ->
    !!(role.comment_moderators) >>= fun comment_moderators ->
    if (not m.m_moderated && 
          ((first_msg && not message_moderators)
           || (not first_msg && not comment_moderators)))
    then Lwt.fail Ocsimore_common.Permission_denied
    else Lwt.return m


let get_thread ~sp ~message_id =
  let message_id_int32 = Forum_sql.Types.sql_of_message message_id in
  let comment_filter comment_moderators comments =
    let rec aux min = function
      | [] -> []
      | c::l ->
          let c = get_message_info c in
          if c.m_tree_min < min
          then aux min l
          else if (*AEFF c.m_deleted || *) (not c.m_moderated && not comment_moderators)
          then aux c.m_tree_max l
          else c::aux min l
    in aux message_id_int32 comments
  in
  Forum_sql.get_thread ~message_id ()
  >>= function
    | [] -> Lwt.fail Not_found
    | (m::l) as th ->
        let m = get_message_info m in
        assert (message_id = m.m_id);
(*AEFF        if m.m_deleted
        then Lwt.fail Not_found
        else *)

        Forum_sql.get_forum ~forum:m.m_forum () >>= fun _ ->
        (* get_forum only to verify that the forum is not deleted *)
        Forum.get_role sp m.m_forum >>= fun role ->
        let first_msg = m.m_parent_id = None in
        !!(role.message_readers) >>= fun message_readers ->
        !!(role.comment_readers) >>= fun comment_readers ->
        !!(role.message_moderators) >>= fun message_moderators ->
        !!(role.comment_moderators) >>= fun comment_moderators ->
        if (first_msg && message_readers)
        then 
          (if (not m.m_moderated && (not message_moderators))
           then Lwt.fail Ocsimore_common.Permission_denied
           else 
             (if comment_readers
              then Lwt.return (m::comment_filter comment_moderators l)
              else Lwt.return [m]))
        else 
          if (not first_msg && comment_readers)
          then Lwt.return (comment_filter comment_moderators th)
          else Lwt.fail Ocsimore_common.Permission_denied

let get_message_list ~sp ~forum ~first ~number () =
  Forum_sql.get_forum ~forum () >>= fun _ ->
  (* get_forum only to verify that the forum is not deleted *)
  Forum.get_role sp forum >>= fun role ->
  !!(role.message_readers) >>= fun message_readers ->
  if not message_readers
  then Lwt.fail Ocsimore_common.Permission_denied
  else
    !!(role.message_moderators) >>= fun message_moderators ->
    Forum_sql.get_message_list ~forum ~first ~number 
      ~moderated_only:(not message_moderators) ()

