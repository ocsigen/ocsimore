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


let (>>=) = Lwt.bind
let (!!) = Lazy.force

open User_sql.Types
open Forum

(** {2 Database access with verification of permissions} *)

let new_forum ~sp ~sd ~title ~descr ?arborescent () =
  Users.get_user_data sp sd >>= fun u ->
  Users.in_group ~sp ~sd ~user:u.user_id
    ~group:Forum.forum_creators.user_id ()
  >>= fun b ->
  if b
  then Forum_sql.new_forum ~title ~descr ?arborescent ()
  else Lwt.fail Ocsimore_common.Permission_denied

let new_message ~sp ~sd ~forum_id ~author_id
    ?subject ?parent_id ?sticky ~text () = 
  Forum.get_role sp sd forum_id >>= fun role ->
  Forum_sql.get_forum ~forum_id () >>= fun (_, _, _, arborescent, deleted) ->
  if deleted
  then Lwt.fail Ocsimore_common.Permission_denied
  else
    let first_msg = parent_id = None in
    !!(role.message_writers) >>= fun message_writers ->
    !!(role.comment_writers) >>= fun comment_writers ->
    if (first_msg && message_writers)
      || (not first_msg && comment_writers)
    then begin
      !!(role.message_moderators) >>= fun message_moderators ->
      !!(role.message_writers_notmod) >>= fun message_writers_notmod ->
      !!(role.comment_moderators) >>= fun comment_moderators ->
      !!(role.comment_writers_notmod) >>= fun comment_writers_notmod ->
      let moderated =
        (first_msg && (message_moderators || message_writers_notmod))
        || (not first_msg && (comment_moderators || comment_writers_notmod))
      in
      ((* If the forum is not arborescent, we must not comment comments *)
        match parent_id with
         | None -> (* it is not a comment *) Lwt.return true
         | Some parent_id -> (* it is a comment *)
             if arborescent
             then Lwt.return true
             else 
               Forum_sql.get_message ~message_id:parent_id ()
               >>= fun (_id, _, _, _, parparent_id, _root_id, _, _, _, _, _, _, _) ->
               Lwt.return (parparent_id = None)
      ) >>= fun ok ->
      if ok
      then
        Forum_sql.new_message ~forum_id ~author_id
          ?subject ?parent_id ~moderated ?sticky ~text
      else Lwt.fail Ocsimore_common.Permission_denied
    end
    else Lwt.fail Ocsimore_common.Permission_denied

let set_deleted ~sp ~sd ~message_id ~deleted =
  Forum_sql.get_message ~message_id ()
  >>= fun (_, _, author_id, _, parent_id, _, forum_id, _, _, _, _, _, _) ->
  Forum.get_role sp sd forum_id >>= fun role ->
  Users.get_user_data sp sd >>= fun u ->
  let uid = u.user_id in
  let first_msg = parent_id = None in
  !!(role.message_deletors) >>= fun message_deletors ->
  !!(role.message_deletors_if_author) >>= fun message_deletors_if_author ->
  !!(role.comment_deletors) >>= fun comment_deletors ->
  !!(role.comment_deletors_if_author) >>= fun comment_deletors_if_author ->
  if ((first_msg && (message_deletors ||
                       (author_id = uid && message_deletors_if_author)))
      || (not first_msg &&
            (comment_deletors ||
               (author_id = uid && comment_deletors_if_author))))
  then Forum_sql.set_deleted ~message_id ~deleted
  else Lwt.fail Ocsimore_common.Permission_denied

let set_moderated ~sp ~sd ~message_id ~moderated =
  Forum_sql.get_message ~message_id ()
  >>= fun (_, _, _, _, parent_id, _, forum_id, _, _, _, _, _, _) ->
  Forum.get_role sp sd forum_id >>= fun role ->
  let first_msg = parent_id = None in
  !!(role.message_moderators) >>= fun message_moderators ->
  !!(role.comment_moderators) >>= fun comment_moderators ->
  if ((first_msg && message_moderators)
      || (not first_msg && comment_moderators))
  then Forum_sql.set_moderated ~message_id ~moderated
  else Lwt.fail Ocsimore_common.Permission_denied

let set_sticky ~sp ~sd ~message_id ~sticky =
  Forum_sql.get_message ~message_id ()
  >>= fun (_, _, _, _, parent_id, _, forum_id, _, _, _, _, _, _) ->
  Forum.get_role sp sd forum_id >>= fun role ->
  let first_msg = parent_id = None in
  !!(role.message_sticky_setters) >>= fun message_sticky_setters ->
  !!(role.comment_sticky_setters) >>= fun comment_sticky_setters ->
  if ((first_msg && message_sticky_setters)
      || (not first_msg && comment_sticky_setters))
  then Forum_sql.set_sticky ~message_id ~sticky
  else Lwt.fail Ocsimore_common.Permission_denied

let get_forum ~sp ~sd ?forum_id ?title () =
  Forum_sql.get_forum ?forum_id ?title () 
  >>= fun ((i, _, _, _, _deleted) as f) ->
  Users.get_user_data sp sd >>= fun u ->
  let u = u.user_id in
  Forum.forum_visible_group i >>= fun g ->
  Users.in_group ~sp ~sd ~user:u ~group:g () >>= fun b ->
  if b
  then Lwt.return f
  else Lwt.fail Ocsimore_common.Permission_denied

let get_forums_list ~sp ~sd () =
  Users.get_user_data sp sd >>= fun u ->
  let u = u.user_id in
  Forum_sql.get_forums_list () >>= fun l ->
  Ocsimore_lib.lwt_filter
    (fun (i, _, _, _, _) ->
       Forum.forum_visible_group i >>= fun g ->
       Users.in_group ~sp ~sd ~user:u ~group:g ())
    l

let get_message ~sp ~sd ~message_id =
  Forum_sql.get_message ~message_id ()
  >>= fun ((_, _, _, _, parent_id, _, forum_id, _, moderated, _deleted, _sticky, _, _)
             as msg) ->
  Forum_sql.get_forum ~forum_id () >>= fun (_, _, _, _, _deleted) ->
  Forum.get_role sp sd forum_id >>= fun role ->
  let first_msg = parent_id = None in
  !!(role.message_readers) >>= fun message_readers ->
  !!(role.comment_readers) >>= fun comment_readers ->
  if not ((first_msg && message_readers)
          || (not first_msg && comment_readers))
  then Lwt.fail Ocsimore_common.Permission_denied
  else 
    !!(role.message_moderators) >>= fun message_moderators ->
    !!(role.comment_moderators) >>= fun comment_moderators ->
    if (not moderated && 
          ((first_msg && not message_moderators)
           || (not first_msg && not comment_moderators)))
    then Lwt.fail Ocsimore_common.Permission_denied
    else Lwt.return msg


let get_thread ~sp ~sd ~message_id =
  let comment_filter comment_moderators comments =
    let rec aux min = function
      | [] -> []
      | ((_, _, _, _, _, _, _, _, 
          moderated, deleted, _, tree_min, tree_max) as c)::l ->
          if tree_min < min
          then aux min l
          else if deleted || (not moderated && not comment_moderators)
          then aux tree_max l
          else c::aux min l
    in aux message_id comments
  in
  Forum_sql.get_thread ~message_id ()
  >>= function
    | [] -> Lwt.fail Not_found
    | (((id, _, _, _, parent_id, _, forum_id, _, moderated, deleted, _sticky, _, _)
          as msg)::l) as th ->
        assert (message_id = id);
        if deleted
        then Lwt.fail Not_found
        else
          Forum_sql.get_forum ~forum_id () >>= fun (_, _, _, _, _) ->
          (* get_forum only to verify that the forum is not deleted *)
          Forum.get_role sp sd forum_id >>= fun role ->
          let first_msg = parent_id = None in
          !!(role.message_readers) >>= fun message_readers ->
          !!(role.comment_readers) >>= fun comment_readers ->
          !!(role.message_moderators) >>= fun message_moderators ->
          !!(role.comment_moderators) >>= fun comment_moderators ->
          if (first_msg && message_readers)
          then 
            (if (not moderated && (not message_moderators))
             then Lwt.fail Ocsimore_common.Permission_denied
             else 
               (if comment_readers
                then Lwt.return (msg::comment_filter comment_moderators l)
                else Lwt.return [msg]))
          else 
            if (not first_msg && comment_readers)
            then Lwt.return (comment_filter comment_moderators th)
            else Lwt.fail Ocsimore_common.Permission_denied
