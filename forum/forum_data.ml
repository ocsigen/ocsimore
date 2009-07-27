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
open Forum_types

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group


(** {2 Database access with verification of permissions} *)

let new_forum
    ~sp ~title ~descr ?arborescent ~title_syntax
    ~messages_wiki ~comments_wiki () =
  User.in_group ~sp ~group:forum_creators () >>= fun b ->
  if b
  then Forum_sql.new_forum
    ~title ~descr ?arborescent ~title_syntax ~messages_wiki ~comments_wiki ()
  else Lwt.fail Ocsimore_common.Permission_denied

let can_create_message ~sp ~parent_id f role =
  (* returns (wiki * bool (* moderated *)) or fails with Permission_denied *)
  let first_msg = parent_id = None in
  (match parent_id with
     | None -> Lwt.return None
     | Some parent_id -> 
        Forum_sql.get_message ~message_id:parent_id () >>= fun parent ->
        (* If the forum is not arborescent, we must not comment comments *)
        if (not f.f_arborescent) && (parent.m_parent_id != None)
        then Lwt.fail Ocsimore_common.Permission_denied
        else
          !!(parent.m_has_special_rights) >>= fun has_special_rights ->
          if has_special_rights
          then Lwt.return (Some parent)
          else Lwt.return None)
  >>= fun special_rights ->
  let (mod_creator, notmod_creator) =
    match special_rights with
      | Some parent ->
          (User.in_group 
             ~sp ~group:(Forum.thread_comments_creators $ parent.m_root_id) (),
           User.in_group
             ~sp 
             ~group:(Forum.thread_comments_creators_notmod $ parent.m_root_id)
             ())
      | None ->
          ((!!(role.message_creators) >>= fun message_creators ->
            !!(role.comment_creators) >>= fun comment_creators ->
            Lwt.return ((first_msg && message_creators) ||
                          (not first_msg && comment_creators))),
           (!!(role.message_creators_notmod) >>= fun message_creators_notmod ->
            !!(role.comment_creators_notmod) >>= fun comment_creators_notmod ->
            Lwt.return ((first_msg && message_creators_notmod) ||
                          (not first_msg && comment_creators_notmod))))
  in
  let wiki = if first_msg then f.f_messages_wiki else f.f_comments_wiki in
  notmod_creator >>= fun notmod_creator ->
  if notmod_creator
  then Lwt.return (wiki, true)
  else
    mod_creator >>= fun mod_creator ->
    if not mod_creator
    then Lwt.fail Ocsimore_common.Permission_denied
    else 
      !!(role.message_moderators) >>= fun message_moderators ->
      !!(role.comment_moderators) >>= fun comment_moderators ->
      let moderator =
        ((first_msg && message_moderators) ||
           (not first_msg && comment_moderators))
      in
      Lwt.return (wiki, moderator)

let new_message ~sp ~forum ~creator_id ?subject ?parent_id ?sticky ~text () = 
  Forum.get_role sp forum >>= fun role ->
  Forum_sql.get_forum ~forum () >>= fun f ->
  if f.f_deleted
  then Lwt.fail Ocsimore_common.Permission_denied
  else
    can_create_message ~sp ~parent_id f role >>= fun (wiki, moderated) ->
    let title_syntax = f.f_title_syntax in
    Forum_sql.new_message ~sp ~forum ~wiki ~creator_id
      ?subject ?parent_id ~moderated ~title_syntax ?sticky ~text

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
  User.in_group ~sp ~group:(forum_visible $ f.f_id) () >>= fun b ->
  if b
  then Lwt.return f
  else Lwt.fail Ocsimore_common.Permission_denied

let get_forums_list ~sp () =
  Forum_sql.get_forums_list () >>= fun l ->
  List.fold_right
    (fun f e -> 
       e >>= fun e ->
       let f = get_forum_info f in
       User.in_group ~sp ~group:(forum_visible $ f.f_id) () >>= fun b ->
       if b
       then Lwt.return (f::e)
       else Lwt.return e)
    l
    (Lwt.return [])

let can_read_message ~sp m role =
  let first_msg = m.m_parent_id = None in
  !!(m.m_has_special_rights) >>= fun has_special_rights ->
  let (mod_reader, notmod_reader) =
    if has_special_rights
    then
      (User.in_group
         ~sp ~group:(Forum.thread_moderated_readers $ m.m_root_id) (),
       lazy 
         (User.in_group
            ~sp 
            ~group:(Forum.thread_readers_evennotmoderated $ m.m_root_id) ()))
    else
      ((!!(role.moderated_message_readers) >>= fun moderated_message_readers ->
        !!(role.moderated_comment_readers) >>= fun moderated_comment_readers ->
        Lwt.return ((first_msg && moderated_message_readers)
                    || (not first_msg && moderated_comment_readers))),
       (lazy
          (!!(role.message_readers_evennotmoderated)
           >>= fun message_readers_evennotmoderated ->
           !!(role.comment_readers_evennotmoderated)
           >>= fun comment_readers_evennotmoderated ->
           Lwt.return
             ((first_msg && message_readers_evennotmoderated)
              || (not first_msg && comment_readers_evennotmoderated)))))
  in
  mod_reader >>= fun mod_reader ->
  if not mod_reader
  then Lwt.return false
  else 
    if m.m_moderated
    then Lwt.return true
    else !!notmod_reader


let get_message ~sp ~message_id =
  Forum_sql.get_message ~message_id () >>= fun m ->
  Forum_sql.get_forum ~forum:m.m_forum () >>= fun _ ->
  (* get_forum only to verify that the forum is not deleted? *)
  Forum.get_role sp m.m_forum >>= fun role ->
  can_read_message ~sp m role >>= fun b ->
  if b
  then Lwt.return m
  else Lwt.fail Ocsimore_common.Permission_denied


let get_thread ~sp ~message_id =
  Forum_sql.get_thread ~message_id ()
  >>= function
    | [] -> Lwt.fail Not_found
    | (m::l) as th ->
        let m = get_message_info m in
        assert (message_id = m.m_id);
        Forum_sql.get_forum ~forum:m.m_forum () >>= fun _ ->
        (* get_forum only to verify that the forum is not deleted *)
        Forum.get_role sp m.m_forum >>= fun role ->
        let first_msg = m.m_parent_id = None in
        !!(m.m_has_special_rights) >>= fun has_special_rights ->

        let (moderated_message_readers, 
             moderated_comment_readers,
             message_readers_evennotmoderated, 
             comment_readers_evennotmoderated) =
          if has_special_rights
          then
            let rm = 
              lazy (User.in_group
                      ~sp 
                      ~group:(Forum.thread_moderated_readers $ m.m_root_id) ())
            in
            let rnm =
              lazy 
                (User.in_group
                   ~sp 
                   ~group:(Forum.thread_readers_evennotmoderated $ m.m_root_id)
                   ())
            in
            (rm, rm, rnm,rnm)
          else
            (role.moderated_message_readers,
             role.moderated_comment_readers,
             role.message_readers_evennotmoderated,
             role.comment_readers_evennotmoderated)
        in

        !!moderated_message_readers >>= fun moderated_message_readers ->
        !!moderated_comment_readers >>= fun moderated_comment_readers ->
        !!message_readers_evennotmoderated >>= fun message_readers_evennotmoderated ->
        !!comment_readers_evennotmoderated >>= fun comment_readers_evennotmoderated ->

        let comment_filter l =
          let rec aux min = function
            | [] -> []
            | c::l ->
                let c = get_message_info c in
                if c.m_tree_min < min
                then aux min l
                else if (*AEFF c.m_deleted || *) c.m_moderated
                then c::aux min l    
                else aux c.m_tree_max l
          in
          if moderated_comment_readers
          then begin
            if comment_readers_evennotmoderated
            then List.map get_message_info l
            else aux m.m_tree_min l (* only moderated comments *)
          end
          else raise Ocsimore_common.Permission_denied
        in

        try
          Lwt.return
            (if first_msg
             then 
               if message_readers_evennotmoderated || 
                 (moderated_message_readers && m.m_moderated)
               then m::(comment_filter l)
               else raise Ocsimore_common.Permission_denied
             else comment_filter th)
        with e -> Lwt.fail e

type raw_message = Forum_types.raw_message_info

let get_message_list ~sp ~forum ~first ~number () =
  Forum_sql.get_forum ~forum () >>= fun _ ->
  (* get_forum only to verify that the forum is not deleted *)
  Forum.get_role sp forum >>= fun role ->
  !!(role.moderated_message_readers) >>= fun moderated_message_readers ->
  if not moderated_message_readers
  then Lwt.fail Ocsimore_common.Permission_denied
  else
    !!(role.message_readers_evennotmoderated) 
    >>= fun message_readers_evennotmoderated ->
    Forum_sql.get_message_list ~forum ~first ~number 
      ~moderated_only:(not message_readers_evennotmoderated) ()

let message_info_of_raw_message ~sp m =
  let m = Forum_types.get_message_info m in
  !!(m.m_has_special_rights) >>= fun has_special_rights ->
  if not has_special_rights
  then Lwt.return m
  else 
    User.in_group 
      ~sp ~group:(Forum.thread_readers_evennotmoderated $ m.m_root_id) ()
    >>= fun b ->
    if b
    then Lwt.return m
    else
      if not m.m_moderated
      then Lwt.fail Ocsimore_common.Permission_denied
      else
        User.in_group 
          ~sp ~group:(Forum.thread_moderated_readers $ m.m_root_id) ()
        >>= fun b ->
        if b
        then Lwt.return m
        else Lwt.fail Ocsimore_common.Permission_denied
