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

let (>>=) = Lwt.bind

type forum_info = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  arborescent: bool;
  deleted: bool;
}

let create_group_ name fullname =
  Users.create_user 
    ~name
    ~pwd:User_sql.Types.Connect_forbidden
    ~fullname
    ~groups:[]
    ()

let forum_creators =
  Lwt_unix.run
    (Users.create_user 
       ~name:"forum_creators"
       ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"users who can create new forums"
       ~groups:[]
       ()
    )

let message_writers_group_name i = "forum"^i^"_message_writers"
let message_writers_notmod_group_name i = "forum"^i^"_message_writers_notmod"
let message_moderators_group_name i = "forum"^i^"_message_moderators"
let message_deletors_group_name i = "forum"^i^"_message_deletors"
let message_deletors_if_author_group_name i = "forum"^i^"_message_deletors_if_author"
let message_sticky_setters_group_name i = "forum"^i^"_message_sticky_setters"
let message_readers_group_name i = "forum"^i^"_message_readers"

let comment_writers_group_name i = "forum"^i^"_comment_writers"
let comment_writers_notmod_group_name i = "forum"^i^"_comment_writers_notmod"
let comment_moderators_group_name i = "forum"^i^"_comment_moderators"
let comment_deletors_group_name i = "forum"^i^"_comment_deletors"
let comment_deletors_if_author_group_name i = "forum"^i^"_comment_deletors_if_author"
let comment_sticky_setters_group_name i = "forum"^i^"_comment_sticky_setters"
let comment_readers_group_name i = "forum"^i^"_comment_readers"

let writers_group_name i = "forum"^i^"_writers"
let writers_notmod_group_name i = "forum"^i^"_writers_notmod"
let moderators_group_name i = "forum"^i^"_moderators"
let deletors_group_name i = "forum"^i^"_deletors"
let deletors_if_author_group_name i = "forum"^i^"_deletors_if_author"
let sticky_setters_group_name i = "forum"^i^"_sticky_setters"
let readers_group_name i = "forum"^i^"_readers"

let forum_admin_group_name i = "forum"^i^"_admin"
let forum_visible_group_name i = "forum"^i^"_visible"

let message_writers_group i = Users.get_user_id_by_name (message_writers_group_name i)
let message_writers_notmod_group i = Users.get_user_id_by_name (message_writers_notmod_group_name i)
let message_moderators_group i = Users.get_user_id_by_name (message_moderators_group_name i)
let message_deletors_group i = Users.get_user_id_by_name (message_deletors_group_name i)
let message_deletors_if_author_group i = Users.get_user_id_by_name (message_deletors_if_author_group_name i)
let message_sticky_setters_group i = Users.get_user_id_by_name (message_sticky_setters_group_name i)
let message_readers_group i = Users.get_user_id_by_name (message_readers_group_name i)

let comment_writers_group i = Users.get_user_id_by_name (comment_writers_group_name i)
let comment_writers_notmod_group i = Users.get_user_id_by_name (comment_writers_notmod_group_name i)
let comment_moderators_group i = Users.get_user_id_by_name (comment_moderators_group_name i)
let comment_deletors_group i = Users.get_user_id_by_name (comment_deletors_group_name i)
let comment_deletors_if_author_group i = Users.get_user_id_by_name (comment_deletors_if_author_group_name i)
let comment_sticky_setters_group i = Users.get_user_id_by_name (comment_sticky_setters_group_name i)
let comment_readers_group i = Users.get_user_id_by_name (comment_readers_group_name i)

let writers_group i = Users.get_user_id_by_name (writers_group_name i)
let writers_notmod_group i = Users.get_user_id_by_name (writers_notmod_group_name i)
let moderators_group i = Users.get_user_id_by_name (moderators_group_name i)
let deletors_group i = Users.get_user_id_by_name (deletors_group_name i)
let deletors_if_author_group i = Users.get_user_id_by_name (deletors_if_author_group_name i)
let sticky_setters_group i = Users.get_user_id_by_name (sticky_setters_group_name i)
let readers_group i = Users.get_user_id_by_name (readers_group_name i)

let forum_admin_group i = Users.get_user_id_by_name (forum_admin_group_name i)

let forum_visible_group i = 
  Users.get_user_id_by_name 
    (forum_visible_group_name (Int32.to_string i))

let add_to_group_ l g =
  List.fold_left
    (fun beg u -> 
       beg >>= fun () ->
       Users.add_to_group ~user:u ~group:g)
    (Lwt.return ())
    l



let really_create_forum ~title ~descr ~arborescent () =
  Forum_sql.new_forum ~title ~descr ~arborescent () >>= fun forum_id ->

  let forum_id_s = Forum_sql.forum_id_s forum_id in
   (* Creating groups *)
   create_group_ (message_readers_group_name forum_id_s)
     ("Users who can read messages in forum "^forum_id_s)
   >>= fun message_readers_data ->
   create_group_ (message_writers_group_name forum_id_s)
     ("Users who can create new messages in forum "^forum_id_s)
   >>= fun message_writers_data ->
   create_group_ (message_writers_notmod_group_name forum_id_s)
       ("Users who can create new messages in forum "^forum_id_s^
          " without beeing moderated")
   >>= fun message_writers_notmod_data ->
   create_group_ (message_moderators_group_name forum_id_s)
       ("Users who can moderate messages in "^forum_id_s)
   >>= fun message_moderators_data ->
   create_group_ (message_deletors_group_name forum_id_s)
       ("Users who can delete messages in "^forum_id_s)
   >>= fun message_deletors_data ->
   create_group_ (message_deletors_if_author_group_name forum_id_s)
       ("Users who can delete their own messages in "^forum_id_s)
   >>= fun message_deletors_if_author_data ->
   create_group_ (message_sticky_setters_group_name forum_id_s)
       ("Users who can set messages sticky in "^forum_id_s)
   >>= fun message_sticky_setters_data ->

   create_group_ (comment_readers_group_name forum_id_s)
     ("Users who can read comments in forum "^forum_id_s)
   >>= fun comment_readers_data ->
   create_group_ (comment_writers_group_name forum_id_s)
     ("Users who can create comments in forum "^forum_id_s)
   >>= fun comment_writers_data ->
     create_group_ (comment_writers_notmod_group_name forum_id_s)
       ("Users who can create comments in forum "^forum_id_s^
          " without beeing moderated")
   >>= fun comment_writers_notmod_data ->
     create_group_ (comment_moderators_group_name forum_id_s)
       ("Users who can moderate new comments in "^forum_id_s)
   >>= fun comment_moderators_data ->
     create_group_ (comment_deletors_group_name forum_id_s)
       ("Users who can delete comments in "^forum_id_s)
   >>= fun comment_deletors_data ->
     create_group_ (comment_deletors_if_author_group_name forum_id_s)
       ("Users who can delete their own comments in "^forum_id_s)
   >>= fun comment_deletors_if_author_data ->
     create_group_ (comment_sticky_setters_group_name forum_id_s)
       ("Users who can set comments sticky in "^forum_id_s)
   >>= fun comment_sticky_setters_data ->

     create_group_ (readers_group_name forum_id_s)
     ("Users who can read messages and comments in forum "^forum_id_s)
   >>= fun readers_data ->
     create_group_ (writers_group_name forum_id_s)
     ("Users who can create messages or comments in forum "^forum_id_s)
   >>= fun writers_data ->
     create_group_ (writers_notmod_group_name forum_id_s)
       ("Users who can create messages or comments in forum "^forum_id_s^
          " without beeing moderated")
   >>= fun writers_notmod_data ->
     create_group_ (moderators_group_name forum_id_s)
       ("Users who can moderate messages and comments in "^forum_id_s)
   >>= fun moderators_data ->
     create_group_ (deletors_group_name forum_id_s)
       ("Users who can delete messages and comments in "^forum_id_s)
   >>= fun deletors_data ->
     create_group_ (deletors_if_author_group_name forum_id_s)
       ("Users who can delete their own messages and comments in "^forum_id_s)
   >>= fun deletors_if_author_data ->
     create_group_ (sticky_setters_group_name forum_id_s)
       ("Users who can set messages or comments sticky in "^forum_id_s)
   >>= fun sticky_setters_data ->

     create_group_ (forum_admin_group_name forum_id_s)
       ("Users who have all rights for "^forum_id_s)
   >>= fun forum_admin_data ->
     create_group_ (forum_visible_group_name forum_id_s)
       ("Users who can see that forum "^forum_id_s^" exists")
   >>= fun forum_visible_data ->


   (* Relation between groups *)
     add_to_group_ [forum_admin_data.user_id] moderators_data.user_id
   >>= fun () ->
     add_to_group_ [forum_admin_data.user_id] deletors_data.user_id
   >>= fun () ->
     add_to_group_ [forum_admin_data.user_id] sticky_setters_data.user_id
   >>= fun () ->
     add_to_group_ [forum_admin_data.user_id] writers_notmod_data.user_id
   >>= fun () ->

     add_to_group_ [moderators_data.user_id] message_moderators_data.user_id
   >>= fun () ->
     add_to_group_ [moderators_data.user_id] comment_moderators_data.user_id
   >>= fun () ->
     add_to_group_ [deletors_data.user_id] message_deletors_data.user_id
   >>= fun () ->
     add_to_group_ [deletors_data.user_id] comment_deletors_data.user_id
   >>= fun () ->
     add_to_group_ [deletors_data.user_id] deletors_if_author_data.user_id
   >>= fun () ->
     add_to_group_ [deletors_if_author_data.user_id] message_deletors_if_author_data.user_id
   >>= fun () ->
     add_to_group_ [deletors_if_author_data.user_id] comment_deletors_if_author_data.user_id
   >>= fun () ->
     add_to_group_ [message_deletors_data.user_id] message_deletors_if_author_data.user_id
   >>= fun () ->
     add_to_group_ [comment_deletors_data.user_id] comment_deletors_if_author_data.user_id
   >>= fun () ->
     add_to_group_ [sticky_setters_data.user_id] message_sticky_setters_data.user_id
   >>= fun () ->
     add_to_group_ [sticky_setters_data.user_id] comment_sticky_setters_data.user_id
   >>= fun () ->
     add_to_group_ [writers_notmod_data.user_id] message_writers_notmod_data.user_id
   >>= fun () ->
     add_to_group_ [writers_notmod_data.user_id] comment_writers_notmod_data.user_id

   >>= fun () ->
     add_to_group_ [writers_notmod_data.user_id] writers_data.user_id
   >>= fun () ->
     add_to_group_ [writers_data.user_id] readers_data.user_id

   >>= fun () ->
     add_to_group_ [message_deletors_data.user_id;
                    message_moderators_data.user_id;
                    message_sticky_setters_data.user_id;
                    message_writers_data.user_id;
                    readers_data.user_id
                   ] message_readers_data.user_id
   >>= fun () ->
     add_to_group_ [comment_deletors_data.user_id;
                    comment_moderators_data.user_id;
                    comment_sticky_setters_data.user_id;
                    comment_writers_data.user_id;
                    readers_data.user_id
                   ] comment_readers_data.user_id
   >>= fun () ->
     add_to_group_ [message_writers_notmod_data.user_id;
                    writers_data.user_id] message_writers_data.user_id
   >>= fun () ->
     add_to_group_ [comment_writers_notmod_data.user_id;
                    writers_data.user_id] comment_writers_data.user_id

   >>= fun () ->
     add_to_group_ [message_readers_data.user_id] forum_visible_data.user_id
   >>= fun () ->
     add_to_group_ [comment_readers_data.user_id] forum_visible_data.user_id

   >>= fun () ->
   Lwt.return forum_id


let create_forum
    ~title
    ~descr
    ?(arborescent=true)
    () =
  Lwt.catch
    (fun () -> 
       Forum_sql.get_forum ~title ()
       >>= fun (id, title, descr, arborescent, deleted) ->
       Lwt.return { id = id; 
                    title = title; 
                    descr = descr;
                    arborescent = arborescent;
                    deleted = deleted
                  }
    )
    (function
       | Not_found ->
           really_create_forum ~title ~descr ~arborescent ()
             >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr;
                        arborescent = arborescent;
                        deleted = false
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
      message_sticky_setters : bool Lwt.t Lazy.t;
      message_readers : bool Lwt.t Lazy.t;

      comment_writers : bool Lwt.t Lazy.t;
      comment_writers_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_author : bool Lwt.t Lazy.t;
      comment_sticky_setters : bool Lwt.t Lazy.t;
      comment_readers : bool Lwt.t Lazy.t;

      writers : bool Lwt.t Lazy.t;
      writers_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_author : bool Lwt.t Lazy.t;
      sticky_setters : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

let get_role ~sp ~sd ~forum_id =
  let i = Forum_sql.forum_id_s forum_id in
  Users.get_user_data sp sd >>= fun u ->
  let u = u.user_id in

  Lwt.return
    {
      message_writers = 
        lazy 
          (message_writers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_writers_notmod = 
        lazy 
          (message_writers_notmod_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_moderators = 
        lazy 
          (message_moderators_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_deletors = 
        lazy 
          (message_deletors_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_deletors_if_author = 
        lazy 
          (message_deletors_if_author_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_sticky_setters = 
        lazy 
          (message_sticky_setters_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      message_readers = 
        lazy 
          (message_readers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      
      comment_writers = 
        lazy 
          (comment_writers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_writers_notmod = 
        lazy 
          (comment_writers_notmod_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_moderators = 
        lazy 
          (comment_moderators_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_deletors = 
        lazy 
          (comment_deletors_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_deletors_if_author = 
        lazy 
          (comment_deletors_if_author_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_sticky_setters = 
        lazy 
          (comment_sticky_setters_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      comment_readers = 
        lazy 
          (comment_readers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      
      writers = 
        lazy 
          (writers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      writers_notmod = 
        lazy 
          (writers_notmod_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      moderators = 
        lazy 
          (moderators_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      deletors = 
        lazy 
          (deletors_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      deletors_if_author = 
        lazy 
          (deletors_if_author_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      sticky_setters = 
        lazy 
          (sticky_setters_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      readers = 
        lazy 
          (readers_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
      
      forum_admin = 
        lazy 
          (forum_admin_group i >>= fun group ->
           Users.in_group ~sp ~sd ~user:u ~group ());
    }

module Roles = Map.Make(struct
                          type t = Forum_sql.forum
                          let compare = compare
                        end)

type forum_sd = Forum_sql.forum -> role Lwt.t

let default_forum_sd ~sp ~sd =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  fun k -> 
    try 
      Lwt.return (Roles.find k !cache)
    with Not_found -> 
      get_role ~sp ~sd ~forum_id:k >>= fun v ->
      cache := Roles.add k v !cache;
      Lwt.return v

(** The polytable key for retrieving forum data inside session data *)
let forum_key : forum_sd Polytables.key = Polytables.make_key ()

let get_forum_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:forum_key
  with Not_found -> 
    let fsd = default_forum_sd ~sp ~sd in
    Polytables.set sd forum_key fsd;
    fsd

let get_role ~sp ~sd k =
  let forum_sd = get_forum_sd ~sp ~sd in
  forum_sd k



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_sql.forum * int32 option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_sql.forum * int32 option (* parent *))

exception Forum_action_info of forum_action_info

