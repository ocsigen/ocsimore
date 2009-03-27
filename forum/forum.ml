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

let (>>=) = Lwt.bind

type forum_info = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  arborescent: bool;
  deleted: bool;
  readonly: bool;
}

let create_group_ name fullname =
  Users.create_user 
    ~name
    ~pwd:User_sql.Connect_forbidden
    ~fullname
    ~groups:[]
    ()

let messages_writers_group_name i = "forum"^i^"_messages_writers"
let messages_writers_notmod_group_name i = "forum"^i^"_messages_writers_notmod"
let messages_moderators_group_name i = "forum"^i^"_messages_moderators"
let messages_deletors_group_name i = "forum"^i^"_messages_deletors"
let messages_sticky_setters_group_name i = "forum"^i^"_messages_sticky_setters"
let messages_readers_group_name i = "forum"^i^"_messages_readers"

let comments_writers_group_name i = "forum"^i^"_comments_writers"
let comments_writers_notmod_group_name i = "forum"^i^"_comments_writers_notmod"
let comments_moderators_group_name i = "forum"^i^"_comments_moderators"
let comments_deletors_group_name i = "forum"^i^"_comments_deletors"
let comments_sticky_setters_group_name i = "forum"^i^"_comments_sticky_setters"
let comments_readers_group_name i = "forum"^i^"_comments_readers"

let writers_group_name i = "forum"^i^"_writers"
let writers_notmod_group_name i = "forum"^i^"_writers_notmod"
let moderators_group_name i = "forum"^i^"_moderators"
let deletors_group_name i = "forum"^i^"_deletors"
let sticky_setters_group_name i = "forum"^i^"_sticky_setters"
let readers_group_name i = "forum"^i^"_readers"

let forum_admin_group_name i = "forum"^i^"_admin"

let messages_writers_group i = Users.get_user_id_by_name (messages_writers_group_name i)
let messages_writers_notmod_group i = Users.get_user_id_by_name (messages_writers_notmod_group_name i)
let messages_moderators_group i = Users.get_user_id_by_name (messages_moderators_group_name i)
let messages_deletors_group i = Users.get_user_id_by_name (messages_deletors_group_name i)
let messages_sticky_setters_group i = Users.get_user_id_by_name (messages_sticky_setters_group_name i)
let messages_readers_group i = Users.get_user_id_by_name (messages_readers_group_name i)

let comments_writers_group i = Users.get_user_id_by_name (comments_writers_group_name i)
let comments_writers_notmod_group i = Users.get_user_id_by_name (comments_writers_notmod_group_name i)
let comments_moderators_group i = Users.get_user_id_by_name (comments_moderators_group_name i)
let comments_deletors_group i = Users.get_user_id_by_name (comments_deletors_group_name i)
let comments_sticky_setters_group i = Users.get_user_id_by_name (comments_sticky_setters_group_name i)
let comments_readers_group i = Users.get_user_id_by_name (comments_readers_group_name i)

let writers_group i = Users.get_user_id_by_name (writers_group_name i)
let writers_notmod_group i = Users.get_user_id_by_name (writers_notmod_group_name i)
let moderators_group i = Users.get_user_id_by_name (moderators_group_name i)
let deletors_group i = Users.get_user_id_by_name (deletors_group_name i)
let sticky_setters_group i = Users.get_user_id_by_name (sticky_setters_group_name i)
let readers_group i = Users.get_user_id_by_name (readers_group_name i)

let forum_admin_group i = Users.get_user_id_by_name (forum_admin_group_name i)

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
   create_group_ (messages_readers_group_name forum_id_s)
     ("Users who can read messages in forum "^forum_id_s)
   >>= fun messages_readers_data ->
   create_group_ (messages_writers_group_name forum_id_s)
     ("Users who can create new messages in forum "^forum_id_s)
   >>= fun messages_writers_data ->
   create_group_ (messages_writers_notmod_group_name forum_id_s)
       ("Users who can create new messages in forum "^forum_id_s^
          " without beeing moderated")
   >>= fun messages_writers_notmod_data ->
   create_group_ (messages_moderators_group_name forum_id_s)
       ("Users who can moderate messages in "^forum_id_s)
   >>= fun messages_moderators_data ->
   create_group_ (messages_deletors_group_name forum_id_s)
       ("Users who can delete messages in "^forum_id_s)
   >>= fun messages_deletors_data ->
   create_group_ (messages_sticky_setters_group_name forum_id_s)
       ("Users who can set messages sticky in "^forum_id_s)
   >>= fun messages_sticky_setters_data ->

   create_group_ (comments_readers_group_name forum_id_s)
     ("Users who can read comments in forum "^forum_id_s)
   >>= fun comments_readers_data ->
   create_group_ (comments_writers_group_name forum_id_s)
     ("Users who can create comments in forum "^forum_id_s)
   >>= fun comments_writers_data ->
     create_group_ (comments_writers_notmod_group_name forum_id_s)
       ("Users who can create comments in forum "^forum_id_s^
          " without beeing moderated")
   >>= fun comments_writers_notmod_data ->
     create_group_ (comments_moderators_group_name forum_id_s)
       ("Users who can moderate new comments in "^forum_id_s)
   >>= fun comments_moderators_data ->
     create_group_ (comments_deletors_group_name forum_id_s)
       ("Users who can delete comments in "^forum_id_s)
   >>= fun comments_deletors_data ->
     create_group_ (comments_sticky_setters_group_name forum_id_s)
       ("Users who can set comments sticky in "^forum_id_s)
   >>= fun comments_sticky_setters_data ->

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
     create_group_ (sticky_setters_group_name forum_id_s)
       ("Users who can set messages or comments sticky in "^forum_id_s)
   >>= fun sticky_setters_data ->

     create_group_ (forum_admin_group_name forum_id_s)
       ("Users who have all rights for "^forum_id_s)
   >>= fun forum_admin_data ->


   (* Relation between groups *)
     add_to_group_ [forum_admin_data.Users.id] moderators_data.Users.id
   >>= fun () ->
     add_to_group_ [forum_admin_data.Users.id] deletors_data.Users.id
   >>= fun () ->
     add_to_group_ [forum_admin_data.Users.id] sticky_setters_data.Users.id
   >>= fun () ->
     add_to_group_ [forum_admin_data.Users.id] writers_notmod_data.Users.id
   >>= fun () ->

     add_to_group_ [moderators_data.Users.id] messages_moderators_data.Users.id
   >>= fun () ->
     add_to_group_ [moderators_data.Users.id] comments_moderators_data.Users.id
   >>= fun () ->
     add_to_group_ [deletors_data.Users.id] messages_deletors_data.Users.id
   >>= fun () ->
     add_to_group_ [deletors_data.Users.id] comments_deletors_data.Users.id
   >>= fun () ->
     add_to_group_ [sticky_setters_data.Users.id] messages_sticky_setters_data.Users.id
   >>= fun () ->
     add_to_group_ [sticky_setters_data.Users.id] comments_sticky_setters_data.Users.id
   >>= fun () ->
     add_to_group_ [writers_notmod_data.Users.id] messages_writers_notmod_data.Users.id
   >>= fun () ->
     add_to_group_ [writers_notmod_data.Users.id] comments_writers_notmod_data.Users.id

   >>= fun () ->
     add_to_group_ [writers_notmod_data.Users.id] writers_data.Users.id
   >>= fun () ->
     add_to_group_ [writers_data.Users.id] readers_data.Users.id

   >>= fun () ->
     add_to_group_ [messages_deletors_data.Users.id;
                    messages_moderators_data.Users.id;
                    messages_sticky_setters_data.Users.id;
                    messages_writers_data.Users.id;
                    readers_data.Users.id
                   ] messages_readers_data.Users.id
   >>= fun () ->
     add_to_group_ [comments_deletors_data.Users.id;
                    comments_moderators_data.Users.id;
                    comments_sticky_setters_data.Users.id;
                    comments_writers_data.Users.id;
                    readers_data.Users.id
                   ] comments_readers_data.Users.id
   >>= fun () ->
     add_to_group_ [messages_writers_notmod_data.Users.id;
                    writers_data.Users.id] messages_writers_data.Users.id
   >>= fun () ->
     add_to_group_ [comments_writers_notmod_data.Users.id;
                    writers_data.Users.id] comments_writers_data.Users.id

   >>= fun () ->
   Lwt.return forum_id


let create_forum
    ~title
    ~descr
    ?(arborescent=true)
    () =
  Lwt.catch
    (fun () -> Forum_sql.find_forum ~title () >>= fun (id, title, descr, arborescent, deleted, readonly) ->
       Lwt.return { id = id; 
                    title = title; 
                    descr = descr;
                    arborescent = arborescent;
                    deleted = deleted;
                    readonly = readonly;
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
                        deleted = false;
                        readonly = false;
                      }
       | e -> Lwt.fail e)





(** {2 Session data} *)

type role = 
    {
      messages_writers : bool;
      messages_writers_notmod : bool;
      messages_moderators : bool;
      messages_deletors : bool;
      messages_sticky_setters : bool;
      messages_readers : bool;

      comments_writers : bool;
      comments_writers_notmod : bool;
      comments_moderators : bool;
      comments_deletors : bool;
      comments_sticky_setters : bool;
      comments_readers : bool;

      writers : bool;
      writers_notmod : bool;
      moderators : bool;
      deletors : bool;
      sticky_setters : bool;
      readers : bool;

      forum_admin : bool;
    }

let get_role ~sp ~sd ~forum_id =
  let i = Forum_sql.forum_id_s forum_id in
  Users.get_user_data sp sd >>= fun u ->
  let u = u.Users.id in

  messages_writers_group i >>= fun messages_writers_group ->
  messages_writers_notmod_group i >>= fun  messages_writers_notmod_group ->
  messages_moderators_group i >>= fun messages_moderators_group ->
  messages_deletors_group i >>= fun messages_deletors_group ->
  messages_sticky_setters_group i >>= fun messages_sticky_setters_group ->
  messages_readers_group i >>= fun messages_readers_group ->
  
  comments_writers_group i >>= fun comments_writers_group ->
  comments_writers_notmod_group i >>= fun comments_writers_notmod_group ->
  comments_moderators_group i >>= fun comments_moderators_group ->
  comments_deletors_group i >>= fun comments_deletors_group ->
  comments_sticky_setters_group i >>= fun comments_sticky_setters_group ->
  comments_readers_group i >>= fun comments_readers_group ->
  
  writers_group i >>= fun writers_group ->
  writers_notmod_group i >>= fun writers_notmod_group ->
  moderators_group i >>= fun moderators_group ->
  deletors_group i >>= fun deletors_group ->
  sticky_setters_group i >>= fun sticky_setters_group ->
  readers_group i >>= fun readers_group ->
  
  forum_admin_group i >>= fun forum_admin_group ->

  Users.in_group ~sp ~sd ~user:u ~group:messages_writers_group () >>= fun messages_writers ->
  Users.in_group ~sp ~sd ~user:u ~group:messages_writers_notmod_group () >>= fun messages_writers_notmod ->
  Users.in_group ~sp ~sd ~user:u ~group:messages_moderators_group () >>= fun messages_moderators ->
  Users.in_group ~sp ~sd ~user:u ~group:messages_deletors_group () >>= fun messages_deletors ->
  Users.in_group ~sp ~sd ~user:u ~group:messages_sticky_setters_group () >>= fun messages_sticky_setters ->
  Users.in_group ~sp ~sd ~user:u ~group:messages_readers_group () >>= fun messages_readers ->
    
  Users.in_group ~sp ~sd ~user:u ~group:comments_writers_group () >>= fun comments_writers ->
  Users.in_group ~sp ~sd ~user:u ~group:comments_writers_notmod_group () >>= fun comments_writers_notmod ->
  Users.in_group ~sp ~sd ~user:u ~group:comments_moderators_group () >>= fun comments_moderators ->
  Users.in_group ~sp ~sd ~user:u ~group:comments_deletors_group () >>= fun comments_deletors ->
  Users.in_group ~sp ~sd ~user:u ~group:comments_sticky_setters_group () >>= fun comments_sticky_setters ->
  Users.in_group ~sp ~sd ~user:u ~group:comments_readers_group () >>= fun comments_readers ->
    
  Users.in_group ~sp ~sd ~user:u ~group:writers_group () >>= fun writers ->
  Users.in_group ~sp ~sd ~user:u ~group:writers_notmod_group () >>= fun writers_notmod ->
  Users.in_group ~sp ~sd ~user:u ~group:moderators_group () >>= fun moderators ->
  Users.in_group ~sp ~sd ~user:u ~group:deletors_group () >>= fun deletors ->
  Users.in_group ~sp ~sd ~user:u ~group:sticky_setters_group () >>= fun sticky_setters ->
  Users.in_group ~sp ~sd ~user:u ~group:readers_group () >>= fun readers ->
     
  Users.in_group ~sp ~sd ~user:u ~group:forum_admin_group () >>= fun forum_admin ->

  Lwt.return
    {
      messages_writers = messages_writers;
      messages_writers_notmod = messages_writers_notmod;
      messages_moderators = messages_moderators;
      messages_deletors = messages_deletors;
      messages_sticky_setters = messages_sticky_setters;
      messages_readers = messages_readers;
      
      comments_writers = comments_writers;
      comments_writers_notmod = comments_writers_notmod;
      comments_moderators = comments_moderators;
      comments_deletors = comments_deletors;
      comments_sticky_setters = comments_sticky_setters;
      comments_readers = comments_readers;
      
      writers = writers;
      writers_notmod = writers_notmod;
      moderators = moderators;
      deletors = deletors;
      sticky_setters = sticky_setters;
      readers = readers;
      
      forum_admin = forum_admin;
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


(*

(* open XHTML.M *)
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Lwt
open Users

exception Unauthorized


let get_forum_by_id id =
  Forum_sql.find_forum ~id ()
  >>= fun (id, title, descr, mo, a, r, w, m) -> 
  return { id = id; 
           title = title; 
           descr = descr;
           moderated = mo;
           arborescent = a;
           readable_by = r;
           writable_by = w; 
           moderated_by = m;
         }

let get_forum_by_name title =
  Forum_sql.find_forum ~title () >>= fun (id, title, descr, mo, a, r, w, m) -> 
  return { id = id; 
           title = title; 
           descr = descr;
           moderated = mo;
           arborescent = a;
           readable_by = r;
           writable_by = w; 
           moderated_by = m;
         }


let can_read ~sp ~sd forum user =
  Users.in_group ~sp ~sd ~user ~group:forum.readable_by ()
    
let can_write ~sp ~sd forum user =
  Users.in_group ~sp ~sd ~user ~group:forum.writable_by ()
    
let can_moderate ~sp ~sd forum user =
  Users.in_group ~sp ~sd ~user ~group:forum.moderated_by ()

let get_role ~sp ~sd (forum_id : Forum_sql.forum) =
  get_forum_by_id forum_id >>= fun f -> 
  Users.get_user_data sp sd >>= fun u ->
  can_moderate ~sp ~sd f u.Users.id >>= fun b ->
  if b
  then Lwt.return Forum_sql.Moderator
  else (can_write ~sp ~sd f u.Users.id >>= fun b ->
        if b
        then Lwt.return (Forum_sql.Author u.Users.id)
        else (can_read ~sp ~sd f u.Users.id >>= fun b ->
              if b
              then Lwt.return (Forum_sql.Lurker u.Users.name)
              else Lwt.return Forum_sql.Unknown)
          )


*)
