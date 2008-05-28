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
*)

(* open XHTML.M *)
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Lwt
open Users

exception Unauthorized

type forum_info = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  moderated: bool;
  arborescent: bool;
  readable_by: User_sql.userid;
  writable_by: User_sql.userid;
  moderated_by: User_sql.userid;
}

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


let create_forum
    ~title
    ~descr
    ~moderated
    ~arborescent
    ?(reader = Users.anonymous.Users.id)
    ?(writer = Users.anonymous.Users.id)
    ?(moderator = Users.anonymous.Users.id) (*VVV anonymous??? *)
    () =
  catch
    (fun () -> get_forum_by_name title)
    (function
       | Not_found -> 
           Forum_sql.new_forum
             title descr moderated arborescent reader writer moderator
             >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr;
                        moderated = moderated;
                        arborescent = arborescent;
                        readable_by = reader;
                        writable_by = writer; 
                        moderated_by = moderator
                      }
       | e -> fail e)

let can_read forum user =
  Users.in_group user forum.readable_by
    
let can_write forum user =
  Users.in_group user forum.writable_by
    
let can_moderate forum user =
  Users.in_group user forum.moderated_by

let get_role ~sp ~sd (forum_id : Forum_sql.forum) =
  get_forum_by_id forum_id >>= fun f -> 
  Users.get_user_data sp sd >>= fun u ->
  can_moderate f u.Users.id >>= fun b ->
  if b
  then Lwt.return Forum_sql.Moderator
  else (can_write f u.Users.id >>= fun b ->
        if b
        then Lwt.return (Forum_sql.Author u.Users.id)
        else (can_read f u.Users.id >>= fun b ->
              if b
              then Lwt.return (Forum_sql.Lurker u.Users.name)
              else Lwt.return Forum_sql.Unknown)
          )


(** {2 Session data} *)

module Roles = Map.Make(struct
                          type t = Forum_sql.forum
                          let compare = compare
                        end)

type forum_sd = Forum_sql.forum -> Forum_sql.role Lwt.t

let default_forum_sd ~sp ~sd =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  fun k -> 
    try 
      Lwt.return (Roles.find k !cache)
    with Not_found -> 
      get_role ~sp ~sd k >>= fun v ->
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
