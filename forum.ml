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
  readable_by: Users.group;
  writable_by: Users.group;
  moderated_by: Users.group;
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
    ?(reader = Users.anonymous_group)
    ?(writer = Users.anonymous_group)
    ?(moderator = Users.anonymous_group) (*VVV anonymous_group??? *)
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

let get_role ~sd (forum_id : Forum_sql.forum) =
  get_forum_by_id forum_id >>= fun f -> 
  Lwt.return
    (match sd with
       | Data u -> 
           if can_moderate f u
           then Forum_sql.Moderator
           else if can_write f u
           then Forum_sql.Author u.Users.id
           else if can_read f u
           then Forum_sql.Lurker u.Users.name
           else Forum_sql.Unknown
       | _ -> Forum_sql.Unknown)

