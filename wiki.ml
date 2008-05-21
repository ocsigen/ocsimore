(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
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
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)


let (>>=) = Lwt.bind

(** Role of user in the wiki (for one box) *)
type role = Admin | Author | Lurker | Nonauthorized;;
(* Admin can changes the permissions on boxes *)


type wiki_info = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  default_reader: Users.group;
  default_writer: Users.group;
  default_admin: Users.group option; (** the (default) group of users
                                         who can change rights for boxes
                                         if acl enabled *)
}

module H = Hashtbl.Make(struct
                          type t = int32
                          let equal = (=)
                          let hash = Hashtbl.hash 
                        end)

let wiki_info_table = H.create 8

let find_wiki id =
  try
    Lwt.return (H.find wiki_info_table id)
  with Not_found -> Wiki_sql.find_wiki ~id ()

let get_wiki_by_id id =
  find_wiki id
  >>= fun (id, title, descr, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = r;
               default_writer = w; 
               default_admin = a;
             }

let get_wiki_by_name title =
  Wiki_sql.find_wiki ~title () >>= fun (id, title, descr, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = r;
               default_writer = w; 
               default_admin = a
             }

let create_wiki ~title ~descr
    ?(reader = Users.anonymous_group)
    ?(writer = Users.anonymous_group)
    ?admin
    () =
  Lwt.catch 
    (fun () -> get_wiki_by_name title)
    (function
       | Not_found -> 
           Wiki_sql.new_wiki ~title ~descr ~reader ~writer ?admin ()
               >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr; 
                        default_reader = reader;
                        default_writer = writer; 
                        default_admin = admin;
                      }
       | e -> Lwt.fail e)

let new_wikibox ~wiki ~author ~comment ~content =
  fun
    ?(readers = [wiki.default_reader]) 
    ?(writers = [wiki.default_writer]) 
    ?admins 
    () ->
  Wiki_sql.new_wikibox
    ~wiki:wiki.id
    ~author
    ~comment
    ~content
    ?rights:(match wiki.default_admin with
               | Some a -> Some (readers, 
                                 writers, 
                                 match admins with
                                   | None -> [a]
                                   | Some a -> a)
               | None -> None)
    ()


let can_admin wiki wikibox user =
  Lwt.return
    ((not (user == Users.anonymous)) &&
       ((user == Users.admin) ||
          match wiki.default_admin with
            | Some admin -> (* acl are activated *)
                Users.in_group user admin
            | None -> false))

let can_read wiki wikibox user =
  if user == Users.admin
  then Lwt.return true
  else
    match wiki.default_admin with
      | Some admin -> (* acl are activated *)
          Wiki_sql.get_readers ~wiki:wiki.id ~id:wikibox >>= fun l ->
          Lwt.return (List.exists (fun a -> Users.in_group user a) l)
      | None -> (* acl are not activated *)
          Lwt.return (Users.in_group user wiki.default_reader)
    
let can_write wiki wikibox user =
  if user == Users.admin
  then Lwt.return true
  else
    match wiki.default_admin with
      | Some admin -> (* acl are activated *)
          Wiki_sql.get_writers ~wiki:wiki.id ~id:wikibox >>= fun l ->
          Lwt.return (List.exists (fun a -> Users.in_group user a) l)
      | None -> (* acl are not activated *)
          Lwt.return (Users.in_group user wiki.default_writer)
    
let get_role_ ~sp ~sd ((wiki_id : Wiki_sql.wiki), wikibox) =
  get_wiki_by_id wiki_id >>= fun f -> 
  Users.get_user_data sp sd >>= fun u ->
  can_admin f wikibox u >>= fun cana ->
  if cana
  then Lwt.return Admin
  else
    can_write f wikibox u >>= fun canw ->
    if canw
    then Lwt.return Author
    else 
      can_read f wikibox u >>= fun canr ->
      if canr
      then Lwt.return Lurker
      else Lwt.return Nonauthorized



(** {2 Session data} *)

module Roles = Map.Make(struct
                          type t = int32 * int32
                          let compare = compare
                        end)

type wiki_sd = (int32 * int32) -> role Lwt.t

let default_wiki_sd ~sp ~sd =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  fun k -> 
    try 
      Lwt.return (Roles.find k !cache)
    with Not_found -> 
      get_role_ ~sp ~sd k >>= fun v ->
      cache := Roles.add k v !cache;
      Lwt.return v

(** The polytable key for retrieving wiki data inside session data *)
let wiki_key : wiki_sd Polytables.key = Polytables.make_key ()

let get_wiki_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:wiki_key
  with Not_found -> 
    let wsd = default_wiki_sd ~sp ~sd in
    Polytables.set sd wiki_key wsd;
    wsd

let get_role ~sp ~sd k =
  let wiki_sd = get_wiki_sd ~sp ~sd in
  wiki_sd k


(** {2 } *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (int32 * int32)
  | History of ((int32 * int32) * (int option * int option))
  | Oldversion of ((int32 * int32) * int32)
  | Src of ((int32 * int32) * int32)
  | Error of ((int32 * int32) * wiki_errors)

exception Wiki_action_info of wiki_action_info

let save_wikibox ~sp ~sd ((((wiki_id, box_id) as d), content), 
                          (addr, (addw, (adda, (delr, (delw, dela)))))) =
  get_role sp sd d >>= fun role ->
  (match role with
    | Admin
    | Author ->
        get_wiki_by_id wiki_id >>= fun wiki_info ->
        Users.get_user_data sp sd >>= fun user ->
        can_write wiki_info box_id user >>= fun canw ->
        if canw
        then
          Lwt.catch
            (fun () ->
               Wiki_sql.update_wikibox
                 wiki_id box_id
                 user.Users.name
                 "" content () >>= fun _ ->
               Lwt.return [])
          (fun e -> 
             Lwt.return 
               [Ocsimore_common.Session_data sd;
                Wiki_action_info (Error (d, Action_failed e))])
        else
          Lwt.return [Ocsimore_common.Session_data sd;
                      Wiki_action_info (Error (d, Operation_not_allowed))]
    | _ -> Lwt.return [Ocsimore_common.Session_data sd;
                       Wiki_action_info (Error (d, Operation_not_allowed))])
    >>= fun r ->
  (match role with
    | Admin ->
        (match addr with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let readers = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_readers wiki_id box_id readers)
          >>= fun () ->
        (match addw with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let w = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_writers wiki_id box_id w)
          >>= fun () ->
        (match adda with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let a = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_wbadmins wiki_id box_id a)
          >>= fun () ->
        (match delr with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let readers = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_readers wiki_id box_id readers)
          >>= fun () ->
        (match delw with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let w = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_writers wiki_id box_id w)
          >>= fun () ->
        (match dela with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let a = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_wbadmins wiki_id box_id a)
    | _ -> Lwt.return ()) >>= fun () ->
  Lwt.return r
