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

let get_wiki_by_id id =
  Wiki_sql.find_wiki ~id ()
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
  match wiki.default_admin with
    | Some admin -> (* acl are activated *)
        Lwt.return (Users.in_group user admin)
    | None -> Lwt.return false

let can_read wiki wikibox user =
  match wiki.default_admin with
    | Some admin -> (* acl are activated *)
        Wiki_sql.get_readers ~wiki:wiki.id ~id:wikibox >>= fun l ->
        Lwt.return (List.exists (fun a -> Users.in_group user a) l)
    | None -> (* acl are not activated *)
        Lwt.return (Users.in_group user wiki.default_reader)
    
let can_write wiki wikibox user =
  match wiki.default_admin with
    | Some admin -> (* acl are activated *)
        Wiki_sql.get_writers ~wiki:wiki.id ~id:wikibox >>= fun l ->
        Lwt.return (List.exists (fun a -> Users.in_group user a) l)
    | None -> (* acl are not activated *)
    Lwt.return (Users.in_group user wiki.default_writer)
    
let get_role ~sd (wiki_id : Wiki_sql.wiki) wikibox =
  get_wiki_by_id wiki_id >>= fun f -> 
  let u =
    match sd with
      | Eliom_sessions.Data u -> u
      | _ -> Users.anonymous
  in
  if u = Users.admin
  then Lwt.return (Wiki_sql.Admin (Users.get_user_id sd))
  else
    can_admin f wikibox u >>= fun cana ->
    if cana
    then Lwt.return (Wiki_sql.Admin (Users.get_user_id sd))
    else
      can_write f wikibox u >>= fun canw ->
      if canw
      then Lwt.return (Wiki_sql.Author (Users.get_user_id sd))
      else 
        can_read f wikibox u >>= fun canr ->
        if canr
        then Lwt.return (Wiki_sql.Lurker (Users.get_user_name sd))
        else Lwt.return Wiki_sql.Unknown


(****)
exception Editbox of (int32 * int32)
exception Action_failed of (int32 * int32 * exn)
exception Operation_not_allowed of (int32 * int32)

let edit_in_progress ~sp d =
  List.mem (Editbox d) (Eliom_sessions.get_exn sp)

let save_wikibox ~sp ~sd (((wiki_id, box_id), content), 
                          (addr, (addw, (adda, (delr, (delw, dela)))))) =
  get_role sd wiki_id box_id >>= fun role ->
  (match role with
    | Wiki_sql.Admin userid
    | Wiki_sql.Author userid ->
        get_wiki_by_id wiki_id >>= fun wiki_info ->
        Users.get_user_by_id userid >>= fun user ->
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
          (fun e -> Lwt.return [Action_failed (wiki_id, box_id, e)])
        else
          Lwt.return [Operation_not_allowed (wiki_id, box_id)]
    | _ -> Lwt.return [Operation_not_allowed (wiki_id, box_id)])
    >>= fun r ->
  (match role with
    | Wiki_sql.Admin _ ->
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
(*          >>= fun () ->
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
        (match addr with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let a = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_wbadmins wiki_id box_id a)
          >>= fun () -> *)
    | _ -> Lwt.return ()) >>= fun () ->
  Lwt.return r
