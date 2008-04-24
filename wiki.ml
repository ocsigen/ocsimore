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
  acl_enabled: bool;
}

let get_wiki_by_id id =
  Wiki_sql.find_wiki ~id ()
  >>= fun (id, title, descr, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = r;
               default_writer = w; 
               acl_enabled = a
             }

let get_wiki_by_name title =
  Wiki_sql.find_wiki ~title () >>= fun (id, title, descr, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = r;
               default_writer = w; 
               acl_enabled = a
             }

let create_wiki ~title ~descr
    ?(acl_enabled=false)
    ?(reader = Users.anonymous_group)
    ?(writer = Users.anonymous_group) () =
  Lwt.catch 
    (fun () -> get_wiki_by_name title)
    (function
       | Not_found -> 
           let r_id = Users.id_of_group reader in
           let w_id = Users.id_of_group writer in
           Wiki_sql.new_wiki title descr r_id w_id acl_enabled
               >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr; 
                        default_reader = reader;
                        default_writer = writer; 
                        acl_enabled = acl_enabled;
                      }
    | e -> Lwt.fail e)

let new_wikibox ~wiki ~author ~comment ~content =
  fun ?(readers = [wiki.default_reader]) ?(writers = [wiki.default_writer]) 
    () ->
  Wiki_sql.new_wikibox
    ~wiki:wiki.id
    ~author
    ~comment
    ~content
    ?rights:(if wiki.acl_enabled then Some (readers, writers) else None)
    ()


let can_read wiki wikibox user =
  if wiki.acl_enabled
  then
    Wiki_sql.get_readers ~wiki:wiki.id ~id:wikibox >>= fun l ->
    Lwt.return (List.exists (fun a -> Users.in_group user a) l)
  else
    Lwt.return (Users.in_group user wiki.default_reader)
    
let can_write wiki wikibox user =
  if wiki.acl_enabled
  then
    Wiki_sql.get_writers ~wiki:wiki.id ~id:wikibox >>= fun l ->
    Lwt.return (List.exists (fun a -> Users.in_group user a) l)
  else
    Lwt.return (Users.in_group user wiki.default_writer)
    
let get_role sm (wiki_id : Wiki_sql.wiki) wikibox =
  get_wiki_by_id wiki_id >>= fun f -> 
  match sm#get_user with
    | Eliom_sessions.Data u -> 
        can_write f wikibox u >>= fun canw ->
        if canw && sm#is_logged_on
        then Lwt.return (Wiki_sql.Author sm#get_user_id)
        else 
          can_read f wikibox u >>= fun canr ->
          if canr && sm#is_logged_on
          then Lwt.return (Wiki_sql.Lurker sm#get_user_name)
          else Lwt.return Wiki_sql.Unknown
   | _ ->  Lwt.return Wiki_sql.Unknown

