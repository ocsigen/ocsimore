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

type wiki = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  default_reader: Users.user;
  default_writer: Users.user;
  acl_enabled: bool;
}

let get_wiki_by_id db id =
  Wiki_sql.find_wiki db ~id ()
  >>= fun (id, title, descr, r, w, a) -> 
  Users.get_user_by_name db ~name:r >>= fun read -> 
  Users.get_user_by_name db ~name:w >>= fun write -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = read;
	       default_writer = write; 
               acl_enabled = a
	     }

let get_wiki_by_name db title =
  Wiki_sql.find_wiki db ~title () >>= fun (id, title, descr, r, w, a) -> 
  Users.get_user_by_name db ~name:r >>= fun read -> 
  Users.get_user_by_name db ~name:w >>= fun write -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               default_reader = read;
	       default_writer = write; 
               acl_enabled = a
	     }

let create_wiki db ~title ~descr ?(acl_enabled=false)
    ?(reader = Users.anonymous) ?(writer = Users.anonymous) () =
  Lwt.catch 
    (fun () -> get_wiki_by_name db title)
    (function
       | Not_found -> 
           let (r_id, _, _, _, _) = Users.get_user_data reader in
           let (w_id, _, _, _, _) = Users.get_user_data writer in
           Wiki_sql.new_wiki db title descr r_id w_id acl_enabled
               >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr; 
                        default_reader = reader;
	                default_writer = writer; 
                        acl_enabled = acl_enabled;
	              }
    | e -> Lwt.fail e)

  


