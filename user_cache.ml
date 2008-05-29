(* Ocsimore
 * Copyright (C) 2008
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
Cached access to the database.

@author Vincent Balat
*)

let (>>=) = Lwt.bind

module GroupCache = Cache.Make (struct 
                             type key = int32 
                             type value = int32 list 
                           end) 

let group_cache = GroupCache.create 256

let get_groups =
  fun ~userid ->
    try
      Lwt.return (GroupCache.find group_cache userid)
    with Not_found ->
      print_string (Int32.to_string userid);
      print_endline " -> groups: db access";
      User_sql.get_groups userid >>= fun r ->
      GroupCache.add group_cache userid r;
      Lwt.return r



module IUserCache = Cache.Make (struct 
                          type key = int32 
                          type value = (User_sql.userid * 
                                          string * 
                                          string option * 
                                          string * 
                                          string option) *
                              User_sql.userid list 
                        end) 

let iusercache = IUserCache.create 64

  
let find_user =
  fun ?id ?name () ->
    try
      match id, name with
        | Some i, _ -> Lwt.return (IUserCache.find iusercache i)
        | _ -> User_sql.find_user ?id ?name ()
    with Not_found ->
      (match id with Some i -> print_string (Int32.to_string i) | _ -> ());
      (match name with Some n -> print_string n | _ -> ());
      print_endline " -> find_user: db access";
      User_sql.find_user ?id ?name () 
      >>= fun (((i, n, _, _, _), _) as r) ->
      IUserCache.add iusercache i r;
      Lwt.return r

let add_to_group ~userid ~groupid =
  IUserCache.remove iusercache userid;
  GroupCache.remove group_cache userid;
  User_sql.add_to_group ~userid ~groupid

let remove_from_group ~userid ~groupid =
  IUserCache.remove iusercache userid;
  GroupCache.remove group_cache userid;
  User_sql.remove_from_group ~userid ~groupid

let delete_user ~userid =
  IUserCache.remove iusercache userid;
  GroupCache.remove group_cache userid;
  User_sql.delete_user ~userid

let update_data ~userid ~name ~password ~fullname ~email ?groups () =
  IUserCache.remove iusercache userid;
  GroupCache.remove group_cache userid;
  User_sql.update_data ~userid ~name ~password ~fullname ~email ?groups ()
