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

let debug_print_cache = ref false

let print_cache s =
  if !debug_print_cache then print_endline s


module GroupCache = Cache.Make (struct 
                             type key = int32 
                             type value = int32 list 
                           end) 

let group_cache = GroupCache.create (fun u -> User_sql.get_groups_ u) 256

let get_groups ~userid = 
  print_cache "cache groups ";
  GroupCache.find group_cache userid




module IUserCache = Cache.Make (struct 
                          type key = int32 
                          type value = (User_sql.userid * 
                                          string * 
                                          User_sql.pwd * 
                                          string * 
                                          string option *
                                          bool) *
                              User_sql.userid list 
                        end) 

module NUserCache = Cache.Make (struct 
                          type key = string
                          type value = (User_sql.userid * 
                                          string * 
                                          User_sql.pwd * 
                                          string * 
                                          string option *
                                          bool) *
                              User_sql.userid list 
                        end) 

let iusercache = 
  IUserCache.create (fun id -> User_sql.find_user_ ~id ()) 64

let nusercache = 
  NUserCache.create (fun name -> User_sql.find_user_ ~name ()) 64

  
let find_user =
  fun ?id ?name () ->
    match id, name with
      | Some i, _ -> 
(*          IUserCache.find iusercache i *)
          (try
             print_cache "cache iuser ";
             Lwt.return (IUserCache.find_in_cache iusercache i)
           with Not_found ->
print_cache "            cache: db access (iu)";
             User_sql.find_user_ ~id:i () >>= fun (((_, n, _, _, _, _), _) as r) ->
             IUserCache.add iusercache i r;
             NUserCache.add nusercache n r;
             Lwt.return r)

      | _, Some n -> 
          (try
             print_cache "cache nuser ";
             Lwt.return (NUserCache.find_in_cache nusercache n)
           with Not_found ->
             print_cache "           cache: db access (nu)";
             User_sql.find_user_ ~name:n () >>= fun (((i, _, _, _, _, _), _) as r) ->
             IUserCache.add iusercache i r;
             NUserCache.add nusercache n r;
             Lwt.return r)

      | _ -> User_sql.find_user_ ?id ?name ()


let add_to_group ~userid ~groupid =
  IUserCache.find iusercache userid >>= fun ((_, n, _, _, _, _), _) ->
  IUserCache.remove iusercache userid;
  NUserCache.remove nusercache n;
  GroupCache.remove group_cache userid;
  User_sql.add_to_group_ ~userid ~groupid

let remove_from_group ~userid ~groupid =
  IUserCache.find iusercache userid >>= fun ((_, n, _, _, _, _), _) ->
  IUserCache.remove iusercache userid;
  NUserCache.remove nusercache n;
  GroupCache.remove group_cache userid;
  User_sql.remove_from_group_ ~userid ~groupid

let delete_user ~userid =
  IUserCache.find iusercache userid >>= fun ((_, n, _, _, _, _), _) ->
  IUserCache.remove iusercache userid;
  NUserCache.remove nusercache n;
  GroupCache.remove group_cache userid;
  User_sql.delete_user_ ~userid

(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
let update_data ~userid ~password ~fullname ~email ?groups () =
  IUserCache.find iusercache userid >>= fun ((_, n, _, _, _, _), _) ->
  IUserCache.remove iusercache userid;
  NUserCache.remove nusercache n;
  GroupCache.remove group_cache userid;
  User_sql.update_data_ ~userid ~password ~fullname ~email ?groups ()
*)
