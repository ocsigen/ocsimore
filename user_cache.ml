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

let get_groups =
  let module C = Cache.Make (struct 
                               type key = int32 
                               type value = int32 list 
                             end) 
  in
  let cache = C.create 256 in
  fun ~userid ->
    try
      Lwt.return (C.find cache userid)
    with Not_found ->
      print_string (Int32.to_string userid);
      print_endline " -> groups: db access";
      User_sql.get_groups userid >>= fun r ->
      C.add cache userid r;
      Lwt.return r

  
let find_user =
(* Two caches (but sharing) *)
  let module C1 = Cache.Make (struct 
                               type key = int32 
                               type value = (User_sql.userid * 
                                               string * 
                                               string option * 
                                               string * 
                                               string) *
                                   User_sql.userid list 
                             end) 
  in
  let module C2 = Cache.Make (struct 
                               type key = string
                               type value = (User_sql.userid * 
                                               string * 
                                               string option * 
                                               string * 
                                               string) *
                                   User_sql.userid list
                             end) 
  in
  let cache1 = C1.create 64 in
  let cache2 = C2.create 64 in
  fun ?id ?name () ->
    try
      match id, name with
        | Some i, _ -> Lwt.return (C1.find cache1 i)
        | None, Some n -> Lwt.return (C2.find cache2 n)
        | _ ->
            Lwt.fail 
              (Failure "User_cache.find_user: Neither name nor id specified")
    with Not_found ->
      (match id with Some i -> print_string (Int32.to_string i) | _ -> ());
      (match name with Some n -> print_string n | _ -> ());
      print_endline " -> find_user: db access";
      User_sql.find_user ?id ?name () 
      >>= fun (((i, n, _, _, _), _) as r) ->
      C1.add cache1 i r;
      C2.add cache2 n r;
      Lwt.return r
