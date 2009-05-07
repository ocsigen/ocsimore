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

let (>>=)= Lwt.bind

open Sql.PGOCaml
open Ocsimore_lib
open CalendarLib
open Sql

module Types = struct

  type userid = [`User ] Opaque.int32_t

  let user_from_sql (u : int32) = (Opaque.int32_t u : userid)
  let sql_from_user (u : userid) = Opaque.t_int32 u

  type pwd =
    | Connect_forbidden
    | Ocsimore_user_plain of string
    | Ocsimore_user_crypt of string
    | External_Auth

  type userdata = {
    user_id: userid;
    user_login: string;
    mutable user_pwd: pwd;
    mutable user_fullname: string;
    mutable user_email: string option;
    user_dyn: bool;
    user_parameterized_group: bool;
  }


  (* 'a is a phantome type, used to represent the type of the argument
     of the user *)
  type 'a parameterized_group = userid

  type user =
    | Ground of userid
    | Applied of userid * int32


  let apply_parameterized_group g v = Applied (g, Opaque.t_int32 v)
  let ($) = apply_parameterized_group
  let basic_user v = Ground v

  let userid_from_user = function
    | Ground u -> u
    | Applied (u, _) -> u


  type 'a admin_writer_reader = {
    grp_admin: 'a parameterized_group;
    grp_writer: 'a parameterized_group;
    grp_reader: 'a parameterized_group;
  }

end
open Types


let decompose_user = function
  | Ground u -> (sql_from_user u, None)
  | Applied (u, v) -> (sql_from_user u, Some v)


(* Transforms an incoming value of type [pwd], in which
   [Ocsimore_user_crypt] is supposed to contain the unencrypted password,
   into the outgoing value of type [pwd] ([Ocsimore_user_crypt] contains the
   encrypted password) and the database representation of this value,
   for the columns password and authtype respectively *)
(* Notice that "g" is used in the database for parametrized groups. However
   it is still translated as having authtype [Connect_forbidden] *)
let pass_authtype_from_pass pwd = match pwd with
  | Connect_forbidden ->
      Lwt.return (pwd, (None, "l"))

  | External_Auth ->
      Lwt.return (pwd, (None, "p"))

  | Ocsimore_user_plain p ->
      Lwt.return (pwd, (Some p, "l"))

  | Ocsimore_user_crypt pass ->
      Nis_chkpwd.crypt_passwd pass >>=
      fun crypt ->
      Lwt.return (Ocsimore_user_crypt crypt, (Some crypt, "c"))



let add_to_group_aux db (u, vu) (g, vg) =
  Lwt.catch
    (fun () ->
       PGSQL(db) "INSERT INTO userrights (id, groupid, idarg, groupidarg)\
                  VALUES ($u, $g, $?vu, $?vg)")
    (function
       | Sql.PGOCaml.PostgreSQL_Error _ ->
           (*Ocsigen_messages.warning "Ocsimore: duplicate group insertion";*)
           Lwt.return ()
       | e -> Lwt.fail e
    )

let populate_groups db user groups =
  let (u, vu) = decompose_user user in
  Lwt_util.iter_serial (fun g -> add_to_group_aux db (u, vu) (decompose_user g))
    groups

let add_to_group_ ~user ~group =
  Lwt_pool.use Sql.pool
    (fun db ->
       add_to_group_aux db (decompose_user user) (decompose_user group))

(* Constant used in the database to detect parameterized edge. Must
   of course be different from all other parameters. This must be
   enforced in all the modules using parameterized groups, for example
   by using serial types that statr at 0 *)
let ct_parameterized_edge = -1l

let add_generic_inclusion_ ~subset ~superset =
  Lwt_pool.use Sql.pool
    (fun db ->
       let subset = sql_from_user subset
       and superset = sql_from_user superset in
       add_to_group_aux db (subset, Some ct_parameterized_edge)
         (superset, Some ct_parameterized_edge)
    )


let remove_from_group_ ~user ~group =
  let (u, vu) = decompose_user user
  and (g, vg) = decompose_user group
  in
  Lwt_pool.use Sql.pool
    (fun db ->
       match vu, vg with
         | None, None ->
             PGSQL(db) "DELETE FROM userrights
                  WHERE id = $u AND groupid = $g
                  AND   idarg IS NULL AND groupidarg IS NULL"
         | Some vu, None ->
             PGSQL(db) "DELETE FROM userrights
                  WHERE id = $u AND groupid = $g
                  AND   idarg = $vu AND groupidarg IS NULL"
         | Some vu, Some vg ->
             PGSQL(db) "DELETE FROM userrights
                  WHERE id = $u AND groupid = $g
                  AND   idarg = $vu AND groupidarg = $vg"
         | None, Some vg ->
             PGSQL(db) "DELETE FROM userrights
                  WHERE id = $u AND groupid = $g
                  AND   idarg IS NULL AND groupidarg = $vg"
    )



let new_user ~name ~password ~fullname ~email ~groups ~dyn =
  pass_authtype_from_pass password
  >>= fun (pwd, (password, authtype)) ->
  Sql.full_transaction_block
    (fun db ->
       (match password, email with
          | None, None ->
              PGSQL(db) "INSERT INTO users (login, fullname, dyn, authtype)\
                    VALUES ($name, $fullname, $dyn, $authtype)"
          | Some pwd, None ->
              PGSQL(db) "INSERT INTO users (login, password, fullname, dyn, authtype) \
                    VALUES ($name, $pwd, $fullname, $dyn, $authtype)"
          | None, Some email ->
              PGSQL(db) "INSERT INTO users (login, fullname, email, dyn, authtype) \
                    VALUES ($name, $fullname, $email, $dyn, $authtype)"
          | Some pwd, Some email ->
              PGSQL(db) "INSERT INTO users (login, password, fullname, email, dyn, authtype) \
                    VALUES ($name, $pwd, $fullname, $email, $dyn, $authtype)"
       ) >>= fun () ->
       serial4 db "users_id_seq"
       >>= fun id ->
       let id = user_from_sql id in
       populate_groups db (basic_user id) groups >>= fun () ->
       Lwt.return (id, pwd)
    )

let find_userid_by_name_aux_ db name =
  PGSQL(db) "SELECT id FROM users WHERE login = $name"
  >>= function
    | [] -> Lwt.fail Not_found
    | r :: _ -> Lwt.return (user_from_sql r)


let new_parametrized_group ~prefix ~name ~fullname =
  let fullname = "#" ^ prefix ^ "." ^ fullname in
  let authtype = "g" in
  Sql.full_transaction_block
    (fun db ->
       Lwt.catch
         (fun () -> find_userid_by_name_aux_ db name)
         (function
            | Not_found ->
                PGSQL(db) "INSERT INTO users (login, fullname, dyn, authtype)\
                           VALUES ($name, $fullname, FALSE, $authtype)"
                >>= fun () ->
                serial4 db "users_id_seq"
                >>= fun id -> Lwt.return (user_from_sql id)
            | e -> Lwt.fail e
         ))

let wrap_userdata (id, login, pwd, name, email, dyn, authtype) =
  let password = match authtype, pwd with
    | "p", _ -> External_Auth
    | "c", Some p -> Ocsimore_user_crypt p
    | "l", Some p -> Ocsimore_user_plain p
    | _ -> Connect_forbidden
  in
  Lwt.return ({ user_id = user_from_sql id; user_login = login;
                user_pwd = password; user_fullname = name;
                user_email = email; user_dyn = dyn;
                user_parameterized_group = (authtype = "g") })


let find_user_by_name_ name =
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                  FROM users WHERE login = $name"
       >>= function
         | [] -> Lwt.fail Not_found
         | r :: _ -> wrap_userdata r)

let find_userid_by_name_ name =
  Lwt_pool.use Sql.pool (fun db -> find_userid_by_name_aux_ db name)


let find_user_by_id_ id =
  let id = sql_from_user id in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                  FROM users WHERE id = $id"
       >>= function
         | [] -> Lwt.fail Not_found
         | r :: _ -> wrap_userdata r)


let delete_user_ ~userid =
  let userid = sql_from_user userid in
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "DELETE FROM users WHERE id = $userid")


(* BY 2009-03-13: deactivated because probably buggued. Check the DEFAULT below *)
(*
let update_data_ ~userid ~password ~fullname ~email ?groups ?dyn () =
  pass_authtype_from_pass password
  >>= fun (pwd, (password, authtype)) ->
  Sql.full_transaction_block
    (fun db ->
       (match password, email with
         | None, None -> 
             PGSQL(db) "UPDATE users SET fullname = $fullname, email = DEFAULT, password = DEFAULT, authtype = $authtype WHERE id = $userid"
         | None, Some email -> 
             PGSQL(db) "UPDATE users SET fullname = $fullname, password = DEFAULT, email = $email, authtype = $authtype WHERE id = $userid"
         | Some pwd, None -> 
             PGSQL(db) "UPDATE users SET password = $pwd, fullname = $fullname, email = DEFAULT, authtype = $authtype WHERE id = $userid"
         | Some pwd, Some email -> 
             PGSQL(db) "UPDATE users SET password = $pwd, fullname = $fullname, email = $email, authtype = $authtype WHERE id = $userid"
       ) 
       >>= fun () ->
         (match dyn with
            | Some dyn -> 
                PGSQL(db) "UPDATE users SET dyn = $dyn WHERE id = $userid"
            | None -> Lwt.return ())
       >>= fun () ->
         (match groups with
            | Some groups ->
                PGSQL(db) "DELETE FROM userrights WHERE id=$userid"
                >>= fun () ->
                populate_groups db userid groups;
            | None -> Lwt.return ())
       >>= fun () -> Lwt.return pwd
    )
*)

let get_groups_ ~user =
  let u, vu = decompose_user user in
  let convert_list = List.map (function
                                 | (g, None) -> basic_user (user_from_sql g)
                                 | (g, Some v) -> Applied (user_from_sql g, v)
                              ) in
  Lwt_pool.use Sql.pool
    (fun db ->
       (match vu with
          | None ->
              PGSQL(db) "SELECT groupid, groupidarg FROM userrights
                         WHERE id = $u AND idarg IS NULL"
              >>= fun l -> Lwt.return (convert_list l)
          | Some vu ->
              (* Standard inclusions *)
              PGSQL(db) "SELECT groupid, groupidarg FROM userrights
                         WHERE id = $u AND idarg = $vu"
              >>= fun r1 ->
              let l1 = convert_list r1 in
              (* Generic edges. If the database is correct, groupidarg
                 is always [ct_parameterized_edge] *)
              PGSQL(db) "SELECT groupid FROM userrights
                         WHERE id = $u AND idarg = $ct_parameterized_edge"
              >>= fun r2 ->
              let l2 = List.map (fun g -> Applied (user_from_sql g, vu)) r2 in
              Lwt.return (l1 @ l2)
       )
    )


(** Cached version of the functions above *)

let debug_print_cache = ref false

let print_cache s =
  if !debug_print_cache then print_endline s


(* Find user info by id *)

module IUserCache = Cache.Make (struct
                          type key = userid
                          type value = userdata
                        end)

let iusercache = IUserCache.create
  (fun id -> find_user_by_id_ id) 64

let get_basicuser_data i =
  print_cache "cache iuser ";
  IUserCache.find iusercache i

let get_parameterized_user_data = get_basicuser_data

let get_user_data = function
  | Applied (i, _) | Ground i -> get_basicuser_data i


(* Find userid by login *)

module NUseridCache = Cache.Make (struct
                          type key = string
                          type value = userid
                        end)

exception NotBasicUser of userdata

let nuseridcache = NUseridCache.create
  (fun name ->
     find_user_by_name_ name
     >>= fun u ->
     if u.user_parameterized_group then
       Lwt.fail (NotBasicUser u)
     else (
       Lwt.return u.user_id)
  ) 64

let get_basicuser_by_login n =
  print_cache "cache nuserid ";
  NUseridCache.find nuseridcache n


(* Find user from string *)

module NUserCache = Cache.Make (struct
                          type key = string
                          type value = user
                        end)

let nusercache = NUserCache.create
  (fun s ->
     if String.length s > 0 && s.[0] = '#' then
       try
         Scanf.scanf "%s(%ld)"
           (fun g v ->
              find_user_by_name_ g
              >>= fun u ->
                if u.user_parameterized_group then
                  Lwt.return (Applied (u.user_id, v))
                else
                  Lwt.fail Not_found
           )
       with _ -> Lwt.fail Not_found
     else
       get_basicuser_by_login s >>= fun u ->
         Lwt.return (basic_user u)
  ) 64

let get_user_by_name name =
  print_cache "cache nuser ";
  NUserCache.find nusercache name





(* Groups-related functions *)

module GroupCache = Cache.Make (struct
                             type key = user
                             type value = user list
                           end)

let group_cache = GroupCache.create (fun u -> get_groups_ u) 256

let get_groups ~user =
  print_cache "cache groups ";
  GroupCache.find group_cache user


let add_to_group ~user ~group =
  GroupCache.remove group_cache user;
  add_to_group_ ~user ~group

let add_generic_inclusion ~subset ~superset =
  GroupCache.clear group_cache;
  add_generic_inclusion_ ~subset ~superset

let remove_from_group ~user ~group =
  GroupCache.remove group_cache user;
  remove_from_group_ ~user ~group


(* Deletion *)

let delete_user ~userid =
  (* We clear all the caches, as we should iterate over all the
     parameters if [userid] is a parametrized group *)
  IUserCache.clear iusercache;
  NUseridCache.clear nuseridcache;
  NUserCache.clear nusercache;
  GroupCache.clear group_cache;
  delete_user_ ~userid


(* Conversion to string *)

let userid_to_string u =
  get_basicuser_data u
  >>= fun { user_login = r } -> Lwt.return r

let user_to_string = function
  | Ground u -> userid_to_string u
  | Applied (u, v) ->
      userid_to_string u >>= fun s ->
      Lwt.return (Printf.sprintf "%s(%ld)" s v)




(* BY 2009-03-13: deactivated because update_data_ is deactivated. *)
(*
let update_data ~userid ~password ~fullname ~email ?groups () =
  IUserCache.find iusercache userid >>= fun ((_, n, _, _, _, _), _) ->
  IUserCache.remove iusercache userid;
  NUserCache.remove nusercache n;
  GroupCache.remove group_cache userid;
  User_sql.update_data_ ~userid ~password ~fullname ~email ?groups ()
*)
