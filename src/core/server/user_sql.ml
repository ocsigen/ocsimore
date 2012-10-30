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

open Eliom_lib
open Eliom_lib.Lwt_ops

open Ocsi_sql

module Types = struct

  type userid = [`User ] Opaque.int32_t

  let userid_from_sql (u : int32) = (Opaque.int32_t u : userid)
  let sql_from_userid (u : userid) = Opaque.t_int32 u

  let string_from_userid i = Int32.to_string (Opaque.t_int32 i)


  type pwd =
    | Connect_forbidden
    | Ocsimore_user_plain of string
    | Ocsimore_user_crypt of string
    | Ocsimore_user_safe of Bcrypt.hash_t
    | External_Auth


  type find_param = {
    param_description: string;
    param_display: (int32 -> string Lwt.t) option;
    find_param_functions:
      ((string -> int32 Lwt.t) * (int32 -> string Lwt.t)) option;
  }

  let hash_find_param : (string, find_param) Hashtbl.t = Hashtbl.create 17


  type userdata = {
    user_id: userid;
    user_login: string;
    user_pwd: pwd;
    user_fullname: string;
    user_email: string option;
    user_dyn: bool;
    user_kind: [ `BasicUser
               | `ParameterizedGroup of find_param option
               | `NonParameterizedGroup];
  }


  (* 'a is a phantome type, used to represent the type of the argument
     of the user *)
  type 'a parameterized_group = userid

  type user =
    | BasicUser of userid
    | AppliedParameterizedGroup of userid * int32
    | NonParameterizedGroup of userid


  let apply_parameterized_group g v =
    AppliedParameterizedGroup (g, Opaque.t_int32 v)

  let ($) = apply_parameterized_group

  let basic_user v = BasicUser v

  let userid_from_user = function
    | BasicUser u | NonParameterizedGroup u -> u
    | AppliedParameterizedGroup (u, _) -> u

  let is_basic_user = function
    | BasicUser u -> Some u
    | NonParameterizedGroup _
    | AppliedParameterizedGroup _ -> None

let user_is_applied_parameterized_group ~user ~pgroup =
  match user with
    | BasicUser _ | NonParameterizedGroup _ -> None
    | AppliedParameterizedGroup (g, v) ->
        if pgroup = g then Some (Opaque.int32_t v) else None


  type 'a admin_writer_reader = {
    grp_admin: 'a parameterized_group;
    grp_writer: 'a parameterized_group;
    grp_reader: 'a parameterized_group;
  }

  type users = {
    users : userdata list Lazy.t;
    groups : userdata list Lazy.t;
    roles : userdata list Lazy.t;
  }
end

open Types


(* Constant used in the database to detect parameterized edge. Must
   of course be different from all other parameters. This must be
   enforced in all the modules using parameterized groups, for example
   by using serial types that statr at 0 *)
let ct_parameterized_edge = -1l

(* Same kind of things for non parameterized groups *)
let ct_non_parameterized_group = -2l

let sql_from_user = function
  | BasicUser u -> (sql_from_userid u, None)
  | AppliedParameterizedGroup (u, v) -> (sql_from_userid u, Some v)
  | NonParameterizedGroup u ->
      (sql_from_userid u, Some ct_non_parameterized_group)

let user_from_sql data =
  match (data#!id, data#?idarg) with
  | (u, None) -> BasicUser (userid_from_sql u)
  | (u, Some v) ->
      if v = ct_non_parameterized_group
      then NonParameterizedGroup (userid_from_sql u)
      else AppliedParameterizedGroup (userid_from_sql u, v)


(* Transforms an incoming value of type [pwd], in which
   [Ocsimore_user_crypt] is supposed to contain the unencrypted password,
   into the outgoing value of type [pwd] ([Ocsimore_user_crypt] contains the
   encrypted password) and the database representation of this value,
   for the columns password and authtype respectively *)
(* Notice that "g" and "h" are used in the database for parametrized groups.
   However they are still translated as having authtype [Connect_forbidden] *)
let pass_authtype_from_pass pwd = match pwd with
  | Connect_forbidden ->
      Lwt.return (pwd, (None, "l"))

  | External_Auth ->
      Lwt.return (pwd, (None, "p"))

  | Ocsimore_user_plain p ->
      Lwt.return (pwd, (Some p, "l"))

  | Ocsimore_user_crypt pass ->
      Crypt.crypt_passwd pass >>=
      fun crypt ->
      Lwt.return (Ocsimore_user_crypt crypt, (Some crypt, "c"))

  | Ocsimore_user_safe pass ->
    let crypt = Bcrypt.string_of_hash pass in
    Lwt.return (Ocsimore_user_safe pass, (Some crypt, "s"))

let userrights = <:table< userrights (
  id integer NOT NULL,
  groupid integer NOT NULL,
  idarg integer,
  groupidarg integer
) >>

let remove_from_group_aux db (u, vu) (g, vg) =
  Lwt_Query.query db (<:delete< d in $userrights$ |
      d.id = $int32:u$;
      d.groupid = $int32:g$;
      is_not_distinct_from d.idarg (of_option $Option.map Sql.Value.int32 vu$);
      is_not_distinct_from d.groupidarg (of_option $Option.map Sql.Value.int32 vg$) >>)

let remove_from_group_ ~user ~group =
  Lwt_pool.use Ocsi_sql.pool (fun db -> remove_from_group_aux db
                           (sql_from_user user) (sql_from_user group))

let add_to_group_aux db (u, vu) (g, vg) =
  remove_from_group_aux db (u, vu) (g, vg) >>= fun () ->
  Lwt_Query.query db (<:insert< $userrights$ := {
    id = $int32:u$;
    groupid = $int32:g$;
    idarg = of_option $Option.map Sql.Value.int32 vu$;
    groupidarg = of_option $Option.map Sql.Value.int32 vg$
  } >>)

let add_to_group_ ~user ~group =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
       add_to_group_aux db (sql_from_user user) (sql_from_user group))


let add_generic_inclusion_ ~subset ~superset =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
       let subset = sql_from_userid subset
       and superset = sql_from_userid superset in
       add_to_group_aux db (subset, Some ct_parameterized_edge)
         (superset, Some ct_parameterized_edge)
    )

let users_id_seq = <:sequence< serial "users_id_seq" >>

let users = <:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  login text NOT NULL,
  password text DEFAULT(null),
  fullname text NOT NULL,
  email text DEFAULT(null),
  dyn boolean NOT NULL,
  authtype text NOT NULL
) >>

let new_user ~name ~password ~fullname ~email ~dyn =
  pass_authtype_from_pass password
  >>= fun (pwd, (password, authtype)) ->
  Ocsi_sql.full_transaction_block
    (fun db ->
      let password = match password with
        | Some x -> (<:value< $string:x$ >>)
        | None -> (<:value< users?password >>)
      and email = match email with
        | Some x -> (<:value< $string:x$ >>)
        | None -> (<:value< users?email >>)
      in
      Lwt_Query.query db (<:insert< $users$ := {
        id = users?id;
        login = $string:name$;
        password = $password$;
        fullname = $string:fullname$;
        email = $email$;
        dyn = $bool:dyn$;
        authtype = $string:authtype$
      } >>)
      >>= fun () ->
      Lwt_Query.value db (<:value< currval $users_id_seq$ >>)
      >>= fun id ->
      let id = userid_from_sql id in
      Lwt.return (id, pwd)
    )

exception NotAnUser

let find_userid_by_name_aux_ db name =
  Lwt_Query.view_opt db (<:view< {
    u.id
  } | u in $users$; u.login = $string:name$ >>)
  >>= function
    | None -> Lwt.fail NotAnUser
    | Some r -> Lwt.return (userid_from_sql (r#!id))


let new_group authtype find_param ~prefix ~name ~descr =
  let name = "#" ^ prefix ^ "." ^ name in
  (match find_param with
     | None -> ()
     | Some v -> Hashtbl.add hash_find_param name v);
  Ocsi_sql.full_transaction_block
    (fun db ->
       Lwt.catch
         (fun () -> find_userid_by_name_aux_ db name >>= fun user ->
                    let us = sql_from_userid user in
                    Lwt_Query.query db (<:update< u in $users$ := {
                      fullname = $string:descr$
                    } | u.id = $int32:us$ >>) >>=
                    fun () -> Lwt.return user)
         (function
            | NotAnUser ->
              Lwt_Query.query db (<:insert< $users$ := {
                id = users?id;
                login = $string:name$;
                password = users?password;
                fullname = $string:descr$;
                email = users?email;
                dyn = false;
                authtype = $string:authtype$
              } >>)
              >>= fun () ->
              Lwt_Query.value db (<:value< currval $users_id_seq$ >>)
              >>= fun id -> Lwt.return (userid_from_sql id)
            | e -> Lwt.fail e
         ))

let new_parameterized_group ~prefix ~name ~descr ~find_param =
  new_group "g" (Some find_param) ~prefix ~name ~descr
let new_nonparameterized_group ~prefix ~name ~descr =
  new_group "h" None ~prefix ~name ~descr >>= fun id ->
  Lwt.return (NonParameterizedGroup id)

let wrap_userdata userdata =
  let password = match userdata#!authtype, userdata#?password with
    | "p", _ -> External_Auth
    | "c", Some p -> Ocsimore_user_crypt p
    | "s", Some p -> Ocsimore_user_safe (Bcrypt.hash_of_string p)
    | "l", Some p -> Ocsimore_user_plain p
    | _ -> Connect_forbidden
  in
  { user_id = userid_from_sql userdata#!id; user_login = userdata#!login;
    user_pwd = password; user_fullname = userdata#!fullname;
    user_email = userdata#?email; user_dyn = userdata#!dyn;
    user_kind =
      match userdata#!authtype with
        | "g" -> `ParameterizedGroup
            (try Some (Hashtbl.find hash_find_param userdata#!login)
             with Not_found -> None)
        | "h" -> `NonParameterizedGroup
        | _ -> `BasicUser}


let find_user_by_name_ name =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
      Lwt_Query.view_opt db
        (<:view< u | u in $users$; u.login = $string:name$ >>)
      >>= function
        | None -> Lwt.fail NotAnUser
        | Some r -> Lwt.return (wrap_userdata r))


let find_user_by_id_ id =
  let id = sql_from_userid id in
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
      Lwt_Query.view_opt db (<:view< u | u in $users$; u.id = $int32:id$ >>)
      >>= function
        | None -> Lwt.fail NotAnUser
        | Some r -> Lwt.return (wrap_userdata r))

let all_users () =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
      Lwt_Query.view db (<:view< u | u in $users$ >>)
    )
  >|= (List.map wrap_userdata)
  >>= fun list ->
  Lwt.return {
    users = lazy (
      List.filter
        (fun {user_kind = u; user_pwd = a; _} ->
          u = `BasicUser && a <> Connect_forbidden
        ) list
    );
    groups = lazy (
      List.filter
        (fun {user_kind = u; user_pwd = a; _} ->
          u = `BasicUser && a = Connect_forbidden
        ) list
    );
    roles = lazy (
      List.filter
        (fun {user_kind = u; _} ->
          u <> `BasicUser
        ) list
    );
  }


let delete_user_ ~userid =
  let userid = sql_from_userid userid in
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.query db (<:delete< d in $users$ | d.id = $int32:userid$ >>))

let update_data_ ~userid ?password ?fullname ?email ?dyn () =
  let userid = sql_from_userid userid in
  Ocsi_sql.full_transaction_block
    (fun db ->
       (match password with
          | None -> Lwt.return ()
          | Some pwd ->
              pass_authtype_from_pass pwd
              >>= fun (_pwd, (password, authtype)) ->
              Lwt_Query.query db (<:update< u in $users$ := {
                password = of_option $Option.map Sql.Value.string password$;
                authtype = $string:authtype$
              } | u.id = $int32:userid$ >>)
       ) >>= fun () ->
       (match fullname with
          | None -> Lwt.return ()
          | Some fullname ->
            Lwt_Query.query db (<:update< u in $users$ := {
              fullname = $string:fullname$
            } | u.id = $int32:userid$ >>)
       ) >>= fun () ->
       (match email with
          | None -> Lwt.return ()
          | Some email ->
            Lwt_Query.query db (<:update< u in $users$ := {
              email = of_option $Option.map Sql.Value.string email$
            } | u.id = $int32:userid$ >>)
       ) >>= fun () ->
       (match dyn with
          | Some dyn ->
            Lwt_Query.query db (<:update< u in $users$ := {
              dyn = $bool:dyn$
            } | u.id = $int32:userid$ >>)
          | None -> Lwt.return ()
       )
    )

(* Auxiliary functions to read permissions from the userrights table *)
let convert_group_list = List.map user_from_sql

let convert_generic_lists r1 r2 v =
  let l1 = convert_group_list r1
  and l2 = List.map (fun g ->
                       AppliedParameterizedGroup (userid_from_sql g#!id, v)) r2 in
  l1 @ l2


let groups_of_user_ ~user =
  let u, vu = sql_from_user user in
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
       (match vu with
          | None ->
            Lwt_Query.view db (<:view< {
              id = ur.groupid;
              idarg = ur.groupidarg
            } | ur in $userrights$; ur.id = $int32:u$;
                is_null ur.idarg >>)
              >>= fun l -> Lwt.return (convert_group_list l)
          | Some vu ->
              (* Standard inclusions *)
            Lwt_Query.view db (<:view< {
              id = ur.groupid;
              idarg = ur.groupidarg
            } | ur in $userrights$; ur.id = $int32:u$;
                ur.idarg = nullable $int32:vu$ >>)
              >>= fun r1 ->
              (* Generic edges. If the database is correct, groupidarg
                 is always [ct_parameterized_edge] *)
              Lwt_Query.view db (<:view< {
                id = ur.groupid
              } | ur in $userrights$; ur.id = $int32:u$;
                  ur.idarg = nullable $int32:ct_parameterized_edge$ >>)
              >>= fun r2 ->
              Lwt.return (convert_generic_lists r1 r2 vu)
       )
    )

(* as above *)
let users_in_group_ ?(generic=true) ~group =
  let g, vg = sql_from_user group in
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
       (match vg with
          | None ->
            Lwt_Query.view db (<:view< {
              ur.id;
              ur.idarg
            } | ur in $userrights$; ur.groupid = $int32:g$;
                is_null ur.groupidarg >>)
              >>= fun l -> Lwt.return (convert_group_list l)
          | Some vg ->
            Lwt_Query.view db (<:view< {
              ur.id;
              ur.idarg
            } | ur in $userrights$; ur.groupid = $int32:g$;
                ur.groupidarg = nullable $int32:vg$ >>)
              >>= fun r1 ->
              (if generic then
                  Lwt_Query.view db (<:view< {
                    ur.id
                  } | ur in $userrights$; ur.groupid = $int32:g$;
                      ur.groupidarg = nullable $int32:ct_parameterized_edge$ >>)
               else
                 Lwt.return []
              )
              >>= fun r2 ->
              Lwt.return (convert_generic_lists r1 r2 vg)
       )
    )


(** Cached version of the functions above *)

let debug_print_cache = ref false

let print_cache s =
  if !debug_print_cache then print_endline s


(* Find user info by id *)

module IUserCache = Ocsigen_cache.Make (struct
                          type key = userid
                          type value = userdata
                        end)

let iusercache = new IUserCache.cache
  (fun id -> find_user_by_id_ id) 64

let get_basicuser_data i =
  print_cache "cache iuser ";
  iusercache#find i

let get_parameterized_user_data = get_basicuser_data

let get_user_data = function
  | BasicUser i | NonParameterizedGroup i -> get_basicuser_data i
  | AppliedParameterizedGroup (i, v) ->
      get_basicuser_data i >>= fun ud ->
      match ud.user_kind with
        | `ParameterizedGroup param ->
            (match param with
               | Some { param_display = Some f; _ } ->
                   Lwt.catch
                     (fun () -> f v)
                     (fun _ -> Lwt.return (Int32.to_string v))
               | _ -> Lwt.return (Int32.to_string v))
            >>= fun arg ->
            Lwt.return { ud with user_fullname =
                Printf.sprintf "%s %s" ud.user_fullname arg }
        | _ (* impossible cases *) ->
            Lwt.return ud


(* Find userid by login *)

module NUseridCache = Ocsigen_cache.Make (struct
                          type key = string
                          type value = userid
                        end)

exception NotBasicUser of userdata

let nuseridcache = new NUseridCache.cache
  (fun name ->
     find_user_by_name_ name
     >>= fun u ->
     if u.user_kind <> `BasicUser then
       Lwt.fail (NotBasicUser u)
     else (
       Lwt.return u.user_id)
  ) 64

let get_basicuser_by_login n =
  print_cache "cache nuserid ";
  nuseridcache#find n


(* Find user from string *)

module NUserCache = Ocsigen_cache.Make (struct
                          type key = string
                          type value = user
                        end)

let regexp_groups = Netstring_pcre.regexp "^(.*)\\((.*)\\)$"

let nusercache = new NUserCache.cache
  (fun s ->
     if String.length s > 0 && s.[0] = '#' then
       match Netstring_pcre.string_match regexp_groups s 0 with
         | None ->
             find_user_by_name_ s
             >>= fun u ->
             if u.user_kind = `NonParameterizedGroup then
               Lwt.return (NonParameterizedGroup u.user_id)
             else
               Lwt.fail NotAnUser

         | Some rmatch ->
             let g = Netstring_pcre.matched_group rmatch 1 s
             and v = Netstring_pcre.matched_group rmatch 2 s in
             find_user_by_name_ g >>= fun u ->
             match u.user_kind with
               | `ParameterizedGroup param ->
                   (try Lwt.return (Int32.of_string v)
                    with _ ->
                      match param with
                        | Some { find_param_functions = Some (f1, _); _ } ->
                            Lwt.catch (fun () -> f1 v)
                              (function _ -> Lwt.fail NotAnUser)
                        | _ -> raise NotAnUser
                   ) >>= fun v ->
                   Lwt.return (AppliedParameterizedGroup (u.user_id, v))

               | _ -> Lwt.fail NotAnUser
     else
       get_basicuser_by_login s >>= fun u ->
       Lwt.return (basic_user u)
  ) 64

let get_user_by_name name =
  print_cache "cache nuser ";
  nusercache#find name



let update_data ~userid ?password ?fullname ?email ?dyn () =
  iusercache#clear ();
  nusercache#clear ();
  update_data_ ~userid ?password ?fullname ?email ?dyn ()



(* Groups-related functions *)

module GroupUsersCache = Ocsigen_cache.Make (struct
                             type key = user
                             type value = user list
                           end)

module UsersGroupCache = Ocsigen_cache.Make (struct
                             type key = user * bool
                             type value = user list
                           end)

let group_of_users_cache = new GroupUsersCache.cache
  (fun user -> groups_of_user_ ~user) 8777
let users_in_group_cache = new UsersGroupCache.cache
  (fun (g, b) -> users_in_group_ ~generic:b ~group:g) 8777

let groups_of_user ~user =
  print_cache "cache groups_of_user ";
  group_of_users_cache#find user

let users_in_group ?(generic=true) ~group =
  print_cache "cache users_in_group ";
  users_in_group_cache#find (group, generic)

let add_to_group ~user ~group =
  group_of_users_cache#remove user;
  users_in_group_cache#clear ();
  add_to_group_ ~user ~group

let add_generic_inclusion ~subset ~superset =
  group_of_users_cache#clear ();
  add_generic_inclusion_ ~subset ~superset

let add_to_group_generic ~user ~group =
  add_generic_inclusion ~subset:user ~superset:group

let remove_from_group ~user ~group =
  group_of_users_cache#remove user;
  users_in_group_cache#clear ();
  remove_from_group_ ~user ~group


(* Deletion *)

let delete_user ~userid =
  (* We clear all the caches, as we should iterate over all the
     parameters if [userid] is a parametrized group *)
  iusercache#clear ();
  nuseridcache#clear ();
  nusercache#clear ();
  group_of_users_cache#clear ();
  users_in_group_cache#clear ();
  delete_user_ ~userid


(* Conversion to string *)

let userid_to_string u =
  get_basicuser_data u
  >>= fun { user_login = r; _ } -> Lwt.return r

let user_to_string ?(expand_param=true) = function
  | BasicUser u | NonParameterizedGroup u -> userid_to_string u
  | AppliedParameterizedGroup (u, v) ->
      userid_to_string u >>= fun s ->
      (if expand_param then
         Lwt.catch
           (fun () ->
              match (Hashtbl.find hash_find_param s).find_param_functions with
                | None -> Lwt.fail Not_found
                | Some (_f1, f2) -> f2 v
           )
           (function _ -> Lwt.return (Int32.to_string v))
       else
         Lwt.return (Int32.to_string v))
      >>= fun v ->
      Lwt.return (Printf.sprintf "%s(%s)" s v)


(*
let user_descr = function
  | BasicUser u ->
      userid_to_string u >>= fun u ->
      Lwt.return (`BasicUser u)

  | NonParameterizedGroup u ->
      userid_to_string u >>= fun u ->
      Lwt.return (`NonParameterizedGroup u)

  | AppliedParameterizedGroup (u, v) ->
      userid_to_string u >>= fun u ->
      Lwt.return (`AppliedParameterizedGroup (u, v))
*)


let user_type = function
  | BasicUser u ->
      (get_basicuser_data u >>= function
         | { user_pwd = Connect_forbidden; _ } -> Lwt.return `Group
         | { user_pwd = Ocsimore_user_plain _; _ }
         | { user_pwd = Ocsimore_user_crypt _; _ }
         | { user_pwd = Ocsimore_user_safe _; _ }
         | { user_pwd = External_Auth; _ } -> Lwt.return `User)
  | NonParameterizedGroup _ | AppliedParameterizedGroup _ -> Lwt.return `Role

(** Users settings *)

let users_settings = (<:table< users_settings (
  id integer NOT NULL,
  basicusercreation boolean NOT NULL,
  registration_mail_from text NOT NULL,
  registration_mail_addr text NOT NULL,
  registration_mail_subject text NOT NULL,
  groups text NOT NULL,
  non_admin_can_create boolean NOT NULL
) >>)

type user_settings = {
  basicusercreation : bool;
  registration_mail_from : string;
  registration_mail_addr : string;
  registration_mail_subject : string;
  groups : string;
  non_admin_can_create : bool
}

let get_users_settings () =
  Lwt_pool.use Ocsi_sql.pool (fun db -> (* TODO: Remove id from the selection *)
    Lwt_Query.view_one db (<:view< u | u in $users_settings$ >>) >>= (fun data ->
      Lwt.return {
        basicusercreation = data#!basicusercreation;
        registration_mail_from = data#!registration_mail_from;
        registration_mail_addr = data#!registration_mail_addr;
        registration_mail_subject = data#!registration_mail_subject;
        groups = data#!groups;
        non_admin_can_create = data#!non_admin_can_create
      }
    )
  )

let set_users_settings data =
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.query db (<:update< u in $users_settings$ := {
      basicusercreation = $bool:data.basicusercreation$;
      registration_mail_from = $string:data.registration_mail_from$;
      registration_mail_addr = $string:data.registration_mail_addr$;
      registration_mail_subject = $string:data.registration_mail_subject$;
      groups = $string:data.groups$;
      non_admin_can_create = $bool:data.non_admin_can_create$
    } | u.id = 1 >>)
  )

let () =
  Lwt_main.run (
    Lwt_pool.use Ocsi_sql.pool (fun db ->
      Lwt_Query.view_opt db
        (<:view< u | u in $users_settings$ >>) >>= (function
          | None ->
            Lwt_Query.query db (<:insert< $users_settings$ := {
              id = 1;
              basicusercreation = false;
              registration_mail_from = "";
              registration_mail_addr = "";
              registration_mail_subject = "";
              groups = "";
              non_admin_can_create = false;
            } >>)
          | _ -> Lwt.return ()
         )
    )
  )

let get_users_login () =
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.view db (<:view< {
      u.id;
      title = nullable u.login;
    } | u in $users$; u.authtype <> "g"; u.authtype <> "h" >>)
  )
