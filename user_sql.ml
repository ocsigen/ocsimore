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

  let userid_s i = Int32.to_string (sql_from_user i)
  let s_userid s = (Opaque.int32_t (Int32.of_string s) : userid)


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
  }
end
open Types


(* Transforms an incoming value of type [pwd], in which
   [Ocsimore_user_crypt] is supposed to contain the unencrypted password,
   into the outgoing value of type [pwd] ([Ocsimore_user_crypt] contains the
   encrypted password) and the database representation of this value,
   for the columns password and authtype respectively *)
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


let populate_groups db id groups =
  let id = sql_from_user id in
  match groups with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun groupid ->
             let groupid = sql_from_user groupid in
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO userrights \
                   VALUES ($id, $groupid)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting user groups: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          groups

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
    serial4 db "users_id_seq" >>= fun id ->
    let id = user_from_sql id in
    populate_groups db id groups >>= fun () ->
    Lwt.return (id, pwd))

let wrap_userdata (id, login, pwd, name, email, dyn, authtype) =
  let password = match authtype, pwd with
    | "p", _ -> External_Auth
    | "c", Some p -> Ocsimore_user_crypt p
    | "l", Some p -> Ocsimore_user_plain p
    | _ -> Connect_forbidden
  in
  Lwt.return ({ user_id = user_from_sql id; user_login = login;
                user_pwd = password; user_fullname = name;
                user_email = email; user_dyn = dyn })


let find_user_by_name_ name =
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                  FROM users WHERE login = $name"
       >>= function
         | [] -> Lwt.fail Not_found
         | r :: _ -> wrap_userdata r)

let find_user_by_id_ id =
  let id = sql_from_user id in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                  FROM users WHERE id = $id"
       >>= function
         | [] -> Lwt.fail Not_found
         | r :: _ -> wrap_userdata r)

let add_to_group_ ~userid ~groupid =
  let userid = sql_from_user userid and groupid = sql_from_user groupid in
  Lwt_pool.use Sql.pool (fun db ->
  Lwt.catch
    (fun () -> PGSQL(db) "INSERT INTO userrights VALUES ($userid, $groupid)")
    (function
(*VVV How to insert only if it does not exists? *)
       | Sql.PGOCaml.PostgreSQL_Error _ -> 
           Ocsigen_messages.warning "Ocsimore: duplicate group insertion";
           Lwt.return ()
       | e -> Lwt.fail e
    ))

let remove_from_group_ ~userid ~groupid =
  let userid = sql_from_user userid and groupid = sql_from_user groupid in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "DELETE FROM userrights
                  WHERE id = $userid AND groupid = $groupid")

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

let get_groups_ ~userid =
  let userid = sql_from_user userid in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT groupid FROM userrights WHERE id = $userid" >>=
       fun r -> Lwt.return (Opaque.int32_t_list r)
    )

