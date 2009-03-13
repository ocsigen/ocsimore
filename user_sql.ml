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

type userid = int32

type pwd =
  | Connect_forbidden
  | Ocsimore_user_plain of string
  | Ocsimore_user_crypt of string
  | External_Auth

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
  match groups with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun groupid ->
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
    populate_groups db id groups >>= fun () ->
    Lwt.return (id, pwd))

let find_user_ ?db ?id ?name () =
  (match db with
    | None -> Lwt_pool.use Sql.pool
    | Some db -> (fun f -> f db))
    (fun db ->
       (match (name, id) with
          | (Some n, Some i) -> 
              PGSQL(db)
                "SELECT id, login, password, fullname, email, dyn, authtype FROM users \
                 WHERE id = $i AND login = $n"
          | (None, Some i) -> 
              PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                         FROM users WHERE id = $i"
          | (Some n, None) -> 
              PGSQL(db) "SELECT id, login, password, fullname, email, dyn, authtype \
                         FROM users WHERE login = $n"
          | (None, None) -> 
              Lwt.fail (Failure
                          "User_sql.find_user: Neither name nor id specified")) 
       >>= fun res -> 
     (match res with
        | [u] -> Lwt.return u
        | u::_ -> 
            Ocsigen_messages.warning
              "Ocsimore: Two users have the same name or id"; 
            Lwt.return u
        | _ -> Lwt.fail Not_found) 
     >>= fun (id, login, pwd, name, email, dyn, authtype) ->
     PGSQL(db) "SELECT groupid FROM userrights WHERE id = $id" >>= fun perm ->
     let password =
       match authtype, pwd with
         | "p", _ -> External_Auth
         | "c", Some p -> Ocsimore_user_crypt p
         | "l", Some p -> Ocsimore_user_plain p
         | _ -> Connect_forbidden
     in
     Lwt.return ((id, login, password, name, email, dyn), perm)
    )

let add_to_group_ ~userid ~groupid =
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
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db)
    "DELETE FROM userrights WHERE id = $userid AND groupid = $groupid")

let delete_user_ ~userid =
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
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT groupid FROM userrights WHERE id = $userid"
    )

