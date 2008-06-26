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
  | Ocsimore_user of string
  | Pam

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
  let password, authtype =
    match password with
      | Connect_forbidden -> None, "l"
      | Ocsimore_user p -> (Some p), "l"
      | Pam -> None, "p"
  in
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
    Lwt.return id)

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
       if authtype = "p"
       then Pam
       else match pwd with
         | None -> Connect_forbidden
         | Some p -> Ocsimore_user p
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

let update_data_ ~userid ~name ~password ~fullname ~email ?groups ?dyn () =
  let password, authtype =
    match password with
      | Connect_forbidden -> None, "l"
      | Ocsimore_user p -> (Some p), "l"
      | Pam -> None, "p"
  in
  Sql.full_transaction_block
    (fun db ->
       (match password, email with
         | None, None -> 
             PGSQL(db) "UPDATE users SET fullname = $fullname, email = DEFAULT, password = DEFAULT, authtype = $authtype WHERE id = $userid"
         | None, Some email -> 
             PGSQL(db) "UPDATE users SET fullname = $fullname, password = DEFAULT, email = $email, authtype = $authtype WHERE id = $userid"
               (*VVV is it ok when pwd/email becomes NULL? *)
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
                PGSQL(db) "DELETE FROM userrights WHERE id=$userid" >>= fun () ->
                  populate_groups db userid groups;
                | None -> Lwt.return ()))

let get_groups_ ~userid =
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT groupid FROM userrights WHERE id = $userid"
    )

