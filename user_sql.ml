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

let (>>=)= Lwt.bind

open Sql.PGOCaml
open Ocsimorelib
open CalendarLib
open Sql

let new_user ~name ~password ~fullname ~email =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  (match password with
     | None -> 
         PGSQL(db) "INSERT INTO users (login, fullname, email)\
                    VALUES ($name, $fullname, $email)"
     | Some pwd -> 
         PGSQL(db) "INSERT INTO users (login, password, fullname, email) \
                    VALUES ($name, $pwd, $fullname, $email)") >>= fun () -> 
  serial4 db "users_id_seq" >>= fun frm_id -> 
  commit db >>=	fun _ -> 
  return frm_id)

let find_user ?db ?id ?name () =
  (match db with
    | None -> Lwt_pool.use Sql.pool
    | Some db -> (fun f -> f db))
    (fun db ->
       (match (name, id) with
          | (Some n, Some i) -> 
              PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE id = $i AND login = $n"
          | (None, Some i) -> 
              PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE id = $i"
          | (Some n, None) -> 
              PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE login = $n"
          | (None, None) -> Lwt.fail (Failure "Neither name nor id specified")) 
       >>= fun res -> 
         (match res with
            | [u] -> return u
            | _ -> Lwt.fail Not_found))

let update_permissions ~name ~perm =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 (Printf.sprintf "[Sql] update_permissions [%s]" perm);
  begin_work db >>= fun () -> 
  find_user ~db ~name () >>= fun (id, _, _, _, _, _) -> 
  PGSQL(db) "UPDATE users SET permissions = $perm WHERE id = $id" >>= fun () ->
  Ocsigen_messages.debug2 "[Sql] update_permissions: finish"; 
  commit db >>= fun () -> 
  return ())

let update_data ~id ~name ~password ~fullname ~email =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] update_data";
  begin_work db >>= fun _ -> 
  find_user ~db ~id ~name () >>= fun (id, _, _, _, _, _) -> 
  (match password with
     | None -> 
         PGSQL(db) "UPDATE users SET fullname = $fullname, email = $email WHERE id = $id"
     | Some pwd -> 
         PGSQL(db) "UPDATE users SET password = $pwd, fullname = $fullname, email = $email WHERE id = $id") 
    >>=	fun () -> 
  Ocsigen_messages.debug2 "[Sql] update_data: finish"; 
  commit db >>= fun _ -> 
  Lwt.return ())

