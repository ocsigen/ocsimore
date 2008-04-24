(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi, Jaap Boender and Vincent Balat
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

type wiki = int32

open Lwt
open Sql.PGOCaml
open Ocsimorelib
open CalendarLib
open Sql

(** Role of user in the wiki *)
type role = Author of int32 | Lurker of string | Unknown;;

(** inserts a new wiki *)
let new_wiki ~title ~descr ~reader ~writer ~acl =
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun _ ->
       PGSQL(db) "INSERT INTO wikis (title, descr, reader, writer, acl) \
                  VALUES ($title, $descr, $reader, $writer, $acl)" >>= fun () ->
       serial4 db "wikis_id_seq" >>= fun wik_id ->
       commit db >>= fun _ ->
       return wik_id)


let populate_readers db wiki_id id readers =
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             let reader = Users.id_of_group reader in
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxreaders \
                             VALUES ($wiki_id, $id, $reader)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox readers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          readers

let populate_writers db wiki_id id writers =
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             let writer = Users.id_of_group writer in
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxwriters \
                             VALUES ($wiki_id, $id, $writer)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox writers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          writers

(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox ~wiki ~author ~comment ~content ?rights () = 
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun () ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (wiki_id, author, comment, content) \
                  VALUES ($wiki, $author, $comment, $content)" >>= fun () ->
       serial4 db "wikiboxes_id_seq" >>= fun wbx_id ->
       (match rights with
         | None -> Lwt.return ()
         | Some (r, w) -> 
             populate_writers db wiki wbx_id w >>= fun () ->
             populate_readers db wiki wbx_id r
       ) >>= fun () ->
       commit db >>= fun () ->
       Lwt.return wbx_id)


(** returns subject, text, author, datetime of a wikibox; 
    None if non-existant *)
let get_wikibox_data ~wiki ~id =
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT comment, author, content, datetime \
                  FROM wikiboxes \
                  WHERE wiki_id = $wiki \
                  AND id = $id \
                  AND version = \
                     (SELECT max(version) \
                      FROM wikiboxes \
                      WHERE wiki_id = $wiki \
                      AND id = $id)"
       >>= function
         | [] -> Lwt.return None
         | [x] -> Lwt.return (Some x)
         | x::_ -> 
             Ocsigen_messages.warning
               "Ocsimore: database error (Wiki_sql.get_wikibox_data)";
             Lwt.return (Some x))

let find_wiki ?id ?title () =
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun _ -> 
       (match (title, id) with
          | (Some t, Some i) -> 
              PGSQL(db) "SELECT * \
                FROM wikis \
                WHERE title = $t AND id = $i"
          | (Some t, None) -> 
              PGSQL(db) "SELECT * \
                FROM wikis \
                WHERE title = $t"
          | (None, Some i) -> 
              PGSQL(db) "SELECT * \
                FROM wikis \
                WHERE id = $i"
          | (None, None) -> fail (Invalid_argument "Wiki_sql.find_wiki")) 
         >>= fun r -> 
       commit db >>= fun () -> 
       (match r with
          | [(id, title, descr, r, w, a)] -> 
              return (id, title, descr, 
                      Users.group_of_id r, 
                      Users.group_of_id w, 
                      a)
          | (id, title, descr, r, w, a)::_ -> 
              Ocsigen_messages.warning "Ocsimore: More than one wiki have the same name or id (ignored)";
              return (id, title, descr,
                      Users.group_of_id r, 
                      Users.group_of_id w, 
                      a)
          | [] -> Lwt.fail Not_found))


let get_writers ~wiki ~id =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT writer FROM wikiboxwriters \
             WHERE id = $id AND wiki_id = $wiki"
    >>= fun r -> 
  commit db >>= fun () -> 
(*VVV we should avoid this map: *)
  Lwt.return (List.map Users.group_of_id r))

let get_readers ~wiki ~id =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT reader FROM wikiboxreaders \
             WHERE id = $id AND wiki_id = $wiki"
    >>= fun r -> 
  commit db >>= fun () -> 
(*VVV we should avoid this map: *)
  Lwt.return (List.map Users.group_of_id r))
(*
let new_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* inserts a new wikipage in an existing wiki; returns [None] if
     [~suffix] is already used in that wiki. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_id =
        (match 
          PGSQL(db) "SELECT id FROM wikipages \
            WHERE wik_id = $wik_id AND suffix = $suffix" 
        with
        | [] ->
            PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
            let txt_id = serial4 db "textdata_id_seq" in
            PGSQL(db) "INSERT INTO wikipages \
              (wik_id, suffix, author, subject, txt_id) \
              VALUES ($wik_id,$suffix,$author,$subject,$txt_id)";
              Some (serial4 db "wikipages_id_seq")
        | _ -> None) in
      commit db;
      wpg_id)
    ()


let add_or_change_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* updates, or inserts, a wikipage. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      (match
        PGSQL(db) "SELECT id, txt_id FROM wikipages \
          WHERE wik_id = $wik_id AND suffix = $suffix" 
      with
      | [(wpg_id,txt_id)] ->
          PGSQL(db) "UPDATE textdata SET txt = $txt WHERE id = $txt_id";
          PGSQL(db) "UPDATE wikipages \
            SET suffix = $suffix, author = $author, \
            subject = $subject \
            WHERE id = $wpg_id"
      | _ ->
          PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
          let txt_id = serial4 db "textdata_id_seq" in
          PGSQL(db) "INSERT INTO wikipages \
            (wik_id, suffix, author, subject, txt_id) \
            VALUES ($wik_id,$suffix,$author,$subject,$txt_id)");
            commit db)
        ()

let wiki_get_data ~wik_id = 
  (* returns title, description, number of wikipages of a wiki. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let (title, description) = 
        (match PGSQL(db) "SELECT title, descr FROM wikis WHERE id = $wik_id"
        with [x] -> x | _ -> assert false) in
      let n_pages = 
        (match PGSQL(db)
            "SELECT COUNT(*) FROM wikipages WHERE wik_id = $wik_id"
        with [Some x] -> x | _ -> assert false) in
      commit db;
      (title, description, int_of_db_size n_pages))
    ()

let wiki_get_pages_list ~wik_id =
  (* returns the list of wikipages *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_l = PGSQL(db) "SELECT subject, suffix, author, datetime \
          FROM wikipages \
          WHERE wik_id = $wik_id \
          ORDER BY subject ASC" in
          commit db;
        wpg_l)
    ()
*)    
