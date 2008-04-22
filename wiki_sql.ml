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

(** inserts a new wiki *)
let new_wiki ~title ~descr ~reader ~writer ~acl =
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun _ ->
       PGSQL(db) "INSERT INTO wikis (title, descr, reader, writer, acl) \
                  VALUES ($title, $descr, $reader, $writer, $acl)" >>= fun () ->
       serial4 db "wikis_id_seq" >>= fun wik_id ->
       commit db >>= fun _ ->
       return wik_id)

(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox ~wiki ~author ~comment ~content = 
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun () ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (wiki_id, author, comment, content) \
                  VALUES ($wiki, $author, $comment, $content)" >>= fun () ->
       serial4 db "wikiboxes_id_seq" >>= fun wbx_id ->
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
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       begin_work db >>= fun _ -> 
       (match (title, id) with
          | (Some t, Some i) -> 
              PGSQL(db) "SELECT wikis.id, title, descr, r.login, w.login, acl \
                         FROM wikis, users AS r, users AS w \
                         WHERE r.id = reader AND w.id = writer \
                         AND title = $t AND wikis.id = $i"
          | (Some t, None) -> 
              PGSQL(db) "SELECT wikis.id, title, descr, r.login, w.login, acl \
		FROM wikis, users AS r, users AS w \
		WHERE r.id = reader AND w.id = writer \
		AND title = $t"
          | (None, Some i) -> 
              PGSQL(db) "SELECT wikis.id, title, descr, r.login, w.login, acl \
		FROM wikis, users AS r, users AS w \
		WHERE r.id = reader AND w.id = writer \
		AND wikis.id = $i"
          | (None, None) -> fail (Invalid_argument "Wiki_sql.find_wiki")) 
         >>= fun r -> 
       commit db >>= fun () -> 
       (match r with
          | [x] -> Lwt.return x
          | x::_ -> 
              Ocsigen_messages.warning
                "Ocsimore: two wikis have the same name"; 
              Lwt.return x
          | [] -> Lwt.fail Not_found))

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
