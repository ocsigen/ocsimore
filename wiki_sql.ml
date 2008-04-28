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

type wiki = int32

open Lwt
open Sql.PGOCaml
open Ocsimorelib
open CalendarLib
open Sql

(** Role of user in the wiki (for one box) *)
type role = Admin of int32 | Author of int32 | Lurker of string | Unknown;;
(* Admin can changes the permissions on boxes *)

(** inserts a new wiki *)
let new_wiki ~title ~descr ~reader ~writer ?admin () =
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun _ ->
       (match admin with
         | Some admin ->
             PGSQL(db) "INSERT INTO wikis (title, descr, \
                                           reader, writer, wikiadmin) \
                        VALUES ($title, $descr, $reader, $writer, $admin)"
         | None ->
             PGSQL(db) "INSERT INTO wikis (title, descr, \
                                           reader, writer) \
                        VALUES ($title, $descr, $reader, $writer)")
         >>= fun () ->
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

let populate_wbadmins db wiki_id id admins =
  match admins with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun admin ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxadmins \
                             VALUES ($wiki_id, $id, $admin)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox admins: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          admins

let remove_readers db wiki_id id readers =
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxreaders \
                             WHERE wiki_id = $wiki_id \
                             AND id = $id \
                             AND reader = $reader")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox readers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          readers

let remove_writers db wiki_id id writers =
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxwriters \
                             WHERE wiki_id = $wiki_id \
                             AND id = $id \
                             AND writer = $writer")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox writers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          writers

let remove_wbadmins db wiki_id id admins =
  match admins with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun admin ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxadmins \
                             WHERE wiki_id = $wiki_id \
                             AND id = $id \
                             AND wbadmin = $admin")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox admins: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          admins

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
         | Some (r, w, a) -> 
             populate_writers db wiki wbx_id w >>= fun () ->
             populate_readers db wiki wbx_id r >>= fun () ->
             populate_wbadmins db wiki wbx_id a
       ) >>= fun () ->
       commit db >>= fun () ->
       Lwt.return wbx_id)

(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
let update_wikibox ~wiki ~wikibox ~author ~comment ~content 
    ?readers ?writers ?admins () = 
  Lwt_pool.use Sql.pool (fun db ->
       begin_work db >>= fun () ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content) \
                  VALUES ($wikibox, $wiki, $author, \
                          $comment, $content)" >>= fun () ->
       serial4 db "wikiboxes_version_seq" >>= fun version ->
       (match readers with
         | None -> Lwt.return ()
         | Some r -> 
             PGSQL(db) "DELETE FROM wikiboxreaders \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_readers db wiki wikibox r) >>= fun () ->
       (match writers with
         | None -> Lwt.return ()
         | Some w -> 
             PGSQL(db) "DELETE FROM wikiboxwriters \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_writers db wiki wikibox w) >>= fun () ->
       (match admins with
         | None -> Lwt.return ()
         | Some a -> 
             PGSQL(db) "DELETE FROM wikiboxadmins \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_wbadmins db wiki wikibox a) >>= fun () ->
       commit db >>= fun () ->
       Lwt.return version)


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
              return (id, title, descr, r, w, a)
          | (id, title, descr, r, w, a)::_ -> 
              Ocsigen_messages.warning "Ocsimore: More than one wiki have the same name or id (ignored)";
              return (id, title, descr, r, w, a)
          | [] -> Lwt.fail Not_found))


let get_writers ~wiki ~id =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT writer FROM wikiboxwriters \
             WHERE id = $id AND wiki_id = $wiki"
    >>= fun r -> 
  commit db >>= fun () -> 
  Lwt.return r)

let get_readers ~wiki ~id =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT reader FROM wikiboxreaders \
             WHERE id = $id AND wiki_id = $wiki"
    >>= fun r -> 
  commit db >>= fun () -> 
  Lwt.return r)

let get_admins ~wiki ~id =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT wbadmin FROM wikiboxadmins \
             WHERE id = $id AND wiki_id = $wiki"
    >>= fun r -> 
  commit db >>= fun () -> 
  Lwt.return r)

(****)
let populate_readers wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_readers db wiki_id id readers >>= fun () ->
  commit db)

let populate_writers wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_writers db wiki_id id writers >>= fun () ->
  commit db)

let populate_wbadmins wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  populate_wbadmins db wiki_id id wbadmins >>= fun () ->
  commit db)

let remove_readers wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_readers db wiki_id id readers >>= fun () ->
  commit db)

let remove_writers wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_writers db wiki_id id writers >>= fun () ->
  commit db)

let remove_wbadmins wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  remove_wbadmins db wiki_id id wbadmins >>= fun () ->
  commit db)

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
