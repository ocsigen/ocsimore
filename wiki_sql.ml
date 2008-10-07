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

open Opaque

type wiki = [`Wiki] int32_t
let wiki_id (i : wiki) = t_int32 i
let wiki_id_s i = Int32.to_string (wiki_id i)
let s_wiki_id s = (Opaque.int32_t (Int32.of_string s) : wiki)

let eliom_wiki = Eliom_parameters.user_type
  (fun s -> (int32_t (Int32.of_string s) : wiki))
  wiki_id_s

open Lwt
open Sql.PGOCaml
open Ocsimore_lib
open CalendarLib
open Sql

(** inserts a new wiki *)
let new_wiki ~title ~descr ~pages ~boxrights ~staticdir () =
  Sql.full_transaction_block
    (fun db ->
       (match staticdir with
         | Some staticdir ->
             PGSQL(db) "INSERT INTO wikis (title, descr, pages, boxrights, staticdir) \
                    VALUES ($title, $descr, $pages, $boxrights, $staticdir)"
         | None ->
             PGSQL(db) "INSERT INTO wikis (title, descr, pages, boxrights) \
                    VALUES ($title, $descr, $pages, $boxrights)")
       >>= fun () ->
       serial4 db "wikis_id_seq")
  >>= fun wiki ->
    (return (int32_t wiki : [`Wiki] int32_t))


(** Update container_id (for now). *)
let update_wiki_ ~wiki ~container_id () =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "UPDATE wikis SET container_id = $container_id \
                  WHERE id = $wiki")



let populate_readers_ db wiki id readers =
  let wiki = t_int32 (wiki : wiki) in
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxreaders \
                             VALUES ($wiki, $id, $reader)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox readers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          readers

let populate_writers_ db wiki id writers =
  let wiki = t_int32 (wiki : wiki) in
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxwriters \
                             VALUES ($wiki, $id, $writer)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox writers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          writers

let populate_rights_adm_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxrightsgivers \
                             VALUES ($wiki, $id, $ra)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox rights givers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let populate_wikiboxes_creators_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxcreators \
                             VALUES ($wiki, $id, $ra)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox creators: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let remove_readers_ db wiki id readers =
  let wiki = t_int32 (wiki : wiki) in
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxreaders \
                             WHERE wiki_id = $wiki \
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

let remove_writers_ db wiki id writers =
  let wiki = t_int32 (wiki : wiki) in
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxwriters \
                             WHERE wiki_id = $wiki \
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

let remove_rights_adm_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxrightsgivers \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND wbadmin = $ra")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox rights givers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let remove_wikiboxes_creators_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxcreators \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND creator = $ra")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox creators: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox ~wiki ?box ~author ~comment ~content ?rights () = 
  let wiki' = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       (match box with
          | None ->
              PGSQL(db) "INSERT INTO wikiboxes \
                    (wiki_id, author, comment, content) \
                  VALUES ($wiki', $author, $comment, $content)"
              >>= fun () ->
              serial4 db "wikiboxes_id_seq"
          | Some box ->
              PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content) \
                  VALUES ($box, $wiki', $author, $comment, $content)"
              >>= fun () ->
              Lwt.return box) >>= fun wbx_id ->
       (match rights with
         | None -> Lwt.return ()
         | Some (r, w, ra, wc) -> 
             populate_writers_ db wiki wbx_id w >>= fun () ->
             populate_readers_ db wiki wbx_id r >>= fun () ->
             populate_rights_adm_ db wiki wbx_id ra >>= fun () ->
             populate_wikiboxes_creators_ db wiki wbx_id wc
       ) >>= fun () ->
      Lwt.return wbx_id)

(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
let update_wikibox_ ~wiki ~wikibox ~author ~comment ~content =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content) \
                  VALUES ($wikibox, $wiki, $author, \
                          $comment, $content)" >>= fun () ->
       serial4 db "wikiboxes_version_seq")


(*
       (match readers with
         | None -> Lwt.return ()
         | Some r -> 
             PGSQL(db) "DELETE FROM wikiboxreaders \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_readers_ db wiki wikibox r) >>= fun () ->
       (match writers with
         | None -> Lwt.return ()
         | Some w -> 
             PGSQL(db) "DELETE FROM wikiboxwriters \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_writers_ db wiki wikibox w) >>= fun () ->
       (match rights_adm with
         | None -> Lwt.return ()
         | Some a -> 
             PGSQL(db) "DELETE FROM wikiboxrightsgivers \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_rights_adm_ db wiki wikibox a) >>= fun () ->
       (match wikiboxes_creators with
         | None -> Lwt.return ()
         | Some a -> 
             PGSQL(db) "DELETE FROM wikiboxcreators \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_wikiboxes_creators_ db wiki wikibox a) >>= fun () ->
*)


(** returns subject, text, author, datetime of a wikibox; 
    None if non-existant *)
let get_wikibox_data_ ?version ~wikibox:(wiki, id) () =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       (match version with
         | None ->
             PGSQL(db) "SELECT comment, author, content, datetime \
                        FROM wikiboxes \
                        WHERE wiki_id = $wiki \
                        AND id = $id \
                        AND version = \
                           (SELECT max(version) \
                            FROM wikiboxes \
                            WHERE wiki_id = $wiki \
                            AND id = $id)"
         | Some version ->
             PGSQL(db) "SELECT comment, author, content, datetime \
                        FROM wikiboxes \
                        WHERE wiki_id = $wiki \
                        AND id = $id \
                        AND version = $version")
       >>= function
         | [] -> Lwt.return None
         | [x] -> Lwt.return (Some x)
         | x::_ -> 
             Ocsigen_messages.warning
               "Ocsimore: database error (Wiki_sql.get_wikibox_data)";
             Lwt.return (Some x))

(** returns subject, text, author, datetime of a wikibox; 
    None if non-existant *)
let get_history ~wiki ~id =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT version, comment, author, datetime \
                  FROM wikiboxes \
                  WHERE wiki_id = $wiki \
                  AND id = $id \
                  ORDER BY version DESC")

(** return the box corresponding to a wikipage *)
let get_box_for_page_ ~wiki ~page =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT id \
                  FROM wikipages \
                  WHERE wiki = $wiki \
                  AND pagename = $page") >>= function
      | [] -> Lwt.fail Not_found
      | id::_ -> Lwt.return id

(** Sets the box corresponding to a wikipage *)
let set_box_for_page_ ~wiki ~id ~page =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db -> 
       PGSQL(db) "DELETE FROM wikipages WHERE wiki=$wiki AND pagename = $page" 
       >>= fun () ->
       PGSQL(db) "INSERT INTO wikipages VALUES ($wiki, $id, $page)"
    )


let find_wiki_ ~id =
  let id = t_int32 (id : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT * \
                  FROM wikis \
                  WHERE id = $id"
       >>= fun r -> 
       PGSQL(db) "SELECT max(id) \
                FROM wikiboxes \
                WHERE wiki_id = $id"
       >>= fun last -> 
         (match last with
            | [] | [None] -> Lwt.return 0l
            | [Some l] -> Lwt.return l
            | _ -> Lwt.fail (Failure "Wiki_sql.find_wiki_"))
       >>= fun last -> 
         (match r with
            | [(_, title, descr, pages, br, ci, stat)] ->
                Lwt.return (title, descr, pages, br, ref last, ci, stat)
            | (_, title, descr, pages, br, ci, stat)::_ -> 
                Ocsigen_messages.warning
                  "Ocsimore: More than one wiki have the same id (ignored)";
                Lwt.return (title, descr, pages, br, ref last, ci, stat)
            | [] -> Lwt.fail Not_found))

let find_wiki_id_by_name ~name =
  Lwt_pool.use Sql.pool 
    (fun db ->
       PGSQL(db) "SELECT * \
                  FROM wikis \
                  WHERE title = $name"
       >>= fun r -> 
       (match r with
          | [(id, title, descr, pages, br, ci, stat)] ->
              Lwt.return (int32_t id : [`Wiki] int32_t)
          | (id, title, descr, pages, br, ci, stat)::_ -> 
              Ocsigen_messages.warning
                "Ocsimore: More than one wiki have the same name (ignored)";
              Lwt.return (int32_t id : [`Wiki] int32_t)
          | [] -> Lwt.fail Not_found))


let get_writers_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT writer FROM wikiboxwriters \
             WHERE id = $id AND wiki_id = $wiki")

let get_readers_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT reader FROM wikiboxreaders \
             WHERE id = $id AND wiki_id = $wiki")

let get_rights_adm_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT wbadmin FROM wikiboxrightsgivers \
             WHERE id = $id AND wiki_id = $wiki")

let get_wikiboxes_creators_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT creator FROM wikiboxcreators \
             WHERE id = $id AND wiki_id = $wiki")

(****)
let populate_readers_ wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_readers_ db wiki_id id readers >>= fun () ->
  commit db)

let populate_writers_ wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_writers_ db wiki_id id writers >>= fun () ->
  commit db)

let populate_rights_adm_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  populate_rights_adm_ db wiki_id id wbadmins >>= fun () ->
  commit db)

let populate_wikiboxes_creators_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  populate_wikiboxes_creators_ db wiki_id id wbadmins >>= fun () ->
  commit db)

let remove_readers_ wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_readers_ db wiki_id id readers >>= fun () ->
  commit db)

let remove_writers_ wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_writers_ db wiki_id id writers >>= fun () ->
  commit db)

let remove_rights_adm_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  remove_rights_adm_ db wiki_id id wbadmins >>= fun () ->
  commit db)

let remove_wikiboxes_creators_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  remove_wikiboxes_creators_ db wiki_id id wbadmins >>= fun () ->
  commit db)

(** returns the css for a page or fails with [Not_found] if it does not exist *)
let get_css_for_page_ ~wiki ~page =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT css FROM css \
                  WHERE wiki = $wiki AND page = $page"
       >>= function
         | [] -> Lwt.return None
         | x::_ -> Lwt.return (Some x))

(** Sets the css for a wikipage *)
let set_css_for_page_ ~wiki ~page content =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db -> 
       PGSQL(db) "DELETE FROM css WHERE wiki = $wiki AND page = $page" 
       >>= fun () ->
       PGSQL(db) "INSERT INTO css VALUES ($wiki, $page, $content)"
    )

(** returns the global css for a wiki or fails with [Not_found] if it does not exist *)
let get_css_for_wiki_ ~wiki =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT css FROM wikicss \
                  WHERE wiki = $wiki"
       >>= function
         | [] -> Lwt.return None
         | x::_ -> Lwt.return (Some x))

(** Sets the global css for a wiki *)
let set_css_for_wiki_ ~wiki content =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db -> 
       PGSQL(db) "DELETE FROM wikicss WHERE wiki = $wiki" 
       >>= fun () ->
       PGSQL(db) "INSERT INTO wikicss VALUES ($wiki, $content)"
    )


(*
let new_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* inserts a new wikipage in an existing wiki; returns [None] if
     [~suffix] is already used in that wiki. *)
  Lwt_preemptive.detach
    (fun () ->
  Sql.full_transaction_block
    (fun db ->
      begin_work db; remove this! use full_transaction_block
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
  Sql.full_transaction_block
    (fun db ->
      begin_work db;remove this! use full_transaction_block
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
  Sql.full_transaction_block
    (fun db ->
      begin_work db;remove this! use full_transaction_block
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
  Sql.full_transaction_block
    (fun db ->
      begin_work db;remove this! use full_transaction_block
      let wpg_l = PGSQL(db) "SELECT subject, suffix, author, datetime \
          FROM wikipages \
          WHERE wik_id = $wik_id \
          ORDER BY subject ASC" in
          commit db;
        wpg_l)
    ()
*)    
