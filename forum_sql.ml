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


  (* Lots of queries here take a ~frm_id parameter, even if other ones
     should be enough to determinate a primary key.  This has been
     done because of the need to match every query request against a
     forum's ACL.  TO BE DONE: A LAYER FOR ACCESS CONTROL *)

(**
@author Jaap Boender
@author Vincent Balat
*)

type forum = int32

open Lwt
open Sql.PGOCaml
open Ocsimorelib
open CalendarLib
open Sql

(** Role of user in the forum *)
type role = Moderator | Author of int32 | Lurker of string | Unknown;;

type message_info =
    int32 * string * string * Calendar.t * bool * int32 option * string option;;
(* type 'a collection = List of 'a list | Forest of 'a tree list;; *)

let get_id x = x
let of_id x = x

let new_forum
    ~title ~descr ~moderated ~arborescent ~reader ~writer
    ~moderator =
  (* inserts a new forum *)
  Ocsigen_messages.debug2 "[Sql] new_forum";
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) 
    "INSERT INTO forums (title, descr, moderated,
     arborescent, reader, writer, moderator) \
     VALUES ($title, $descr, $moderated, $arborescent, $reader, $writer,
     $moderator)" >>= fun () -> 
  serial4 db "forums_id_seq" >>= fun frm_id -> 
  commit db >>=        fun _ -> 
  Ocsigen_messages.debug2 "[Sql] new_forum: finish"; 
  return frm_id)

let new_thread_and_message ~frm_id ~author_id ~subject ~txt = 
  (* inserts a message starting a new thread; both thread and message
     will be hidden if forum is moderated *)
  Ocsigen_messages.debug2 "[Sql] new_thread_and_message";
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun hidden -> 
  PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author_id) \
             VALUES ($frm_id, $subject, $hidden, $author_id)" >>= fun () -> 
  serial4 db "threads_id_seq" >>= fun thr_id -> 
  PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>= fun () ->
  serial4 db "textdata_id_seq" >>= fun txt_id -> 
  PGSQL(db) "SELECT MAX(tree_max) FROM messages \
             WHERE thr_id = $thr_id" >>= fun z -> 
  (match z with
     | [x] -> (match x with
                 | None -> return 0l
                 | Some y -> return y)
     | _ -> return 0l) >>= fun db_max -> 
  PGSQL(db) "INSERT INTO messages \
               (author_id, thr_id, txt_id, hidden, tree_min, tree_max) \
             VALUES ($author_id, $thr_id, $txt_id, $hidden, \
                     $db_max + 1, $db_max + 2)" >>= fun () -> 
  serial4 db "messages_id_seq" >>= fun msg_id -> 
  commit db >>=        fun _ -> 
  Ocsigen_messages.debug2 "[Sql] new_thread_and_message: finish";
  return (thr_id, msg_id))
    
let new_thread_and_article ~frm_id ~author_id ~subject ~txt =
  Ocsigen_messages.debug2 "[Sql] new_thread_and_article";
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>=        fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun hidden -> 
  PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>= fun () -> 
  serial4 db "textdata_id_seq" >>= fun txt_id -> 
  PGSQL(db) "INSERT INTO threads \
               (frm_id, subject, hidden, author_id, article_id) \
             VALUES ($frm_id, $subject, $hidden, $author_id, $txt_id)" 
    >>= fun () -> 
  serial4 db "threads_id_seq" >>= fun thr_id -> 
  commit db >>=        fun _ -> 
  Ocsigen_messages.debug2
    "[Sql] new_thread_and_article: finish"; 
  return (thr_id, txt_id))

let new_message ~thr_id ?parent_id ~author_id ~txt ~sticky () = 
  (* inserts a message in an existing thread; message will be hidden
     if forum is moderated *)
  Ocsigen_messages.debug2 "[Sql] new_message"; 
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT moderated FROM forums,threads \
             WHERE threads.id = $thr_id \
             AND threads.frm_id = forums.id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun hidden -> 
  PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>= fun () -> 
  serial4 db "textdata_id_seq" >>= fun txt_id -> 
  PGSQL(db) "SELECT MAX(tree_max) FROM messages \
             WHERE thr_id = $thr_id" >>= fun z -> 
  (match z with
     | [x] -> (match x with
                 | None -> return 0l
                 | Some y -> return y)
     | _ -> return 0l) >>= fun db_max -> 
    (match parent_id with
       | None -> 
           PGSQL(db) "INSERT INTO messages (author_id, thr_id, txt_id, hidden, \
                        sticky, tree_min, tree_max) \
                      VALUES ($author_id, $thr_id, $txt_id, $hidden, $sticky, \
                                $db_max + 1, $db_max + 2)" >>= fun () -> 
           serial4 db "messages_id_seq"
       | Some pid -> 
           PGSQL(db) "SELECT tree_min, tree_max \
                      FROM messages WHERE id = $pid" >>= fun y -> 
           (match y with 
              | [x] -> return x
              | _ -> fail Not_found) >>= fun (db_min, db_max) -> 
           PGSQL(db) "UPDATE messages SET tree_min = tree_min + 2, \
                      tree_max = tree_max + 2 WHERE tree_min > $db_max" 
             >>= fun () -> 
           PGSQL(db) "UPDATE messages SET tree_max = tree_max + 2 \
                      WHERE $db_min BETWEEN tree_min AND tree_max" 
             >>= fun () -> 
           PGSQL(db) "INSERT INTO messages (author_id, thr_id, txt_id, hidden, \
                      sticky, tree_min, tree_max) \
                      VALUES ($author_id, $thr_id, $txt_id, $hidden, 
                              $sticky, $db_max, $db_max + 1)" >>= fun () -> 
           serial4 db "messages_id_seq") >>= fun msg_id ->
           commit db >>= fun _ -> 
           Ocsigen_messages.debug2 "[Sql] new_message: finish"; 
           return msg_id)

let forum_toggle_moderated ~frm_id =
  (* toggle moderation status of a forum *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] forum_toggle_moderated";
  PGSQL(db) "UPDATE forums SET moderated = NOT moderated WHERE id = $frm_id")
    
let thread_toggle_hidden ~frm_id ~thr_id =
  (* hides/shows a thread *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] thread_toggle_hidden";
  PGSQL(db) "UPDATE threads SET hidden = NOT hidden \
              WHERE id = $thr_id AND frm_id = $frm_id")

let message_toggle_hidden ~frm_id ~msg_id =
  (* hides/shows a message *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_toggle_hidden";
  PGSQL(db) "UPDATE messages \
             SET hidden = NOT messages.hidden \
             FROM threads \
             WHERE messages.id = $msg_id \
             AND messages.thr_id = threads.id \
             AND threads.frm_id = $frm_id")

let message_toggle_sticky ~frm_id ~msg_id =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_toggle_sticky";
  PGSQL(db) "UPDATE messages SET sticky = NOT messages.sticky \
             FROM threads WHERE messages.id = $msg_id \
             AND messages.thr_id = threads.id \
             AND threads.frm_id = $frm_id")

let find_forum ?id ?title () =
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  (match (title, id) with
     | (Some t, Some i) -> 
         PGSQL(db) "SELECT * \
                FROM forums \
                WHERE title = $t AND id = $i"
(* old version was:
         PGSQL(db) "SELECT forums.id, title, descr, r.login, w.login, m.login \
                FROM forums, users AS r, users AS w, users AS m \
                WHERE r.id = reader AND w.id = writer AND m.id = moderator \
                AND title = $t AND forums.id = $i"
*)
     | (Some t, None) -> 
         PGSQL(db) "SELECT * \
                FROM forums \
                WHERE title = $t"
(* old version was:
         PGSQL(db) "SELECT forums.id, title, descr, r.login, w.login, m.login \
                FROM forums, users AS r, users AS w, users AS m \
                WHERE r.id = reader AND w.id = writer AND m.id = moderator \
                AND title = $t"
*)
     | (None, Some i) -> 
         PGSQL(db) "SELECT * \
                FROM forums \
                WHERE id = $i"
(* old version was:
         PGSQL(db) "SELECT forums.id, title, descr, r.login, w.login, m.login \
                FROM forums, users AS r, users AS w, users AS m \
                WHERE r.id = reader AND w.id = writer AND m.id = moderator \
                AND forums.id = $i"
*)
     | (None, None) -> fail (Invalid_argument "Forum_sql.find_forum")) 
    >>= fun r -> 
  commit db >>= fun _ -> 
  (match r with
     | [(id, title, descr, mo, a, r, w, m)] -> 
         return (id, title, descr, mo, a, r, w, m)
     | (id, title, descr, mo, a, r, w, m)::_ -> 
         Ocsigen_messages.warning "Ocsimore: More than one forum have the same name or id (ignored)";
         return (id, title, descr, mo, a, r, w, m)
     | _ -> fail Not_found))

let get_forums_list () =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] get_forums_list";
  begin_work db >>= fun _ ->
  PGSQL(db) 
    "SELECT id, title, descr, moderated, arborescent FROM forums" 
    >>= fun r -> 
  commit db >>= fun _ -> 
  Ocsigen_messages.debug2 "[Sql] get_forums_list: finish"; 
  return r)

let forum_get_data ~frm_id ~role =
  (* returns id, title, description, mod status, number of shown/hidden
     threads and messages of a forum.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] forum_get_data";
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT id, title, descr, moderated FROM forums \
             WHERE id = $frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) 
    >>= fun (id, title, description, moderated) -> 
  PGSQL(db) "SELECT COUNT(*) FROM threads \
             WHERE frm_id = $frm_id AND (NOT hidden)" >>= fun y -> 
  (match y with [Some x] -> return x | _ -> assert false) 
    >>= fun n_shown_thr -> 
  PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
             WHERE threads.frm_id = $frm_id \
             AND messages.thr_id = threads.id \
             AND NOT (messages.hidden OR threads.hidden)" >>= fun y -> 
  (match y with [Some x] -> return x | _ -> assert false)
    >>= fun n_shown_msg -> 
  (match role with        
     | Moderator -> (* counts all hidden stuff *)
         PGSQL(db) "SELECT COUNT(*) FROM threads \
                    WHERE frm_id = $frm_id AND hidden" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> assert false)
     | Author aid ->
         PGSQL(db) "SELECT COUNT(*) FROM threads \
                    WHERE frm_id = $frm_id AND hidden \
                    AND author_id = $aid" >>= fun y -> 
           (match y with [Some x] -> return x | _ -> assert false)
     | Unknown -> return 0L) >>= fun n_hidden_thr -> 
  (match role with
     | Moderator -> 
         PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                    WHERE threads.frm_id = $frm_id \
                    AND messages.thr_id = threads.id \
                    AND (messages.hidden OR threads.hidden)" >>= fun y -> 
           (match y with [Some x] -> return x | _ -> assert false)
     | Author aid ->
         PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                     WHERE threads.frm_id = $frm_id \
                    AND messages.thr_id = threads.id \
                    AND (messages.hidden OR threads.hidden) \
                    AND messages.author_id = $aid" >>= fun y -> 
           (match y with [Some x] -> return x | _ -> assert false)
     | Unknown -> return 0L) >>= fun n_hidden_msg -> 
  commit db >>=        fun () -> 
  Ocsigen_messages.debug2 "[Sql] forum_get_data: finish"; 
  Lwt.return (id, title, description, moderated,
              n_shown_thr, n_hidden_thr, n_shown_msg, n_hidden_msg))

let thread_get_nr_messages ~thr_id ~role =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages";
  begin_work db >>= fun _ -> 
  (match role with
     | Moderator -> (* all messages *)
         PGSQL(db) "SELECT COUNT(*) FROM messages \
                    WHERE messages.thr_id = $thr_id" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> fail (Failure "thread_get_nr_messages"))
     | Author aid -> 
         (* all non-hidden messages AND hidden messages posted by a *)
         PGSQL(db) "SELECT COUNT(*) FROM messages \
                    WHERE messages.thr_id = $thr_id \
                    AND messages.hidden = false OR messages.author_id = $aid"
         >>= fun y -> 
         (match y with
            | [Some x] -> return x
            | _ -> fail (Failure "thread_get_nr_messages"))
     | Unknown -> (* all non-hidden messages *)
         PGSQL(db) "SELECT COUNT(*) FROM messages \
                    WHERE messages.thr_id = $thr_id \
                    AND messages.hidden = false" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> fail (Failure "thread_get_nr_messages"))) >>= fun n_msg ->
    commit db >>= fun _ -> 
    Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages: finish"; 
    Lwt.return n_msg)

let thread_get_data (* ~frm_id *) ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  Lwt_pool.use Sql.pool (fun db ->
  begin_work db >>= fun _ -> 
  PGSQL(db) 
    "SELECT t.id, subject, fullname, t.datetime, t.hidden, COALESCE (txt, '') \
     FROM users AS u, threads AS t LEFT OUTER JOIN textdata ON \
        (t.article_id = textdata.id) \
     WHERE t.id = $thr_id AND u.id = t.author_id" (* AND frm_id = $frm_id *) 
    >>=        fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) 
      >>= fun (id, subject, author_id, datetime, hidden, article) ->
  PGSQL(db) "SELECT COUNT(*) FROM messages \
             WHERE thr_id = $thr_id AND (NOT hidden)" >>= fun y -> 
  (match y with
     | [Some x] -> return x
     | _ -> assert false) >>= fun n_shown_msg -> 
  (match role with
     | Moderator -> (* counts all hidden messages *)
         PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                    WHERE messages.thr_id = $thr_id \
                    AND threads.id = $thr_id \
                    AND (messages.hidden OR threads.hidden)" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> assert false)
     | Author aid -> (* counts only hidden messages posted by her *)
         PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                    WHERE messages.thr_id = $thr_id \
                    AND threads.id = $thr_id \
                    AND (messages.hidden OR threads.hidden) \
                    AND messages.author_id = $aid" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> assert false)
     | Unknown -> (* nothing to be counted *) return 0L)
        >>= fun n_hidden_msg -> 
   commit db >>= fun _ -> 
   Ocsigen_messages.debug2 "[Sql] thread_get_data: finish"; 
   Lwt.return
     (id, subject, author_id, article, datetime, 
      hidden, n_shown_msg, n_hidden_msg))

let message_get_data ~frm_id ~msg_id =
  (* returns id, text, author, datetime, hidden status of a message *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_get_data";
  PGSQL(db) "SELECT messages.id, textdata.txt, fullname, \
             messages.datetime, messages.hidden \
             FROM messages, textdata, threads, users \
             WHERE messages.id = $msg_id \
             AND messages.txt_id = textdata.id \
             AND messages.thr_id = threads.id \
             AND threads.frm_id = $frm_id \
             AND users.id = messages.author_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found)
    >>= fun (id, text, author_id, datetime, hidden) ->
  Ocsigen_messages.debug2 "[Sql] message_get_data: finish";
  Lwt.return (id, text, author_id, datetime, hidden))
      
let thread_get_neighbours ~frm_id ~thr_id ~role =
  (* returns None|Some id of prev & next thread in the same forum. *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] thread_get_neighbours";
  begin_work db >>= fun () -> 
  PGSQL(db) "SELECT datetime FROM threads \
             WHERE id = $thr_id AND frm_id = $frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun datetime -> 
  (match role with
     | Moderator -> (* all kinds of threads *)
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                ORDER BY datetime DESC LIMIT 1"
     | Author aid -> (* only shown threads, or hidden ones posted by her *)
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1"
     | Unknown -> (* only shown threads *)
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1") >>= fun y -> 
  let prev = (match y with [x] -> Some x | _ -> None) in
  (match role with
     | Moderator ->
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                 ORDER BY datetime ASC LIMIT 1"
     | Author aid ->
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                 ORDER BY datetime ASC LIMIT 1"
     | Unknown ->
         PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime ASC LIMIT 1") >>= fun y -> 
  Lwt.return (match y with [x] -> Some x | _ -> None) >>= fun next -> 
  commit db >>=        fun _ -> 
  Ocsigen_messages.debug2 "[Sql] thread_get_neighbours: finish"; 
  Lwt.return (prev, next))

let message_get_neighbours ~frm_id ~msg_id ~role =
  (* returns None|Some id of prev & next message in the same
     thread. *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_get_neighbours";
  begin_work db >>= fun _ -> 
  PGSQL(db) "SELECT messages.thr_id, messages.datetime \
                FROM messages, threads \
                WHERE messages.id = $msg_id \
                AND messages.thr_id = threads.id \
                AND threads.frm_id = $frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found)
    >>= fun (thr_id, datetime) -> 
  (match role with
     | Moderator -> (* all kinds of threads *)
         PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                ORDER BY datetime DESC LIMIT 1"
     | Author aid -> (* only shown messages, or hidden ones posted by her *)
         PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1"
     | Unknown -> (* only shown messages *)
         PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1") >>= fun y -> 
    let prev = (match y with [x] -> Some x | _ -> None) in
    (match role with
       | Moderator ->
           PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                 ORDER BY datetime ASC LIMIT 1"
       | Author aid ->
           PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                 ORDER BY datetime ASC LIMIT 1"
       | Unknown ->
           PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime ASC LIMIT 1") >>= fun y ->
      let next = (match y with [x] -> Some x | _ -> None) in
      commit db >>= fun () ->
      Ocsigen_messages.debug2 "[Sql] message_get_neighbours: finish"; 
      Lwt.return (prev, next))

let forum_get_threads_list ~frm_id ?offset ?limit ~role () =
  (* returns the threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] forum_get_threads_list";
  let db_offset = match offset with
    | None -> 0L
    | Some x -> x in
    match limit with
      | None -> 
          (match role with
             | Moderator -> 
                 PGSQL(db)
                   "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id AND author_id = users.id \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset"
             | Author aid ->
                 PGSQL(db)
                   "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id \
                        AND (author_id = $aid OR NOT hidden) \
                        AND users.id = author_id \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset"
             | Unknown -> 
                 PGSQL(db)
                   "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                        AND NOT hidden \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset")
      | Some x -> 
          let db_limit = x in
            (match role with
               | Moderator -> 
                   PGSQL(db)
                     "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                        ORDER BY datetime DESC \
                        LIMIT $db_limit OFFSET $db_offset"
               | Author aid -> 
                   PGSQL(db)
                     "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                         AND (author_id = $aid OR NOT hidden) \
                        ORDER BY datetime DESC \
                        LIMIT $db_limit OFFSET $db_offset"
               | Unknown -> 
                   PGSQL(db)
                     "SELECT threads.id, subject, fullname, datetime, hidden \
                        FROM threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                        AND NOT hidden \
                        ORDER BY datetime DESC \
                        LIMIT $db_limit OFFSET $db_offset"))

let rec forest_of (get_coords: 'a -> 'b) (l: 'a list): 'a tree list =
  let rec get_children_of (min, max) l2 =         
    begin
      match l2 with
        | [] -> ([], [])
        | x::xs -> let (xmin, xmax) = get_coords x in
            if (xmin > min) && (xmax < max) then
              let (ch, rest) = get_children_of (xmin, xmax) xs in
                ([Node (x, ch)], rest)
            else
              ([], l2)
    end in
    begin
      match l with
        | [] -> []
        | x::xs -> let (ch, rest) = get_children_of (get_coords x) xs in
            (Node (x, ch))::(forest_of get_coords rest)
    end;;

let rec cut f id =
  function
    |        []  -> []
    |        h::t -> if (f h) = id then [h] else h::(cut f id t);;

let thread_get_messages_with_text ~thr_id ?offset ?limit ~role ?bottom () =
  Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text";
  Lwt_pool.use Sql.pool (fun db ->
  let db_offset = match offset with
    | None -> 0L
    | Some x -> x in
    match limit with
      | None -> 
          begin_work db >>= fun _ -> 
          (match role with 
             | Moderator ->
                PGSQL(db)
                  "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                FROM messages, textdata, users \
                WHERE txt_id = textdata.id AND thr_id = $thr_id \
                AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset" 
             | Author aid ->
                PGSQL(db)
                  "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                FROM messages, textdata, users \
                WHERE (author_id = $aid OR NOT hidden) \
                AND txt_id = textdata.id AND \
                thr_id = $thr_id AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset" 
             | Unknown ->
                PGSQL(db)
                  "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                FROM messages, textdata, users \
                WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
                AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset") >>=        fun msg_l -> 
        commit db >>= fun _ -> 
        let final_msg_l = match bottom with
          | None -> msg_l
          | Some btm -> cut (fun (id,_,_,_,_,_) -> id) btm msg_l 
        in
        Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text: finish";
        Lwt.return final_msg_l
        | Some x -> 
            let db_limit = x in
            begin_work db >>= fun _ -> 
            (match role with 
               | Moderator ->
                   PGSQL(db)
                     "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                       FROM messages, textdata, users \
                      WHERE txt_id = textdata.id AND thr_id = $thr_id \
                      AND users.id = author_id \
                      ORDER BY sticky DESC, datetime \
                      LIMIT $db_limit OFFSET $db_offset" 
        | Author aid ->
            PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                FROM messages, textdata, users \
                WHERE (author_id = $aid OR NOT hidden) \
                AND txt_id = textdata.id AND \
                thr_id = $thr_id AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                LIMIT $db_limit OFFSET $db_offset" 
        | Unknown ->
            PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
                FROM messages, textdata, users \
                WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
                        AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                LIMIT $db_limit OFFSET $db_offset") >>= fun msg_l -> 
            commit db >>= fun _ -> 
            let final_msg_l = match bottom with
              | None -> msg_l
              | Some btm -> cut (fun (id,_,_,_,_,_)->id) btm msg_l 
            in
            Ocsigen_messages.debug2
              "[Sql] thread_get_messages_with_text: finish";
            Lwt.return final_msg_l)

let thread_get_messages_with_text_forest
    ~thr_id ?offset ?limit ?top ?bottom ~role () =
  Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text_forest";
  Lwt_pool.use Sql.pool (fun db ->
  let db_offset = match offset with
    | None -> 0L
    | Some x -> x 
  in
  match limit with
    | None -> begin_work db >>=        fun _ -> 
        (match top with
           | None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
                        FROM messages WHERE thr_id = $thr_id" >>= fun z -> 
                      Lwt.return
                        (match z with
                           | [(x,y)] -> 
                               (match x, y with
                                  | Some x', Some y' -> (x',y')
                                  | _-> (Int32.zero,Int32.zero))
                           | _ -> (Int32.zero,Int32.zero)))
                | Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
                        FROM messages WHERE thr_id = $thr_id AND id = $t") 
                    >>=        fun y -> 
                      (match y        with
                         | [x] -> return x
                         | _ -> fail Not_found)) >>= fun (db_min, db_max) -> 
          (match role with 
             | Moderator ->
                 PGSQL(db)
                   "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min, tree_max \
                        FROM messages, textdata, users \
                        WHERE txt_id = textdata.id \
                        AND (tree_min BETWEEN $db_min AND $db_max) \
                        AND thr_id = $thr_id AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        OFFSET $db_offset" 
             | Author aid ->
                 PGSQL(db) 
                   "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                    tree_min, tree_max \
                    FROM messages, textdata, users \
                    WHERE (author_id = $aid OR NOT hidden) \
                    AND txt_id = textdata.id AND \
                    (tree_min BETWEEN $db_min AND $db_max) \
                    AND thr_id = $thr_id \
                    AND users.id = author_id \
                    ORDER BY sticky DESC, tree_min \
                    OFFSET $db_offset" 
             | Unknown ->
                 PGSQL(db)
                   "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                    tree_min, tree_max \
                    FROM messages, textdata, users \
                    WHERE NOT hidden AND txt_id = textdata.id AND \
                        (tree_min BETWEEN $db_min AND $db_max)
                    AND thr_id = $thr_id \
                    AND users.id = author_id \
                    ORDER BY sticky DESC, tree_min \
                    OFFSET $db_offset") >>= fun msg_l -> 
            commit db >>= fun _ -> 
            let final_msg_l = match bottom with
              | None -> msg_l
              | Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l 
            in
            Ocsigen_messages.debug2
              "[Sql] thread_get_messages_with_text_forest: finish";
            return (forest_of
                      (fun (_,_,_,_,_,_,x,y)-> (x, y))
                      final_msg_l)
            | Some x -> 
                let db_limit = x in
                begin_work db >>= fun _ -> 
                (match top with
                   | None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
                        FROM messages WHERE thr_id = $thr_id" >>= fun z -> 
                        Lwt.return
                          (match z with
                             | [(x,y)] -> 
                                 (match x, y with 
                                    | Some x', Some y' -> (x',y')
                                    | _-> (Int32.zero, Int32.zero))
                             | _ -> (Int32.zero,Int32.zero)))
                   | Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
                        FROM messages WHERE thr_id = $thr_id AND id = $t") 
                       >>= fun y -> 
                      (match y with
                         | [x] -> Lwt.return x
                         | _ -> fail Not_found)) >>= fun (db_min, db_max) -> 
                (match role with 
                   | Moderator ->
                       PGSQL(db)
                         "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM messages, textdata, users \
                        WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
                        AND thr_id = $thr_id AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset" 
                   | Author aid ->
                       PGSQL(db) 
                         "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM messages, textdata, users \
                        WHERE (author_id = $aid OR NOT hidden) AND txt_id = textdata.id AND \
                                (tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
                                AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset" 
                   | Unknown ->
                       PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM messages, textdata, users \
                        WHERE NOT hidden AND txt_id = textdata.id AND \
                                (tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
                                AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset") >>=        fun msg_l -> 
                commit db >>= fun _ -> 
                let final_msg_l = match bottom with
                  | None -> msg_l
                  | Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l 
                in
                Ocsigen_messages.debug2
                  "[Sql] thread_get_messages_with_text_forest: finish";
                Lwt.return (forest_of
                              (fun (_,_,_,_,_,_,x,y)->(x, y))
                              final_msg_l))

let get_latest_messages ~frm_ids ~limit () =
  Lwt_pool.use Sql.pool (fun db ->
        PGSQL(db) "SELECT messages.id,txt,fullname \
        FROM messages, textdata, users \
        WHERE messages.txt_id = textdata.id AND \
        thr_id IN (SELECT id FROM threads WHERE frm_id IN $@frm_ids) AND
        NOT messages.hidden AND users.id = author_id \
        ORDER BY datetime DESC LIMIT $limit" >>= fun result -> 
  Ocsigen_messages.debug2 "[Sql] get_latest_messages: finish"; 
  Lwt.return result)



