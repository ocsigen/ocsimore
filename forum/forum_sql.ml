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
   @author Boris Yakobowski
*)

open Sql.PGOCaml
open Sql

let (>>=) = Lwt.bind

type forum = int32

let get_id x = x
let of_id x = x
let forum_id_s = Int32.to_string 

let new_forum ~title ~descr ?(arborescent = true) () =
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) 
         "INSERT INTO forums (title, descr, arborescent) \
          VALUES ($title, $descr, $arborescent)" >>= fun () -> 
       serial4 db "forums_id_seq")

let new_message ~forum_id ~author_id
    ?subject ?parent_id ?(moderated = false) ?(sticky = false) ~text = 
  Sql.full_transaction_block
    (fun db ->
       (match parent_id with
         | None ->
             serial4 db "forums_messages_seq" >>= fun last_id ->
             PGSQL(db) "INSERT INTO forums_messages \
               (subject, author_id, parent_id, root_id, forum_id, text, \
                moderated, sticky) \
             VALUES ($?subject, $author_id, $?parent_id, $last_id + 1, 
                     $forum_id, $text, $moderated, $sticky)"
         | Some p -> 
             PGSQL(db) "SELECT tree_max, root_id FROM forums_messages \
                        WHERE parent_id = $p" >>= fun r ->
             match r with
               | [(m, root_id)] ->
                   (PGSQL(db) "UPDATE forums_messages \
                               SET tree_max = tree_max + 2 \
                               WHERE root_id = $root_id AND \
                                     tree_max >= $m" >>= fun () ->
                    PGSQL(db) "UPDATE forums_messages \
                               SET tree_min = tree_min + 2 \
                               WHERE root_id = $root_id AND \
                                     tree_min >= $m" >>= fun () ->
                    PGSQL(db) "INSERT INTO forums_messages \
                        (subject, author_id, parent_id, root_id, forum_id,
                         text, moderated, sticky, tree_min, tree_max) \
                      VALUES ($?subject, $author_id, $p, \
                              $root_id, $forum_id, $text, \
                              $moderated, $sticky, $m, $m + 1)"
                   )
               | _ -> Lwt.fail (Failure
                                  "Forum_sql.new_message: parent does not exist or is not unique")
       ) >>= fun () -> 
      serial4 db "forums_messages_seq")


let set_deleted ~message_id ~deleted =
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "UPDATE forums_messages SET deleted = $deleted \
             WHERE id = $message_id")

let set_moderated ~message_id ~moderated =
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "UPDATE forums_messages SET moderated = $moderated \
             WHERE id = $message_id")

let set_sticky ~message_id ~sticky =
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "UPDATE forums_messages SET sticky = $sticky \
             WHERE id = $message_id")

let find_forum ?forum_id ?title () =
  Sql.full_transaction_block
    (fun db -> match (title, forum_id) with
     | (Some t, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, readonly \
                FROM forums \
                WHERE title = $t AND id = $i"
     | (Some t, None) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, readonly \
                FROM forums \
                WHERE title = $t"
     | (None, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, readonly \
                FROM forums \
                WHERE id = $i"
     | (None, None) -> Lwt.fail (Invalid_argument "Forum_sql.find_forum"))
  >>= fun r -> 
  (match r with
     | [a] -> Lwt.return a
     | a::_ -> 
         Ocsigen_messages.warning "Ocsimore: More than one forum have the same name or id (ignored)";
         Lwt.return a
     | _ -> Lwt.fail Not_found)

let get_forums_list () =
  Sql.full_transaction_block 
    (fun db ->
       PGSQL(db) 
         "SELECT id, title, descr, arborescent, deleted, readonly \
          FROM forums")

let get_message ~message_id =
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "SELECT id, subject, author_id, datetime, text, \
               moderated, deleted, sticky \
             FROM forums_messages \
             WHERE forums_messages.id = $message_id" >>= fun y -> 
  (match y with
     | [] -> Lwt.fail Not_found
     | [x] -> return x 
     | _ -> 
         Lwt.fail 
           (Failure 
              "Forum_sql.get_message: several messages have the same id")))


    
(*
let new_thread_and_article ~frm_id ~author_id ~subject ~txt =
  Ocsigen_messages.debug2 "[Sql] new_thread_and_article";
  Sql.full_transaction_block
    (fun db ->
  PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>=        fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun hidden -> 
  PGSQL(db) "INSERT INTO forums_textdata (txt) VALUES ($txt)" >>= fun () -> 
  serial4 db "textdata_id_seq" >>= fun txt_id -> 
  PGSQL(db) "INSERT INTO forums_threads \
               (frm_id, subject, hidden, author_id, article_id) \
             VALUES ($frm_id, $subject, $hidden, $author_id, $txt_id)" 
    >>= fun () -> 
  serial4 db "threads_id_seq" >>= fun thr_id -> 
  Ocsigen_messages.debug2 "[Sql] new_thread_and_article: finish"; 
  return (thr_id, txt_id))

let new_message ~thr_id ?parent_id ~author_id ~txt ~sticky () = 
  (* inserts a message in an existing thread; message will be hidden
     if forum is moderated *)
  Ocsigen_messages.debug2 "[Sql] new_message"; 
  Sql.full_transaction_block
    (fun db ->
  PGSQL(db) "SELECT moderated FROM forums,forums_threads \
             WHERE forums_threads.id = $thr_id \
             AND forums_threads.frm_id = forums.id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun hidden -> 
  PGSQL(db) "INSERT INTO forums_textdata (txt) VALUES ($txt)" >>= fun () -> 
  serial4 db "textdata_id_seq" >>= fun txt_id -> 
  PGSQL(db) "SELECT MAX(tree_max) FROM forums_messages \
             WHERE thr_id = $thr_id" >>= fun z -> 
  (match z with
     | [x] -> (match x with
                 | None -> return 0l
                 | Some y -> return y)
     | _ -> return 0l) >>= fun db_max -> 
    (match parent_id with
       | None -> 
           PGSQL(db) "INSERT INTO forums_messages (author_id, thr_id, txt_id, hidden, \
                        sticky, tree_min, tree_max) \
                      VALUES ($author_id, $thr_id, $txt_id, $hidden, $sticky, \
                                $db_max + 1, $db_max + 2)" >>= fun () -> 
           serial4 db "messages_id_seq"
       | Some pid -> 
           PGSQL(db) "SELECT tree_min, tree_max \
                      FROM forums_messages WHERE id = $pid" >>= fun y -> 
           (match y with 
              | [x] -> return x
              | _ -> fail Not_found) >>= fun (db_min, db_max) -> 
           PGSQL(db) "UPDATE forums_messages SET tree_min = tree_min + 2, \
                      tree_max = tree_max + 2 WHERE tree_min > $db_max" 
             >>= fun () -> 
           PGSQL(db) "UPDATE forums_messages SET tree_max = tree_max + 2 \
                      WHERE $db_min BETWEEN tree_min AND tree_max" 
             >>= fun () -> 
           PGSQL(db) "INSERT INTO forums_messages (author_id, thr_id, txt_id, hidden, \
                      sticky, tree_min, tree_max) \
                      VALUES ($author_id, $thr_id, $txt_id, $hidden, 
                              $sticky, $db_max, $db_max + 1)" >>= fun () -> 
           serial4 db "messages_id_seq"))
  >>= fun msg_id ->
  Ocsigen_messages.debug2 "[Sql] new_message: finish"; 
  Lwt.return msg_id

let forum_toggle_moderated ~frm_id =
  (* toggle moderation status of a forum *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] forum_toggle_moderated";
  PGSQL(db) "UPDATE forums SET moderated = NOT moderated WHERE id = $frm_id")
    
let thread_toggle_hidden ~frm_id ~thr_id =
  (* hides/shows a thread *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] thread_toggle_hidden";
  PGSQL(db) "UPDATE forums_threads SET hidden = NOT hidden \
              WHERE id = $thr_id AND frm_id = $frm_id")

let message_toggle_hidden ~frm_id ~msg_id =
  (* hides/shows a message *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_toggle_hidden";
  PGSQL(db) "UPDATE forums_messages \
             SET hidden = NOT forums_messages.hidden \
             FROM forums_threads \
             WHERE forums_messages.id = $msg_id \
             AND forums_messages.thr_id = forums_threads.id \
             AND forums_threads.frm_id = $frm_id")

let message_toggle_sticky ~frm_id ~msg_id =
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] message_toggle_sticky";
  PGSQL(db) "UPDATE forums_messages SET sticky = NOT forums_messages.sticky \
             FROM forums_threads WHERE forums_messages.id = $msg_id \
             AND forums_messages.thr_id = forums_threads.id \
             AND forums_threads.frm_id = $frm_id")


let forum_get_data ~frm_id ~role =
  (* returns id, title, description, mod status, number of shown/hidden
     threads and messages of a forum.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  Sql.full_transaction_block
    (fun db ->
       Ocsigen_messages.debug2 "[Sql] forum_get_data";
       PGSQL(db) "SELECT id, title, descr, moderated FROM forums \
             WHERE id = $frm_id" >>= fun y -> 
         (match y with [x] -> return x | _ -> fail Not_found) 
         >>= fun (id, title, description, moderated) -> 
           PGSQL(db) "SELECT COUNT(*) FROM forums_threads \
             WHERE frm_id = $frm_id AND (NOT hidden)" >>= fun y -> 
             (match y with [Some x] -> return x | _ -> assert false) 
         >>= fun n_shown_thr -> 
           PGSQL(db) "SELECT COUNT(*) FROM forums_messages, forums_threads \
             WHERE forums_threads.frm_id = $frm_id \
             AND forums_messages.thr_id = forums_threads.id \
             AND NOT (forums_messages.hidden OR forums_threads.hidden)" >>= fun y -> 
         (match y with [Some x] -> return x | _ -> assert false)
         >>= fun n_shown_msg -> 
         (match role with        
            | Moderator -> (* counts all hidden stuff *)
                PGSQL(db) "SELECT COUNT(*) FROM forums_threads \
                    WHERE frm_id = $frm_id AND hidden" >>= fun y -> 
                  (match y with
                     | [Some x] -> return x
                     | _ -> assert false)
                | Author aid ->
                    PGSQL(db) "SELECT COUNT(*) FROM forums_threads \
                    WHERE frm_id = $frm_id AND hidden \
                    AND author_id = $aid" >>= fun y -> 
                      (match y with [Some x] -> return x | _ -> assert false)
                    | Unknown -> return 0L) >>= fun n_hidden_thr -> 
           (match role with
              | Moderator -> 
                  PGSQL(db) "SELECT COUNT(*) FROM forums_messages, forums_threads \
                    WHERE forums_threads.frm_id = $frm_id \
                    AND forums_messages.thr_id = forums_threads.id \
                    AND (forums_messages.hidden OR forums_threads.hidden)" >>= fun y -> 
                    (match y with [Some x] -> return x | _ -> assert false)
                  | Author aid ->
                      PGSQL(db) "SELECT COUNT(*) FROM forums_messages, forums_threads \
                     WHERE forums_threads.frm_id = $frm_id \
                    AND forums_messages.thr_id = forums_threads.id \
                    AND (forums_messages.hidden OR forums_threads.hidden) \
                    AND forums_messages.author_id = $aid" >>= fun y -> 
                        (match y with [Some x] -> return x | _ -> assert false)
                      | Unknown -> return 0L)
  >>= fun n_hidden_msg -> 
  Ocsigen_messages.debug2 "[Sql] forum_get_data: finish"; 
  Lwt.return (id, title, description, moderated,
              n_shown_thr, n_hidden_thr, n_shown_msg, n_hidden_msg))

let thread_get_nr_messages ~thr_id ~role =
  Sql.full_transaction_block
    (fun db ->
       Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages";
  (match role with
     | Moderator -> (* all messages *)
         PGSQL(db) "SELECT COUNT(*) FROM forums_messages \
                    WHERE forums_messages.thr_id = $thr_id" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> fail (Failure "thread_get_nr_messages"))
     | Author aid -> 
         (* all non-hidden messages AND hidden messages posted by a *)
         PGSQL(db) "SELECT COUNT(*) FROM forums_messages \
                    WHERE forums_messages.thr_id = $thr_id \
                    AND forums_messages.hidden = false OR forums_messages.author_id = $aid"
         >>= fun y -> 
         (match y with
            | [Some x] -> return x
            | _ -> fail (Failure "thread_get_nr_messages"))
     | Unknown -> (* all non-hidden messages *)
         PGSQL(db) "SELECT COUNT(*) FROM forums_messages \
                    WHERE forums_messages.thr_id = $thr_id \
                    AND forums_messages.hidden = false" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> fail (Failure "thread_get_nr_messages"))))
  >>= fun n_msg ->
  Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages: finish"; 
  Lwt.return n_msg

let thread_get_data (* ~frm_id *) ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  Sql.full_transaction_block
    (fun db ->
  PGSQL(db) 
    "SELECT t.id, subject, fullname, t.datetime, t.hidden, COALESCE (txt, '') \
     FROM users AS u, forums_threads AS t LEFT OUTER JOIN forums_textdata ON \
        (t.article_id = forums_textdata.id) \
     WHERE t.id = $thr_id AND u.id = t.author_id" (* AND frm_id = $frm_id *) 
    >>=        fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) 
      >>= fun (id, subject, author_id, datetime, hidden, article) ->
  PGSQL(db) "SELECT COUNT(*) FROM forums_messages \
             WHERE thr_id = $thr_id AND (NOT hidden)" >>= fun y -> 
  (match y with
     | [Some x] -> return x
     | _ -> assert false) >>= fun n_shown_msg -> 
  (match role with
     | Moderator -> (* counts all hidden messages *)
         PGSQL(db) "SELECT COUNT(*) FROM forums_messages, forums_threads \
                    WHERE forums_messages.thr_id = $thr_id \
                    AND forums_threads.id = $thr_id \
                    AND (forums_messages.hidden OR forums_threads.hidden)" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> assert false)
     | Author aid -> (* counts only hidden messages posted by her *)
         PGSQL(db) "SELECT COUNT(*) FROM forums_messages, forums_threads \
                    WHERE forums_messages.thr_id = $thr_id \
                    AND forums_threads.id = $thr_id \
                    AND (forums_messages.hidden OR forums_threads.hidden) \
                    AND forums_messages.author_id = $aid" >>= fun y -> 
           (match y with
              | [Some x] -> return x
              | _ -> assert false)
     | Unknown -> (* nothing to be counted *) return 0L)
  >>= fun n_hidden_msg -> 
  Ocsigen_messages.debug2 "[Sql] thread_get_data: finish"; 
  Lwt.return
    (id, subject, author_id, article, datetime, 
     hidden, n_shown_msg, n_hidden_msg))

      
let thread_get_neighbours ~frm_id ~thr_id ~role =
  (* returns None|Some id of prev & next thread in the same forum. *)
  Sql.full_transaction_block
    (fun db ->
       Ocsigen_messages.debug2 "[Sql] thread_get_neighbours";
  PGSQL(db) "SELECT datetime FROM forums_threads \
             WHERE id = $thr_id AND frm_id = $frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found) >>= fun datetime -> 
  (match role with
     | Moderator -> (* all kinds of forums_threads *)
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                ORDER BY datetime DESC LIMIT 1"
     | Author aid -> (* only shown forums_threads, or hidden ones posted by her *)
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1"
     | Unknown -> (* only shown forums_threads *)
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime < $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1") >>= fun y -> 
  let prev = (match y with [x] -> Some x | _ -> None) in
  (match role with
     | Moderator ->
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                 ORDER BY datetime ASC LIMIT 1"
     | Author aid ->
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                 ORDER BY datetime ASC LIMIT 1"
     | Unknown ->
         PGSQL(db) "SELECT id FROM forums_threads WHERE frm_id = $frm_id \
                                AND datetime > $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime ASC LIMIT 1") >>= fun y -> 
  Lwt.return (match y with [x] -> Some x | _ -> None)
  >>= fun next -> 
  Ocsigen_messages.debug2 "[Sql] thread_get_neighbours: finish"; 
  Lwt.return (prev, next))

let message_get_neighbours ~frm_id ~msg_id ~role =
  (* returns None|Some id of prev & next message in the same
     thread. *)
  Sql.full_transaction_block
    (fun db ->
       Ocsigen_messages.debug2 "[Sql] message_get_neighbours";
  PGSQL(db) "SELECT forums_messages.thr_id, forums_messages.datetime \
                FROM forums_messages, forums_threads \
                WHERE forums_messages.id = $msg_id \
                AND forums_messages.thr_id = forums_threads.id \
                AND forums_threads.frm_id = $frm_id" >>= fun y -> 
  (match y with [x] -> return x | _ -> fail Not_found)
    >>= fun (thr_id, datetime) -> 
  (match role with
     | Moderator -> (* all kinds of forums_threads *)
         PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                ORDER BY datetime DESC LIMIT 1"
     | Author aid -> (* only shown forums_messages, or hidden ones posted by her *)
         PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1"
     | Unknown -> (* only shown forums_messages *)
         PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime < $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime DESC LIMIT 1") >>= fun y -> 
    let prev = (match y with [x] -> Some x | _ -> None) in
    (match role with
       | Moderator ->
           PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                 ORDER BY datetime ASC LIMIT 1"
       | Author aid ->
           PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                AND (author_id = $aid OR NOT hidden) \
                                 ORDER BY datetime ASC LIMIT 1"
       | Unknown ->
           PGSQL(db) "SELECT id FROM forums_messages WHERE thr_id = $thr_id \
                                AND datetime > $datetime \
                                AND (NOT hidden) \
                                ORDER BY datetime ASC LIMIT 1")
  >>= fun y ->
  let next = (match y with [x] -> Some x | _ -> None) in
  Ocsigen_messages.debug2 "[Sql] message_get_neighbours: finish"; 
  Lwt.return (prev, next))

let forum_get_threads_list ~frm_id ?offset ?limit ~role () =
  (* returns the forums_threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
  Lwt_pool.use Sql.pool (fun db ->
  Ocsigen_messages.debug2 "[Sql] forum_get_forums_threads_list";
  let db_offset = match offset with
    | None -> 0L
    | Some x -> x in
    match limit with
      | None -> 
          (match role with
             | Moderator -> 
                 PGSQL(db)
                   "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
                        WHERE frm_id = $frm_id AND author_id = users.id \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset"
             | Author aid ->
                 PGSQL(db)
                   "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
                        WHERE frm_id = $frm_id \
                        AND (author_id = $aid OR NOT hidden) \
                        AND users.id = author_id \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset"
             | Unknown -> 
                 PGSQL(db)
                   "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                        AND NOT hidden \
                        ORDER BY datetime DESC \
                        OFFSET $db_offset")
      | Some x -> 
          let db_limit = x in
            (match role with
               | Moderator -> 
                   PGSQL(db)
                     "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                        ORDER BY datetime DESC \
                        LIMIT $db_limit OFFSET $db_offset"
               | Author aid -> 
                   PGSQL(db)
                     "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
                        WHERE frm_id = $frm_id AND users.id = author_id \
                         AND (author_id = $aid OR NOT hidden) \
                        ORDER BY datetime DESC \
                        LIMIT $db_limit OFFSET $db_offset"
               | Unknown -> 
                   PGSQL(db)
                     "SELECT forums_threads.id, subject, fullname, datetime, hidden \
                        FROM forums_threads, users \
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
          Sql.transaction_block db
            (fun () ->
          (match role with 
             | Moderator ->
                PGSQL(db)
                  "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                FROM forums_messages, forums_textdata, users \
                WHERE txt_id = forums_textdata.id AND thr_id = $thr_id \
                AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset" 
             | Author aid ->
                PGSQL(db)
                  "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                FROM forums_messages, forums_textdata, users \
                WHERE (author_id = $aid OR NOT hidden) \
                AND txt_id = forums_textdata.id AND \
                thr_id = $thr_id AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset" 
             | Unknown ->
                PGSQL(db)
                  "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                FROM forums_messages, forums_textdata, users \
                WHERE NOT hidden AND txt_id = forums_textdata.id AND thr_id = $thr_id \
                AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                OFFSET $db_offset"))
          >>= fun msg_l -> 
          let final_msg_l = match bottom with
            | None -> msg_l
            | Some btm -> cut (fun (id,_,_,_,_,_) -> id) btm msg_l 
          in
          Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text: finish";
          Lwt.return final_msg_l
          | Some x -> 
              let db_limit = x in
              Sql.transaction_block db
                (fun () ->
            (match role with 
               | Moderator ->
                   PGSQL(db)
                     "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                       FROM forums_messages, forums_textdata, users \
                      WHERE txt_id = forums_textdata.id AND thr_id = $thr_id \
                      AND users.id = author_id \
                      ORDER BY sticky DESC, datetime \
                      LIMIT $db_limit OFFSET $db_offset" 
        | Author aid ->
            PGSQL(db) "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                FROM forums_messages, forums_textdata, users \
                WHERE (author_id = $aid OR NOT hidden) \
                AND txt_id = forums_textdata.id AND \
                thr_id = $thr_id AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                LIMIT $db_limit OFFSET $db_offset" 
        | Unknown ->
            PGSQL(db) "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky \
                FROM forums_messages, forums_textdata, users \
                WHERE NOT hidden AND txt_id = forums_textdata.id AND thr_id = $thr_id \
                        AND users.id = author_id \
                ORDER BY sticky DESC, datetime \
                LIMIT $db_limit OFFSET $db_offset"))
            >>= fun msg_l -> 
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
    | None -> 
        Sql.transaction_block db
          (fun () ->
        (match top with
           | None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
                        FROM forums_messages WHERE thr_id = $thr_id" >>= fun z -> 
                      Lwt.return
                        (match z with
                           | [(x,y)] -> 
                               (match x, y with
                                  | Some x', Some y' -> (x',y')
                                  | _-> (Int32.zero,Int32.zero))
                           | _ -> (Int32.zero,Int32.zero)))
                | Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
                        FROM forums_messages WHERE thr_id = $thr_id AND id = $t") 
                    >>=        fun y -> 
                      (match y        with
                         | [x] -> return x
                         | _ -> fail Not_found)) >>= fun (db_min, db_max) -> 
          (match role with 
             | Moderator ->
                 PGSQL(db)
                   "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min, tree_max \
                        FROM forums_messages, forums_textdata, users \
                        WHERE txt_id = forums_textdata.id \
                        AND (tree_min BETWEEN $db_min AND $db_max) \
                        AND thr_id = $thr_id AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        OFFSET $db_offset" 
             | Author aid ->
                 PGSQL(db) 
                   "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                    tree_min, tree_max \
                    FROM forums_messages, forums_textdata, users \
                    WHERE (author_id = $aid OR NOT hidden) \
                    AND txt_id = forums_textdata.id AND \
                    (tree_min BETWEEN $db_min AND $db_max) \
                    AND thr_id = $thr_id \
                    AND users.id = author_id \
                    ORDER BY sticky DESC, tree_min \
                    OFFSET $db_offset" 
             | Unknown ->
                 PGSQL(db)
                   "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                    tree_min, tree_max \
                    FROM forums_messages, forums_textdata, users \
                    WHERE NOT hidden AND txt_id = forums_textdata.id AND \
                        (tree_min BETWEEN $db_min AND $db_max)
                    AND thr_id = $thr_id \
                    AND users.id = author_id \
                    ORDER BY sticky DESC, tree_min \
                    OFFSET $db_offset"))
        >>= fun msg_l -> 
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
            Sql.transaction_block db
              (fun () ->
                (match top with
                   | None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
                        FROM forums_messages WHERE thr_id = $thr_id" >>= fun z -> 
                        Lwt.return
                          (match z with
                             | [(x,y)] -> 
                                 (match x, y with 
                                    | Some x', Some y' -> (x',y')
                                    | _-> (Int32.zero, Int32.zero))
                             | _ -> (Int32.zero,Int32.zero)))
                   | Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
                        FROM forums_messages WHERE thr_id = $thr_id AND id = $t") 
                       >>= fun y -> 
                      (match y with
                         | [x] -> Lwt.return x
                         | _ -> fail Not_found)) >>= fun (db_min, db_max) -> 
                (match role with 
                   | Moderator ->
                       PGSQL(db)
                         "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM forums_messages, forums_textdata, users \
                        WHERE txt_id = forums_textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
                        AND thr_id = $thr_id AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset" 
                   | Author aid ->
                       PGSQL(db) 
                         "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM forums_messages, forums_textdata, users \
                        WHERE (author_id = $aid OR NOT hidden) AND txt_id = forums_textdata.id AND \
                                (tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
                                AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset" 
                   | Unknown ->
                       PGSQL(db) "SELECT forums_messages.id,txt,fullname,datetime,hidden,sticky, \
                        tree_min,tree_max \
                        FROM forums_messages, forums_textdata, users \
                        WHERE NOT hidden AND txt_id = forums_textdata.id AND \
                                (tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
                                AND users.id = author_id \
                        ORDER BY sticky DESC, tree_min \
                        LIMIT $db_limit OFFSET $db_offset"))
            >>= fun msg_l -> 
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
        PGSQL(db) "SELECT forums_messages.id,txt,fullname \
        FROM forums_messages, forums_textdata, users \
        WHERE forums_messages.txt_id = forums_textdata.id AND \
        thr_id IN (SELECT id FROM forums_threads WHERE frm_id IN $@frm_ids) AND
        NOT forums_messages.hidden AND users.id = author_id \
        ORDER BY datetime DESC LIMIT $limit" >>= fun result -> 
  Ocsigen_messages.debug2 "[Sql] get_latest_messages: finish"; 
  Lwt.return result)



*)
