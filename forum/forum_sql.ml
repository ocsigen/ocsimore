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
             PGSQL(db) "SELECT NEXTVAL('forums_messages_id_seq')"
             >>= (function
               | [Some next_id] ->
                   let next_id = Int64.to_int32 next_id in
             PGSQL(db) "INSERT INTO forums_messages \
               (id, subject, author_id, parent_id, root_id, forum_id, text, \
                moderated, sticky) \
             VALUES ($next_id, $?subject, $author_id, $?parent_id, $next_id, 
                     $forum_id, $text, $moderated, $sticky)"
               | _ -> Lwt.fail 
                   (Failure
                      "Forum_sql.new_message: error in nextval(id) in table forums_messages"))
         | Some p -> 
             PGSQL(db) "SELECT tree_max, root_id FROM forums_messages \
                        WHERE id = $p" >>= fun r ->
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
               | _ -> Lwt.fail 
                   (Failure
                      "Forum_sql.new_message: parent does not exist or is not unique")
       ) >>= fun () -> 
      serial4 db "forums_messages_id_seq")


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

let get_forum ?forum_id ?title () =
  Sql.full_transaction_block
    (fun db -> match (title, forum_id) with
     | (Some t, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted \
                FROM forums \
                WHERE title = $t AND id = $i"
     | (Some t, None) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted \
                FROM forums \
                WHERE title = $t"
     | (None, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted \
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
         "SELECT id, title, descr, arborescent, deleted \
          FROM forums")

let get_message ~message_id =
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "SELECT id, subject, author_id, datetime, parent_id, 
               root_id, forum_id, text, moderated, deleted, sticky \
             FROM forums_messages \
             WHERE forums_messages.id = $message_id" >>= fun y -> 
  (match y with
     | [] -> Lwt.fail Not_found
     | [x] -> return x 
     | _ -> 
         Lwt.fail 
           (Failure 
              "Forum_sql.get_message: several messages have the same id")))

let get_thread ~message_id =
  Sql.full_transaction_block
    (fun db -> 
       PGSQL(db) "SELECT tree_min, tree_max \
                  FROM forums_messages \
                  WHERE forums_messages.id = $message_id" >>= fun y -> 
       (match y with
          | [] -> Lwt.fail Not_found
          | [(min, max)] -> 
              PGSQL(db) "SELECT id, subject, author_id, datetime, parent_id, \
                           root_id, forum_id, text, moderated, deleted, sticky \
                         FROM forums_messages \
                         WHERE tree_min >= $min AND tree_max <= $max \
                         ORDER BY tree_min"
          | _ -> 
              Lwt.fail 
                (Failure 
                   "Forum_sql.get_thread: several messages have the same id")))


