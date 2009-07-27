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

open User_sql.Types
open Sql.PGOCaml
open Sql

let (>>=) = Lwt.bind

open Forum_types

let new_forum
    ~title ~descr ?(arborescent = true) ~title_syntax
    ~messages_wiki ~comments_wiki () =
  let messages_wiki = Wiki_types.sql_of_wiki messages_wiki in
  let comments_wiki = Wiki_types.sql_of_wiki comments_wiki in
  let title_syntax = Wiki_types.string_of_content_type title_syntax in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) 
         "INSERT INTO forums 
            (title, descr, arborescent, title_syntax, messages_wiki, comments_wiki)
          VALUES ($title, $descr, $arborescent, $title_syntax, $messages_wiki, $comments_wiki)" >>= fun () -> 
       serial4 db "forums_id_seq" >>= fun s ->
       Lwt.return (forum_of_sql s)
    )

let new_message ~sp ~forum ~wiki ~creator_id ~title_syntax
    ?subject ?parent_id ?(moderated = false) ?(sticky = false) ~text =
  let creator_id' = sql_from_userid creator_id in
  let parent_id = sql_of_message_option parent_id in
  let forum_id = sql_of_forum forum in
  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
  let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
  let content_type = 
    Wiki_models.get_default_content_type wiki_info.Wiki_types.wiki_model
  in
  Sql.full_transaction_block
    (fun db ->
       Wiki_data.new_wikitextbox
         ~sp ~rights ~db ~wiki ~author:creator_id ~comment:""
         ~content:text ~content_type () >>= fun wikibox ->
       (match subject with
          | None -> Lwt.return None
          | Some subject ->
              Wiki_data.new_wikitextbox
                ~sp ~rights ~db ~wiki ~author:creator_id ~comment:""
                ~content:subject ~content_type:title_syntax ()
              >>= fun subject ->
              Lwt.return (Some (Wiki_types.sql_of_wikibox subject)))
       >>= fun subject ->
       let wikibox = Wiki_types.sql_of_wikibox wikibox in
       (match parent_id with
         | None ->
             PGSQL(db) "SELECT NEXTVAL('forums_messages_id_seq')"
             >>= (function
               | [Some next_id] ->
                   let next_id = Int64.to_int32 next_id in
             PGSQL(db) "INSERT INTO forums_messages \
               (id, creator_id, parent_id, root_id, forum_id, \
                subject, wikibox, moderated, sticky) \
             VALUES ($next_id, $creator_id', $?parent_id, $next_id, 
                     $forum_id, $?subject, $wikibox, $moderated, $sticky)"
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
                        (subject, creator_id, parent_id, root_id, forum_id,
                         wikibox, moderated, sticky, tree_min, tree_max) \
                      VALUES ($?subject, $creator_id', $p, \
                              $root_id, $forum_id, $wikibox, \
                              $moderated, $sticky, $m, $m + 1)"
                   )
               | _ -> Lwt.fail 
                   (Failure
                      "Forum_sql.new_message: parent does not exist or is not unique")
       ) >>= fun () -> 
      serial4 db "forums_messages_id_seq" >>= fun s ->
      Lwt.return (message_of_sql s)
    )

let set_moderated ~message_id ~moderated =
  let message_id = sql_of_message message_id in
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "UPDATE forums_messages SET moderated = $moderated \
             WHERE id = $message_id")

let set_sticky ~message_id ~sticky =
  let message_id = sql_of_message message_id in
  Lwt_pool.use Sql.pool (fun db ->
  PGSQL(db) "UPDATE forums_messages SET sticky = $sticky \
             WHERE id = $message_id")

let get_forum ?(not_deleted_only = true) ?forum ?title () =
  let forum_id = sql_of_forum_option forum in
  Sql.full_transaction_block
    (fun db -> match (title, forum_id) with
     | (Some t, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, \
                           title_syntax, messages_wiki, comments_wiki \
                FROM forums \
                WHERE title = $t AND id = $i"
     | (Some t, None) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, \
                           title_syntax, messages_wiki, comments_wiki \
                FROM forums \
                WHERE title = $t"
     | (None, Some i) -> 
         PGSQL(db) "SELECT id, title, descr, arborescent, deleted, \
                           title_syntax, messages_wiki, comments_wiki \
                FROM forums \
                WHERE id = $i"
     | (None, None) -> Lwt.fail (Invalid_argument "Forum_sql.find_forum"))
  >>= fun r -> 
  (match r with
     | [(_id, _title, _descr, _arborescent, deleted, _ts, _mw, _cw) as a] -> 
         if not_deleted_only && deleted
         then Lwt.fail Not_found
         else Lwt.return (get_forum_info a)
     | ((_id, _title, _descr, _arborescent, deleted, _ts, _mw, _cw) as a)::_ -> 
         Ocsigen_messages.warning "Ocsimore: More than one forum have the same name or id (ignored)";
         if not_deleted_only && deleted
         then Lwt.fail Not_found
         else Lwt.return (get_forum_info a)
     | _ -> Lwt.fail Not_found)

let get_forums_list ?(not_deleted_only = true) () =
  Sql.full_transaction_block 
    (fun db ->
       (if not_deleted_only
        then
          PGSQL(db) 
           "SELECT id, title, descr, arborescent, deleted, \
                   title_syntax, messages_wiki, comments_wiki \
            FROM forums \
            WHERE deleted = false"
        else
          PGSQL(db) 
            "SELECT id, title, descr, arborescent, deleted, \
                    title_syntax, messages_wiki, comments_wiki \
             FROM forums"))

let get_message ~message_id () =
  let message_id = sql_of_message message_id in
  Forum_sql0.get_message_raw ~message_id ()
  >>= fun x -> Lwt.return (get_message_info x)
    

let get_thread ~message_id () =
  let message_id = sql_of_message message_id in
  Sql.full_transaction_block
    (fun db -> 
         PGSQL(db) "SELECT tree_min, tree_max \
                    FROM forums_messages \
                    WHERE forums_messages.id = $message_id" >>= fun y -> 
         (match y with
            | [] -> Lwt.fail Not_found
            | (min, max) :: _ -> 
                PGSQL(db)
                     "SELECT id, creator_id, datetime, parent_id, \
                           root_id, forum_id, subject, wikibox, \
                           moderated, sticky, special_rights, \
                           tree_min, tree_max \
                      FROM forums_messages \
                      WHERE root_id= $message_id \
                      AND tree_min >= $min AND tree_max <= $max \
                      ORDER BY tree_min"
         ))


let get_message_list ~forum ~first ~number ~moderated_only () =
  let forum = sql_of_forum forum in
  let offset = Int64.sub first 1L in
  Sql.full_transaction_block
    (fun db -> 
       if moderated_only
       then
         PGSQL(db) "SELECT *
                    FROM forums_messages \
                    WHERE forum_id = $forum \
                    AND parent_id IS NULL \
                    AND (moderated = true OR special_rights = true) \
                    ORDER BY datetime DESC OFFSET $offset LIMIT $number"
       else
         PGSQL(db) "SELECT *
                    FROM forums_messages \
                    WHERE forum_id = $forum \
                    AND parent_id IS NULL \
                    ORDER BY datetime DESC OFFSET $offset LIMIT $number")

let get_wikibox_creator ~wb =
  let wb = Wiki_types.sql_of_wikibox wb in
  Sql.full_transaction_block
    (fun db -> 
       PGSQL(db) "SELECT creator_id
                  FROM forums_messages \
                  WHERE wikibox = $wb OR subject = $wb"
    ) >>= function
      | [] -> Lwt.return None
      | a::_ -> Lwt.return (Some (User_sql.Types.userid_from_sql a))

let wikibox_is_moderated ~wb =
  let wb = Wiki_types.sql_of_wikibox wb in
  Sql.full_transaction_block
    (fun db -> 
       PGSQL(db) "SELECT moderated
                  FROM forums_messages \
                  WHERE wikibox = $wb OR subject = $wb"
    ) >>= function
      | [] -> Lwt.return false (* ? *)
      | a::_ -> Lwt.return a

