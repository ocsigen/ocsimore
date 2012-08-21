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
open Ocsi_sql

let (>>=) = Lwt.bind

open Forum_types

let forums = (<:table< forums (
  id integer NOT NULL,
  title text NOT NULL DEFAULT(""),
  descr text NOT NULL DEFAULT(""),
  arborescent boolean NOT NULL DEFAULT(true),
  deleted boolean NOT NULL DEFAULT(false),
  title_syntax text NOT NULL,
  messages_wiki integer NOT NULL,
  comments_wiki integer NOT NULL
) >>)

let forums_id_seq = (<:sequence< serial "forums_id_seq" >>)

let new_forum
    ~title ~descr ?(arborescent = true) ~title_syntax
    ~messages_wiki ~comments_wiki () =
  let messages_wiki = Wiki_types.sql_of_wiki messages_wiki in
  let comments_wiki = Wiki_types.sql_of_wiki comments_wiki in
  let title_syntax = Wiki_types.string_of_content_type title_syntax in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.query db (<:insert< $forums$ := {
        id = nextval $forums_id_seq$;
        title = $string:title$;
        descr = $string:descr$;
        arborescent = $bool:arborescent$;
        deleted = forums?deleted;
        title_syntax = $string:title_syntax$;
        messages_wiki = $int32:messages_wiki$;
        comments_wiki = $int32:comments_wiki$
      } >>) >>= fun () ->
       Lwt_Query.value db (<:value< currval $forums_id_seq$ >>) >>= fun s ->
       Lwt.return (forum_of_sql s)
    )

let update_forum ?title ?descr ?arborescent ?title_syntax
    ?messages_wiki ?comments_wiki forum =
  let forum_id = sql_of_forum forum in
  Ocsi_sql.full_transaction_block
    (fun db ->
      lwt () = (match title with
        | None -> Lwt.return ()
        | Some title ->
          Lwt_Query.query db (<:update< f in $forums$ := {
            title = $string:title$
          } | f.id = $int32:forum_id$ >>)
      ) in
      lwt () = (match descr with
        | None -> Lwt.return ()
        | Some descr ->
          Lwt_Query.query db (<:update< f in $forums$ := {
            descr = $string:descr$
          } | f.id = $int32:forum_id$ >>)
      ) in
      lwt () = (match arborescent with
        | None -> Lwt.return ()
        | Some arborescent ->
          Lwt_Query.query db (<:update< f in $forums$ := {
            arborescent = $bool:arborescent$
          } | f.id = $int32:forum_id$ >>)
      ) in
      lwt () = (match messages_wiki with
        | None -> Lwt.return ()
        | Some messages_wiki ->
          let messages_wiki = Wiki_types.sql_of_wiki messages_wiki in
          Lwt_Query.query db (<:update< f in $forums$ := {
            messages_wiki = $int32:messages_wiki$
          } | f.id = $int32:forum_id$ >>)
      ) in
      lwt () = (match comments_wiki with
        | None -> Lwt.return ()
        | Some comments_wiki ->
          let comments_wiki = Wiki_types.sql_of_wiki comments_wiki in
          Lwt_Query.query db (<:update< f in $forums$ := {
            comments_wiki = $int32:comments_wiki$
          } | f.id = $int32:forum_id$ >>)
      ) in
      (match title_syntax with
        | None -> Lwt.return ()
        | Some title_syntax ->
          let title_syntax = Wiki_types.string_of_content_type title_syntax in
          Lwt_Query.query db (<:update< f in $forums$ := {
            title_syntax = $string:title_syntax$
          } | f.id = $int32:forum_id$ >>)
      )
)

let forums_messages = (<:table< forums_messages (
  id integer NOT NULL,
  creator_id integer NOT NULL,
  datetime timestamp NOT NULL DEFAULT(current_timestamp),
  parent_id integer,
  root_id integer NOT NULL,
  forum_id integer NOT NULL,
  subject integer,
  wikibox integer NOT NULL,
  moderated boolean NOT NULL DEFAULT(false),
  sticky boolean NOT NULL DEFAULT(false),
  special_rights boolean NOT NULL DEFAULT(false),
  tree_min integer NOT NULL DEFAULT(1),
  tree_max integer NOT NULL DEFAULT(2)
) >>)

let forums_messages_id_seq = (<:sequence< serial "forums_messages_id_seq" >>)

let new_message ~forum ~wiki ~creator_id ~title_syntax
    ?subject ?parent_id ?(moderated = false) ?(sticky = false) ~text =
  let creator_id' = sql_from_userid creator_id in
  let parent_id = sql_of_message_option parent_id in
  let forum_id = sql_of_forum forum in
  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
  lwt rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
  lwt content_type =
    Wiki_models.get_default_content_type wiki_info.Wiki_types.wiki_model
  in
  Ocsi_sql.full_transaction_block
    (fun db ->
       Wiki_data.new_wikitextbox
         ~rights ~db ~wiki ~author:creator_id ~comment:""
         ~content:text ~content_type () >>= fun wikibox ->
       (match subject with
          | None -> Lwt.return None
          | Some subject ->
              Wiki_data.new_wikitextbox
                ~rights ~db ~wiki ~author:creator_id ~comment:""
                ~content:subject ~content_type:title_syntax ()
              >>= fun subject ->
              Lwt.return (Some (Wiki_types.sql_of_wikibox subject)))
       >>= fun subject ->
       let wikibox = Wiki_types.sql_of_wikibox wikibox in
       (match parent_id with
         | None ->
           let next_id = (<:value< nextval $forums_messages_id_seq$ >>) in
           Lwt_Query.query db (<:insert< $forums_messages$ := {
             id = $next_id$;
             creator_id = $int32:creator_id'$;
             datetime = forums_messages?datetime;
             parent_id = of_option $map_option_int32 parent_id$;
             root_id = $next_id$;
             forum_id = $int32:forum_id$;
             subject = of_option $map_option_int32 subject$;
             wikibox = $int32:wikibox$;
             moderated = $bool:moderated$;
             sticky = $bool:sticky$;
             special_rights = forums_messages?special_rights;
             tree_min = forums_messages?tree_min;
             tree_max = forums_messages?tree_max
           } >>)
         | Some p ->
           Lwt_Query.view db (<:view< {
             f.tree_max;
             f.root_id
           } | f in $forums_messages$; f.id = $int32:p$ >>) >>= (fun r ->
             match r with
               | [data] ->
                 (Lwt_Query.query db (<:update< f in $forums_messages$ := {
                   tree_max = f.tree_max + 2
                  } | f.root_id = $int32:(data#!root_id)$;
                      f.tree_max >= $int32:(data#!tree_max)$ >>) >>= fun () ->
                  Lwt_Query.query db (<:update< f in $forums_messages$ := {
                    tree_min = f.tree_min + 2
                  } | f.root_id = $int32:(data#!root_id)$;
                      f.tree_min >= $int32:(data#!tree_max)$ >>) >>= fun () ->
                  Lwt_Query.query db (<:insert< $forums_messages$ := {
                    id = nextval $forums_messages_id_seq$;
                    creator_id = $int32:creator_id'$;
                    datetime = forums_messages?datetime;
                    parent_id = nullable $int32:p$;
                    root_id = $int32:(data#!root_id)$;
                    forum_id = $int32:forum_id$;
                    subject = of_option $map_option_int32 subject$;
                    wikibox = $int32:wikibox$;
                    moderated = $bool:moderated$;
                    sticky = $bool:sticky$;
                    special_rights = forums_messages?special_rights;
                    tree_min = $int32:(data#!tree_max)$;
                    tree_max = $int32:(data#!tree_max)$ + 1
                  } >>)
                 )
               | _ -> Lwt.fail
                   (Failure
                      "Forum_sql.new_message: parent does not exist or is not unique")
           )
       ) >>= fun () ->
      Lwt_Query.value db (<:value< currval $forums_messages_id_seq$ >>)
      >>= fun s ->
      Lwt.return (message_of_sql s)
    )

let set_moderated ~message_id ~moderated =
  let message_id = sql_of_message message_id in
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.query db (<:update< f in $forums_messages$ := {
      moderated = $bool:moderated$
    } | f.id = $int32:message_id$ >>)
  )

let set_sticky ~message_id ~sticky =
  let message_id = sql_of_message message_id in
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.query db (<:update< f in $forums_messages$ := {
      sticky = $bool:sticky$
    } | f.id = $int32:message_id$ >>)
  )

let get_forum ?(not_deleted_only = true) ?forum ?title () =
  let forum_id = sql_of_forum_option forum in
  Ocsi_sql.full_transaction_block
    (fun db -> match (title, forum_id) with
     | (Some t, Some i) ->
       Lwt_Query.view db (<:view< {
         f.id;
         f.title;
         f.descr;
         f.arborescent;
         f.deleted;
         f.title_syntax;
         f.messages_wiki;
         f.comments_wiki
       } | f in $forums$; f.title = $string:t$; f.id = $int32:i$ >>)
     | (Some t, None) ->
       Lwt_Query.view db (<:view< {
         f.id;
         f.title;
         f.descr;
         f.arborescent;
         f.deleted;
         f.title_syntax;
         f.messages_wiki;
         f.comments_wiki
       } | f in $forums$; f.title = $string:t$ >>)
     | (None, Some i) ->
       Lwt_Query.view db (<:view< {
         f.id;
         f.title;
         f.descr;
         f.arborescent;
         f.deleted;
         f.title_syntax;
         f.messages_wiki;
         f.comments_wiki
       } | f in $forums$; f.id = $int32:i$ >>)
     | (None, None) -> Lwt.fail (Invalid_argument "Forum_sql.find_forum"))
  >>= fun r ->
  (match r with
     | [a] ->
         if not_deleted_only && (a#!deleted)
         then Lwt.fail Not_found
         else Lwt.return
           (get_forum_info
              (a#!id, a#!title, a#!descr, a#!arborescent, a#!deleted,
               a#!title_syntax, a#!messages_wiki, a#!comments_wiki)
           )
     | a::_ ->
         Ocsigen_messages.warning "Ocsimore: More than one forum have the same name or id (ignored)";
         if not_deleted_only && (a#!deleted)
         then Lwt.fail Not_found
         else Lwt.return
           (get_forum_info
              (a#!id, a#!title, a#!descr, a#!arborescent, a#!deleted,
               a#!title_syntax, a#!messages_wiki, a#!comments_wiki)
           )
     | _ -> Lwt.fail Not_found)

let raw_forum_from_sql sql =
  sql >>= (Lwt_list.map_p (fun sql ->
    Lwt.return (
      sql#!id,
      sql#!title,
      sql#!descr,
      sql#!arborescent,
      sql#!deleted,
      sql#!title_syntax,
      sql#!messages_wiki,
      sql#!comments_wiki
    )
  ))

let get_forums_list ?(not_deleted_only = true) () =
  Ocsi_sql.full_transaction_block
    (fun db ->
       (if not_deleted_only
        then raw_forum_from_sql (
          Lwt_Query.view db (<:view< {
            f.id;
            f.title;
            f.descr;
            f.arborescent;
            f.deleted;
            f.title_syntax;
            f.messages_wiki;
            f.comments_wiki
          } | f in $forums$; f.deleted = false >>)
        )
        else raw_forum_from_sql (
          Lwt_Query.view db (<:view< {
            f.id;
            f.title;
            f.descr;
            f.arborescent;
            f.deleted;
            f.title_syntax;
            f.messages_wiki;
            f.comments_wiki
          } | f in $forums$ >>)
        )
       )
    )

let raw_message_from_sql sql =
  sql >>= (Lwt_list.map_p (fun sql ->
    Lwt.return (
      sql#!id,
      sql#!creator_id,
      sql#!datetime,
      sql#?parent_id,
      sql#!root_id,
      sql#!forum_id,
      sql#?subject,
      sql#!wikibox,
      sql#!moderated,
      sql#!sticky,
      sql#!special_rights,
      sql#!tree_min,
      sql#!tree_max
    )
  ))

let get_childs ~message_id () =
  let message_id = sql_of_message message_id in
  Ocsi_sql.full_transaction_block
    (fun db -> raw_message_from_sql (
      Lwt_Query.view db (<:view< {
        f.id;
        f.creator_id;
        f.datetime;
        f.parent_id;
        f.root_id;
        f.forum_id;
        f.subject;
        f.wikibox;
        f.moderated;
        f.sticky;
        f.special_rights;
        f.tree_min;
        f.tree_max
      } order by {f.tree_min} | f in $forums_messages$; f.parent_id = $int32:message_id$ >>)
     )
    )

let get_message ~message_id () =
  let message_id = sql_of_message message_id in
  Forum_sql0.get_message_raw ~message_id ()
  >>= fun x -> Lwt.return (get_message_info x)


let get_thread ~message_id () =
  let message_id = sql_of_message message_id in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view db (<:view< {
        f.tree_min;
        f.tree_max
      } | f in $forums_messages$; f.id = $int32:message_id$ >>) >>= fun y ->
         (match y with
            | [] -> Lwt.fail Not_found
            | data :: _ -> raw_message_from_sql (
              Lwt_Query.view db (<:view< {
                f.id;
                f.creator_id;
                f.datetime;
                f.parent_id;
                f.root_id;
                f.forum_id;
                f.subject;
                f.wikibox;
                f.moderated;
                f.sticky;
                f.special_rights;
                f.tree_min;
                f.tree_max
              } order by {f.tree_min} |
                f in $forums_messages$; f.root_id = $int32:message_id$;
                f.tree_min >= $int32:data#!tree_min$;
                f.tree_max <= $int32:data#!tree_max$ >>)
            )
         )
    )


let get_message_list ~forum ~first ~number ~moderated_only () =
  let forum = sql_of_forum forum in
  let offset = Int64.sub first 1L in
  Ocsi_sql.full_transaction_block
    (fun db ->
       if moderated_only
       then raw_message_from_sql (
         Lwt_Query.view db (<:view< f order by f.tree_min desc
                                  limit $int64:number$ offset $int64:offset$ |
             f in $forums_messages$; f.forum_id = $int32:forum$;
             is_null f.parent_id;
             (f.moderated = true) || (f.special_rights = true) >>)
       )
       else raw_message_from_sql (
         Lwt_Query.view db (<:view< f order by f.datetime desc
                                  limit $int64:number$ offset $int64:offset$ |
             f in $forums_messages$; f.forum_id = $int32:forum$;
             is_null f.parent_id >>)
       )
    )

let get_wikibox_creator ~wb =
  let wb = Wiki_types.sql_of_wikibox wb in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view db (<:view< {
        f.creator_id
      } | f in $forums_messages$;
          (nullable (f.wikibox = $int32:wb$)) || (f.subject = $int32:wb$) >>)
    ) >>= function
      | [] -> Lwt.return None
      | a::_ -> Lwt.return (Some (User_sql.Types.userid_from_sql a#!creator_id))

let wikibox_is_moderated ~wb =
  let wb = Wiki_types.sql_of_wikibox wb in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view db (<:view< {
        f.moderated
      } | f in $forums_messages$;
          (nullable (f.wikibox = $int32:wb$)) || (f.subject = $int32:wb$) >>)
    ) >>= function
      | [] -> Lwt.return false (* ? *)
      | a::_ -> Lwt.return a#!moderated

let get_forums_id () =
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.view db (<:view< {
      f.id
    } | f in $forums$; >>)
  ) >>= (Lwt_list.map_s (fun id ->
    Lwt.return (Lwt_PGOCaml.string_of_int32 id#!id)
  ))

let get_forum_messages_id () =
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.view db (<:view< {
      f.id
    } | f in $forums_messages$; >>)
  ) >>= (Lwt_list.map_s (fun id ->
    Lwt.return (Lwt_PGOCaml.string_of_int32 id#!id)
  ))
