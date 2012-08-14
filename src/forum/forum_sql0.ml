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

open Eliom_lib.Lwt_ops
open User_sql.Types
open Ocsi_sql

let forums_messages = (<:table< forums_messages (
  id integer NOT NULL,
  creator_id integer NOT NULL,
  datetime timestamp NOT NULL,
  parent_id integer,
  root_id integer NOT NULL,
  forum_id integer NOT NULL,
  subject integer,
  wikibox integer NOT NULL,
  moderated boolean NOT NULL,
  sticky boolean NOT NULL,
  special_rights boolean NOT NULL,
  tree_min integer NOT NULL,
  tree_max integer NOT NULL
) >>)

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

let get_message_raw ~message_id () =
  Lwt_pool.use Ocsi_sql.pool
    (fun db -> raw_message_from_sql (
       (* tree_min and tree_max are here only for the interface to be
          compatible with get_thread *)
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
      } | f in $forums_messages$; f.id = $int32:message_id$ >>)
     )
    )
  >>= function
    | [] -> Lwt.fail Not_found
    | x :: _ -> Lwt.return x

