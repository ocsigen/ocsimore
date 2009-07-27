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

let get_message_raw ~message_id () =
  Lwt_pool.use Sql.pool 
    (fun db ->
       (* tree_min and tree_max are here only for the interface to be 
          compatible with get_thread *)
       PGSQL(db) "SELECT id, creator_id, datetime, parent_id, 
                         root_id, forum_id, subject, wikibox, \
                         moderated, sticky, special_rights, tree_min, tree_max \
                  FROM forums_messages \
                  WHERE forums_messages.id = $message_id")
  >>= function
    | [] -> Lwt.fail Not_found
    | x :: _ -> Lwt.return x

