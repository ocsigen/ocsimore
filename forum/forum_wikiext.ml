(* Ocsimore
 * Copyright (C) 2009
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
   @author Vincent Balat
   @author Boris Yakobowski
*)

let (>>=) = Lwt.bind

let register_wikiext (message_widget, thread_widget) =

  Wiki_syntax.add_block_extension "forum_message"
    (fun wiki_id bi args content -> 
       let classe = 
         try Some [List.assoc "class" args]
         with Not_found -> None
       in
       let rows = 
         try Some (int_of_string (List.assoc "rows" args))
         with Not_found | Failure _ -> None
       in
       let cols = 
         try Some (int_of_string (List.assoc "cols" args))
         with Not_found | Failure _ -> None
       in
       try
         let sp = bi.Wiki_syntax.bi_sp in
         let sd = bi.Wiki_syntax.bi_sd in
         let message_id = Int32.of_string (List.assoc "message" args) in
         message_widget#display
           ?commentable:(Some false) ~sp ~sd ?rows ?cols ?classe
           ~data:message_id () >>= fun (b : Xhtmltypes_duce.block) ->
         Lwt.return {{ [ {: b :} ] }}
       with Not_found | Failure _ -> 
         let s = Wiki_syntax.string_of_extension "raw" args content in
         Lwt.return {{ [ <b>{: s :} ] }}
    );

  Wiki_syntax.add_block_extension "forum_thread"
    (fun wiki_id bi args content ->
       let classe = 
         try Some [List.assoc "class" args]
         with Not_found -> None
       in
       let rows = 
         try Some (int_of_string (List.assoc "rows" args))
         with Not_found | Failure _ -> None
       in
       let cols = 
         try Some (int_of_string (List.assoc "cols" args))
         with Not_found | Failure _ -> None
       in
       try
         let sp = bi.Wiki_syntax.bi_sp in
         let sd = bi.Wiki_syntax.bi_sd in
         let message_id = Int32.of_string (List.assoc "message" args) in
         thread_widget#display ?commentable:(Some true) ~sp ~sd
           ?rows ?cols ?classe
           ~data:message_id () >>= fun (b : Xhtmltypes_duce.block) ->
         Lwt.return {{ [ {: b :} ] }}
       with Not_found | Failure _ -> 
         let s = Wiki_syntax.string_of_extension "raw" args content in
         Lwt.return {{ [ <b>{: s :} ] }}
    )
