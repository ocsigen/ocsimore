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

let register_wikiext ((message_widget : Forum_widgets.message_widget), thread_widget, message_list_widget) =
  let add_extension l ~name ~wiki_content f =
    List.iter (fun wp -> Wiki_syntax.add_extension ~wp ~name ~wiki_content f) l 
  in
  add_extension
    [Wiki_syntax.wikicreole_parser]
    ~name:"forum_message" ~wiki_content:true
    (fun bi args content ->
       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let message_id =
              Forum_types.message_of_string (List.assoc "message" args) 
            in
            message_widget#display
              ~sp ?classes
              ~data:message_id () >>= fun (b : Xhtmltypes_duce.block) ->
            Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )
    );

  add_extension
    [Wiki_syntax.wikicreole_parser]
    ~name:"forum_thread" ~wiki_content:true
    (fun bi args content ->
       Wikicreole.Block
         (let classes = 
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
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let message_id =
              Forum_types.message_of_string (List.assoc "message" args) 
            in
            thread_widget#display ?commentable:(Some true) ~sp
              ?rows ?cols ?classes
              ~data:message_id () >>= fun (b : Xhtmltypes_duce.block) ->
            Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )
    );

  add_extension
    [Wiki_syntax.wikicreole_parser]
    ~name:"forum_message_list" ~wiki_content:true
    (fun bi args content ->
       Wikicreole.Block
         (let classes = 
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
          let first = 
            try Int64.of_string (List.assoc "first" args)
            with Not_found | Failure _ -> 1L
          in
          let number = 
            try Int64.of_string (List.assoc "number" args)
            with Not_found | Failure _ -> 1000L
          in
          let add_message_form = 
            Some
              (try match List.assoc "addform" args with
                 | "false" -> false
                 | _ -> true
               with Not_found -> true)
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let forum =
              Forum_types.forum_of_string (List.assoc "forum" args) 
            in
            message_list_widget#display
              ~sp ?rows ?cols ?classes
              ~forum  ~first ~number
              ?add_message_form () >>= fun (b : Xhtmltypes_duce.block) ->
            Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )
    );

