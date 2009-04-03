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

let (>>=) = Lwt.bind
let (!!) = Lazy.force

class message_widget (widget_with_error_box : Widget.widget_with_error_box) =
object (self)

  val msg_class = "ocsiforum_msg"
  val info_class = "ocsiforum_msg_info"
  val comment_class = "ocsiforum_comment_line"

  method get_message ~sp ~sd ~message_id =
    Forum_data.get_message ~sp ~sd ~message_id
    
  method display_message ~classe content =
    let classe = Ocsimore_lib.build_class_attr (msg_class::classe) in
    Lwt.return
      {{ <div class={: classe :}>content }}

  method pretty_print_message ~commentable ~sp ~sd
    (_, subjecto, authorid, datetime, parent_id, root_id, forum_id,
     content, moderated, deleted, sticky) =
    Users.get_user_fullname_by_id authorid >>= fun author ->
    Forum.get_role sp sd forum_id >>= fun role ->
    !!(role.Forum.comment_writers) >>= fun commentator ->
    let comment_line =
      if commentator
      then
        {{ [
             <span class={: comment_class :}>
               {: Ocamlduce.Utf8.make "Comment" :}
           ] }}
      else {{[]}}
    in
    Lwt.return
      {{ [!{: match subjecto with
              | None -> {{ [] }} 
              | Some s -> 
                  let s = Ocamlduce.Utf8.make s in 
                  {{ [<h1>{: s :}] }} :}
          <span class={: info_class :}>
            {: Ocamlduce.Utf8.make
                 (Format.sprintf "posted by %s %s" 
                    author (Ocsimore_lib.sod datetime)) :}
          <p>{: content :}
          !comment_line
         ] }}

  method display ?(commentable = false) ~sp ~sd
    ?(classe=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      ~classe
      (self#get_message ~sp ~sd ~message_id)
      (self#pretty_print_message ~commentable ~sp ~sd)
      (self#display_message)

end

class thread_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget) =
object (self)

  val thr_class = "ocsiforum_thread"
  val thr_msg_class = "ocsiforum_thread_msg"

  method get_thread ~sp ~sd ~message_id =
    Forum_data.get_thread ~sp ~sd ~message_id
    
  method display_thread ~classe content =
    let classe = Ocsimore_lib.build_class_attr (thr_class::classe) in
    Lwt.return
      {{ <div class={: classe :}>content }}

  method pretty_print_thread ~commentable ~sp ~sd thread =
    let rec print_one_message_and_children thread : 
        (Xhtmltypes_duce.block * 'a list) Lwt.t = 
      (match thread with
         | [] -> Lwt.return ({{[]}}, [])
         | ((id, subjecto, authorid, datetime, parent_id, root_id, forum_id, 
             content, moderated, deleted, sticky) as m)::l ->
             message_widget#pretty_print_message ~commentable ~sp ~sd m
             >>= fun msg_info ->
             message_widget#display_message ~classe:[] msg_info >>= fun first ->
             print_children id l >>= fun (s, l) ->
             Lwt.return ({{ [first !s] }}, l))
      >>= fun (s, l) ->
      Lwt.return ({{ <div class={: thr_msg_class :}>s }}, l)
    and print_children pid = function
      | [] -> Lwt.return ({{ [] }}, [])
      | (((_, _, _, _, parent_id, _, _, _, _, _, _)::_) as th) 
          when parent_id = Some pid ->
          (print_one_message_and_children th >>= fun (b, l) ->
           print_children pid l >>= fun (s, l) ->
           Lwt.return (({{ [b !s] }} : Xhtmltypes_duce.flows), l))
      | l -> Lwt.return ({{ [] }}, l)
    in
    print_one_message_and_children thread >>= fun (a, _) -> 
    Lwt.return {{[a]}}

  method display ?(commentable = true) ~sp ~sd
    ?(classe=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      ~classe
      (self#get_thread ~sp ~sd ~message_id)
      (self#pretty_print_thread ~commentable ~sp ~sd)
      (self#display_thread)

end


