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

class message_widget (widget_with_error_box : Widget.widget_with_error_box) 
  (add_message_service, moderate_message_service, delete_message_service) =
object (self)

  val msg_class = "ocsiforum_msg"
  val info_class = "ocsiforum_msg_info"
  val comment_class = "ocsiforum_comment_form"
  val not_moderated_class = "ocsiforum_not_moderated"

  method get_message ~sp ~sd ~message_id =
    Forum_data.get_message ~sp ~sd ~message_id
    
  method display_message ~classes content =
    let classes = Ocsimore_lib.build_class_attr (msg_class::classes) in
    Lwt.return
      {{ <div class={: classes :}>content }}

  method display_comment_line ~sp ~sd:_sd ~role:_role ?(rows = 3) ?(cols = 50)
    (message_id, _subjecto, _authorid, _datetime, _parent_id, _root_id,
     _forum_id, _content, _moderated, _deleted, _sticky, _, _) =
    let draw_form (actionnamename, ((parentname, _), (_, textname))) =
         {{ [<p>[
              {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                 ~name:parentname ~value:message_id () :}
              {: Eliom_duce.Xhtml.textarea ~name:textname ~rows ~cols () :}
              ]
              <p>[{: Eliom_duce.Xhtml.string_button ~name:actionnamename
                     ~value:"save" {{ "Send" }} :} ]
            ]
          }}
    in
    Lwt.return
      {{ [
         <span class={: comment_class :}>
           {: Ocamlduce.Utf8.make "Comment" :}
         <div class={: comment_class :}>[
           {: Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:add_message_service
              ~sp draw_form () :}]
         ] }}
(*
        let rec n1 = {{ <span class={: comment_class :}
                          onclick=(fun () -> show n1 n2) >
                          {: Ocamlduce.Utf8.make "Comment" :} }}
(*               onclick="caml_run_from_table(main_vm, 132)" *)
        and n2 =
          {{ <div class={: comment_class :}>[
               {: Eliom_duce.Xhtml.post_form
                  ~a:{{ { accept-charset="utf-8" } }}
                  ~service:add_message_service
                  ~sp draw_form () :}] }}
        in 
        {{ [ n1 n2 ] }}
*)

  method display_admin_line ~sp ~sd:_sd ~role 
    (message_id, _subjecto, _authorid, _datetime, parent_id, _root_id, _forum_id,
     _content, moderated, _deleted, _sticky, _, _) =

    let draw_moderate_form name =
         {{ [<p>[{: Eliom_duce.Xhtml.int32_button ~name:name
                    ~value:message_id {{ "Accept message" }} :} ]
            ]
          }}
    in

    let draw_delete_form name =
         {{ [<p>[{: Eliom_duce.Xhtml.int32_button ~name:name
                    ~value:message_id {{ "Delete message" }} :} ]
            ]
          }}
    in

    let first_msg = parent_id = None in
    (if not moderated
    then begin
      (if first_msg 
       then !!(role.Forum.message_moderators)
       else !!(role.Forum.comment_moderators)) >>= fun moderator ->
      let s = Ocamlduce.Utf8.make
        "This message has not been accepted by moderators yet." 
      in
      if moderator
      then
        let form = Eliom_duce.Xhtml.post_form
          ~service:moderate_message_service
          ~sp draw_moderate_form () 
        in
        Lwt.return {{ [ !s form ] }}
      else Lwt.return s
    end
    else Lwt.return {{ [] }}) >>= fun moderation_line ->
    (if first_msg 
     then !!(role.Forum.message_deletors)
     else !!(role.Forum.comment_deletors)) >>= fun deletor ->
    if deletor
    then 
      let form = Eliom_duce.Xhtml.post_form
        ~service:delete_message_service
        ~sp draw_delete_form () 
      in
      Lwt.return {{ [ !moderation_line form ] }}
    else Lwt.return moderation_line

  method pretty_print_message
    ~classes
    ~commentable ?(arborescent = true) ~sp ~sd ?rows ?cols
    ((message_id, subjecto, authorid, datetime, _parent_id, root_id, forum_id,
      content, moderated, _deleted, _sticky, _, _) as msg) =
    Users.get_user_fullname_by_id authorid >>= fun author ->
    Forum.get_role sp sd forum_id >>= fun role ->
    self#display_admin_line ~sp ~sd ~role msg >>= fun admin_line ->
    !!(role.Forum.comment_writers) >>= fun commentator ->
    let draw_comment_form =
      (commentable && 
         commentator && (arborescent || (message_id = root_id))) 
    in
    (if draw_comment_form
     then self#display_comment_line ~sp ~sd ~role ?rows ?cols msg
     else Lwt.return {{[]}}) >>= fun comment_line ->
    let classes = if moderated then classes else not_moderated_class::classes in
    Lwt.return
      (classes,
       {{ [
          !admin_line
          !{: match subjecto with
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
         ] }})

  method display ~sp ~sd ?rows ?cols
    ?(classes=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      (self#get_message ~sp ~sd ~message_id)
      (self#pretty_print_message ~classes ~commentable:false ~sp ~sd ?rows ?cols)
      (self#display_message)

end

class thread_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget) 
  (add_message_service, moderate_message_service, delete_message_service) =
object (self)

  val thr_class = "ocsiforum_thread"
  val thr_msg_class = "ocsiforum_thread_msg"

  method get_thread ~sp ~sd ~message_id =
    Forum_data.get_thread ~sp ~sd ~message_id
    
  method display_thread ~classes content =
    let classes = Ocsimore_lib.build_class_attr (thr_class::classes) in
    Lwt.return
      {{ <div class={: classes :}>content }}

  method pretty_print_thread ~classes ~commentable ~sp ~sd ?rows ?cols thread =
    let rec print_one_message_and_children ~arborescent thread : 
        (Xhtmltypes_duce.block * 'a list) Lwt.t = 
      (match thread with
         | [] -> Lwt.return ({{[]}}, [])
         | ((id, _subjecto, _authorid, _datetime, _parent_id, _root_id,
             _forum_id, _content, _moderated, _deleted, _sticky, _, _)
              as m)::l ->
             message_widget#pretty_print_message
               ~classes:[]
               ~commentable ~arborescent ~sp ~sd ?rows ?cols m
             >>= fun (classes, msg_info) ->
             message_widget#display_message ~classes msg_info >>= fun first ->
             print_children ~arborescent id l >>= fun (s, l) ->
             Lwt.return ({{ [first !s] }}, l))
      >>= fun (s, l) ->
      Lwt.return ({{ <div class={: thr_msg_class :}>s }}, l)
    and print_children ~arborescent pid = function
      | [] -> Lwt.return ({{ [] }}, [])
      | (((_, _, _, _, parent_id, _, _, _, _, _, _, _, _)::_) as th) 
          when parent_id = Some pid ->
          (print_one_message_and_children ~arborescent th >>= fun (b, l) ->
           print_children ~arborescent pid l >>= fun (s, l) ->
           Lwt.return (({{ [b !s] }} : Xhtmltypes_duce.flows), l))
      | l -> Lwt.return ({{ [] }}, l)
    in
    match thread with
      | [] -> Lwt.return (classes, {{[]}})
      | (_id, _subjecto, _authorid, _datetime, _parent_id, _root_id, forum_id, 
         _content, _moderated, _deleted, _sticky, _, _)::_l ->
          Forum_sql.get_forum ~forum_id ()
          >>= fun (_, _, _, arborescent, _deleted) ->
          print_one_message_and_children ~arborescent thread >>= fun (a, _) -> 
          Lwt.return (classes, {{[a]}})

  method display ?(commentable = true) ~sp ~sd ?rows ?cols
    ?(classes=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      (self#get_thread ~sp ~sd ~message_id)
      (self#pretty_print_thread ~classes ~commentable ~sp ~sd ?rows ?cols)
      (self#display_thread)

end

