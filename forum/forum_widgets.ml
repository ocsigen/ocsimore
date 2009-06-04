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

open Forum_sql.Types

let (>>=) = Lwt.bind
let (!!) = Lazy.force

class add_message_widget
  (add_message_service, moderate_message_service) =
object (_self)

  val add_msg_class = "ocsiforum_add_message_form"

  method display
    ~sp ?forum ?parent ?(title = true) ?(rows = 3) ?(cols = 50) () =
    let draw_form (actionnamename,
                   ((parentname, forumname), (subjectname, textname))) =
      let num =
        match parent, forum with
          | Some p, _ ->
              Forum.eliom_message_input ~input_type:{: "hidden" :}
                ~name:parentname ~value:p ()
          | None, Some forum ->
              Forum.eliom_forum_input ~input_type:{: "hidden" :}
                ~name:forumname ~value:forum ()
          | _ -> failwith "Forum_widgets.add_message_widget"
      in
      let title = 
        if title
        then
          {{ [ 'Title:'
                 {: Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
                    ~name:subjectname () :}
               <br>[]
             ] }}
        else {{ [] }}
      in
      {{ [<p>[ num
             !title
             {: Eliom_duce.Xhtml.textarea ~name:textname ~rows ~cols () :}
           ]
           <p>[{: Eliom_duce.Xhtml.string_button ~name:actionnamename
                  ~value:"save" {{ "Send" }} :} ]
         ]
       }}
    in
    Eliom_duce.Xhtml.post_form
      ~a:{{ { accept-charset="utf-8" } }}
      ~service:add_message_service
      ~sp draw_form ()

end

class message_widget
  (widget_with_error_box : Widget.widget_with_error_box) 
  (wiki_widgets : Wiki_widgets_interface.interactive_wikibox)
  (add_message_service, moderate_message_service) =
object (self)

  val msg_class = "ocsiforum_msg"
  val info_class = "ocsiforum_msg_info"
  val not_moderated_class = "ocsiforum_not_moderated"

  method get_message ~sp ~message_id =
    Forum_data.get_message ~sp ~message_id
    
  method display_message ~classes content =
    let classes = Ocsimore_lib.build_class_attr (msg_class::classes) in
    Lwt.return
      {{ <div class={: classes :}>content }}

  method display_admin_line ~sp ~role m =

    let draw_moderate_form name =
         {{ [<p>[{: Forum.eliom_message_button ~name:name
                    ~value:m.m_id {{ "Accept message" }} :} ]
            ]
          }}
    in

    let first_msg = m.m_parent_id = None in
    (if not m.m_moderated
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
    Lwt.return moderation_line

  method pretty_print_message
    ~classes
    ?(arborescent:_ = true) ~sp m =
    User_sql.get_basicuser_data m.m_creator_id >>= fun ud ->
    let author = ud.User_sql.Types.user_fullname in
    Forum.get_role sp m.m_forum >>= fun role ->
    self#display_admin_line ~sp ~role m >>= fun admin_line ->
    let classes = 
      if m.m_moderated then classes else not_moderated_class::classes 
    in
    Wiki_sql.wikibox_from_uid m.m_wikibox >>= fun (wiki, box) ->
    Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
    let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
    let bi = Wiki_widgets_interface.default_bi ~sp ~wikibox:(wiki, box) ~rights
    in
    wiki_widgets#display_interactive_wikibox ~bi (wiki, box) >>= fun wikibox ->
    Lwt.return
      (classes,
       {{ [
          !admin_line
          !{: match m.m_subject with
              | None -> {{ [] }} 
              | Some s -> 
                  let s = Ocamlduce.Utf8.make s in 
                  {{ [<h1>{: s :}] }} :}
          <span class={: info_class :}>
            {: Ocamlduce.Utf8.make
                 (Format.sprintf "posted by %s %s" 
                    author (Ocsimore_lib.sod m.m_datetime)) :}
          wikibox
         ] }})

  method display ~sp ?(classes=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      (self#get_message ~sp ~message_id)
      (self#pretty_print_message ~classes ~sp)
      (self#display_message)

end

class thread_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget) 
  (add_message_widget : add_message_widget)
  (add_message_service, moderate_message_service) =
object (self)

  val thr_class = "ocsiforum_thread"
  val thr_msg_class = "ocsiforum_thread_msg"
  val comment_class = "ocsiforum_comment_form"

  method get_thread ~sp ~message_id =
    Forum_data.get_thread ~sp ~message_id
    
  method display_thread ~classes content =
    let classes = Ocsimore_lib.build_class_attr (thr_class::classes) in
    Lwt.return
      {{ <div class={: classes :}>content }}

  method display_comment_line ~sp ~role:_role ?rows ?cols m =
(*    Lwt.return
       {{ [
         <span class={: comment_class :}>
           {: Ocamlduce.Utf8.make "Comment" :}
         <div class={: comment_class :}>[
           {: Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:add_message_service
              ~sp draw_form () :}]
         ] }} *)
  let n1_id = Eliom_obrowser.fresh_id () in
  let n2_id = Eliom_obrowser.fresh_id () in
  let form = 
    add_message_widget#display ~sp ~parent:m.m_id ~title:false ?rows ?cols () 
  in
  let rec n1 = {{ <span class={: comment_class :}
                    id={: n1_id :}
                    onclick={: "caml_run_from_table(main_vm, 1, "
                             ^Eliom_obrowser.jsmarshal (n1_id, n2_id)^")" :} >
                    {: Ocamlduce.Utf8.make "Comment" :} }}
  and n2 = {{ <div id={: n2_id :} class={: comment_class :} >[ form ] }}
  in 
  Lwt.return {{ [ n1 n2 ] }}

  method pretty_print_thread ~classes ~commentable ~sp ?rows ?cols thread =
    let rec print_one_message_and_children
        ~role ~arborescent ~commentable thread : 
        (Xhtmltypes_duce.block * 'a list) Lwt.t = 
      (match thread with
         | [] -> Lwt.return ({{[]}}, [])
         | m::l ->
             message_widget#pretty_print_message
               ~classes:[]
               ~arborescent ~sp m
             >>= fun (classes, msg_info) ->
             let draw_comment_form =
               (commentable && (arborescent || (m.m_id = m.m_root_id)))
             in
             (if draw_comment_form
              then self#display_comment_line ~sp ~role ?rows ?cols m
              else Lwt.return {{[]}}) >>= fun comment_line ->
             message_widget#display_message ~classes msg_info >>= fun first ->
             print_children ~role ~arborescent ~commentable m.m_id l
             >>= fun (s, l) ->
             Lwt.return ({{ [first !comment_line !s] }}, l))
      >>= fun (s, l) ->
      Lwt.return ({{ <div class={: thr_msg_class :}>s }}, l)
    and print_children ~role ~arborescent ~commentable pid = function
      | [] -> Lwt.return ({{ [] }}, [])
      | ((m::_) as th) 
          when m.m_parent_id = Some pid ->
          (print_one_message_and_children ~role ~arborescent ~commentable th
           >>= fun (b, l) ->
           print_children ~role ~arborescent ~commentable pid l
           >>= fun (s, l) ->
           Lwt.return (({{ [b !s] }} : Xhtmltypes_duce.flows), l))
      | l -> Lwt.return ({{ [] }}, l)
    in
    match thread with
      | [] -> Lwt.return (classes, {{[]}})
      | m::_l ->
          Forum_sql.get_forum ~forum:m.m_forum () >>= fun forum ->
          Forum.get_role sp m.m_forum >>= fun role ->
          !!(role.Forum.comment_creators) >>= fun commentator ->
          print_one_message_and_children
            ~role
            ~arborescent:forum.f_arborescent
            ~commentable:(commentable && commentator) thread
          >>= fun (a, _) -> 
          Lwt.return (classes, {{[a]}})

  method display ?(commentable = true) ~sp ?rows ?cols
    ?(classes=[]) ~data:message_id () =
    widget_with_error_box#bind_or_display_error
      (self#get_thread ~sp ~message_id)
      (self#pretty_print_thread ~classes ~commentable ~sp ?rows ?cols)
      (self#display_thread)

end

class message_list_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget) 
  (add_message_widget : add_message_widget) 
  =
object (self)

  val ml_class = "ocsiforum_message_list"

  method get_message_list ~sp ~forum ~first ~number =
    Forum_data.get_message_list ~sp ~forum ~first ~number ()
    
  method display_message_list ~classes content =
    let classes = Ocsimore_lib.build_class_attr (ml_class::classes) in
    Lwt.return
      {{ <div class={: classes :}>content }}

  method pretty_print_message_list ~forum ?rows ?cols ~classes ~sp 
    ~add_message_form list =
    Lwt_util.map
      (fun raw_msg_info -> 
         message_widget#pretty_print_message
           ~classes:[] ~sp
           (Forum_sql.Types.get_message_info raw_msg_info)
         >>= fun (classes, content) ->
         message_widget#display_message ~classes content)
      list
    >>= fun l ->
    let form =
      if add_message_form
      then {{ [ {: add_message_widget#display ~sp ~forum ?rows ?cols () :} ] }}
      else {{ [] }}
    in
    Lwt.return (classes, {{ [ !{: l :} !form ] }})

  method display ~sp ?(rows : int option) ?(cols : int option) ?(classes=[])
    ~forum ~first ~number ?(add_message_form = true) () =
    widget_with_error_box#bind_or_display_error
      (self#get_message_list ~sp ~forum ~first ~number)
      (self#pretty_print_message_list
         ~forum ?rows ?cols ~classes ~sp ?add_message_form)
      (self#display_message_list)

end

