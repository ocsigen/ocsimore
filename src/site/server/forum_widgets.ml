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

open Eliom_pervasives
open Forum_types

let (>>=) = Lwt.bind
let (!!) = Lazy.force


let forum_css_header =
  Page_site.Header.create_header
    (fun () ->
       [Eliom_output.Html5.css_link
          (Page_site.static_file_uri ["ocsiforumstyle.css"]) ()
       ]
    )

let add_forum_css_header () =
  Page_site.Header.require_header forum_css_header


class add_message_widget
  (add_message_service, moderate_message_service) =
object (_self)

  val add_msg_class = "ocsiforum_add_message_form"

  method display
    ?forum ?parent ?(title = true) ?(rows = 3) ?(cols = 50) ()
    : HTML5_types.form Eliom_pervasives.HTML5.M.elt =
    add_forum_css_header ();
    let draw_form (actionnamename,
                   ((parentname, forumname), (subjectname, textname))) =
      let num =
        match parent, forum with
          | Some p, _ ->
              Forum.eliom_message_input ~input_type:`Hidden
                ~name:parentname ~value:p ()
          | None, Some forum ->
              Forum.eliom_forum_input ~input_type:`Hidden
                ~name:forumname ~value:forum ()
          | _ -> failwith "Forum_widgets.add_message_widget"
      in
      let title =
        if title
        then
          [ HTML5.M.pcdata "Title:";
            Eliom_output.Html5.string_input ~input_type:`Text
              ~name:subjectname ();
            HTML5.M.br () ]
        else []
      in
      [ HTML5.M.p
	  ( num ::
              (title @
		 [ Eliom_output.Html5.textarea ~name:textname ~rows ~cols () ]) );
        HTML5.M.p[ Eliom_output.Html5.string_button ~name:actionnamename
                     ~value:"save" [HTML5.M.pcdata "Send"] ]
      ]
    in
    Eliom_output.Html5.post_form
      ~a:[HTML5.M.a_accept_charset ["utf-8"]]
      ~service:add_message_service
      draw_form ()

end

class message_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (wiki_widgets : Wiki_widgets_interface.interactive_wikibox)
  (wiki_phrasing_widgets : Wiki_widgets_interface.interactive_wikibox)
  (add_message_service, moderate_message_service) =
object (self)

  val msg_class = "ocsiforum_msg"
  val info_class = "ocsiforum_msg_info"
  val not_moderated_class = "ocsiforum_not_moderated"

  method get_message ~message_id =
    Forum_data.get_message ~message_id

  method display_message ~classes content =
    let classes = msg_class::classes in
    Lwt.return
      (HTML5.M.div ~a:[HTML5.M.a_class classes] content)

  method display_admin_line ~role m =

    let draw_moderate_form name =
      [HTML5.M.p [Forum.eliom_message_button ~name:name
                     ~value:m.m_id [HTML5.M.pcdata "Accept message"]]]
    in

    let first_msg = m.m_parent_id = None in
    (if not m.m_moderated
    then begin
      (if first_msg
       then !!(role.Forum.message_moderators)
       else !!(role.Forum.comment_moderators)) >>= fun moderator ->
      let s = "This message has not been accepted by moderators yet."
      in
      if moderator
      then
        let form = Eliom_output.Html5.post_form
          ~service:moderate_message_service
          draw_moderate_form ()
        in
        Lwt.return [ HTML5.M.pcdata s; form ]
      else Lwt.return [ HTML5.M.pcdata s ]
    end
    else Lwt.return []) >>= fun moderation_line ->
    Lwt.return moderation_line

  method pretty_print_message ~classes m =
    User_sql.get_basicuser_data m.m_creator_id >>= fun ud ->
    let author = ud.User_sql.Types.user_fullname in
    Forum.get_role m.m_forum >>= fun role ->
    self#display_admin_line ~role m >>= fun admin_line ->
    let classes =
      if m.m_moderated then classes else not_moderated_class::classes
    in
    Wiki_sql.wikibox_wiki m.m_wikibox >>= fun wiki ->
    Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
    let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
    Wiki.default_bi ~wikibox:m.m_wikibox ~rights >>= fun bi ->
    (match m.m_subject with
       | None -> Lwt.return []
       | Some s ->
           Wiki.default_bi ~wikibox:s ~rights >>= fun bi ->
           let bi = { bi with Wiki_widgets_interface.bi_menu_style = `Pencil }
           in
           wiki_phrasing_widgets#display_interactive_wikibox ~bi s)
    >>= fun wikiboxsubject ->
    let bi = { bi with Wiki_widgets_interface.bi_menu_style = `Pencil } in
    wiki_widgets#display_interactive_wikibox ~bi m.m_wikibox >>= fun wikibox ->
    Lwt.return
      (classes,
       List.flatten
	 [ admin_line;
	   (wikiboxsubject :> HTML5_types.flow5
	      HTML5.M.elt list);
	   [ HTML5.M.span ~a:[HTML5.M.a_class ["info_class"]]
               [ HTML5.M.pcdata
		   (Format.sprintf "posted by %s %s" author
		      (CalendarLib.Printer.CalendarPrinter.to_string m.m_datetime))]];
	   (wikibox :> HTML5_types.flow5 HTML5.M.elt list) ]
      )

  method display ?(classes=[]) ~data:message_id ()
    : HTML5_types.div HTML5.M.elt Lwt.t =
    add_forum_css_header ();
    widget_with_error_box#bind_or_display_error
      (self#get_message ~message_id)
      (self#pretty_print_message ~classes)
    >>= fun (classes, r) -> self#display_message ~classes r

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
  val comment_button_class = "ocsiforum_comment_button"
  val main_msg_class = "ocsiforum_main_message"
  val comments_class = "ocsiforum_comments"

  method get_thread ~message_id =
    Forum_data.get_thread ~message_id

  method display_thread ~classes ((first : HTML5_types.flow5 HTML5.M.elt list), coms) : 'a Lwt.t =
    let classes = thr_class::classes in
    Lwt.return
      (HTML5.M.div ~a:[HTML5.M.a_class classes] ( first @ coms ))

  method display_thread_splitted ~classes ((first : HTML5_types.flow5 HTML5.M.elt list ), coms) =
    let classes1 = (main_msg_class::classes) in
    let classes2 = (comments_class::classes) in
    Lwt.return
      (HTML5.M.div ~a:[HTML5.M.a_class classes1] first,
       HTML5.M.div ~a:[HTML5.M.a_class classes2] coms)

  method display_comment_line ~role ?rows ?cols m =
(*    Lwt.return
       {{ [
         <span class={: comment_class :}>
           {: Ocamlduce.Utf8.make "Comment" :}
         <div class={: comment_class :}>[
           {: Eliom_duce.Html5.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:add_message_service
              ~sp draw_form () :}]
         ] }} *)
  !!(role.Forum.comment_creators) >>= fun comment_creators ->
  if comment_creators
  then
    (* TODO obrowser -> js_of_ocaml
    let n1_id = Eliom_obrowser.fresh_id () in
    let n2_id = Eliom_obrowser.fresh_id () in
    *)
    let form =
      add_message_widget#display ~parent:m.m_id ~title:false ?rows ?cols ()
    in
    let rec n1 =
      HTML5.M.div
	~a:[HTML5.M.a_class [comment_button_class];
            (* TODO obrowser -> js_of_ocaml
	      HTML5.M.a_id n1_id;
              HTML5.M.a_onclick (Forum_client_calls.switchshow n1_id n2_id)
	    *)
	   ]
	[HTML5.M.pcdata "Comment" ]
    and n2 =
      HTML5.M.div
	~a:[HTML5.M.a_class [comment_class];
	    HTML5.M.a_style "display:block";
	   (* TODO obrowser -> js_of_ocaml
              HTML5.M.a_id n2_id
	   *)
	   ]
	[ form ]
    in
    Lwt.return [ n1; n2 ]
  else Lwt.return []


  method pretty_print_thread ~classes ~commentable ?rows ?cols thread :
    (string list * (HTML5_types.flow5 HTML5.M.elt list * HTML5_types.flow5 HTML5.M.elt list)) Lwt.t =
    add_forum_css_header ();
    let rec print_one_message_and_children
        ~role ~arborescent ~commentable thread :
        (HTML5_types.flow5 HTML5.M.elt list * HTML5_types.flow5 HTML5.M.elt list * Forum_types.message_info list) Lwt.t =
      (match thread with
         | [] -> Lwt.return ([], [], [])
         | m::l ->
             message_widget#pretty_print_message
               ~classes:[]
               m
             >>= fun (classes, msg_info) ->
             let draw_comment_form =
               (commentable && (arborescent || (m.m_id = m.m_root_id)))
             in
             (if draw_comment_form
              then self#display_comment_line ~role ?rows ?cols m
              else Lwt.return []) >>= fun comment_line ->
             message_widget#display_message ~classes msg_info >>= fun first ->
             print_children ~role ~arborescent ~commentable m.m_id l
             >>= fun (s, l) ->
             Lwt.return ([(first:>HTML5_types.flow5 HTML5.M.elt)],
			 (comment_line @ s :> HTML5_types.flow5 HTML5.M.elt list), l))
    and print_children ~role ~arborescent ~commentable pid = function
      | [] -> Lwt.return ([], [])
      | ((m::_) as th)
          when m.m_parent_id = Some pid ->
          (print_one_message_and_children ~role ~arborescent ~commentable th
           >>= fun (b, c, l) ->
           print_children ~role ~arborescent ~commentable pid l
           >>= fun (s, l) ->
           Lwt.return (( (b @ c @ s)  : HTML5_types.flow5 HTML5.M.elt list), l))
      | l -> Lwt.return ([], l)
    in
    match thread with
      | [] -> Lwt.return (classes, ([], []))
      | m::_l ->
          Forum_sql.get_forum ~forum:m.m_forum () >>= fun forum ->
          Forum.get_role m.m_forum >>= fun role ->
          !!(role.Forum.comment_creators) >>= fun commentator ->
          print_one_message_and_children
            ~role
            ~arborescent:forum.f_arborescent
            ~commentable:(commentable && commentator) thread
          >>= fun (msg, coms, _) ->
          Lwt.return (classes, (msg, coms))

  method display ?(commentable = true) ?rows ?cols
    ?(classes=[]) ~data:message_id () : HTML5_types.flow5 HTML5.M.elt Lwt.t =
    add_forum_css_header ();
    let data = self#get_thread ~message_id in
    let transform_data =
      self#pretty_print_thread ~classes ~commentable ?rows ?cols
    in
    (Lwt.catch
       (fun () -> data >>= transform_data)
       (fun exc ->
          let e =
            ( [ widget_with_error_box#display_error_box ~exc () ] :> HTML5_types.flow5 HTML5.M.elt list )
          in
          Lwt.return ([widget_with_error_box#error_class], (e, e)) ))
    >>= fun (classes, c) ->
    self#display_thread ~classes c

  method display_splitted ?(commentable = true) ?rows ?cols
    ?(classes=[]) ~data:message_id () :
    ( HTML5_types.div Eliom_pervasives.HTML5.M.elt *
        HTML5_types.div Eliom_pervasives.HTML5.M.elt )
    Lwt.t
    =
    add_forum_css_header ();
    let data = self#get_thread ~message_id in
    let transform_data =
      self#pretty_print_thread ~classes ~commentable ?rows ?cols
    in
    (Lwt.catch
       (fun () -> data >>= transform_data)
       (fun exc ->
          let e =
            ( [ widget_with_error_box#display_error_box ~exc () ]
	      :> HTML5_types.flow5 HTML5.M.elt list )
          in
          Lwt.return ([widget_with_error_box#error_class], (e, e)) ))
    >>= fun (classes, c) ->
    self#display_thread_splitted ~classes c


end

class message_list_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget)
  (add_message_widget : add_message_widget)
  =
object (self)

  val ml_class = "ocsiforum_message_list"

  method get_message_list ~forum ~first ~number =
    Forum_data.get_message_list ~forum ~first ~number ()

  method display_message_list ~classes content =
    let classes = (ml_class::classes) in
    Lwt.return
      (HTML5.M.div ~a:[HTML5.M.a_class classes] content : HTML5_types.flow5 HTML5.M.elt)

  method pretty_print_message_list ~forum ?rows ?cols ~classes
    ~add_message_form list =
    Lwt_util.map
      (fun raw_msg_info ->
         Lwt.catch
         (fun() ->
            Forum_data.message_info_of_raw_message raw_msg_info
            >>= fun m ->
            message_widget#pretty_print_message ~classes:[] m
            >>= fun (classes, content) ->
            message_widget#display_message ~classes content >>= fun m ->
            Lwt.return [ m ]
         )
         (function
            | Ocsimore_common.Permission_denied ->
                Lwt.return []
            | e -> Lwt.fail e))
      list
    >>= fun l ->
    (if add_message_form
     then
       Forum.get_role forum >>= fun role ->
       !!(role.Forum.message_creators) >>= fun message_creators ->
       Lwt.return
         (if message_creators
          then
            [ add_message_widget#display ~forum ?rows ?cols () ]
          else [])
     else Lwt.return [])
    >>= fun form ->
    Lwt.return (classes,
		(List.flatten l :> HTML5_types.flow5 HTML5.M.elt list)
		@ ( form :> HTML5_types.flow5 HTML5.M.elt list) )

  method display ?(rows : int option) ?(cols : int option) ?(classes=[])
    ~forum ~first ~number ?(add_message_form = true) () =
    add_forum_css_header ();
    widget_with_error_box#bind_or_display_error
      (self#get_message_list ~forum ~first ~number)
      (self#pretty_print_message_list
         ~forum ?rows ?cols ~classes ?add_message_form)
    >>= fun (classes, r) ->  self#display_message_list ~classes r

end
