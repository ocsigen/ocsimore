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

open Eliom_content
open Forum_types

let (>>=) = Lwt.bind
let (!!) = Lazy.force

let forum_css_header =
  Page_site.Header.create_header
    (fun () ->
       [Html5.D.css_link
          ~uri:(Page_site.static_file_uri ~path:["ocsiforumstyle.css"]) ()
       ]
    )

let add_forum_css_header () =
  Page_site.Header.require_header forum_css_header

let last_msg_date msg_list =
  let max_date t1 t2 = if CalendarLib.Calendar.compare t1 t2 < 0 then t2 else t1 in
  List.fold_left (fun acc m -> max_date acc m.m_datetime)
    (CalendarLib.Calendar.from_jd 0.) msg_list

class add_message_widget services =
object (_self)

  val add_msg_class = "ocsiforum_add_message_form"

  method display
    : 'a. ?forum:_ -> ?parent:_ -> ?title:_ -> ?rows:_ -> ?cols:_ -> _ ->
      ([> Html5_types.form ] as 'a) Html5.F.elt Lwt.t =
    fun ?forum ?parent ?(title = true) ?(rows = 3) ?(cols = 50) () ->
    lwt () = add_forum_css_header () in
    let draw_form (actionnamename, ((parentname, forumname), (subjectname, textname))) =
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
          [ Html5.F.pcdata "Title:";
            Html5.D.string_input ~input_type:`Text
              ~name:subjectname ();
            Html5.F.br () ]
        else []
      in
      [ Html5.F.p
          ( num ::
              (title @
                 [ Html5.D.textarea ~name:textname () ]) );
        Html5.F.p[ Html5.D.string_button ~name:actionnamename
                     ~value:"save" [Html5.F.pcdata "Send"] ]
      ]
    in
    Lwt.return
      (Html5.D.post_form
        ~a:[Html5.F.a_accept_charset ["utf-8"]]
        ~service:services.add_message_service
        draw_form ())

end

class message_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (wiki_widgets : Wiki_widgets_interface.interactive_wikibox)
  (wiki_phrasing_widgets : Wiki_widgets_interface.interactive_wikibox)
  services =

  let xhtml_of_wb wiki wikibox =
    lwt wiki_info = Wiki_sql.get_wiki_info_by_id ~id:wiki in
    lwt rights = Wiki_models.get_rights
      wiki_info.Wiki_types.wiki_model in
    lwt bi = Wiki.default_bi ~wikibox ~rights in
    lwt content =
      wiki_widgets#display_frozen_wikibox
        ~bi
        wikibox
    in
    (*!!! ugly cast: html5 to xhtml !!!*)
    Lwt.return ( Xhtml.F.totl (Html5.F.toeltl content) )
  in

  let atom_title forum_info msg_info =
    match msg_info.m_subject with
      | None -> Lwt.return (Atom_feed.plain "")
      | Some subject ->
        lwt subject = xhtml_of_wb forum_info.f_comments_wiki subject in
        Lwt.return (Atom_feed.xhtml subject)
  in

object (self)

  val msg_class = "ocsiforum_msg"
  val info_class = "ocsiforum_msg_info"
  val not_moderated_class = "ocsiforum_not_moderated"

  method get_message ~message_id =
    Forum_data.get_message ~message_id

  method display_message
    : 'a. classes:_ -> _ -> ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ~classes content ->
    let classes = msg_class::classes in
    Lwt.return
      (Html5.F.div ~a:[Html5.F.a_class classes] content)

  method display_admin_line ~role m =

    let draw_moderate_form name =
      [Html5.F.p [Forum.eliom_message_button ~name:name
                     ~value:m.m_id [Html5.F.pcdata "Accept message"]]]
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
        let form = Html5.D.post_form
          ~service:services.moderate_message_service
          draw_moderate_form ()
        in
        Lwt.return [ Html5.F.pcdata s; form ]
      else Lwt.return [ Html5.F.pcdata s ]
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
    Wiki_sql.get_wiki_info_by_id ~id:wiki >>= fun wiki_info ->
    lwt rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
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
           (wikiboxsubject :> Html5_types.flow5_without_header_footer
              Html5.F.elt list);
           [ Html5.F.span ~a:[Html5.F.a_class ["info_class"]]
               [ Html5.F.pcdata
                   (Format.sprintf "posted by %s %s" author
                      (CalendarLib.Printer.CalendarPrinter.to_string m.m_datetime))]];
           (wikibox :> Html5_types.flow5_without_header_footer Html5.F.elt list) ]
      )

  method display
    : 'a. ?classes:_ -> data:_ -> unit -> ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ?(classes=[]) ~data:message_id () ->
    lwt () = add_forum_css_header () in
    widget_with_error_box#bind_or_display_error
      (self#get_message ~message_id)
      (self#pretty_print_message ~classes :> Forum_types.message_info ->
         (string list * Html5_types.flow5 Html5.F.elt list)
         Lwt.t)
    >>= fun (classes, r) -> self#display_message ~classes r

  (** atom feed generation *)

  method atom_entry msg_info =
    lwt forum_info = Forum_data.get_forum ~forum:msg_info.m_forum () in
    lwt title = atom_title forum_info msg_info in
    lwt content =
      lwt content = xhtml_of_wb forum_info.f_messages_wiki
        msg_info.m_wikibox in
      Lwt.return (Atom_feed.xhtmlC content)
    in
    let id = Xml.uri_of_fun (fun () -> Int32.to_string (Opaque.t_int32 msg_info.m_id)) in
    Lwt.return
      ( Atom_feed.entry ~updated:msg_info.m_datetime ~id ~title [content] )

  method atom_entries msg_list =
    try_lwt
      Lwt_list.map_s (fun msg_info -> self#atom_entry msg_info) msg_list
    with
      | Ocsimore_common.Permission_denied -> Lwt.return []
      | e -> Lwt.fail e

  method atom_childs ~message =
    lwt msg_list = Forum_data.get_childs ~message_id:message in
    let id = Xml.uri_of_fun
      (fun () -> "message_"^(Int32.to_string (Opaque.t_int32 message))) in
    lwt entries = self#atom_entries msg_list in
    let updated = last_msg_date msg_list in
    lwt msg_info = Forum_data.get_message ~message_id:message in
    lwt forum_info = Forum_data.get_forum ~forum:msg_info.m_forum () in
    lwt title = atom_title forum_info msg_info in
    Lwt.return (Atom_feed.feed ~id ~updated ~title entries)

end

class thread_widget
  (widget_with_error_box : Widget.widget_with_error_box)
  (message_widget : message_widget)
  (add_message_widget : add_message_widget)
  services =
object (self)

  val thr_class = "ocsiforum_thread"
  val thr_msg_class = "ocsiforum_thread_msg"
  val comment_class = "ocsiforum_comment_form"
  val comment_button_class = "ocsiforum_comment_button"
  val main_msg_class = "ocsiforum_main_message"
  val comments_class = "ocsiforum_comments"

  method get_thread ~message_id =
    Forum_data.get_thread ~message_id

  method display_thread :
    'a. classes:_ -> _ -> ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ~classes ((first : Html5_types.flow5 Html5.F.elt list), coms) ->
    let classes = thr_class::classes in
    Lwt.return
      (Html5.F.div ~a:[Html5.F.a_class classes] ( first @ coms ))

  method display_thread_splitted ~classes ((first : Html5_types.flow5 Html5.F.elt list ), coms) =
    let classes1 = (main_msg_class::classes) in
    let classes2 = (comments_class::classes) in
    Lwt.return
      (Html5.F.div ~a:[Html5.F.a_class classes1] first,
       Html5.F.div ~a:[Html5.F.a_class classes2] coms)

  method display_comment_line ~role ?rows ?cols m =
  !!(role.Forum.comment_creators) >>= fun comment_creators ->
  if comment_creators
  then
    lwt form =
      add_message_widget#display ~parent:m.m_id ~title:false ?rows ?cols ()
    in
    let comment_div =
      Html5.D.div ~a:[Html5.D.a_class [comment_class]] [ form ]
    in
    let show_form =
      Html5.F.div
        ~a:[Html5.F.a_class [comment_button_class];
            Html5.F.a_onclick
              {{ ignore (
                (Eliom_content.Html5.To_dom.of_div %comment_div)##classList
                  ##toggle(Js.string "showcomment"):bool Js.t)
              }} ]
        [Html5.F.pcdata "Comment" ]
    in
    Lwt.return [ show_form; comment_div ]
  else Lwt.return []

  method pretty_print_thread ~classes ~commentable ?rows ?cols thread :
    (string list * (Html5_types.flow5 Html5.F.elt list * Html5_types.flow5 Html5.F.elt list)) Lwt.t =
    lwt () = add_forum_css_header () in
    let rec print_one_message_and_children
        ~role ~arborescent ~commentable thread :
        (Html5_types.flow5 Html5.F.elt list * Html5_types.flow5 Html5.F.elt list * Forum_types.message_info list) Lwt.t =
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
             message_widget#display_message ~classes (msg_info:> Html5_types.flow5 Html5.F.elt list)  >>= fun first ->
             print_children ~role ~arborescent ~commentable m.m_id l
             >>= fun (s, l) ->
             Lwt.return ([(first:>Html5_types.flow5 Html5.F.elt)],
                         (comment_line @ s :> Html5_types.flow5 Html5.F.elt list), l))
    and print_children ~role ~arborescent ~commentable pid = function
      | [] -> Lwt.return ([], [])
      | ((m::_) as th)
          when m.m_parent_id = Some pid ->
          (print_one_message_and_children ~role ~arborescent ~commentable th
           >>= fun (b, c, l) ->
           print_children ~role ~arborescent ~commentable pid l
           >>= fun (s, l) ->
           Lwt.return (( (b @ c @ s)  : Html5_types.flow5 Html5.F.elt list), l))
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

  method display :
    'a. ?commentable:_ -> ?rows:_ -> ?cols:_ ->
      ?classes:_ -> data:_ -> _ -> ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ?(commentable = true) ?rows ?cols
    ?(classes=[]) ~data:message_id () ->
    lwt () = add_forum_css_header () in
    let data = self#get_thread ~message_id in
    let transform_data =
      self#pretty_print_thread ~classes ~commentable ?rows ?cols
    in
    (Lwt.catch
       (fun () -> data >>= transform_data)
       (fun exc ->
          let e =
            [ widget_with_error_box#display_error_box ~exc () ]
          in
          Lwt.return ([widget_with_error_box#error_class], (e, e)) ))
    >>= fun (classes, c) ->
    self#display_thread ~classes c

  method display_splitted ?(commentable = true) ?rows ?cols
    ?(classes=[]) ~data:message_id () :
    ( Html5_types.div Html5.F.elt *
        Html5_types.div Html5.F.elt )
    Lwt.t
    =
    lwt () = add_forum_css_header () in
    let data = self#get_thread ~message_id in
    let transform_data =
      self#pretty_print_thread ~classes ~commentable ?rows ?cols
    in
    (Lwt.catch
       (fun () -> data >>= transform_data)
       (fun exc ->
          let e =
            ( [ widget_with_error_box#display_error_box ~exc () ]
              :> Html5_types.flow5 Html5.F.elt list )
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

  method display_message_list :
    'a. classes:_ -> _ -> ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ~classes content ->
    let classes = (ml_class::classes) in
    Lwt.return
      (Html5.F.div ~a:[Html5.F.a_class classes] content)

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
            message_widget#display_message ~classes (content:> Html5_types.flow5 Html5.F.elt list) >>= fun m ->
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
         (if message_creators
          then
            add_message_widget#display ~forum ?rows ?cols () >|= Ocsimore_lib.list_singleton
          else Lwt.return [])
     else Lwt.return [])
    >>= fun form ->
    Lwt.return (classes,
                (List.flatten l :> Html5_types.flow5_without_header_footer Html5.F.elt list)
                @ ( form : _ Html5.F.elt list :> Html5_types.flow5_without_header_footer Html5.F.elt list) )

  method display :
    'a. ?rows:_ -> ?cols:_ -> ?classes:_ -> forum:_ ->
      first:_ -> number:_ -> ?add_message_form:_ -> unit ->
        ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =
    fun ?(rows : int option) ?(cols : int option) ?(classes=[])
    ~forum ~first ~number ?(add_message_form = true) () ->
    lwt () = add_forum_css_header () in
    widget_with_error_box#bind_or_display_error
      (self#get_message_list ~forum ~first ~number)
      (self#pretty_print_message_list
         ~forum ?rows ?cols ~classes ?add_message_form :>
         Forum_data.raw_message list ->
       (string list * Html5_types.flow5 Html5.F.elt list)
         Lwt.t)
    >>= fun (classes, r) ->  self#display_message_list ~classes r

  method atom_message_list ?(number=10) forum =
    lwt forum_info = Forum_data.get_forum ~forum () in
    lwt msg_list = Forum_data.get_message_list ~forum ~first:1L ~number:(Int64.of_int number) () in
    lwt msg_list = Lwt_list.map_s
      Forum_data.message_info_of_raw_message msg_list in
    lwt entries = message_widget#atom_entries msg_list in
    let id = Xml.uri_of_fun
      (fun () -> "forum_"^(Int32.to_string (Opaque.t_int32 forum_info.f_id))) in
    let title = Atom_feed.plain forum_info.f_title in
    let updated = last_msg_date msg_list in
    Lwt.return (Atom_feed.feed
                  ~id
                  ~updated
                  ~title
                  entries)

end

class forum_widget
  (widget_with_error_box : Widget.widget_with_error_box) =
object (self)

     method display_all_forums : Html5_types.flow5 Html5.F.elt list Lwt.t =
     lwt forums = Forum_data.get_forums_list () in
     let line forum_info =
       let open Forum_types in
         let id = string_of_forum forum_info.f_id in
         let edit =
           Html5.D.a ~service:Forum_services.edit_forum
             [Page_site.icon ~path:"imgedit.png" ~text:"Edit wiki options"]
             forum_info.f_id
         in
         (Html5.F.tr
            [Html5.F.td ~a:[Html5.F.a_class ["forumid"]] [Html5.F.pcdata id];
             Html5.F.td ~a:[Html5.F.a_class ["forumname"]]
               [Html5.F.strong [Html5.F.pcdata forum_info.f_title]];
             Html5.F.td ~a:[Html5.F.a_class ["forumdescr"]]
               [Html5.F.pcdata forum_info.f_descr];
             Html5.F.td [edit];
            ])
     in
     let l = List.map line forums in
     Lwt.return
       [ Html5.F.table ~a:[Html5.F.a_class ["table_admin"]]
          (Html5.F.tr
             [Html5.F.th [Html5.F.pcdata "Id"];
              Html5.F.th [Html5.F.pcdata "Forum"];
              Html5.F.th [Html5.F.pcdata "Description"];
              Html5.F.th [];
             ]
          )
          l;
       ]

end
