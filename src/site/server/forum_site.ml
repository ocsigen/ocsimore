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

open Eliom_content
open Ocsimore_lib
open Eliom_lib
open Lwt_ops

let forum_wiki_rights = new Forum.wiki_rights

let title_syntax = Wiki_syntax.wikicreole_phrasing_content_type

let wikicreole_forum_model =
  Lwt_main.run (
    Wiki_models.register_wiki_model
      ~name:"wikicreole_forum"
      ~content_type:Wiki_syntax.reduced_wikicreole_content_type0
      ~rights:forum_wiki_rights
      ~widgets:Wiki_site.wikibox_widget
  )

let forum_root = Forum_services.view_forums

let ($) = User_sql.Types.($)

let content_type_input content_type =
  let open Xform.XformLwt in
  let open Xform.XformLwt.Ops in
  string_input (Wiki_types.string_of_content_type content_type)
  |> Wiki_types.content_type_of_string

let edit_forum_form ~serv_path:_ ~service ~arg
    ~(forum:Forum_types.forum) ~title ~descr ~arborescent
    ~title_syntax ~messages_wiki ~comments_wiki
    ?err_handler cont =
  let open Forum_types in
  lwt info = Forum_data.get_forum ~forum () in
  let page _arg error form =
    let title = match error with
      | Xform.NoError ->
          Printf.sprintf "Forum '%s' (ID %s)" info.f_title (string_of_forum forum)
      | Xform.ErrorMsg _
      | Xform.ErrorNoMsg -> "Error" in
    Page_site.admin_page ~service:(service :> Page_site.menu_link_service) ~title
      ((match error with
             | Xform.ErrorMsg err ->
                 [Html5.F.p ~a:[Html5.F.a_class ["errmsg"]]
                    [Html5.F.pcdata err]
                 ]
             | Xform.NoError
             | Xform.ErrorNoMsg -> [])
       @  [form]
      )
  in
  let open Xform.XformLwt in
  let open Xform.XformLwt.Ops in
  lwt form =
    Xform.XformLwt.form ~fallback:service ~get_args:arg ~page ?err_handler
      (table
         (tr (td (Opaque.int32_input_xform ~a:[Html5.F.a_style "display: none"] forum)) @@
          label_input_tr ~label:"Title" (string_input title) @@
          label_input_tr ~label:"Description" (string_input descr) @@
          label_input_tr ~label:"Arborescent" (bool_checkbox arborescent) @@
          label_input_tr ~label:"Comments wiki" (Opaque.int32_input_xform comments_wiki) @@
          label_input_tr ~label:"Messages wiki" (Opaque.int32_input_xform messages_wiki) @@
          label_input_tr ~label:"Title syntax" (content_type_input title_syntax) @@
          tr (td (submit_button "Save")))
     |> cont)
  in
  page arg Xform.NoError form

let edit_forum =
  let err_handler = function
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to edit forums"
    | _ -> Some "An unknown error has occurred"
  in
  Ocsimore_appl.register ~service:Forum_services.edit_forum
    (fun forum () ->
      lwt info = Forum_data.get_forum ~forum () in
      let open Forum_types in
      let title = "Edit forum" in
      match_lwt User.in_group ~group:(Forum.forum_admin $ forum) () with
        | true ->
          edit_forum_form ~serv_path:Forum_services.path_edit_forum
            ~service:Forum_services.view_forums ~arg:()
            ~forum ~title:info.f_title ~descr:info.f_descr
            ~arborescent:info.f_arborescent ~comments_wiki:info.f_comments_wiki
            ~messages_wiki:info.f_messages_wiki
            ~title_syntax:info.f_title_syntax ~err_handler
            (fun (forum, (title, (descr, (arborescent,
               (comments_wiki, (messages_wiki,
               (title_syntax, (_ : bool)))))))) () ->
              lwt () = Forum_data.update_forum ~title ~descr
                ~arborescent ~comments_wiki ~messages_wiki
                ~title_syntax forum in
              Page_site.admin_page
                ~service:Forum_services.view_forums
                ~title
                [Html5.F.(p [pcdata "Forum information sucessfully edited"])])
        | false ->
            Page_site.(no_permission () >>= admin_page ~title))

let create_forum_form ~serv_path:_ ~service ~arg
    ?err_handler cont =
  let page _arg error form =
    let title = match error with
      | Xform.NoError -> "Create forum"
      | Xform.ErrorMsg _
      | Xform.ErrorNoMsg -> "Error" in
    Page_site.admin_page ~title
      ((match error with
             | Xform.ErrorMsg err ->
                 [Html5.F.p ~a:[Html5.F.a_class ["errmsg"]]
                    [Html5.F.pcdata err]
                 ]
             | Xform.ErrorNoMsg
             | Xform.NoError -> [])
       @  [form] )
  in
  let open Xform.XformLwt in
  let open Xform.XformLwt.Ops in
  lwt form =
    Xform.XformLwt.form ~fallback:service ~get_args:arg ~page ?err_handler
     (table
       (label_input_tr ~label:"Title" (string_input "") @@
        label_input_tr ~label:"Description" (string_input "") @@
        label_input_tr ~label:"Arborescent" (bool_checkbox true) @@
        label_input_tr ~label:"Title syntax" (content_type_input title_syntax) @@
        tr (td (submit_button "Create")))
       |> cont)
  in
  page arg Xform.NoError form

let create_forum =
  let err_handler = function
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to create forums"
    | _ -> Some "An unknown error has occurred"
  in
  Ocsimore_appl.register ~service:Forum_services.create_forum
    (fun () () ->
      match_lwt User.in_group ~group:Forum.forum_creators () with
        | true ->
          create_forum_form
            ~serv_path:Forum_services.path_create_forum
            ~service:Forum_services.view_forums ~arg:()
            ~err_handler
            (fun (title, (descr, (arborescent,
               (title_syntax, (_ : bool))))) () ->
              lwt _ =
                Forum.create_forum
                  ~title ~descr ~arborescent ~title_syntax
                  ~wiki_model:Wiki_site.wikicreole_model () in
              Page_site.admin_page ~service:Forum_services.view_forums
                ~title:"Create forum"
                Html5.F.([h2 [pcdata "Forum information sucessfully created"]]))
        | false ->
            Page_site.(no_permission () >>= admin_page ~title:"Create forum"))

(** We register the service that lists all the forums *)

let () = Lwt_main.run (
  lwt wiki_widgets = Wiki_models.get_widgets wikicreole_forum_model in
  let wiki_phrasing_widgets = new Wiki_widgets.phrasing_wikibox Wiki_site.error_box User_site.user_widget in
  let services = Forum_services.register_services () in
  let widget_err = new Widget.widget_with_error_box in
  let add_message_widget = new Forum_widgets.add_message_widget services in
  let message_widget =
    new Forum_widgets.message_widget
      widget_err wiki_widgets wiki_phrasing_widgets services in
  let thread_widget =
    new Forum_widgets.thread_widget
      widget_err message_widget add_message_widget services in
  let message_list_widget =
    new Forum_widgets.message_list_widget
      widget_err message_widget add_message_widget in
  let forum_widget =
    new Forum_widgets.forum_widget widget_err in
  let threads_list_widget =
    new Forum_widgets.threads_list_widget
      widget_err message_widget add_message_widget thread_widget
  in
  let () = Forum_wikiext.register_wikiext
    (message_widget, thread_widget, message_list_widget, threads_list_widget)
  in
  Ocsimore_appl.register ~service:Forum_services.view_forums
    (Page_site.admin_body_content_with_permission_handler
       ~title:(fun () () -> Lwt.return "View forums")
       ~permissions:(fun () () -> Page_site.userid_permissions (Lwt.return -| (=) User.admin))
       ~display:(fun () () -> forum_widget#display_all_forums));
(* See the comment in forum_widgets.eliom ligne 100 *)
(*  Eliom_atom.Reg.register
    ~service:services.Forum_types.thread_feed_service
    (fun message () -> message_widget#atom_childs ~message);
  Eliom_atom.Reg.register
    ~service:services.Forum_types.forum_feed_service
    (fun forum () -> message_list_widget#atom_message_list forum);*)
  Lwt.return ()
)

let () = Page_site.add_to_admin_menu ~root:forum_root ~name:"Forum"
  ~links:[
    "View all forums", Forum_services.view_forums, (fun () -> Lwt.return true);
    "Create forum", Forum_services.create_forum,
    (fun () -> User.in_group ~group:Forum.forum_creators ());
  ]
