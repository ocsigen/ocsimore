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

open Eliom_pervasives

let forum_wiki_rights = new Forum.wiki_rights

let title_syntax = Wiki_syntax.wikicreole_phrasing_content_type

let wikicreole_forum_model =
  Wiki_models.register_wiki_model
    ~name:"wikicreole_forum"
    ~content_type:Wiki_syntax.reduced_wikicreole_content_type0
    ~rights:forum_wiki_rights
    ~widgets:Wiki_site.wikibox_widget


let wiki_widgets = Wiki_models.get_widgets wikicreole_forum_model
let wiki_phrasing_widgets =
  new Wiki_widgets.phrasing_wikibox Wiki_site.error_box User_site.user_widgets
let services = Forum_services.register_services ()
let widget_err = new Widget.widget_with_error_box
let add_message_widget = new Forum_widgets.add_message_widget services
let message_widget =
  new Forum_widgets.message_widget
    widget_err wiki_widgets wiki_phrasing_widgets services
let thread_widget =
  new Forum_widgets.thread_widget
    widget_err message_widget add_message_widget services
let message_list_widget =
  new Forum_widgets.message_list_widget
    widget_err message_widget add_message_widget
let forum_widget =
  new Forum_widgets.forum_widget widget_err

let _ = Forum_wikiext.register_wikiext
  (message_widget, thread_widget, message_list_widget)

let forum_root =
  Eliom_services.service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"forums"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_output.Html5.register forum_root
  (fun () () ->
     Page_site.admin_page ~service:forum_root ~title:"Ocsimore - Forum module"
       [HTML5.M.h1 [HTML5.M.pcdata "Forum module"];
        HTML5.M.p
          [HTML5.M.pcdata "This is the Ocsimore admin page for the forum \
                           module. The links on the right will help you \
                           configure your installation." ];
       ]
  )

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
  let page _arg error form =
    let title = match error with
      | Xform.NoError -> "Forum edition"
      | _ -> "Error" in
    Page_site.admin_page ~service:(service :> Page_site.menu_link_service) ~title
      ( HTML5.M.h1 [HTML5.M.pcdata title]
       :: (match error with
             | Xform.ErrorMsg err ->
                 [HTML5.M.p ~a:[HTML5.M.a_class ["errmsg"]]
                    [HTML5.M.pcdata err]
                 ]
             | _ -> [])
       @  [form]
      )
  in
  lwt info = Forum_data.get_forum ~forum () in
  let open Forum_types in
  let open Xform.XformLwt in
  let open Xform.XformLwt.Ops in
  (* TODO: allow editing other fields *)
  lwt form =
    Xform.XformLwt.form ~fallback:service ~get_args:arg ~page ?err_handler
    (p (strong (text (Printf.sprintf "Forum '%s'" info.f_title)) ::
          text (Printf.sprintf " (id %s)" (string_of_forum forum))  @+
          Opaque.int32_input_xform ~a:[HTML5.M.a_input_type `Hidden] forum) @@
       p (text "Title: " @+ string_input title) @@
       p (text "Description: " @+ string_input descr) @@
       p (text "Arborescent: " @+ bool_checkbox arborescent) @@
       p (text "Comments wiki: "  @+ Opaque.int32_input_xform comments_wiki) @@
       p (text "Messages wiki: "  @+ Opaque.int32_input_xform messages_wiki) @@
       p (text "Title syntax: " @+ content_type_input title_syntax) @@
       p (submit_button "Save")
      |> cont)
  in
  page arg Xform.NoError form


let edit_forum =
  let err_handler = function
    | Ocsimore_common.Permission_denied ->
        Some "You do not have sufficient permissions to edit forums"
    | _ -> Some "An unknown error has occurred"
  in
  Eliom_output.Html5.register Forum_services.edit_forum
    (fun forum () ->
      lwt info = Forum_data.get_forum ~forum () in
      let open Forum_types in
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
              Page_site.admin_page ~service:Forum_services.view_forums
                [HTML5.M.h1
                [HTML5.M.pcdata "Forum information sucessfully edited"]])
        | false ->
          Page_site.admin_page
            [HTML5.M.h1 [HTML5.M.pcdata "Insufficient permissions"];
             HTML5.M.p [HTML5.M.pcdata "You do not have enough rights to \
                                           edit this forum"];])

let forum_empty_menu =
  Eliom_services.service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"forums_do_nothing"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_output.Html5.register forum_empty_menu
  (fun () () ->
    Page_site.admin_page ~service:forum_empty_menu
      [HTML5.M.h1 [HTML5.M.pcdata "An empty configuration page for forums"];
       HTML5.M.p
         [HTML5.M.pcdata "This is empty and do nothing." ];
      ]
  )

(** We register the service that lists all the forums *)
let () =  Eliom_output.Html5.register Forum_services.view_forums
    (fun () () ->
      lwt body = forum_widget#display_all_forums in
      Page_site.admin_page body)

let () = Page_site.add_to_admin_menu ~root:forum_root ~name:"Forum"
  ~links:[
    "View all forums", Forum_services.view_forums, (fun () -> Lwt.return true);
    "empty menu", forum_empty_menu, (fun () -> Lwt.return true);
  ]
