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

let forum_wiki_rights = new Forum.wiki_rights

let title_syntax = Wiki_syntax.wikicreole_inline_content_type

let wikicreole_forum_model =
  Wiki_models.register_wiki_model
    ~name:"wikicreole_forum"
    ~content_type:Wiki_syntax.reduced_wikicreole_content_type0
    ~rights:forum_wiki_rights
    ~widgets:Wiki_site.wikibox_widget


let wiki_widgets = Wiki_models.get_widgets wikicreole_forum_model
let wiki_inline_widgets =
  new Wiki_widgets.inline_wikibox Wiki_site.error_box User_site.user_widgets
let services = Forum_services.register_services ()
let widget_err = new Widget.widget_with_error_box
let add_message_widget = new Forum_widgets.add_message_widget services
let message_widget =
  new Forum_widgets.message_widget
    widget_err wiki_widgets wiki_inline_widgets services
let thread_widget =
  new Forum_widgets.thread_widget
    widget_err message_widget add_message_widget services
let message_list_widget =
  new Forum_widgets.message_list_widget
    widget_err message_widget add_message_widget

let _ = Forum_wikiext.register_wikiext
  (message_widget, thread_widget, message_list_widget)


