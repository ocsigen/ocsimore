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
*)


type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.userdata option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

type menu_item =
  | Edit
  | Edit_perm
  | Edit_css
  | History
  | View

class virtual noneditable_wikibox :
  object

    inherit Widget.widget_with_error_box

     method container :
       ?css:{{ [ Xhtmltypes_duce.link* ] }} ->
       Xhtmltypes_duce.blocks ->
       Xhtmltypes_duce.html

     method display_noneditable_box :
      classe:string list ->
      Xhtmltypes_duce.flows ->
      Xhtmltypes_duce.block Lwt.t

    method noneditable_wikibox :
      bi:Wiki_syntax.box_info ->
      ?classe:string list ->
      data:Wiki_sql.wiki * int32 ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method virtual pretty_print_wikisyntax :
      wiki:Wiki_sql.wiki ->
      bi:Wiki_syntax.box_info ->
      string -> Xhtmltypes_duce.flows Lwt.t

    method private retrieve_wikibox_content :
      Wiki_sql.wiki * int32 -> string Lwt.t

  end

class virtual editable_wikibox :
  ?sp:Eliom_sessions.server_params ->
  unit -> (int32 * int32) ->
  object

    inherit Widget.widget_with_error_box
    inherit noneditable_wikibox

    method display_edit_box :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_edit_perm :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_edit_form :
      bi:Wiki_syntax.box_info ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      Wiki_sql.wiki * int32 -> string * int32 -> Xhtmltypes_duce.form Lwt.t

    method display_full_edit_form :
      bi:Wiki_syntax.box_info ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      Wiki_sql.wiki * int32 -> string * int32 -> Xhtmltypes_duce.flows Lwt.t

    method display_edit_perm_form :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      Xhtmltypes_duce.flows Lwt.t

    method display_editable_box :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_history :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list ->
      Xhtmltypes_duce.flows Lwt.t

    method display_history_box :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_menu_box :
      classe:string list ->
      ?service:menu_item ->
      ?cssmenu:string option ->
      ?title:string ->
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_old_wikibox :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_src_wikibox :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox :
      bi:Wiki_syntax.box_info ->
      data:Wiki_sql.wiki * int32 ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox_allowed :
      bi:Wiki_syntax.box_info ->
      data:Wiki_sql.wiki * int32 ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> (Xhtmltypes_duce.block * bool) Lwt.t

    method private retrieve_history :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      unit ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list Lwt.t

    method retrieve_old_wikibox_content :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 -> int32 -> string Lwt.t

    method display_edit_css_form :
      bi:Wiki_syntax.box_info ->
      ?rows:int ->
      ?cols:int ->
      data:Wiki_sql.wiki * string ->
      Ocamlduce.Utf8.repr ->
      Xhtmltypes_duce.flows Lwt.t

    method display_edit_css_box :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      string ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method edit_css_box :
      bi:Wiki_syntax.box_info ->
      data:Wiki_sql.wiki * string ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method display_edit_wikicss_box :
      bi:Wiki_syntax.box_info ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      ?cssmenu:string option ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_edit_wikicss_form :
      bi:Wiki_syntax.box_info ->
      ?rows:int ->
      ?cols:int ->
      wiki:Wiki_sql.wiki ->
      Ocamlduce.Utf8.repr -> Xhtmltypes_duce.flows Lwt.t

    method edit_wikicss_box :
      bi:Wiki_syntax.box_info ->
      wiki:Wiki_sql.wiki ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list -> unit -> Xhtmltypes_duce.block Lwt.t

    (** returns the css headers for one wiki and optionally one page.
        Set [?admin] to [true] for administration pages.
    *)
    method get_css_header :
      bi:Wiki_syntax.box_info ->
      wiki:Wiki_sql.wiki ->
      ?admin:bool ->
      ?page:string ->
      unit ->
      {{ [ Xhtmltypes_duce.link* ] }} Lwt.t

  end

class creole_wikibox :
  ?sp:Eliom_sessions.server_params ->
  unit -> (int32 * int32) ->
object
  inherit editable_wikibox

  method pretty_print_wikisyntax :
    wiki:Wiki_sql.wiki ->
    bi:Wiki_syntax.box_info ->
    string -> Xhtmltypes_duce.flows Lwt.t

end
