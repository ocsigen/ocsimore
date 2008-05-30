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



class noneditable_wikibox :
  object

    inherit Widget.widget_with_error_box

    method display_noneditable_box :
      classe:string list ->
      Xhtmltypes_duce.flows -> 
      Xhtmltypes_duce.block Lwt.t

    method noneditable_wikibox :
      ?subbox:Xhtmltypes_duce.flows ->
      ancestors:Wiki_syntax.ancestors ->
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      ?classe:string list ->
      data:Wiki_sql.wiki * int32 -> Xhtmltypes_duce.block Lwt.t

    method pretty_print_wikisyntax : 
      ?subbox:Xhtmltypes_duce.flows ->
      ancestors:Wiki_syntax.ancestors ->
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      int32 ->
      string -> Xhtmltypes_duce.flows Lwt.t
    
    method private retrieve_wikibox_content :
      Wiki_sql.wiki * int32 -> string Lwt.t

  end

class editable_wikibox :
  unit ->
  object

    inherit Widget.widget_with_error_box
    inherit noneditable_wikibox

    method display_edit_box :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_edit_form :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      Wiki_sql.wiki * int32 -> string -> Xhtmltypes_duce.flows Lwt.t

    method display_edit_perm_form :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 -> 
      Xhtmltypes_duce.flows Lwt.t

    method display_editable_box :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_history :
      sp:Eliom_sessions.server_params ->
      Wiki_sql.wiki * int32 ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list ->
      Xhtmltypes_duce.flows Lwt.t

    method display_history_box :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 ->
      classe:string list ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_menu_box :
      classe:string list ->
      ?service:(Wiki_sql.wiki * int32 ->
                  (Eliom_services.get_service_kind, [ `Unregistrable ])
                    Eliom_duce_tools.one_page) ->
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 -> 
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method display_old_wikibox :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 ->
      int32 ->
      classe:string list ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t
      
    method display_src_wikibox :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 ->
      int32 ->
      classe:string list ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t
      
    method editable_wikibox :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      data:Wiki_sql.wiki * int32 ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list -> 
      ?subbox:Xhtmltypes_duce.flows ->
      ancestors:Wiki_syntax.ancestors ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method private retrieve_history :
      sp:Eliom_sessions.server_params ->
      Wiki_sql.wiki * int32 ->
      ?first:int ->
      ?last:int ->
      unit ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list Lwt.t

    method retrieve_old_wikibox_content :
      sp:Eliom_sessions.server_params ->
      sd:Ocsimore_common.session_data ->
      Wiki_sql.wiki * int32 -> int32 -> string Lwt.t

  end
