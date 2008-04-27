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

class wikibox :
  parent:Session_manager.sessionmanager ->
  [Wiki_sql.wiki * int32, wiki_data option] Widget.parametrized_div_widget_t

class editable_wikibox :
  parent:Session_manager.sessionmanager ->
  unit ->
  [Wiki_sql.wiki * int32, 
   wiki_data option * Wiki_sql.role,
   ?rows:int -> ?cols:int -> unit -> Xhtmltypes_duce._div Lwt.t] 
    Widget.parametrized_widget_t

