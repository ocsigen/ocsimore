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


(*
open Lwt
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_duce
open Ocsimore_lib
open CalendarLib
open Session_manager
open Widget


type message_data =
{
  id: Forum_sql.forum;
  text: string;
  author: string;
  datetime: Calendar.t;
  hidden: bool;
}


class message_toggle_action : 
  [Forum_sql.forum * int32] parametrized_unit_div_widget_t


class message_list_widget : 
object
  inherit [message_data] list_widget
  inherit [Forum_sql.forum * int32 * int64 option * int64 option, 
           message_data list] 
    parametrized_div_widget_t
end;;

class message_navigation_widget : 
  srv_thread:(int32 * (int32 * int64 option), 
              unit, 
              get_service_kind,
              [`WithoutSuffix],
              [`One of int32] param_name *
                ([`One of int32] param_name * [`Radio of int64] param_name),
              unit,
              [`Registrable]) service ->
      [Forum_sql.forum * int32 * int64 option * int64 option, int64]
        parametrized_div_widget_t


class message_forest_widget : 
  srv_reply_message:(int32 * (int32 * (int32 option * int32)), 
                     unit,
                     get_service_kind,
                     [`WithoutSuffix],
                     [`One of int32] param_name *
                       ([`One of int32] param_name * 
                          ([`Radio of int32] param_name *
                             [`One of int32] param_name)), 
                     unit, [`Registrable]) service -> 
      srv_message_toggle:(int32 * (int32 * int32 option), 
                          int32,
                          post_service_kind,
                          [ `WithoutSuffix ], 
                          [ `One of int32 ] param_name * 
                            ([ `One of int32 ] param_name *
                               [ `Radio of int32 ] param_name), 
                          [`One of int32] param_name,
                          [ `Registrable ]) service ->
      [Forum_sql.forum * int32 * int32 option, 
       (message_data Ocsimore_lib.tree list * Forum_sql.role)] 
        parametrized_div_widget_t
        

class message_form_widget : 
  srv_add_message:(int32 * (int32 * int32 option), 
                   string * (int32 option * bool), 
                   post_service_kind,
                   [`WithoutSuffix],
                   [`One of int32] param_name * 
                     ([`One of int32] param_name *
                        [`Radio of int32] param_name), 
                   [`One of string] param_name * 
                     ([`Radio of int32] param_name * 
                        [`One of bool] param_name), 
                   [`Registrable]) service ->
      [Forum_sql.forum * int32 * int32 option * int32 option] 
        parametrized_unit_div_widget_t
        
class message_add_action :
      [Forum_sql.forum * int32 * int32 option * string * bool]
        parametrized_unit_div_widget_t


class latest_messages_widget :
      [int64, (Forum_sql.forum * string * string) list]
        parametrized_div_widget_t

type thread_data =
{
	id: Forum_sql.forum;
	subject: string;
	author: string;
	datetime: Calendar.t
}

class thread_widget: 
  srv_thread_toggle:(int32 * (int32 * int32 option), 
                     unit, 
                     post_service_kind, 
                     [`WithoutSuffix], 
                     [`One of int32] param_name * 
                       ([`One of int32] param_name * 
                          [`Radio of int32] param_name), 
                     unit, [`Registrable]) service -> 
      [Forum_sql.forum * int32, 
       ((int32 * string * string * string option *
           CalendarLib.Printer.CalendarPrinter.t * bool * int64 * int64) *
          Forum_sql.role)] parametrized_div_widget_t
        
        
class thread_toggle_action :
  [Forum_sql.forum * int32] parametrized_unit_div_widget_t

class thread_list_widget :
  srv_thread: (int32 * (int32 * int32 option), 
               unit, 
               get_service_kind, 
               [`WithoutSuffix], 
               [`One of int32] param_name * 
                 ([`One of int32] param_name * 
                    [`Radio of int32] param_name), 
               unit, 
               [`Registrable]) service ->
object
  inherit [thread_data] list_widget
  inherit [Forum_sql.forum, 
           (thread_data list * Forum_sql.role)] parametrized_div_widget_t
end;;

class thread_form_widget : 
  srv_add_thread: (int32, 
                   bool * (string * string), 
                   post_service_kind,
                   [`WithoutSuffix], 
                   [`One of int32] param_name, 
                   [`One of bool] param_name *
                     ([`One of string] param_name *
                        [`One of string] param_name),
                   [`Registrable]) service -> 
      [Forum_sql.forum] parametrized_unit_div_widget_t


class thread_add_action :
  [Forum_sql.forum * bool * string * string] parametrized_unit_div_widget_t


type forum_data =
{
  id: Forum_sql.forum;
  name: string;
  description: string;
  moderated: bool;
  arborescent: bool;
};;

class forums_list_widget: 
  srv_forum:(int32, 
             unit, 
             get_service_kind,
             [`WithoutSuffix], 
             [`One of int32] param_name, 
             unit, [`Registrable]) service ->
object
  inherit [unit, forum_data list] parametrized_div_widget_t
  inherit [forum_data] list_widget
end;;

class forum_form_widget: 
  srv_add_forum: (unit, 
                  string * (string * (string * (bool * bool))), 
                  post_service_kind,
                  [`WithoutSuffix], 
                  unit, 
                  [`One of string] param_name * 
                    ([`One of string] param_name * 
                       ([`One of string] param_name * 
                          ([`One of bool] param_name * 
                             [`One of bool] param_name))), 
                  [`Registrable]) service -> 
      [unit] parametrized_unit_div_widget_t
        
class forum_add_action :
  [string * string * string * bool * bool] parametrized_unit_div_widget_t

*)

(*
(** A parametrized_div_widget that displays one message *)
class message_widget : srv_message_toggle:unit -> 
object
  inherit [Forum_sql.forum * int32] parametrized_div_widget
    
  (** Set the message subject *)
  method set_subject: string -> unit
    
  (** Set the message author *)
  method set_author: string -> unit
    
  (** Set the message contents *)
  method set_text: string -> unit
    
  (** Set the message date and time *)
  method set_datetime: Calendar.t -> unit
    
  (** Indicate whether the message is hidden *)
  method set_hidden: bool -> unit
    
  (** Indicate whether the message is sticky *)
  method set_sticky: bool -> unit
end;;

*)
