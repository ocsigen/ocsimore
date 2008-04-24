(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
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
open Lwt
open Eliommod
open Eliom_duce.Xhtml
open Eliom_sessions
open Session_manager
open Ocsimorelib

class widget ~(parent: sessionmanager) =
object (self)

  val xhtml_class = "widget"
    
  method name = xhtml_class
end;;

class virtual ['param_type, 'data_type, 'result_type] parametrized_widget 
  ~(parent: sessionmanager) =
object (self)
  inherit widget parent
    
  val xhtml_class = "parametrized_widget"
    
  method virtual private retrieve_data : 'param_type -> 'data_type Lwt.t
      
  method virtual apply : sp:server_params -> 'param_type -> 'result_type Lwt.t

end

class type ['param_type, 'data_type, 'result_type] parametrized_widget_t =
object
  inherit widget
  method private retrieve_data: 'param_type -> 'data_type Lwt.t
  method apply: sp:server_params -> 'param_type -> 'result_type Lwt.t
end

class virtual ['param_type, 'data_type] parametrized_div_widget 
  ~(parent: sessionmanager) =
object (self)
  inherit ['param_type, 'data_type, Xhtmltypes_duce._div] parametrized_widget parent
    
  val xhtml_class = "parametrized_div_widget"

end

class type ['param_type, 'data_type] parametrized_div_widget_t =
          ['param_type, 'data_type, Xhtmltypes_duce._div] parametrized_widget_t
  
class virtual ['param_type, 'result_type] parametrized_unit_widget 
  ~(parent: sessionmanager) =
object (self)
  inherit ['param_type, unit, 'result_type] parametrized_widget parent
    
  val xhtml_class = "parametrized_unit_widget"

  method private retrieve_data _ = Lwt.return ()
    
end

class type ['param_type, 'result_type] parametrized_unit_widget_t =
          ['param_type, unit, 'result_type] parametrized_widget_t
  
class virtual ['param_type] parametrized_unit_div_widget 
  ~(parent: sessionmanager) =
object (self)
  inherit ['param_type, unit] parametrized_div_widget parent
  inherit ['param_type, Xhtmltypes_duce._div] parametrized_unit_widget parent
    
  val xhtml_class = "parametrized_unit_div_widget"

  method private retrieve_data _ = Lwt.return ()
    
end

class type ['param_type] parametrized_unit_div_widget_t =
          ['param_type, unit, Xhtmltypes_duce._div] parametrized_widget_t


  
class ['child_type] list_widget ~(parent: sessionmanager) =
object (self)
  inherit widget parent
    
  val xhtml_class = "list"
    
  method display ~(sp:server_params) : Xhtmltypes_duce._div Lwt.t =
    return
      {{
         <div class={: xhtml_class :}>[]
       }}
      
end

