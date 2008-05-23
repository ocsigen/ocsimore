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
open Ocsimore_lib

class widget =
object
end

class widget_with_error_box =
object(self)

  val error_class = "errormsg"

  method private display_error_message = function
    | Some message ->
        {{ [ {{ self#display_error_box ~message () }} ] }}
    | None ->
        {{ [] }}

  method display_error_box ?(classe=[]) ?(message = "Error") ?exn () =
    let classe = Ocsimore_lib.build_class_attr (error_class::classe) in
    let message = 
      match exn with
        | None -> {{ [<strong>{: message :}] }}
        | Some exn ->
            if Ocsigen_config.get_debugmode ()
            then {{ [<strong>{: message :}
                     <br>[]
                     !{: Printexc.to_string exn :}
                     <br>[]
                     <em>[ '(Ocsigen running in debug mode)' ]
                    ] }}
            else {{ [<strong>{: message :} ] }}
    in
    {{ <p class={:error_class:}>message }}

  method bind_or_display_error : 'a.
    classe:string list -> 
    ?error: string ->
    'a Lwt.t -> 
    ('a -> Xhtmltypes_duce.flows Lwt.t) -> 
    (classe:string list -> 
      Xhtmltypes_duce.flows -> 
      Xhtmltypes_duce.block Lwt.t) -> 
    Xhtmltypes_duce.block Lwt.t
    = fun ~classe ?error data transform_data display_box  ->
      (Lwt.catch
         (fun () -> data >>= transform_data)
         (fun exn -> 
            Lwt.return {{ [ {{ self#display_error_box ~exn () }} ] }} ))
      >>= fun content ->
      let err = self#display_error_message error in
      display_box ~classe {{ [ !err !content ] }}

end

class virtual ['param_type, 'data_type, 'result_type] parametrized_widget =
object (self)
  inherit widget
    
  val xhtml_class = "parametrized_widget"
    
  method virtual private retrieve_data :
    sp:Eliom_sessions.server_params ->
    sd:Ocsimore_common.session_data ->
    'param_type -> 'data_type Lwt.t
      
  method virtual apply : 
    sp:server_params -> 
    sd:Ocsimore_common.session_data ->
    data:'param_type -> 'result_type

end

class type ['param_type, 'data_type, 'result_type] parametrized_widget_t =
object
  inherit widget
  method private retrieve_data :
    sp:Eliom_sessions.server_params ->
    sd:Ocsimore_common.session_data ->
    'param_type -> 'data_type Lwt.t
  method apply :
    sp:Eliom_sessions.server_params -> 
    sd:Ocsimore_common.session_data ->
    data:'param_type -> 'result_type
end

class virtual ['param_type, 'data_type] parametrized_div_widget =
object (self)
  inherit ['param_type, 'data_type, Xhtmltypes_duce._div Lwt.t] parametrized_widget
    
  val xhtml_class = "parametrized_div_widget"

end

class type ['param_type, 'data_type] parametrized_div_widget_t =
          ['param_type, 'data_type, Xhtmltypes_duce._div Lwt.t] parametrized_widget_t
  
class virtual ['param_type, 'result_type] parametrized_unit_widget =
object (self)
  inherit ['param_type, unit, 'result_type] parametrized_widget
    
  val xhtml_class = "parametrized_unit_widget"

  method private retrieve_data ~sp ~sd _ = Lwt.return ()
    
end

class type ['param_type, 'result_type] parametrized_unit_widget_t =
          ['param_type, unit, 'result_type] parametrized_widget_t
  
class virtual ['param_type] parametrized_unit_div_widget =
object (self)
  inherit ['param_type, unit] parametrized_div_widget
  inherit ['param_type, Xhtmltypes_duce._div Lwt.t] parametrized_unit_widget
    
  val xhtml_class = "parametrized_unit_div_widget"

  method private retrieve_data ~sp ~sd _ = Lwt.return ()
    
end

class type ['param_type] parametrized_unit_div_widget_t =
          ['param_type, unit, Xhtmltypes_duce._div Lwt.t] parametrized_widget_t


  
class ['child_type] list_widget =
object (self)
  inherit widget
    
  val xhtml_class = "list"
    
  method display ~(sp:server_params) : Xhtmltypes_duce._div Lwt.t =
    return
      {{
         <div class={: xhtml_class :}>[]
       }}
      
end

