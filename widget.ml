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
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
open Eliommod
open Eliom_predefmod.Xhtml
open Eliom_sessions
open Ocsimore_lib

class widget = object end

class widget_with_error_box =
object(self)

  val error_class = "errormsg"

  method error_class = error_class

  method display_error_message ?message ?exc () =
    match message, exc with
      | None, None -> []
      | message, exc -> [self#display_error_box ?message ?exc ()]

  method display_error_box ?(classes=[]) ?(message = "Error") ?exc () =
    let classes = error_class::classes in
    let message = match exc with
      | None -> XHTML.M.strong [XHTML.M.pcdata message]
      | Some Ocsimore_common.Permission_denied ->
          XHTML.M.strong
            [XHTML.M.pcdata "You are not allowed to see this content"]
      | Some exc ->
          if Ocsigen_config.get_debugmode ()
          then
            XHTML.M.strong
              [
                XHTML.M.pcdata message;
                XHTML.M.br ();
                XHTML.M.pcdata (Printexc.to_string exc);
                XHTML.M.br ();
                XHTML.M.em [XHTML.M.pcdata "Ocisgen running in debug mode"];
              ]
          else
            XHTML.M.strong [XHTML.M.pcdata message]
    in
    (XHTML.M.p ~a:[XHTML.M.a_class classes] [message]
       : Xhtmltypes.block XHTML.M.elt)

  method bind_or_display_error : 'a.
    'a Lwt.t ->
    ('a -> (string list * Xhtmltypes.div_content XHTML.M.elt list) Lwt.t) -> _
    = fun data transform_data ->
      Lwt.catch
        (fun () -> data >>= transform_data)
        (fun exc ->
           Lwt.return
             ([error_class],
              [(self#display_error_box ~exc ()
                 : Xhtmltypes.block XHTML.M.elt :> Xhtmltypes.div_content XHTML.M.elt)]
             ))

end

class virtual ['param, 'data, 'result] parametrized_widget =
object
  inherit widget

  val xhtml_class = "parametrized_widget"

  method virtual private retrieve_data :
    sp:Eliom_sessions.server_params ->
    'param -> 'data Lwt.t

  method virtual apply :
    sp:server_params ->
    data:'param -> 'result

end

class type ['param, 'data, 'result] parametrized_widget_t =
object
  inherit widget
  method private retrieve_data :
    sp:Eliom_sessions.server_params ->
    'param -> 'data Lwt.t
  method apply :
    sp:Eliom_sessions.server_params ->
    data:'param -> 'result
end

class virtual ['param, 'data] parametrized_div_widget =
object
  inherit ['param, 'data, [`Div] XHTML.M.elt Lwt.t] parametrized_widget

  val xhtml_class = "parametrized_div_widget"

end

class type ['param, 'data] parametrized_div_widget_t =
    ['param, 'data, [`Div] XHTML.M.elt Lwt.t] parametrized_widget_t

class virtual ['param, 'result] parametrized_unit_widget =
object
  inherit ['param, unit, 'result] parametrized_widget

  val xhtml_class = "parametrized_unit_widget"

  method private retrieve_data ~sp:_ _ = Lwt.return ()

end

class type ['param, 'result] parametrized_unit_widget_t =
          ['param, unit, 'result] parametrized_widget_t

class virtual ['param] parametrized_unit_div_widget =
object
  inherit ['param, unit] parametrized_div_widget
  inherit ['param, [`Div] XHTML.M.elt Lwt.t] parametrized_unit_widget

  val xhtml_class = "parametrized_unit_div_widget"

  method private retrieve_data ~sp:_ _ = Lwt.return ()

end

class type ['param] parametrized_unit_div_widget_t =
          ['param, unit, [`Div] XHTML.M.elt Lwt.t] parametrized_widget_t



class ['child] list_widget =
object
  inherit widget

  val xhtml_class = "list"

  method display ~sp:(_ : server_params) : [`Div] XHTML.M.elt Lwt.t =
    Lwt.return (XHTML.M.div ~a:[XHTML.M.a_class [xhtml_class]] [])

end

