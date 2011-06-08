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
open Eliom_pervasives
open Eliom_output.Html5
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
      | None -> HTML5.M.strong [HTML5.M.pcdata message]
      | Some Ocsimore_common.Permission_denied ->
          HTML5.M.strong
            [HTML5.M.pcdata "You are not allowed to see this content"]
      | Some exc ->
          if Ocsigen_config.get_debugmode ()
          then
            HTML5.M.strong
              [
                HTML5.M.pcdata message;
                HTML5.M.br ();
                HTML5.M.pcdata (Printexc.to_string exc);
                HTML5.M.br ();
                HTML5.M.em [HTML5.M.pcdata "Ocisgen running in debug mode"];
              ]
          else
            HTML5.M.strong [HTML5.M.pcdata message]
    in
    (HTML5.M.p ~a:[HTML5.M.a_class classes] [message]
       : HTML5_types.flow5 HTML5.M.elt)

  method bind_or_display_error : 'a.
    'a Lwt.t ->
    ('a -> (string list * HTML5_types.flow5 HTML5.M.elt list) Lwt.t) -> _
    = fun data transform_data ->
      Lwt.catch
        (fun () -> data >>= transform_data)
        (fun exc ->
           Lwt.return
             ([error_class],
              [(self#display_error_box ~exc ()
                 : HTML5_types.flow5 HTML5.M.elt :> HTML5_types.flow5 HTML5.M.elt)]
             ))

end

class virtual ['param, 'data, 'result] parametrized_widget =
object
  inherit widget

  val xhtml_class = "parametrized_widget"

  method virtual private retrieve_data :
    'param -> 'data Lwt.t

  method virtual apply :
    data:'param -> 'result

end

class type ['param, 'data, 'result] parametrized_widget_t =
object
  inherit widget
  method private retrieve_data :
    'param -> 'data Lwt.t
  method apply :
    data:'param -> 'result
end

class virtual ['param, 'data] parametrized_div_widget =
object
  inherit ['param, 'data, [`Div] HTML5.M.elt Lwt.t] parametrized_widget

  val xhtml_class = "parametrized_div_widget"

end

class type ['param, 'data] parametrized_div_widget_t =
    ['param, 'data, [`Div] HTML5.M.elt Lwt.t] parametrized_widget_t

class virtual ['param, 'result] parametrized_unit_widget =
object
  inherit ['param, unit, 'result] parametrized_widget

  val xhtml_class = "parametrized_unit_widget"

  method private retrieve_data _ = Lwt.return ()

end

class type ['param, 'result] parametrized_unit_widget_t =
          ['param, unit, 'result] parametrized_widget_t

class virtual ['param] parametrized_unit_div_widget =
object
  inherit ['param, unit] parametrized_div_widget
  inherit ['param, [`Div] HTML5.M.elt Lwt.t] parametrized_unit_widget

  val xhtml_class = "parametrized_unit_div_widget"

  method private retrieve_data _ = Lwt.return ()

end

class type ['param] parametrized_unit_div_widget_t =
          ['param, unit, [`Div] HTML5.M.elt Lwt.t] parametrized_widget_t


(*
class ['child] list_widget =
object
  inherit widget

  val xhtml_class = "list"

  method display : [`Div] HTML5.M.elt Lwt.t =
    Lwt.return (HTML5.M.div ~a:[HTML5.M.a_class [xhtml_class]] [])

end
*)
