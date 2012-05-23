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
open Eliom_content
open Eliom_lib.Lwt_ops
open Eliom_output.Html5
open Ocsimore_lib

class widget = object end

(* let bind_or_display_error *)
    (* (error_class : string) *)
    (* (display_error_box : *)
       (* ?classes:Html5_types.nmtoken list -> *)
     (* ?message:string -> *)
     (* ?exc:exn -> unit -> [> Html5_types.p ] Eliom_content.Html5.F.elt) *)
    (* data transform_data = *)

class widget_with_error_box =
object(self)

  val error_class = "errormsg"

  method error_class = error_class

  method display_error_box : 'a.
    ?classes:string list -> ?message:string -> ?exc:exn ->
    unit -> ([> `P ] as 'a) Html5.F.elt
  = fun ?(classes=[]) ?(message = "Error") ?exc () ->
      let classes = error_class :: classes in
      let message =
        match exc with
          | None ->
              Html5.F.strong [Html5.F.pcdata message]
          | Some Ocsimore_common.Permission_denied ->
              Html5.F.strong
                [Html5.F.pcdata "You are not allowed to see this content"]
          | Some exc ->
              if Ocsigen_config.get_debugmode () then
                Html5.F.strong [
                  Html5.F.pcdata message;
                  Html5.F.br ();
                  Html5.F.pcdata (Printexc.to_string exc);
                  Html5.F.br ();
                  Html5.F.em [Html5.F.pcdata "Ocisgen running in debug mode"];
                ]
              else
                Html5.F.strong [Html5.F.pcdata message]
      in
      Html5.F.p ~a:[Html5.F.a_class classes] [message]

  method bind_or_display_error : 'a 'b.
    'a Lwt.t ->
    ('a -> ((Html5_types.nmtoken list * ([> Html5_types.p ] as 'b) Html5.F.elt list) as 'c) Lwt.t) ->
    'c Lwt.t
  = fun data transform_data ->
    try_lwt
      data >>= transform_data
    with exc ->
      Lwt.return
        ([error_class], [self#display_error_box ~exc ()])

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
  method private retrieve_data : 'param -> 'data Lwt.t
  method apply : data:'param -> 'result
end


class virtual ['param, 'data] parametrized_div_widget =
object
  inherit ['param, 'data, [`Div] Html5.F.elt Lwt.t] parametrized_widget
  val! xhtml_class = "parametrized_div_widget"
end


class type ['param, 'data] parametrized_div_widget_t =
    ['param, 'data, [`Div] Html5.F.elt Lwt.t] parametrized_widget_t


class virtual ['param, 'result] parametrized_unit_widget = object
  inherit ['param, unit, 'result] parametrized_widget
  val! xhtml_class = "parametrized_unit_widget"
  method private retrieve_data _ = Lwt.return ()
end

class type ['param, 'result] parametrized_unit_widget_t =
    ['param, unit, 'result] parametrized_widget_t

class virtual ['param] parametrized_unit_div_widget = object
  inherit ['param, unit] parametrized_div_widget
  inherit ['param, [`Div] Html5.F.elt Lwt.t] parametrized_unit_widget
  val! xhtml_class = "parametrized_unit_div_widget"
  method private retrieve_data _ = Lwt.return ()
end

class type ['param] parametrized_unit_div_widget_t =
    ['param, unit, [`Div] Html5.F.elt Lwt.t] parametrized_widget_t


(*
class ['child] list_widget =
object
  inherit widget

  val xhtml_class = "list"

  method display : [`Div] Html5.F.elt Lwt.t =
    Lwt.return (Html5.F.div ~a:[Html5.F.a_class [xhtml_class]] [])

end
*)
