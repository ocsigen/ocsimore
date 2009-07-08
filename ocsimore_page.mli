(* Ocsimore
 * Copyright (C) 2008
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
   @author Boris Yakobowski
*)


(** Registers the service that answers for static files.
    This function must be called at eliom initialisation time,
    as it registers another service automatically *)
val set_service_for_static_files :
  ((string list, unit, Eliom_services.get_service_kind, [ `WithSuffix ],
    [ `One of string list ] Eliom_parameters.param_name, unit,
    [ `Registrable ])
   Eliom_services.service) -> unit

(** Path to a static file, suitable for inclusion in a <a> tag *)
val static_file_uri :
  sp:Eliom_sessions.server_params -> path:string list -> Eliom_duce.Xhtml.uri


(** Register a function that will be called when generating the
    html header for a page. The result of the function will be
    added to this header *)
val add_html_header_hook :
  (Eliom_sessions.server_params ->
  {{ [ (Xhtmltypes_duce.link | Xhtmltypes_duce.script)* ] }}) -> unit


(** Function to be called when Obrowser is used inside a page.
    The relevant javascript files will be included *)
val add_obrowser_header : Eliom_sessions.server_params -> unit


(** Generic headers for an html page. The arguments [css] is added
    after the links resulting from the hooks added by the function
    [add_html_header_hook] above. The argument [body_classes] is
    used for the classes of the [body] html element. *)
val html_page :
  sp:Eliom_sessions.server_params ->
  ?body_classes:string list ->
  ?css:Xhtmltypes_duce.links ->
  ?title:string ->
  Xhtmltypes_duce.blocks ->
  Xhtmltypes_duce.html Lwt.t
