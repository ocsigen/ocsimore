(* Ocsimore
 * Copyright (C) 2009
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

open Eliom_content

(** An alias for the services that are accepted in the admin menu. *)
type menu_link_service =
    (Eliom_service.get_service_kind,
     [ `Unregistrable | `Registrable ],
     Eliom_registration.non_caml_service)
    Eliom_tools_common.one_page



(** The service that answers for Ocsimore static files. *)
val static_service :
  ((string list, unit, Eliom_service.get_service_kind, [ `WithSuffix ],
    [ `One of string list ] Eliom_parameter.param_name, unit,
    [ `Registrable ], Eliom_registration.http_service)
   Eliom_service.service)

(** Path to a static file, suitable for inclusion in a <a> tag *)
val static_file_uri :
  path:string list
  -> Html5.F.uri


(** Allows to add HTML headers required by page components *)
module Header : sig

    type header

    (** Define a new header *)
    val create_header :
      (unit -> Html5_types.head_content_fun Html5.F.elt list) -> header

    (** Call this function every time you need a header to be included
        in the page. If this function is called several times for the same
        page with the same header, the header will be included only once.
    *)
    val require_header : header -> unit Lwt.t

    (** This function is called to generate the headers for one page.
        Only required headers are included.
    *)
    val generate_headers : unit -> Html5_types.head_content_fun Html5.F.elt list Lwt.t

  end

(*
(** Function to be called when Obrowser is used inside a page.
    The relevant javascript files will be included *)
val add_obrowser_header : sp:Eliom_common.server_params -> unit
 *)


(** Function to be called on admin pages, and which add the
    relevant css (including for the admin menu) *)
val add_admin_pages_header : unit -> unit Lwt.t


(** Registers the string passed as argument so that it is called
    when the onload event on the body tag of the page fires. The
    string must thus be well-formed javascript (without ; at the end) *)
val add_onload_function: string -> unit Lwt.t

(** Generic headers for an html page. The arguments [css] is added
    after the links resulting from the hooks added by the function
    [add_html_header_hook] above. The argument [body_classes] is
    used for the classes of the [body] html element. *)
val html_page :
  ?body_classes:Html5_types.nmtokens ->
  ?css:Html5_types.link Html5.F.elt list->
  ?title:string ->
  ?heading:string ->
  Html5_types.body_content Html5.F.elt list ->
  Html5.F.html Lwt.t

(** Functions related to the administration menu *)


(** Adds an entire subsection, labelled by [name] to the admin menu.
    The service [root] is used to represent this section. For the
    list of links, if the function returns [false], the link is
    not displayed.
*)
val add_to_admin_menu :
  name:string ->
  links:(string *
         menu_link_service *
         (unit -> bool Lwt.t)) list ->
  root:menu_link_service ->
  unit

(* No need to export thisn
(** The admin menu itself. The option [service] parameter is the service
    currently active, which will be displayed in a different way *)
val admin_menu:
  ?service:menu_link_service ->
  Eliom_common.server_params ->
  Html5_types.block Html5.F.elt list Lwt.t
*)

(** Displays a complete admin page, with the admin menu and the status bar.
    If [allow_unlogged] is false, users that have not logged-in will not
    be able to see the page *)
val admin_page:
  ?service:menu_link_service ->
  ?body_classes:string list ->
  ?css:Html5_types.link Html5.F.elt list ->
  title:string ->
  Html5_types.flow5 Html5.F.elt list ->
  Html5.F.html Lwt.t

val body_to_div : Html5_types.body_content Html5.F.elt list -> Html5_types.flow5 Html5.F.elt list

val userid_permissions : (User_sql.Types.userid -> bool Lwt.t) -> bool Lwt.t

val no_permission : unit -> Html5_types.body_content Html5.F.elt list Lwt.t

(** Display a [widget#display] as a [admin_page] under certain [permissions]. Also, some parameters of [admin_page] are
    made available here. *)
val admin_body_content_with_permission_handler :
  title:('get_params -> 'post_params -> string Lwt.t) ->
  ?service:('get_params -> 'post_params -> menu_link_service Lwt.t) ->
  permissions:('get_params -> 'post_params -> bool Lwt.t) ->
  display:('get_params -> 'post_params -> [< Html5_types.body_content] Html5.F.elt list Lwt.t) ->
  'get_params -> 'post_params -> Html5.F.html Lwt.t

val icon:
  path:string ->
  text:string ->
  [> `Img ] Html5.F.elt


val add_status_function:
  (unit
   -> Html5_types.flow5 Html5.F.elt Lwt.t)
   -> unit

