open Lwt
open Eliommod
open Eliom_sessions
open Eliom_duce.Xhtml
open Session_manager

(**
This module contains general-use widgets for Ocsimore.

@author Jaap Boender
*)

class widget: parent:sessionmanager ->
object
	method name: string
end;;

class ['param_type] parametrized_widget: parent:sessionmanager ->
object
	inherit widget
	(**
	This method retrieves the parametrized_widget's data, for example from an SQL database.
	It is normally called by the parametrized_widget's [display] method, and can be
	overridden in order to use another type of storage.
	*)
	method private retrieve_data: 'param_type -> unit Lwt.t

	method apply: sp:server_params -> 'param_type -> Xhtmltypes_duce._div Lwt.t
end;;

(** The base parametrized_widget list class *)
class ['child_type] list_widget: parent:sessionmanager ->
object
	inherit widget
	
	(**
	Display the parametrized_widget list. Calls the display procedure for every item of the
	contents in turn.
	*)
	method display: sp:server_params -> Xhtmltypes_duce._div Lwt.t

	(** Clear the contents *)
	method clear_children: unit

	(** Set the contents *)
	method set_children: 'child_type list -> unit

	(** Get the contents *)
	method get_children: 'child_type list

	(** Add an item to the contents *)
	method add_child: 'child_type -> unit
end;;
