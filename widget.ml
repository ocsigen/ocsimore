open Lwt
open Eliommod
open Eliom_duce.Xhtml
open Eliom_sessions
open SessionManager
open Ocsimorelib

class widget ~(parent: sessionmanager) =
object (self)

	val div_class = "widget"
	(* val mutable role: Sql.role = Sql.Unknown *)

	method name = div_class
end;;

class ['param_type] parametrized_widget ~(parent: sessionmanager) =
object (self)
	inherit widget parent

	val div_class = "parametrized_widget"

	method private retrieve_data (p: 'param_type) =
		return ()
	
	method apply ~(sp: server_params) (p: 'param_type) =
		return {{ <div class={: div_class :}>[] }}
end;;

class ['child_type] list_widget ~(parent: sessionmanager) =
object (self)
	inherit widget parent

	val div_class = "list"
	val mutable children: 'child_type list = []

	method display ~(sp:server_params): Xhtmltypes_duce._div Lwt.t =
	return
	{{
		<div class={: div_class :}>[]
	}}

	method clear_children: unit =
		children <- [];

	method set_children c =
		children <- c;

	method get_children =
		children

	method add_child i =
		children <- children @ [i]
end;;
