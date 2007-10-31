open Lwt
open Eliommod
open Eliomparameters
open Eliomservices
open Eliomduce
open Ocsimorelib
open SessionManager
open Widget

(**
This module contains forum widgets for use with the {!Forum.forum} class

@author Jaap Boender
*)

type message_data =
{
	id: int;
	text: string;
	author: string;
	datetime: Calendar.t
}

(** A parametrized_widget that displays one message*)
class message_widget: parent:sessionmanager ->
object
	inherit [int] parametrized_widget

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

class message_list_widget: parent:sessionmanager ->
object
	inherit [message_data] list_widget
	inherit [int * int * int option * int option] parametrized_widget
end;;

class message_navigation_widget: parent:sessionmanager -> srv_thread: (int * (int * int option), unit, get_service_kind, [`WithoutSuffix], [`One of int] param_name * ([`One of int] param_name * [`Opt of int] param_name), unit, [`Registrable]) service ->
object
	inherit [int * int * int option * int option] parametrized_widget
end;;

class message_forest_widget: parent:sessionmanager -> srv_reply_message:(int * (int * (int option * int)), unit, get_service_kind, [`WithoutSuffix], [`One of int] param_name * ([`One of int] param_name * ([`Opt of int] param_name * [`One of int] param_name)), unit, [`Registrable]) service ->
object
	inherit [int * int * int option] parametrized_widget	

	method get_children: message_data Ocsimorelib.tree list

	method set_children: message_data Ocsimorelib.tree list -> unit
end;;

class message_form_widget: parent:sessionmanager -> srv_add_message: (int * (int * int option), string * (int option * bool), post_service_kind, [`WithoutSuffix], [`One of int] param_name * ([`One of int] param_name * [`Opt of int] param_name), [`One of string] param_name * ([`Opt of int] param_name * [`One of bool] param_name), [`Registrable]) service ->
object
	inherit [int * int * int option * int option] parametrized_widget
end;;

class message_add_action: parent:sessionmanager ->
object
	inherit [int * int * int option * string * bool] parametrized_widget
end;;

class latest_messages_widget: parent:sessionmanager ->
object
	inherit [int list * int] parametrized_widget
end;;

type thread_data =
{
	id: int;
	subject: string;
	author: string;
	datetime: Calendar.t
}

class thread_widget: parent:sessionmanager -> 
object
	inherit [int * int] parametrized_widget
	
	(** Set the thread subject *)
	method set_subject: string -> unit

	(** Set the tread author *)
	method set_author: string -> unit

	(**
	Set the thread article. What we call an article is a special sort of message
	in a thread that can only be specified when creating the tread; this is for
	example useful if the thread contains a news article that can be reacted
	upon.
	*)
	method set_article: string -> unit

	(** Set the thread date and time *)
	method set_datetime: Calendar.t -> unit

	(**
	Indicate whether the thread is hidden (if not, individual messages can
	still be hidden)
	*)
	method set_hidden: bool -> unit

	(** Set the number of shown messages *)
	method set_shown_messages: int -> unit

	(** Set the number of hidden messages *)
	method set_hidden_messages: int -> unit

	(** Get the thread subject *)
	method get_subject: string

	(** Get the thread author *)
	method get_author: string

	(** Get the thread's article (if applicable) *)
	method get_article: string option

	(** Get the thread creation time *)
	method get_datetime: Calendar.t

	(** Query whether the thread is hidden *)
	method get_hidden: bool

	(** Get the number of shown messages in the thread *)
	method get_shown_messages: int

	(** Get the number of shown messages in the thread *)
	method get_hidden_messages: int
end;;

class thread_list_widget: parent:sessionmanager -> srv_thread: (int * (int * int option), unit, get_service_kind, [`WithoutSuffix], [`One of int] param_name * ([`One of int] param_name * [`Opt of int] param_name), unit, [`Registrable]) service ->
object
	inherit [thread_data] list_widget
	inherit [int] parametrized_widget 
end;;

class thread_form_widget: parent: sessionmanager -> srv_add_thread: (int, bool * (string * string), post_service_kind, [`WithoutSuffix], [`One of int] param_name, [`One of bool] param_name * ([`One of string] param_name * [`One of string] param_name), [`Registrable]) service -> 
object
	inherit [int] parametrized_widget
end;;

class thread_add_action: parent:sessionmanager ->
object
	inherit [int * bool * string * string] parametrized_widget
end;;
