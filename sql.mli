(** Database interface. 
    All SQL commands go here. *)

open Ocsimorelib
open CalendarLib

module PGOCaml : sig type 'a t end

type db_t = (string, bool) Hashtbl.t PGOCaml.t

(* val connect: unit -> db_t Lwt.t *)

(** Pool of SQL connections *)
val pool : db_t Lwt_pool.t

(** type for database integers (and IDs) *)
type db_int_t;;

(** type for database offsets, and limits *)
type db_size_t;;

(** type for counts *)
type db_count_t;;

val db_int_of_int: int -> db_int_t
val int_of_db_int: db_int_t -> int
val db_int_of_string: string -> db_int_t
val string_of_db_int: db_int_t -> string
val db_size_of_int: int -> db_size_t
val db_count_of_int: int -> db_count_t




(*
(** create a new service *)
val new_service: db_t -> url:string -> db_int_t Lwt.t

(** list services *)
val list_services: db_t -> string list Lwt.t

val get_service_parameters: db_t -> url:string -> (db_int_t * string) list Lwt.t

val add_parameter_to_service: db_t -> url:string -> param_name:string -> db_int_t Lwt.t
*)
