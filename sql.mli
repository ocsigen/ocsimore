(** Database interface. 
    All SQL commands go here. *)

open Ocsimore_lib
open CalendarLib

module PGOCaml : PGOCaml_generic.PGOCAML_GENERIC with type 'a monad = 'a Lwt.t


type db_t = PGOCaml.pa_pg_data PGOCaml.t

(* val connect: unit -> db_t Lwt.t *)

(** Pool of SQL connections *)
val pool : db_t Lwt_pool.t

(** Perform an atomic transaction (using BEGIN and COMMIT/ROLLBACK *)
val transaction_block : db_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Same as [transaction_block] but takes a db connection in the pool. *)
val full_transaction_block : (db_t -> 'a Lwt.t) -> 'a Lwt.t





(*
(** create a new service *)
val new_service: db_t -> url:string -> int32 Lwt.t

(** list services *)
val list_services: db_t -> string list Lwt.t

val get_service_parameters: db_t -> url:string -> (int32 * string) list Lwt.t

val add_parameter_to_service: db_t -> url:string -> param_name:string -> int32 Lwt.t
*)
