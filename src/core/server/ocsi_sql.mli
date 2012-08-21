(** Database interface to the Ocsimore database. *)

module Lwt_PGOCaml : PGOCaml_generic.PGOCAML_GENERIC
  with type 'a monad = 'a Lwt.t
module Lwt_Query : Query.QUERY
  with type 'a Db.t = 'a Lwt_PGOCaml.t and type 'a Db.monad = 'a Lwt.t

type db_t = Lwt_PGOCaml.pa_pg_data Lwt_PGOCaml.t

val pool : db_t Lwt_pool.t

(** Perform an atomic transaction (using BEGIN and COMMIT/ROLLBACK *)
val transaction_block : db_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Same as [transaction_block] but takes a db connection in the pool. *)
val full_transaction_block : (db_t -> 'a Lwt.t) -> 'a Lwt.t

(** Functions that transform 'a option to 'b Sql.t option *)
val map_option_int32 : int32 option ->
  < nul : 'a; t : Sql.int32_t > Sql.t option
val map_option_string : string option ->
  < nul : 'a; t : Sql.string_t > Sql.t option
