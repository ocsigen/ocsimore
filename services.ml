open Lwt
open Eliom_parameters
open Eliom_services

let services_table: (string, (string list, unit, [`Attached of [`Internal of [`Service ] * [`Get ] ] Eliom_services.a_s ], [ `WithoutSuffix ], [ `One of string ] Eliom_parameters.param_name Eliom_parameters.listnames, unit, [`Registrable ]) Eliom_services.service) Hashtbl.t = Hashtbl.create 1;;

(* For the moment, all parameters are strings. *)
(* type service_type =
  Int
| Float
| String
| Bool
| File
| Unit *)

type service_parameter =
{
	name: string;
	(* p_type: service_type *)
}

type service_widget = unit

(* let string_of_type t =
	match t with
	| Int -> "int"
	| Float -> "float"
	| String -> "string"
	| Bool -> "bool"
	| File -> "file"
	| Unit -> "unit" *)

(* let type_of_string s =
	if s = "int" then Int
	else if s = "float" then Float
	else if s = "string" then String
	else if s = "bool" then Bool
	else if s = "file" then File
	else if s = "unit" then Unit
	else raise Not_found *)

let create_service db ~url =
	Sql.new_service db url >>=
	fun _ -> return ()

let get_services db =
	Sql.list_services db 

let get_service_parameters db ~url =
	Sql.get_service_parameters db ~url >>=
	fun t -> return (List.map (fun (id, name) -> {name = name}) t)

let add_parameter db ~url ~param =
	Sql.add_parameter_to_service db ~url ~param_name:param.name >>=
	fun _ -> return ()

let get_service_widgets ~url =
	return []

let register_service ?sp db ~url =
	get_service_parameters db ~url >>=
	fun params -> 
	let srv = Eliom_duce.Xhtml.register_new_service ?sp ~path:[url] ~get_params:(list "param" (string "value"))
		(fun sp _ _ ->
			return {{
				<html>[<head>[<title>"Your service"]
				<body>[<h1>"Service"]]
			}}
		) in
	Hashtbl.add services_table url srv;
	return ()

let register_services db =
	get_services db >>=
	fun services -> Lwt_util.iter (fun url -> register_service db ~url) services
