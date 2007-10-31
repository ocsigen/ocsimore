open Lwt

type service_type =
	String
| Int

type service_parameter =
{
	name: string;
	p_type: service_type
}

type service_widget = unit

let create_service db ~url =
	Sql.new_service db url >>=
	fun _ -> return ()

let get_services db =
	Sql.list_services db 

let get_service_parameters db ~url =
	Sql.get_service_parameters db ~url >>=
	fun res -> Lwt_util.map (fun (id, name, tp) ->
	(if tp = "string" then return String
	else if tp = "int" then return Int
	else fail (Failure "Unknown database type for parameter")) >>=
	fun t -> return {name=name; p_type=t}) res

let add_parameter db ~url ~param =
	Sql.add_parameter_to_service db ~url ~param_name:param.name ~param_type:
		(match param.p_type with
		| String -> "string"
		| Int -> "int") >>=
	fun _ -> return ()

let get_service_widgets ~url =
	return []
