(* OASIS_START *)

let () =
  let command = Printf.sprintf "oasis setup-dev -run %s %s" Sys.executable_name (String.concat " " (Array.to_list Sys.argv)) in
  Printf.eprintf "I: Running command '%s'\n%!" command;
  exit (Sys.command command)
;;

(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "site_client" ->
	  (cs, bs, lib, ["_build/src/site/client/ocsimore.js"])
        | _ ->
          (cs, bs, lib, [])
;;

let add_option (desc,name,value) =
  BaseEnv.var_define
    ~hide:false
    ~dump:true
    ~cli:BaseEnv.CLIAuto
    ~arg_help:desc
    name
    (lazy value)

let _ =
  List.map add_option
    [ "database host address (none meaning using local unix socket)",
      "pghost",
      "none";

      "database port",
      "pgport",
      "5432";

      "database name",
      "pgdatabase",
      "ocsimore";

      "database user",
      "pguser",
      "ocsimore";

      "database password",
      "pgpassword",
      "ocsimore";

      "database unix socket domain directory (default to none)",
      "pg_socket_domain_dir",
      "none" ]


let () = setup ();;
