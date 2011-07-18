(* OASIS_START *)
(* DO NOT EDIT (digest: 0968dee0c310a0332f26f78bf85ade1c) *)
module OASISGettext = struct
# 21 "/home/chambart/bordel/oasis/oasis/src/oasis/OASISGettext.ml"

  let ns_ str =
    str

  let s_ str =
    str

  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str

  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""

  let init =
    []

end

module OASISExpr = struct
# 21 "/home/chambart/bordel/oasis/oasis/src/oasis/OASISExpr.ml"



  open OASISGettext

  type test = string

  type flag = string

  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string


  type 'a choices = (t * 'a) list

  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b

        | ENot e ->
            not (eval' e)

        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)

        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)

        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")

        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t

  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)

end


module BaseEnvLight = struct
# 21 "/home/chambart/bordel/oasis/oasis/src/base/BaseEnvLight.ml"

  module MapString = Map.Make(String)

  type t = string MapString.t

  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"

  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end

  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute
          buff
          (fun var ->
             try
               var_expand (MapString.find var env)
             with Not_found ->
               failwith
                 (Printf.sprintf
                    "No variable %s defined when trying to expand %S."
                    var
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)

  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


module MyOCamlbuildFindlib = struct
# 21 "/home/chambart/bordel/oasis/oasis/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml"

  (** OCamlbuild extension, copied from
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall
    *)
  open Ocamlbuild_plugin

  (* these functions are not really officially exported *)
  let run_and_read =
    Ocamlbuild_pack.My_unix.run_and_read

  let blank_sep_strings =
    Ocamlbuild_pack.Lexers.blank_sep_strings

  let split s ch =
    let x =
      ref []
    in
    let rec go s =
      let pos =
        String.index s ch
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x

  let split_nl s = split s '\n'

  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"

      | After_rules ->

          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";

          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

      | _ ->
          ()

end

module MyOCamlbuildBase = struct
# 21 "/home/chambart/bordel/oasis/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"

  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)



  open Ocamlbuild_plugin

  type dir = string
  type file = string
  type name = string
  type tag = string

# 55 "/home/chambart/bordel/oasis/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"

  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list;
        flags:     (tag list * (spec OASISExpr.choices)) list;
      }

  let env_filename =
    Pathname.basename
      BaseEnvLight.default_filename

  let dispatch_combine lst =
    fun e ->
      List.iter
        (fun dispatch -> dispatch e)
        lst

  let dispatch t e =
    let env =
      BaseEnvLight.load
        ~filename:env_filename
        ~allow_empty:true
        ()
    in
      match e with
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]

        | After_rules ->
            (* Declare OCaml libraries *)
            List.iter
              (function
                 | lib, [] ->
                     ocaml_lib lib;
                 | lib, dir :: tl ->
                     ocaml_lib ~dir:dir lib;
                     List.iter
                       (fun dir ->
                          flag
                            ["ocaml"; "use_"^lib; "compile"]
                            (S[A"-I"; P dir]))
                       tl)
              t.lib_ocaml;

            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; "use_lib"^lib]
                     (S[A"-dllib"; A("-l"^lib); A"-cclib"; A("-l"^lib)]);

                   flag ["link"; "library"; "ocaml"; "native"; "use_lib"^lib]
                     (S[A"-cclib"; A("-l"^lib)]);

                   flag ["link"; "program"; "ocaml"; "byte"; "use_lib"^lib]
                     (S[A"-dllib"; A("dll"^lib)]);

                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep  ["link"; "ocaml"; "use_lib"^lib]
                     [dir/"lib"^lib^"."^(!Options.ext_lib)];

                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"]
                     headers;

                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib]
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;

              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec =
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ ->
            ()

  let dispatch_default t =
    dispatch_combine
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]

end


open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [
          ("src/core/ocsimore", ["src/core"]);
          ("src/user/user", ["src/user"]);
          ("src/wiki/wiki", ["src/wiki"]);
          ("src/site/core_site", ["src/site"; "src/site/server"]);
          ("src/forum/forum", ["src/forum"]);
          ("src/site/wiki_site", ["src/site"; "src/site/server"]);
          ("src/site/client/core_site_client", ["src/site/client"]);
          ("src/site/user_site", ["src/site"; "src/site/server"]);
          ("src/site/forum_site", ["src/site"; "src/site/server"])
       ];
     lib_c = [("ocsimore", "src/core", [])];
     flags =
       [
          (["oasis_library_ocsimore_cclib"; "link"],
            [(OASISExpr.EBool true, S [A "-cclib"; A "-lcrypt"])]);
          (["oasis_library_ocsimore_cclib"; "ocamlmklib"; "c"],
            [(OASISExpr.EBool true, S [A "-lcrypt"])])
       ];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

(* OASIS_STOP *)

Ocamlbuild_pack.Log.classic_display := true;;

let env =
  BaseEnvLight.load
    ~filename:MyOCamlbuildBase.env_filename
    ~allow_empty:true
    ()

let set_var var_name env_var =
  let s = BaseEnvLight.var_get var_name env in
  Unix.putenv env_var s

let set_var_option var_name env_var =
  match BaseEnvLight.var_get var_name env with
    | "none" -> ()
    | s -> Unix.putenv env_var s

let () =
  set_var "pgdatabase" "PGDATABASE";
  set_var "pgport" "PGPORT";
  set_var "pguser" "PGUSER";
  set_var "pgpassword" "PGPASSWORD";
  set_var_option "pghost" "PGHOST";
  set_var_option "pg_socket_domain_dir" "UNIX_SOCKET_DOMAIN_DIR"

let copy_with_header src prod =
  let dir = Filename.dirname prod in
  let mkdir = Cmd (Sh ("mkdir -p " ^ dir)) in
  let contents = Pathname.read src in
  let header = "# 1 \""^src^"\"\n" in
  Seq [mkdir;Echo ([header;contents],prod)]

let copy_rule_with_header name ?(deps=[]) src prod =
  rule name ~deps:(src::deps) ~prod
    ( fun env _ ->
      let prod = env prod in
      let src = env src in
      copy_with_header src prod )

(* we browser throught the file (skipping _build and _darcs directories) searching
   for .eliom files and annotataing them with tags for camlp4 and
   ocamlfind tags *)
let tag_eliom_files () =
  let entries = Ocamlbuild_pack.Slurp.slurp "." in
  let entries = Ocamlbuild_pack.Slurp.filter
    (fun path _ _ ->
      not (Pathname.is_prefix "_build" path)
      &&  not (Pathname.is_prefix "_darcs" path) ) entries in
  let f path name () () =
    if Pathname.get_extension name = "eliom"
    then
      begin
	let ml_name = (Pathname.update_extension "ml" name) in
	let client_file = Pathname.concat (Pathname.concat path "client") ml_name in
	let server_file = Pathname.concat (Pathname.concat path "server") ml_name in
	let type_file = Pathname.concat (Pathname.concat path "type") ml_name in
	let type_inferred = Pathname.concat (Pathname.concat path "type")
	  (Pathname.update_extension "inferred.mli" name) in
	tag_file client_file
	  [ "pkg_eliom.client"; "pkg_eliom.syntax.client";
	    Printf.sprintf "need_eliom_type(%s)" type_inferred; "syntax(camlp4o)";];
	tag_file server_file
	  [ "pkg_eliom.server"; "pkg_eliom.syntax.server";
	    "syntax(camlp4o)";];
	tag_file type_file
	  ( Tags.elements (tags_of_pathname server_file)
	    @ [ "pkg_eliom.server"; "pkg_eliom.syntax.type";
		"syntax(camlp4o)";]);
	(* server files are available from type directory *)
	Pathname.define_context (Pathname.concat path "type") [Pathname.concat path "server"];
      end
  in
  Ocamlbuild_pack.Slurp.fold f entries ()

(* give camlp4 the right mli file with server side inferred types when
   the file is annotated with need_eliom_type *)
let () =
  let use_type_file need =
    S [A"-ppopt"; A"-type"; A"-ppopt"; P need] in
  let tags =
    [["ocaml";"ocamldep"];
     ["ocaml";"compile"];
     ["ocaml";"infer_interface"]]
  in
  List.iter (fun tags -> pflag tags "need_eliom_type" use_type_file) tags;;

(* ocamlfind needs -thread or -vmthread when using a package wich
   depends on the threads, even for the type inferrence pass *)
flag ["ocaml"; "infer_interface"; "thread"] (A"-thread");;
flag ["ocaml"; "library"; "thread"] (A"-thread");;

dep ["js_core_site_client"] ["src/site/client/ocsimore.js"];;

rule "js_of_ocaml: .byte -> .js" ~deps:["%.byte"] ~prod:"%.js"
  begin fun env _ ->
    let eliom_client_js =
      Pathname.concat
	(Ocamlbuild_pack.Findlib.query "eliom.client").Ocamlbuild_pack.Findlib.location
	"eliom_client.js" in
    Cmd (S [A "js_of_ocaml"; A "-pretty"; A "-noinline"; P eliom_client_js; A (env "%.byte")])
  end;;

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
	 | Before_options ->
	   tag_eliom_files ()
         | After_rules ->
	   (* only works in subdirectories: no source at toplevel *)
	   copy_rule "shared/*.ml -> client/*.ml"
	     "%(path)/shared/%(file).ml" "%(path)/client/%(file).ml";
	   copy_rule "shared/*.ml -> server/*.ml"
	     "%(path)/shared/%(file).ml" "%(path)/server/%(file).ml";
	   copy_rule_with_header "*.eliom -> server/*.ml"
	     "%(path)/%(file).eliom" "%(path)/server/%(file).ml";
	   copy_rule_with_header "*.eliom -> type/*.ml"
	     "%(path)/%(file).eliom" "%(path)/type/%(file).ml";
	   copy_rule_with_header "*.eliom -> client/*.ml"
	     ~deps:["%(path)/type/%(file).inferred.mli"]
	     "%(path)/%(file).eliom" "%(path)/client/%(file).ml"
	 | _ -> ()
    )

