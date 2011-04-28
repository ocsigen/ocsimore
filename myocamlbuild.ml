open Ocamlbuild_plugin;;
open Command;;

Options.use_ocamlfind := true;;

dispatch begin function
  | After_rules ->

    flag ["ocaml"; "compile"; "pa_trace"] (S[A"-ppopt"; A"trace.cmo"]);
    flag ["ocaml"; "ocamldep"; "pa_trace"] (S[A"-ppopt"; A"trace.cmo"]);
    flag ["ocaml"; "doc"; "pa_trace"] (S[A"-ppopt"; A"trace.cmo"]);
    dep ["ocaml"; "ocamldep"; "pa_trace"] ["trace.cmo"];

    flag ["ocamlmklib"; "c"; "use_crypt"]
      (A "-lcrypt");

(*
    flag ["link"; "ocaml"; "library"; "use_crypt"]
      (S[A"-cclib"; A "-lcrypt"; A"-dllib"; A "-lcrypt"]);
*)

    (* cryptokit is an ocaml library.
       This will declare use_nis_chkpwd and include_nis_chkpwd *)
    (*ocaml_lib "nis_chkpwd";*)
    flag ["link"; "library"; "ocaml"; "byte"; "use_nis_chkpwd"]
      (S[ A"-cclib"; A"-Lnis_chkpwd"; A"-dllib"; A"-lnis_chkpwd"; A"-cclib"; A"-lnis_chkpwd" ]);
    flag ["link"; "library"; "ocaml"; "native"; "use_nis_chkpwd"]
      (S[ A"-cclib"; A"-Lnis_chkpwd"; A"-cclib"; A"-lnis_chkpwd" ]);
(*    flag ["shared"; "library"; "ocaml"; "native"; "use_nis_chkpwd"]
      (S[ A"-cclib"; A"-Inis_chkpwd"; A"-dllib"; A"-lnis_chkpwd" ]); *)


    (* When ocaml link something that use the libnis_chkpwd,
       then one need that file to be up to date. *)
    dep  ["link"; "ocaml"; "use_nis_chkpwd"] ["nis_chkpwd/libnis_chkpwd.a"];

  | _ -> ()
end
