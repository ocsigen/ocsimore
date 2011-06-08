
(*
let appl_ref :
    (module Eliom_output.Eliom_appl) option ref
    = ref None

let appl = Eliom_common.lazy_site_value_from_fun
  (fun () ->
    match !appl_ref with
    | None -> failwith "you must register an eliom application before registering a service"
    | Some a -> a)

let register_appl reg_appl =
  print_endline "register application";
  appl_ref := Some (reg_appl ());
  ignore (Eliom_common.force_lazy_site_value appl);
  appl_ref := None

let eliom_appl () = Eliom_common.force_lazy_site_value appl

module Default_param : Eliom_output.APPL_PARAMS =
struct
  let application_name = "ocsimore"
end

let default_appl =
  (module Eliom_output.Eliom_appl(Default_param) : Eliom_output.Eliom_appl)

let register ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register ?scope

let register_service ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_service ?scope

let register_coservice ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_coservice ?scope

let register_coservice' ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_coservice' ?scope

let register_post_service ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_post_service ?scope

let register_post_coservice ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_post_coservice ?scope

let register_post_coservice' ?scope =
  let module Appl = (val (eliom_appl ()) : Eliom_output.Eliom_appl) in
  Appl.register_post_coservice' ?scope
*)

module Default_param : Eliom_output.APPL_PARAMS =
struct
  let application_name = "ocsimore"
end

module Appl = Eliom_output.Eliom_appl(Default_param)

include Appl
