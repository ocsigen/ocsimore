open Js
open Html

let switchshow (i1, i2) =
  Js.Node.set_attribute (Js.get_element_by_id i2) "style" "display:block";
  Js.Node.set_attribute (Js.get_element_by_id i1) "style" "display:none"

let _ =
  Eliom_obrowser_client.register_closure
    1
    switchshow

