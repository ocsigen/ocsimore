
let switchshow =
  fun.client (i1 : string) (i2 : string) -> 
  Js.Node.set_attribute (Js.get_element_by_id i2) "style" "display:block";
  Js.Node.set_attribute (Js.get_element_by_id i1) "style" "display:none"


