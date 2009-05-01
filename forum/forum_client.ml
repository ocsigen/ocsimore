open Js

let show (i1, i2) =
  Js.Node.set_attribute (Js.get_element_by_id i2) "style" "display:block";
  Js.Node.set_attribute (Js.get_element_by_id i1) "style" "display:none"

let _ =
  Eliom_obrowser.register_closure
    132
    show


(*
let i1 = Js.get_element_by_id "__23"
let i2 = Js.get_element_by_id "__24"


let _ = Js.Node.register_event i1 "onclick" (show i1) i2
*)

(*
module Table = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

let table = ref Table.empty

let _ = 
  table := Table.add "__1" 
    (fun () -> 
       Js.alert "aa";
    )
    !table

let rec browse node =
  (try
     let onclick = Js.Node.get_attribute node "onclick" in
     let f = Table.find onclick !table in
     Js.Node.register_event node "onclick" f () 
   with _ -> ());
  Js.Node.iter browse node

let _ = browse Js.Node.document

let _ = Js.alert "Ready!"
*)
