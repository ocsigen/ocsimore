(* Copyright Vincent Balat, Séverine Maingaud *)

let dollar (elt : Dom_html.element Js.t) : 'a Js.t =
  Js.Unsafe.fun_call (Js.Unsafe.variable "$") [|Js.Unsafe.inject elt|]

let toggle ?animate elt : unit =
  match animate with
    | None -> (dollar elt)##toggle()
    | Some s -> (dollar elt)##toggle(s)
