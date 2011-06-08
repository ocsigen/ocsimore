
module type Null = sig end

let v = (module Page_site : Null)
let v = (module Wiki_ext : Null)
let v = (module Wiki_widgets : Null)

let () = Firebug.console##log(Js.string "ocsimore loaded")
