module type Null = sig end

let v = (module Page_site : Null)
let v = (module Wiki_ext : Null)
let v = (module Wiki_widgets : Null)
let v = (module Forum_widgets : Null)
let v = (module User_widgets : Null)

(** Just a value to keep this module in the .js file even if there
    is no explicit calls *)
let _link = ()

let () = Firebug.console##log(Js.string "ocsimore loaded")
