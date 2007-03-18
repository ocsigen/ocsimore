(* Some useful functions missing in Ocsigen.Xhtml... *)

open XHTML.M
open Ocsigen
open Ocsigen.Xhtml

(* BAD CODE HERE -- BAD CODE HERE -- BAD CODE HERE *)
let string_of_param_name (p:'b param_name) = ((Obj.magic p):string)
  (* In Ocsigen, 'a param_name = string, so I forced a type cast to put
     the parameter name straight into XHTML code. *)

let reset_input ?(a=[]) v =
  input ~a:((a_input_type `Reset) :: (a_value v) :: a) ()

let select_option ?(a=[]) ?default val_opt_list string_of param = 
  let opttag ?(a=[]) (v,o) =
    let attr = (a_value (string_of v)) :: a in
      if default = Some v then 	 
	option ~a:((a_selected `Selected) :: attr) (pcdata o) 
      else
	option ~a:attr (pcdata o) 
  in match val_opt_list with
    | hd::tl -> 
        XHTML.M.select ~a:((a_name (string_of_param_name param)) :: a)
          (opttag hd) 
          (List.map opttag tl)
    | [] -> failwith "Empty options list"
