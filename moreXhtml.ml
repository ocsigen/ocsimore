(* Some useful functions missing in Ocsigen.Xhtml... *)

open XHTML.M
open Eliom
open Eliom.Xhtml

let reset_input ?(a=[]) v =
  input ~a:((a_input_type `Reset) :: (a_value v) :: a) ()

