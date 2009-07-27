external identity : 'a -> 'a = "%identity"

type 'a int_t = int
let int_t = identity
let t_int = identity
let int_t_option = identity
let t_int_option = identity
let any_int = identity

let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let neg = ( ~- )

let successor = succ
let predecessor = pred
let increment = incr

let print_int_t = print_int

type 'a int32_t = int32

let int32_t = identity
let t_int32 = identity
let int32_t_option = identity
let t_int32_option = identity
let any_int32 = identity

let int32_t_list = identity
let t_int32_list = identity


let print_int32_t i = print_string (Int32.to_string i)
let int32_t_to_string = Int32.to_string

type 'a string_t = string
let string_t = identity
let t_string = identity
let string_t_option = identity
let t_string_option = identity
let any_string = identity

let concat = ( ^ )
let concat_list = String.concat

let print_string_t = print_string


open Xform.XformLwt

let int32_input_aux_xform ?a s =
  convert (string_input ?a s)
    (fun s ->
       Lwt.return (
         try Xform.Converted (Int32.of_string s)
         with Failure _ -> Xform.ConvError ("Invalid value " ^ s)
       ))

let int32_input_xform ?a (i : 'a int32_t) :
    (Xform.inline, 'a int32_t) Xform.XformLwt.t =
  int32_input_aux_xform ?a (Int32.to_string i)

let int32_input_opt_aux_xform ?a s =
  convert (string_input ?a s)
    (fun s ->
       Lwt.return (
         if s = "" then
           Xform.Converted None
         else
           try Xform.Converted (Some (Int32.of_string s))
           with Failure _ -> Xform.ConvError ("Invalid value " ^ s)
       ))

let int32_input_opt_xform ?a : 'a int32_t option -> (Xform.inline, 'a int32_t option) Xform.XformLwt.t = function
  | None -> int32_input_opt_aux_xform ?a ""
  | Some v -> int32_input_opt_aux_xform ?a (Int32.to_string  v)
