external identity : 'a -> 'a = "%identity"

type 'a int_t = int
let int_t = identity
let t_int = identity
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
let any_int32 = identity

let int32_t_list = identity
let t_int32_list = identity


let print_int32_t i = print_string (Int32.to_string i)


type 'a string_t = string
let string_t = identity
let t_string = identity
let any_string = identity

let concat = ( ^ )
let concat_list = String.concat

let print_string_t = print_string
