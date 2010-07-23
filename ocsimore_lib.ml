open Lwt


let ocsimore_admin_dir = "ocsimoreadmin"

      (* USEFUL STUFF *)

      (* these operators allow to write something like this:
         list_item_1 ^:  
         false % list_item_2 ^?  
         true % list_item_3 ^?  
         false % list_item_4 ^?  
         list_item_5 ^:
         []
         which evaluates to [list_item_1; list_item_3; list_item_5]. *)
let ( ^? ) (cond, x) xs = if cond then x::xs else xs (* right assoc *)
let ( ^: ) x xs = x :: xs (* right assoc, same precedence of ^? *)
let ( % ) x y = x,y  (* left assoc, higher precedence *)


type 'a tree = Node of 'a * ('a tree list);;

    (* A user defined parameter type *)
(* let id p = user_type Sql.db_int_of_string Sql.string_of_db_int p  *)
(* let int64 p = user_type Int64.of_string Int64.to_string p *)

let rec lwt_tree_map (f: 'a -> 'b Lwt.t) (tree: 'a tree): 'b tree Lwt.t =
let Node (p, cs) = tree in
  f p >>=
        fun start -> lwt_forest_map f cs >>=
        fun rest -> return (Node (start, rest))
and lwt_forest_map (f: 'a -> 'b Lwt.t) (forest: 'a tree list): 'b tree list Lwt.t =
        Lwt_util.map (fun t -> lwt_tree_map f t) forest

let rec lwt_flatten (l: 'a list list): 'a list Lwt.t =
match l with
| [] -> return []
| h::t -> lwt_flatten t >>=
        fun ft -> return (List.append h ft);;

let rec lwt_tree_flatten (tree: 'a tree): 'a list Lwt.t =
let Node (p, cs) = tree in
        lwt_forest_flatten cs >>=
        fun rest -> return (p::rest)
and lwt_forest_flatten (forest: 'a tree list): 'a list Lwt.t =
        Lwt_util.map lwt_tree_flatten forest >>=
        fun f -> lwt_flatten f


let list_assoc_opt a l =
  try
    Some (List.assoc a l)
  with Not_found -> None

let list_assoc_default a l default =
  try
    List.assoc a l
  with Not_found -> default

let list_assoc_exn a l exn =
  try List.assoc a l
  with Not_found -> raise exn

let bind_opt o f = match o with
  | None -> None
  | Some s -> Some (f s)

let lwt_bind_opt o f = match o with
  | None -> Lwt.return None
  | Some s -> f s >>= fun r -> Lwt.return (Some r)

let int_of_string_opt s = 
  bind_opt s int_of_string

let string_of_string_opt = function
  | None -> ""
  | Some s -> s

let rec lwt_filter f = function
  | [] -> Lwt.return []
  | a::l -> 
      let llt = lwt_filter f l in
      f a >>= fun b ->
      llt >>= fun ll ->
      if b
      then Lwt.return (a::ll)
      else Lwt.return ll

let rec find_opt f = function
  | [] -> None
  | e :: l ->
      match f e with
        | None -> find_opt f l
        | Some v -> Some v

let rec concat_list_opt lo l = match lo with
  | [] -> l
  | None :: q -> concat_list_opt q l
  | Some e :: q -> e :: concat_list_opt q l



(** Association maps with default values (which thus never raise [Not_found] *)
module type DefaultMap = sig
  type key
  type 'a t

  val empty : (key -> 'a) -> 'a t
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
end

module DefaultMap (X : Map.OrderedType) : DefaultMap with type key = X.t
= struct
  type key = X.t
  module XMap = Map.Make(X)

  type 'a t = {
    default: X.t -> 'a;
    map: 'a XMap.t
  }
  let empty default = {
    default = default;
    map = XMap.empty
  }

  let find k map =
    try XMap.find k map.map
    with Not_found -> map.default k

  let add k v map =
    { map with map = XMap.add k v map.map }
end



let remove_prefix ~s ~prefix =
  let slen = String.length s
  and preflen = String.length prefix in
  let preflast = preflen - 1 in
  let first_diff = Ocsigen_lib.string_first_diff prefix s 0 preflen in
  if first_diff = preflen
  then Some (String.sub s preflen (slen - preflen))
  else if first_diff = preflast && slen = preflast
  then Some ""
  else None

let remove_begin_slash s =
  if s = "" then ""
  else if s.[0] = '/' then
    String.sub s 1 ((String.length s) - 1)
  else s


let hidden_bool_input :
  value:bool ->
  [< bool Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
  [>Xhtmltypes.input] XHTML.M.elt
 = fun ~value name ->
   Eliom_predefmod.Xhtml.user_type_input string_of_bool
     ~input_type:`Hidden ~value ~name ()


let eliom_bool =
  Eliom_parameters.user_type ~to_string:string_of_bool ~of_string:bool_of_string
