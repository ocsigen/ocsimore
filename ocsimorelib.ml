open Lwt
(* open Eliommod *)
open Eliomsessions

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

    (* some shortnamed functions for conversions *)
let soL (* "string of Long" *) = Int64.to_string
let sol (* "string of long" *) = Int32.to_string
let sod (* "string of date" *) = Printer.CalendarPrinter.to_string

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
