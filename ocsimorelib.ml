open Eliom


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

    (* A user defined parameter type *)
let int32 p = user_type Int32.of_string Int32.to_string p 

