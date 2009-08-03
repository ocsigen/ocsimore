open Js

let (>=>) = JSOO.(>>>)


let split_string s chars =
  let len = String.length s in
  (* pos is the current position in the string,
     acc is the start of the next supposed substring *)
  let rec iter acc pos =
    if pos >= len then
      if acc = pos then [] else [String.sub s acc (pos-acc)]
    else
      if List.mem s.[pos] chars then
        if acc = pos then
          iter_start (pos+1)
        else
          (String.sub s acc (pos-acc)) :: iter_start (pos+1)
      else
        iter acc (pos + 1)
  (* Function starting a potential new substring *)
  and iter_start pos =
    iter pos pos
  in
  iter_start 0



(*
let document = eval "document"
let css = 
  document >>> get "styleSheets" >>> get "0"
let get_element_by_id id =
  document >>> call_method "getElementById" [| string id |]

let highlight_wikibox id =
  let wikibox = get_element_by_id id in
  css >>> call_method "addRule"
    [| string "div.wikibox"; string "background-color: #fafafa; color: #e0e0e0" |] >>> ignore ;
  wikibox >>> get "style" >>> set "background-color" (string "white");
  wikibox >>> get "style" >>> set "color" (string "black")

let _ =
  Eliom_obrowser_client.register_closure
    778
    highlight_wikibox

let un_highlight_wikibox id =
  let wikibox = get_element_by_id id in
  css >>> call_method "addRule"
    [| string ".wikibox"; string "background-color: #ffffff" |] >>> ignore ;
  wikibox >>> get "style" >>> set "background-color" (string "white")

let _ =
  Eliom_obrowser_client.register_closure
    779
    un_highlight_wikibox

*)
