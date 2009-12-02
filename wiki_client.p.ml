begin.client

open Js

let (>>>) x f = f x

(*
let rec f () =
  Js.alert "a";
  Lwt.bind (Lwt_obrowser.sleep 2.) f

let _ = f ()
*)


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

end


let delete_wikibox =
  fun.client (href : string) (id : string) -> 
  let parent = Js.get_element_by_id id in
  let box =
    Js.Html.div ~attrs:[("class","deletewidget")] 
      [Js.Html.a ~href [Js.Html.string "Confirm deletion"];
       Js.Html.br ()]
  in
  Js.Node.append box
    (Js.Html.a ~onclick:(fun () -> Js.Node.remove parent box)
       [Js.Html.string "Cancel"]);
  Js.Node.append parent box
  

let switch_menu =
  fun.client (id : string) (msgon : string) (msgoff : string) ->
  let link = Js.get_element_by_id id
  and body = Js.get_element_by_id "body"
  and class_no_menu = "nomenu" in
  let classes_body =
    split_string (Js.Node.get_attribute body "className") [' ']
  and update_classes_body classes =
    Js.Node.set_attribute body "class" (String.concat " " classes)
  in

  if List.mem class_no_menu classes_body then (
    JSOO.set "innerHTML" (JSOO.string msgoff) link;
    update_classes_body (List.filter ((<>) class_no_menu) classes_body)
  )
  else (
    JSOO.set "innerHTML" (JSOO.string msgon) link;
    update_classes_body (class_no_menu :: classes_body)
  )
