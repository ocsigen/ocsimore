
let delete_wikibox =
  <:obrofun< (href : string) (id : string) -> 
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
  >>

let switch_menu =
  <:obrofun< (id : string) ->
  let link = Js.get_element_by_id id
  and body = Js.get_element_by_id "body"
  and class_no_menu = "nomenu" in
  let classes_body =
    split_string (Js.Node.get_attribute body "className") [' ']
  and update_classes_body classes =
    Js.Node.set_attribute body "class" (String.concat " " classes)
  in

  if List.mem class_no_menu classes_body then (
    JSOO.set "innerHTML" (JSOO.string "Hide menus") link;
    update_classes_body (List.filter ((<>) class_no_menu) classes_body)
  )
  else (
    JSOO.set "innerHTML" (JSOO.string "Show menus") link;
    update_classes_body (class_no_menu :: classes_body)
  )
    >>

let toggle_wikibox_permissions =
  <:obrofun< () ->
  let div = Js.get_element_by_id "wikiboxpermissions"
  and checkbox = Js.get_element_by_id "checkwikiboxpermissions"
  in
  let v = checkbox >=> JSOO.get "checked" >=>
    JSOO.call_method "toString" [||] >=> JSOO.as_string in
  div >=> JSOO.get "style" >=> JSOO.set "display"
    (JSOO.string (if v = "true" then "block" else "none"))
  >>
