let delete_widget (href, id) =
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

let _ =
  Eliom_obrowser_client.register_closure
    777
    delete_widget

