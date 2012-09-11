
{shared{
  open Eliom_content
}}

{client{

  exception Not_implemented

  let sectionning_content =
    ["article"; "aside"; "nav"; "section"]

  let sectionning_root =
    ["blockquote"; "body"; "details"; "fieldset"; "figure"; "td"]

  let sectionning_tag = sectionning_root @ sectionning_content

  let heading_content =
    ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "hgroup"]


  (* The h1 element is said to have the highest rank, the h6 element
     has the lowest rank, and two elements with the same name have
     equal rank. *)
  type rank = H6 | H5 | H4 | H3 | H2 | H1 | Top

  type outline = section list
  and section = Section of Dom.node Js.t list * string option * outline (* FIXME heading_content *)

  type heading =
    | Unnamed of string
    | Named of (rank * Dom.node Js.t list * string option)

  type state = {
    heading: heading;
    outline: outline;
    context: ((rank * Dom.node Js.t list * string option) * outline) list;
    ignore: Dom.node Js.t -> bool
  }

  exception FoundNode of Dom.node Js.t

  let find_first_heading node =
    let rec find node =
      let tag = String.lowercase (Js.to_string node##nodeName) in
      if List.mem tag ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] then
        raise (FoundNode node)
      else if not (List.mem tag sectionning_tag) then
        List.iter find (Dom.list_of_nodeList node##childNodes)
    in
    try find node; raise Not_found with FoundNode t -> t

  let find_previous_heading node =
    let rec previous node =
      Js.Opt.case (node##previousSibling)
        (fun () ->
          Js.Opt.case (node##parentNode)
            (fun () -> raise Not_found)
            previous )
        (fun node -> node)
    in
    let rec find node =
      let tag = String.lowercase (Js.to_string node##nodeName) in
      if List.mem tag ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] then
        raise (FoundNode node)
      else if not (List.mem tag sectionning_tag) then
        ( List.iter find (List.rev (Dom.list_of_nodeList node##childNodes));
          find (previous node) )
    in
    try find (previous node); raise Not_found
    with
    | FoundNode t -> Js.Opt.return t
    | Not_found -> Js.Opt.empty

  let rank_of_elt node =
    match String.lowercase (Js.to_string node##nodeName) with
    | "h1" -> H1
    | "h2" -> H2
    | "h3" -> H3
    | "h4" -> H4
    | "h5" -> H5
    | "h6" -> H6
    | _ -> assert false

  let new_fragment =
    let cpt = ref 0 in
    (* TODO use content of the node to name the target like ocamldoc ?? *)
    fun () -> incr cpt; Format.sprintf "h5o-%d" !cpt

  let get_fragment node =
    Js.Opt.case (Dom_html.CoerceTo.element node)
      (fun () -> None)
      (fun elt ->
        Js.Opt.case (elt##getAttribute(Js.string "id"))
          (fun () ->
            let fragment = new_fragment () in
            elt##setAttribute(Js.string "id", Js.string fragment);
            Some fragment)
          (fun s -> Some (Js.to_string s)))

  let unnamed tag =
    [(Dom_html.document##createTextNode (Js.string ("Unnamed " ^ tag)) :> Dom.node Js.t)]

  let step_up st =
    match st.context with
    | [] -> assert false
    | (upper_heading, upper_outline) :: context ->
        let (heading, fragment) =
          match st.heading with
          | Named (_, nodes, fragment) -> (nodes, fragment)
          | Unnamed tag -> (unnamed tag, None)
        in
        { st with
          heading = Named upper_heading;
          outline = (Section (heading, fragment, List.rev st.outline) :: upper_outline);
          context;
          }

  let rec insert_heading st (rank, _, _ as node) =
    match st.heading with
    | Unnamed _ -> assert false
    | Named (candidate_rank, _, _ as candidate) ->
        if candidate_rank > rank then
          { st with
            heading = Named node;
            outline = [];
            context = (candidate, st.outline) :: st.context }
        else
          insert_heading (step_up st) node

  let rec rebuild st =
    match st.context, st.heading with
    | [], _
    | [(Top, [], None), []], Unnamed _ -> st.outline
    | _, Named _ | _, Unnamed _ -> rebuild (step_up st)

  let init_st ?(ignore = (fun _ -> false)) tag =
    { heading = Unnamed tag; outline = []; context = [(Top, [], None), []]; ignore }

  let rec walk st node =
    let tag = String.lowercase (Js.to_string node##nodeName) in
    if List.mem tag heading_content then
      let node = find_first_heading node in
      let childrens =
        List.map
          (fun node -> node##cloneNode(Js._true))
          (Dom.list_of_nodeList node##childNodes)
      and fragment = get_fragment node in
      let candidate = (rank_of_elt node, childrens, fragment) in
      match st.heading with
      | Unnamed _ when st.outline = [] -> { st with heading = Named candidate }
      | Unnamed tag when st.context = [(Top, [], None), []] ->
          { st with
            heading = Named candidate;
            outline = [];
            context = ((Top, unnamed tag, fragment), st.outline) :: st.context }
      | Unnamed _ -> assert false
      | Named _ -> insert_heading st candidate
    else if List.mem tag sectionning_root then
      st (* ignore these nodes... *)
    else if List.mem tag sectionning_content then
      let st =
        match st.context with
        | [] | [(Top,_,_),_] -> st
        | _ -> step_up st
      in
      let nodes = Dom.list_of_nodeList node##childNodes in
      let outline =
        rebuild (List.fold_left walk (init_st ~ignore:st.ignore tag) nodes) @ st.outline in
      { st with outline }
    else
      List.fold_left walk st (Dom.list_of_nodeList node##childNodes)

  let outline ?ignore (nodes : Dom.node Js.t list) =
    List.rev (rebuild (List.fold_left walk (init_st ?ignore "toplevel") nodes))

  (* let ignore_target_header () = *)
    (* let first_node = ref true *)
    (* and first_section = ref true in *)
    (* fun n -> *)
      (* if !first_node then *)
        (* ( first_node := false; *)
          (* if Js.to_string n##nodeName <> "ARTICLE" *)
            (* && Js.to_string n##nodeName <> "SECTION"  then *)
            (* first_section := false; *)
          (* false ) *)
      (* else if !first_section then *)
        (* ( match Js.to_string n##nodeName with *)
          (* | "HEADER" -> true *)
          (* | "SECTION" | "ARTICLE" | "ASIDE" | "NAV" -> *)
            (* first_section := false; false *)
          (* | _ -> false ) *)
      (* else false *)

  let find_container n  =
    let rec find (n: Dom.node Js.t Js.Opt.t) =
      Js.Opt.case n
        (fun () -> Js.Opt.empty)
        (fun n ->
          match String.lowercase (Js.to_string n##nodeName) with
          | tag when List.mem tag sectionning_tag -> Js.Opt.return n
          | _ -> find n##parentNode)
    in
    find n##parentNode

  exception FoundFragment of outline
  let find_fragment fragment outline =
    let rec find (Section (_, f, outline)) =
      if f = Some fragment
      then raise (FoundFragment outline)
      else List.iter find outline
    in
    try List.iter find outline; [] with FoundFragment outline -> outline

  let rec build_ol ?(depth = 0) outline =
    (* depth = 0 means as deep as infinity (for a value of infinity equal to 2^32) *)
    let ol = Dom_html.createOl Dom_html.document in
    List.iter (fun s -> Dom.appendChild ol (build_li ~depth:(pred depth) s)) outline;
    ol
  and build_li ~depth (Section (heading, fragment, outline)) =
    let li = Dom_html.createLi Dom_html.document in
    ( match fragment with
      | None -> List.iter (Dom.appendChild li) heading;
      | Some fragment ->
        let a = Dom_html.createA Dom_html.document in
        let uri = Eliom_uri.make_string_uri ~service:Eliom_service.void_coservice' ~fragment () in
        a##setAttribute(Js.string "href", Js.string uri);
          List.iter (Dom.appendChild a) heading;
          Dom.appendChild li a );
    if outline <> [] && depth <> 0 then Dom.appendChild li (build_ol ~depth outline);
    li

}}
