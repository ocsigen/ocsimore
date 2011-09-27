let fake_script =
  HTML5.M.unique
    (Eliom_output.Html5.js_script ~uri:"" ())

let add_outliner_js () =
  Page_site.Header.require_header
    (Page_site.Header.create_header
       (fun () ->
	 [
	   HTML5.M.unique ~copy:fake_script
	     (Eliom_output.Html5.js_script
		~uri:(Page_site.static_file_uri ["outliner.js"])
		())
	 ]
       ))

{client{

   let ignore_target_header () =
     let first_node = ref true
     and first_section = ref true in
     fun n ->
       if !first_node then
	 ( first_node := false;
	   if Js.to_string n##nodeName <> "ARTICLE"
	      && Js.to_string n##nodeName <> "SECTION"  then
	     first_section := false;
	   Js._false )
       else if !first_section then
	 ( match Js.to_string n##nodeName with
	   | "HEADER" -> Js._true
	   | "SECTION" | "ARTICLE" | "ASIDE" | "NAV" ->
	       first_section := false; Js._false
	   | _ -> Js._false )
       else Js._false

   let find_container n  =
     let rec find ignore (n: Dom.node Js.t) =
       match Js.to_string n##nodeName with
       | "SECTION" | "ARTICLE" -> (Js.Opt.return n, ignore)
       | tag ->
	   let ignore =
	     if tag = "HEADER" then
	       Some (fun n' ->
		       Js.bool
			 ((Js.Opt.get (n'##parentNode) (fun () -> n)) == n
			  && Js.to_string (n'##nodeName) <> "H1"))
	     else
	       ignore
	   in
	   Js.Opt.case (n##parentNode)
	     (fun () -> Js.Opt.empty, None)
	     (fun n -> find ignore n)
     in
     find None (n : Dom_html.element Js.t :> Dom.node Js.t)

   class type outline = object
     method asHTML: bool Js.t -> Js.js_string Js.t Js.meth
     method asDOM: bool Js.t -> Dom_html.oListElement Js.t Js.meth
   end

   let outline
       ?(ignore = fun (_: Dom.node Js.t) -> Js._false)
       (obj: Dom.node Js.t Js.opt) : outline Js.t Js.opt =
     let outliner = Js.Unsafe.variable("HTML5Outline") in
     Js.Unsafe.fun_call outliner
       [| Js.Unsafe.inject obj;
	  Js.Unsafe.inject (Js.wrap_callback ignore)|]

}}
