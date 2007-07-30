(** Tool for wiki creation. *)

(* open XHTML.M *)
open Eliom
open Eliomduce.Xhtml
open Lwt
open Ocsimorelib

exception Unauthorized

type wiki_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     readable_by: Users.user;
     writable_by: Users.user;
     url: string list;
   }

class type wiki = 
  object
		method container: 
        Eliom.server_params -> Users.user option -> title:string -> 
          {{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
		method srv_main:
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      Eliom.service
  end


(********************************************)
class makewiki
    ~(wikiinfo : wiki_in)      
    ~(sessionmanager : SessionManager.sessionmanager)
    = 

  (* SERVICES *)
(* mostra i dati di presentazione del Wiki (Sql.wiki_get_data),
   una form per "saltare" ad una wikipage,
   l'elenco alfabetico delle wikipages definite (Sql.wiki_get_pages_list) *)
  let srv_main = new_service
      ~url:wikiinfo.url
      ~get_params:unit
      ()
  in

(* No permess lettura -> fallimento
   Solo lettura -> se c'è la pagina, la mostra, se non c'è, dice che non c'è
   Scrittura -> se c'è, la mostra + form modifica precompilato; se no, form inserimento *)
  let srv_wikipage = new_service
      ~url:wikiinfo.url
      ~get_params:(suffix (string "page"))
      ()
  in
          
          
          (* ACTIONS *)
  let act_edit = new_post_coservice'
      ~post_params:(string "sfx" ** (string "subj" ** string "txt"))
      ()
  in
          
        
        (* creates the new wiki once and gets its id *)
  let wik_id = 
    Lwt_unix.run
      (Sql.Persist.lwtcreate 
         wikiinfo.identifier 
         (fun () -> Sql.new_wiki
             ~title:wikiinfo.title 
             ~descr:wikiinfo.descr) >>=
       (fun a -> return (Sql.Persist.get a)))
  in
          
  object (me)

	method container (sp: Eliom.server_params) (user: Users.user option) ~title:(t: string) (contents: {{ Xhtml1_strict.blocks }}): {{ Xhtml1_strict.html }} Lwt.t =
	begin
		return {{ <html>[
			<head>[
				<title>{: t :}
			]
			<body>{: contents :}
		] }}	
	end

          (* USEFUL STUFF *)
          
          (* true if user is logged on *)
    method private l = function
      | Some _ -> true
      | _ -> false
              
              (* true if user can read wikipages *)
    method private r = function
      | Some user -> 
          Users.in_group ~user ~group:wikiinfo.readable_by
      | _ -> wikiinfo.readable_by = Users.anonymous ()
            
            (* true if user can write wikipages *)
    method private w = function
      | Some user -> 
          Users.in_group ~user ~group:wikiinfo.writable_by
      | _ -> wikiinfo.writable_by = Users.anonymous ()
            
            (* gets login name *)
    method private name = function
      | Some user -> 
          let (n, _, _, _) = Users.get_user_data ~user in n
      | _ -> "<anonymous>"

              (* SERVICES *)

    method srv_main :
        (unit, unit, Eliom.get_service_kind,
         [ `WithoutSuffix ], unit, unit, [ `Registrable ])
        Eliom.service 
        = srv_main

          (* HTML FRAGMENTS: <form> CONTENTS *)

          (* a form to get to a wikipage by suffix *)
      method private goto_wikipage_form sfx =
				{{ [<p>[
					'Wikipage suffix: '
					{: string_input ~input_type:{:"text":} ~name:sfx () :}
					{: string_input ~input_type:{:"submit":} ~value:"Go!" () :}
				]] }}

          (* a form for wikipage editing *)
      method private edit_wikipage_form 
          sfx' subject' txt' (sfx,(subject,txt)) =
				{{ [<p>[
					'Subject: '
					{: string_input ~input_type:{:"text":} ~name:subject ~value:subject' () :}
					<br>[]
					'Page body: '
					<br>[]
					{: textarea ~name:txt ~rows:5 ~cols:80 ~value:{{ {: txt' :} }} () :}
					<br>[]
					{: string_input ~input_type:{:"hidden":} ~name:sfx ~value:sfx' () :}
					{: string_input ~input_type:{:"submit":} ~value:"Submit" () :}
				]] }}

          (* HTML FRAGMENTS: <div> BOXES *) 
          (* the Wiki description box *)
      method private wiki_data_box data =
        let (title, description, n_pages) = data in
				{{ <div class="wiki_data">[
					<h1>{: Format.sprintf "Wiki: %s" title :}
					<h2>{: Format.sprintf "Description: %s" description :}
					<h3>{: Format.sprintf "There are %d wikipages at present." n_pages :}
				] }}

          (* the wikipages list *)
      method private wikipages_list_box sp wpg_l =
        let fields = ["subject"; "author"; "date/time"] in
        let tblhead = List.map (fun i -> {{ <th>{: i :} }}) fields in
        let tbldata = List.map 
            (fun (subject, suffix, author, datetime) ->
							{{ [
								<td>[{: a srv_wikipage sp {{ {: subject :} }} suffix :}]
								<td>{: author :}
								<td>{: sod datetime :}
							] }})		
            wpg_l in
        (*let tr' = function (x::xs) -> tr x xs | [] -> assert false in *)
				{{ <div class="wikipages_list">[
					<table>[<tr>[{: List.hd tblhead :} !{: List.tl tblhead :}] !{: List.map (fun x -> {{ <tr>{: x :} }}) tbldata :}]
				] }}

          (* the wikipage content; text is parsed for commands *)
      method private wikipage_data_box sp (subject, text, author, datetime) =
				{{ <div class="wikipage_data">[
					<h1>{: subject :}
					<div class="wikipage_parsed_data">{: Wikiparser.parse (srv_wikipage, sp) text :}
					<h4>{: Format.sprintf "Last modified: %s %s" author (sod datetime) :}
				] }}

          (* how to get to a wikipage? *)
      method private wiki_howto_box =
				{{ <div class="wiki_howto">[
					<h3>"To visit a wikipage, fill in the following form or \
						choose one of the links listed below."
				] }}

                     (* how to edit a wikipage? *)
      method private wikipage_howto_box =
				{{ <div class="wikipage_howto">[
					<h3>"The following form allows you to edit the content of \
                 this Wikipage."
          <p>['The \'subject\' field will be the main title of the page.'
					<br>[]
          'The \'body\' may contain formatted text, \
					using the well-known \'%\'-commands.']
				] }}

                      (* default message for blank wikipages *)
      method private blank_wikipage_box =
				{{ <div class="blank_wikipage">[
					<h3>"This wikipage does not exist yet."
					<p>"Authorized users can create a new page at this address. \
                If this is not your case, please check the wikipage address \
                for typos."
				] }}
                    (* a link to the Wiki main page *)
      method private main_link_box sp =
				{{ <div class="main_link">[
					{: a srv_main sp {{ {: Format.sprintf "Back to \"%s\" Wiki homepage" wikiinfo.title :} }} () :}
				] }}


          (* HTML FRAGMENTS: <html> PAGES *)
          
          (* code for the Wiki main page *)
      method private mk_main_page sp sess wik_data wpg_l =
        me#container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
					{{ [
						{: me#wiki_data_box wik_data :}
           	{: me#wiki_howto_box :}
           	{: get_form srv_wikipage sp me#goto_wikipage_form :}
           	{: me#wikipages_list_box sp wpg_l :}
          ] }}
          (* code for an existing Wikipage *)
      method private mk_existing_wikipage sp sess sfx wpg_data =
        let (subj,txt,_,_) = wpg_data in
        me#container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
					{{ [
						{: me#main_link_box sp :}
           	{: me#wikipage_data_box sp wpg_data :}
						!{: if (me#w sess) then
							{{ [
								{: me#wikipage_howto_box :}
            		{: post_form act_edit sp (me#edit_wikipage_form sfx subj txt) () :} ] }}
							else
							{{ [] }} :}
					] }}
          (* code for a blank Wikipage *)
      method private mk_blank_wikipage sp sess sfx =
        me#container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
					{{ [
						{: me#blank_wikipage_box :}
           	{: me#main_link_box sp :}
						!{: if (me#w sess) then
							{{ [
								{: me#wikipage_howto_box :}
                {: post_form act_edit sp (me#edit_wikipage_form sfx "" "") () :}
							] }}
							else
							{{ [] }} :}
					] }}

          (* code for a failsafe page *)
      method private mk_exception_page sp from exc =
        me#container
          sp
          None
          ~title:(wikiinfo.title^" Wiki")
					{{ [<div class="exception">[
						<h1>"Exception"
						<p>{: Format.sprintf "%s in function: %s" (Printexc.to_string exc) from :}
					]] }}

          (* code for unauthorized users' fallback page *)
      method private mk_unauthorized_page sp sess =
        me#container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
					{{ [<div class="unauthorized">[
						<h1>"Restricted area"
						<p>"Please log in with an authorized account."
          ]] }}

(* SERVICES & ACTIONS IMPLEMENTATION *)
      method private lwt_page_with_exception_handling :
          'a. Eliom.server_params -> Users.user option -> 
            string -> (unit -> 'a Lwt.t) -> ('a -> {{ Xhtml1_strict.html }} Lwt.t) -> {{ Xhtml1_strict.html }} Lwt.t =
        fun sp sess failmsg f1 f2 ->
          catch      
            (function () -> f1 () >>= fun x -> f2 x)
            (function
              | Unauthorized -> me#mk_unauthorized_page sp sess
              | exc -> me#mk_exception_page sp failmsg exc)

      method private page_main sp () () =
        get_persistent_data SessionManager.user_table sp >>=
        (fun sess ->
          let prepare () = 
            Sql.wiki_get_data ~wik_id >>=
            (fun a ->
              Sql.wiki_get_pages_list ~wik_id >>=
              (fun b -> return (a,b)))
          and gen_html (wik_data, wpg_l) = 
            me#mk_main_page sp sess wik_data wpg_l
          in 
          me#lwt_page_with_exception_handling 
            sp sess "page_main" prepare gen_html)

      method private page_wikipage sp sfx () =
        get_persistent_data SessionManager.user_table sp >>=
        (fun sess ->
          let prepare () = 
            if (me#r sess) 
            then Sql.wikipage_get_data ~wik_id ~suffix:sfx
            else fail Unauthorized
          and gen_html = function
            | Some wpg_data -> me#mk_existing_wikipage sp sess sfx wpg_data
            | None -> me#mk_blank_wikipage sp sess sfx
          in me#lwt_page_with_exception_handling 
            sp sess "page_wikipage" prepare gen_html)

      method private edit_action sp () (suffix,(subject,txt)) =
        get_persistent_data SessionManager.user_table sp >>=
        (fun sess ->
          Sql.add_or_change_wikipage
            ~wik_id ~suffix ~subject ~txt ~author:(me#name sess) >>=
          (fun () -> return []))


       method private login_actions sp sess =
         return
           (if (me#w sess)
           then Actions.register_for_session sp act_edit me#edit_action
           else ())
             
       method private logout_actions (sp : Eliom.server_params) = 
         return ()
                      
       initializer
         register srv_main me#page_main;
         register srv_wikipage me#page_wikipage;
         sessionmanager#add_login_actions me#login_actions;
         sessionmanager#add_logout_actions me#logout_actions;
         if wikiinfo.writable_by = Users.anonymous ()
         then Actions.register act_edit me#edit_action
         else ()

end

let newwiki
    ~(wikiinfo : wiki_in)      
    ~(sessionmanager : SessionManager.sessionmanager)
    : wiki Lwt.t
    = 
  let o = new makewiki ~wikiinfo ~sessionmanager in
  return (o :> wiki)
  

