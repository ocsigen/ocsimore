(** Tool for wiki creation. *)

open XHTML.M
open Eliom
open Eliom.Xhtml
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
     method srv_main :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
  end


(********************************************)
class makewiki
    ~(wikiinfo : wiki_in)      
    ~(sessionmanager : SessionManager.sessionmanager)
    ~(container : 
        Eliom.server_params -> Users.user option -> title:string -> 
          XHTML.M.block XHTML.M.elt list -> html Lwt.t)
    : wiki
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
  let wik_id = Sql.Persist.get (Sql.Persist.create 
                                  wikiinfo.identifier 
                                  (fun () -> Sql.new_wiki
                                      ~title:wikiinfo.title 
                                      ~descr:wikiinfo.descr))
  in
          
  object (me)

          (* USEFUL STUFF *)
          
          (* true if user is logged on *)
    method private l = function
      | Some _ -> true
      | _ -> false
              
              (* true if user can read wikipages *)
    method private r = function
      | Some user -> 
          Users.in_group ~user ~group:wikiinfo.readable_by
      | _ -> wikiinfo.readable_by = Users.anonymous()
            
            (* true if user can write wikipages *)
    method private w = function
      | Some user -> 
          Users.in_group ~user ~group:wikiinfo.writable_by
      | _ -> wikiinfo.writable_by = Users.anonymous()
            
            (* gets login name *)
    method private name = function
      | Some user -> 
          let (n, _, _, _) = Users.get_user_data ~user in n
      | _ -> "<anonymous>"

              (* SERVICES *)

    method srv_main :
        (unit, unit, Eliom.get_service_kind,
         [< Eliom.suff ] as 'c, unit Eliom.param_name, unit Eliom.param_name,
         [< Eliom.registrable ] as 'd)
        Eliom.service
        = srv_main

          (* HTML FRAGMENTS: <form> CONTENTS *)

          (* a form to get to a wikipage by suffix *)
      method private goto_wikipage_form sfx =
        [p [pcdata "Wikipage suffix: ";
            string_input sfx;
            submit_input "go!"]]

          (* a form for wikipage editing *)
      method private edit_wikipage_form 
          sfx' subject' txt' (sfx,(subject,txt)) =
        [p [pcdata "Subject:";
            string_input ~a:[a_value subject'] subject;
            br();
            pcdata "Page body:";
            br();
            textarea txt 5 80 (pcdata txt');
            br();
            hidden_string_input sfx sfx';
            submit_input "submit"]]

          (* HTML FRAGMENTS: <div> BOXES *) 
          (* the Wiki description box *)
      method private wiki_data_box data =
        let (title, description, n_pages) = data in
        div ~a:[a_class ["wiki_data"]]
          [h1 [pcdata ("Wiki: " ^ title)];
           h2 [pcdata ("Description: " ^ description)];
           h3 [pcdata ("There are " ^ soL n_pages ^ " wikipages at present.")]]

          (* the wikipages list *)
      method private wikipages_list_box sp wpg_l =
        let fields = ["subject"; "author"; "date/time"] in
        let tblhead = List.map (fun i -> th [pcdata i]) fields in
        let tbldata = List.map 
            (fun (subject, suffix, author, datetime) ->
              [td [a srv_wikipage sp [pcdata subject] suffix];
               td [pcdata author];
               td [pcdata (sod datetime)]])
            wpg_l in
        let tr' = function (x::xs) -> tr x xs | [] -> assert false in
        div ~a:[a_class ["wikipages_list"]]
          [table (tr' tblhead) (List.map tr' tbldata)]

          (* the wikipage content; text is parsed for commands *)
      method private wikipage_data_box sp (subject, text, author, datetime) =
        div ~a:[a_class ["wikipage_data"]]
          [h1 [pcdata subject];
           div ~a:[a_class ["Wikipage_parsed_data"]]
             (Wikiparser.parse (srv_wikipage, sp) text);
           h4 [pcdata ("Last modified: " ^ author ^ " " ^ (sod datetime))]]

          (* how to get to a wikipage? *)
      method private wiki_howto_box =
        div ~a:[a_class ["wiki_howto"]]
          [h3 [pcdata "To visit a wikipage, fill in the following form or \
                 choose one of the links listed below."]]

                     (* how to edit a wikipage? *)
      method private wikipage_howto_box =
        div ~a:[a_class ["wikipage_howto"]]
          [h3 [pcdata "The following form allows you to edit the content of \
                 this Wikipage."];
                 p [pcdata "The 'subject' field will be the main title \
                    of the page.";
                        br();
                    pcdata "The 'body' may contain formatted text, \
                      using the well-known '%'-commands."]]

                      (* default message for blank wikipages *)
      method private blank_wikipage_box =
        div ~a:[a_class ["blank_wikipage"]]
          [h3 [pcdata "This wikipage does not exist yet."];
           p [pcdata "Authorized users can create a new page at this address. \
                If this is not your case, please check the wikipage address \
                for typos."]]

                    (* a link to the Wiki main page *)
      method private main_link_box sp =
        div ~a:[a_class ["main_link"]]
          [a srv_main sp 
             [pcdata ("Back to \"" ^wikiinfo.title^ "\" Wiki homepage")] ()]



          (* HTML FRAGMENTS: <html> PAGES *)
          
          (* code for the Wiki main page *)
      method private mk_main_page sp sess wik_data wpg_l =
        container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
          [me#wiki_data_box wik_data;
           me#wiki_howto_box;
           get_form srv_wikipage sp me#goto_wikipage_form; 
           me#wikipages_list_box sp wpg_l;
           ]

          (* code for an existing Wikipage *)
      method private mk_existing_wikipage sp sess sfx wpg_data =
        let (subj,txt,_,_) = wpg_data in
        container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
          (me#main_link_box sp ^:
           me#wikipage_data_box sp wpg_data ^:
           (me#w sess) % me#wikipage_howto_box ^?
           (me#w sess) % (post_form act_edit sp 
           (me#edit_wikipage_form sfx subj txt) ()) ^?
           [])

          (* code for a blank Wikipage *)
      method private mk_blank_wikipage sp sess sfx =
        container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
          (me#blank_wikipage_box ^:
           me#main_link_box sp ^:
           (me#w sess) % me#wikipage_howto_box ^?
           (me#w sess) % (post_form act_edit sp
           (me#edit_wikipage_form sfx "" "") ())^? 
           [])

          (* code for a failsafe page *)
      method private mk_exception_page sp from exc =
        container
          sp
          None
          ~title:(wikiinfo.title^" Wiki")
          [div ~a:[a_class ["exception"]]
             [h1 [pcdata "Exception:"];
              p [pcdata ((Printexc.to_string exc)^" in function: "^from)]; 
            ]]

          (* code for unauthorized users' fallback page *)
      method private mk_unauthorized_page sp sess =
        container
          sp
          sess
          ~title:(wikiinfo.title^" Wiki")
          [div ~a:[a_class ["unauthorized"]]
             [h1 [pcdata "Restricted area:"];
              p [pcdata "Please log in with an authorized account."]];
           ]

(* SERVICES & ACTIONS IMPLEMENTATION *)
      method private lwt_page_with_exception_handling :
          'a. Eliom.server_params -> Users.user option -> 
            string -> (unit -> 'a) -> ('a -> html Lwt.t) -> html Lwt.t =
        fun sp sess failmsg f1 f2 ->
          catch      
            (function () -> Preemptive.detach f1 () >>= fun x -> f2 x)
            (function
              | Unauthorized -> me#mk_unauthorized_page sp sess
              | exc -> me#mk_exception_page sp failmsg exc)

      method private page_main sp () () =
        get_persistent_data SessionManager.user_table sp >>=
        (fun sess ->
          let prepare () = 
            (Sql.wiki_get_data ~wik_id, 
             Sql.wiki_get_pages_list ~wik_id)
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
            else raise Unauthorized
          and gen_html = function
            | Some wpg_data -> me#mk_existing_wikipage sp sess sfx wpg_data
            | None -> me#mk_blank_wikipage sp sess sfx
          in me#lwt_page_with_exception_handling 
            sp sess "page_wikipage" prepare gen_html)

      method private edit_action sp () (suffix,(subject,txt)) =
        get_persistent_data SessionManager.user_table sp >>=
        (fun sess ->
          Preemptive.detach 
            (fun a -> 
              Sql.add_or_change_wikipage ~wik_id ~suffix ~subject ~txt ~author:a)
            (me#name sess) >>=
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
         if wikiinfo.writable_by = Users.anonymous()
         then Actions.register act_edit me#edit_action
         else ()

end
