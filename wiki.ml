(** Tool for wiki creation. *)

open XHTML.M
open Eliom
open Eliom.Xhtml
open Lwt

module type IN = sig
  val identifier: string
  val title: string
  val descr: string
  val readable_by: Users.user
  val writable_by: Users.user
  val url: string list
  val exit_link: Eliom.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
  val mk_log_form : Eliom.server_params -> Users.user option -> 
    [> Xhtmltypes.form ] XHTML.M.elt
end

module type OUT = sig
  val srv_main :
    (unit, unit,
     [> `Attached of [> `Internal of [> `Service ] * [> `Get ] ] Eliom.a_s ],
     [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
     [> `Registrable ])
    Eliom.service
  val login_actions : Eliom.server_params -> Users.user option -> unit
  val logout_actions : Eliom.server_params -> unit
end


module Make (A: IN) = struct

  (* creates the new wiki once and gets its id *)
  let wik_id = Sql.Persist.get (Sql.Persist.create 
                                  A.identifier 
                                  (fun () -> Sql.new_wiki
                                     ~title:A.title 
                                     ~descr:A.descr))

  exception Unauthorized

  (* SERVICES *)

(* mostra i dati di presentazione del Wiki (Sql.wiki_get_data),
   una form per "saltare" ad una wikipage,
   l'elenco alfabetico delle wikipages definite (Sql.wiki_get_pages_list) *)
  let srv_main = new_service
    ~url:A.url
    ~get_params:unit
    ()

(* No permess lettura -> fallimento
   Solo lettura -> se c'è la pagina, la mostra, se non c'è, dice che non c'è
   Scrittura -> se c'è, la mostra + form modifica precompilato; se no, form inserimento *)
  let srv_wikipage = new_service
    ~url:A.url
    ~get_params:(suffix (string "page"))
    ()


  (* ACTIONS *)

  let act_edit = new_post_coservice'
      ~post_params:(string "sfx" ** (string "subj" ** string "txt"))
      ()


  (* USEFUL STUFF *)

  (* true if user is logged on *)
  let l = function
    | Some _ -> true
    | _ -> false

  (* true if user can read wikipages *)
  let r = function
    | Some user -> 
        Users.in_group ~user ~group:A.readable_by
    | _ -> A.readable_by = Users.anonymous()

  (* true if user can write wikipages *)
  let w = function
    | Some user -> 
        Users.in_group ~user ~group:A.writable_by
    | _ -> A.writable_by = Users.anonymous()

  (* gets login name *)
  let name = function
    | Some user -> 
        let (n, _, _, _) = Users.get_user_data ~user in n
    | _ -> "<anonymous>"

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

  (* HTML FRAGMENTS: <form> CONTENTS *)

  (* a form to get to a wikipage by suffix *)
  let goto_wikipage_form = fun sfx ->
    [p [pcdata "Wikipage suffix: ";
        string_input sfx;
        submit_input "go!"]]

  (* a form for wikipage editing *)
  let edit_wikipage_form sfx' subject' txt' = fun (sfx,(subject,txt)) ->
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
  let wiki_data_box data =
    let (title, description, n_pages) = data in
      div ~a:[a_class ["wiki_data"]]
        [h1 [pcdata ("Wiki: " ^ title)];
         h2 [pcdata ("Description: " ^ description)];
         h3 [pcdata ("There are " ^ soL n_pages ^ " wikipages at present.")]]

  (* the wikipages list *)
  let wikipages_list_box sp wpg_l =
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
  let wikipage_data_box sp (subject, text, author, datetime) =
    div ~a:[a_class ["wikipage_data"]]
      [h1 [pcdata subject];
       div ~a:[a_class ["Wikipage_parsed_data"]]
         (Wikiparser.parse (srv_wikipage, sp) text);
       h4 [pcdata ("Last modified: " ^ author ^ " " ^ (sod datetime))]]

  (* how to get to a wikipage? *)
  let wiki_howto_box =
    div ~a:[a_class ["wiki_howto"]]
      [h3 [pcdata "To visit a wikipage, fill in the following form or \
                   choose one of the links listed below."]]

  (* how to edit a wikipage? *)
  let wikipage_howto_box =
    div ~a:[a_class ["wikipage_howto"]]
      [h3 [pcdata "The following form allows you to edit the content of this \
                   Wikipage."];
       p [pcdata "The 'subject' field will be the main title of the page.";
          br();
          pcdata "The 'body' may contain formatted text, using the well-known \
                  '%'-commands."]]

  (* default message for blank wikipages *)
  let blank_wikipage_box =
    div ~a:[a_class ["blank_wikipage"]]
      [h3 [pcdata "This wikipage does not exist yet."];
       p [pcdata "Authorized users can create a new page at this address. If \
                  this is not your case, please check the wikipage address \
                  for typos."]]

  (* a link to the Wiki main page *)
  let main_link_box sp = 
    div ~a:[a_class ["main_link"]]
      [a srv_main sp [pcdata ("Back to \"" ^A.title^ "\" Wiki homepage")] ()]

  (* an escape *)
  let exit_link_box sp = 
    div ~a:[a_class ["exit_link"]]
      [A.exit_link sp]


  (* HTML FRAGMENTS: <html> PAGES *)
      
  (* code for the Wiki main page *)
  let mk_main_page sp sess wik_data wpg_l =
    html
      (head (title (pcdata (A.title^" Wiki"))) [])
      (body [A.mk_log_form sp sess;
             wiki_data_box wik_data;
             wiki_howto_box;
             get_form srv_wikipage sp goto_wikipage_form; 
             wikipages_list_box sp wpg_l;
             exit_link_box sp])

  (* code for an existing Wikipage *)
  let mk_existing_wikipage sp sess sfx wpg_data =
    let (subj,txt,_,_) = wpg_data in
      html
        (head (title (pcdata (A.title^" Wiki"))) [])
        (body (A.mk_log_form sp sess ^:
                 main_link_box sp ^:
                 wikipage_data_box sp wpg_data ^:
                 (w sess) % wikipage_howto_box ^?
                 (w sess) % (post_form act_edit sp 
		               (edit_wikipage_form sfx subj txt) ()) ^?
                 []))

  (* code for a blank Wikipage *)
  let mk_blank_wikipage sp sess sfx =
    html
      (head (title (pcdata (A.title^" Wiki"))) [])
      (body (A.mk_log_form sp sess ^:
             blank_wikipage_box ^:
             main_link_box sp ^:
             (w sess) % wikipage_howto_box ^?
             (w sess) % (post_form act_edit sp
			   (edit_wikipage_form sfx "" "") ())^? 
             []))

  (* code for a failsafe page *)
  let mk_exception_page sp from exc =
    html 
      (head (title (pcdata "Error")) [])
      (body
         [div ~a:[a_class ["exception"]]
            [h1 [pcdata "Exception:"];
             p [pcdata ((Printexc.to_string exc)^" in function: "^from)]; 
             exit_link_box sp]])

  (* code for unauthorized users' fallback page *)
  let mk_unauthorized_page sp sess =
    html 
      (head (title (pcdata "Error")) [])
      (body 
        [A.mk_log_form sp sess;
        div ~a:[a_class ["unauthorized"]]
          [h1 [pcdata "Restricted area:"];
          p [pcdata "Please log in with an authorized account."]];
        exit_link_box sp])


  (* SERVICES & ACTIONS IMPLEMENTATION *)

  let lwt_page_with_exception_handling sp sess failmsg f1 f2 =
  catch      
    (function () -> Preemptive.detach f1 () >>= fun x -> return (f2 x))
    (function
      | Unauthorized -> return (mk_unauthorized_page sp sess)
      | exc -> return (mk_exception_page sp failmsg exc))

  let page_main = fun sp () () ->
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      let prepare () = 
        (Sql.wiki_get_data ~wik_id, 
         Sql.wiki_get_pages_list ~wik_id)
      and gen_html (wik_data, wpg_l) = 
        mk_main_page sp sess wik_data wpg_l
      in 
      lwt_page_with_exception_handling 
        sp sess "page_main" prepare gen_html)

  let page_wikipage = fun sp sfx () ->
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      let prepare () = 
        if (r sess) 
        then Sql.wikipage_get_data ~wik_id ~suffix:sfx 
        else raise Unauthorized
      and gen_html = function
        | Some wpg_data -> mk_existing_wikipage sp sess sfx wpg_data
        | None -> mk_blank_wikipage sp sess sfx
      in lwt_page_with_exception_handling 
        sp sess "page_wikipage" prepare gen_html)

  let edit_action = fun sp () (suffix,(subject,txt)) ->
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      Preemptive.detach 
        (fun a -> 
          Sql.add_or_change_wikipage ~wik_id ~suffix ~subject ~txt ~author:a)
        (name sess) >>=
      (fun () -> return []))

  let login_actions sp sess =
    if (w sess)
    then Actions.register_for_session sp act_edit edit_action
    else ()
      
  let logout_actions sp = ()
    
  let _ =
    register srv_main page_main;
    register srv_wikipage page_wikipage;
    if A.writable_by = Users.anonymous()
    then Actions.register act_edit edit_action
    else ()

end
