(** Tool for forum creation. *)

open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open MoreXhtml
open Lwt
open Sql
open Users

module type IN = sig
  val identifier: string
  val title: string
  val descr: string
  val moderated: bool
  val readable_by: Users.user
  val writable_by: Users.user
  val moderators: Users.user
  val url: string list
  val exit_link: Ocsigen.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
  val mk_log_form : Ocsigen.server_params -> Users.auth option -> 
    [> Xhtmltypes.form ] XHTML.M.elt
  val max_rows: int32
end

module type OUT = sig
  val srv_forum: (unit, unit, [ `Internal_Service of [ `Public_Service ] ],
		  [ `WithoutSuffix ], unit Ocsigen.param_name,
		  unit Ocsigen.param_name) Ocsigen.service
  val login_actions : Ocsigen.server_params -> Users.auth option -> unit
  val logout_actions : 'a -> unit
end


module Make (A: IN) = struct

  (* creates the new forum once and gets its id *)
  let frm_id = Persist.get (Persist.create 
                              A.identifier 
                              (fun () -> new_forum 
                                 ~title:A.title 
                                 ~descr:A.descr 
                                 ~moderated:A.moderated))
 
  (* SERVICES *)

  (* A user defined parameter type *)
  let int32 p = user_type Int32.of_string Int32.to_string p 

  (* shows the forum main page, with threads list *)
  let srv_forum = new_service 
    ~url:(A.url @ ["main"]) 
    ~get_params:unit
    ()

  (* as above, within a given interval *)
  let srv_forum' = new_service 
    ~url:(A.url @ ["main"]) 
    ~get_params:(int32 "offset" ** int32 "limit")
    ()

  (* shows a thread detail, with messages list *)
  let srv_thread = new_service
    ~url:(A.url @ ["thread"])
    ~get_params:(int32 "thr_id") 
    ()

  (* as above, within a given interval *)
  let srv_thread' = new_service
    ~url:(A.url @ ["thread"])
    ~get_params:(int32 "thr_id" ** (int32 "offset" ** int32 "limit")) 
    ()

(*---
  (* shows a message detail *)
  let srv_message = new_service
    ~url:(A.url @ ["message"])
    ~get_params:(int32 "msg_id") 
    ()
  ---*)

  (* inserts the new thread and redisplays the threads list *)
  let srv_newthread = new_post_auxiliary_service 
    ~fallback:srv_forum
    ~post_params:(string "subject" ** string "text")

  (* inserts the new message and redisplays the messages list *)
  let srv_newmessage = new_post_auxiliary_service 
    ~fallback:srv_forum
    ~post_params:(string "text")


  (* ACTIONS *)

  (* toggle the forum moderation status *)
  let act_forumtoggle = new_action
    ~post_params:unit

  (* toggle the hidden flag of a thread *)
  let act_threadtoggle = new_action
    ~post_params:(int32 "thr_id")

  (* toggle the hidden flag of a message *)
  let act_messagetoggle = new_action
    ~post_params:(int32 "msg_id")


  (* USEFUL STUFF *)

  (* true if user is logged on *)
  let l = function
    | Some (Authenticated _) -> true
    | _ -> false

  (* true if user can read messages *)
  let r = function
    | Some (Authenticated user) -> in_group ~user ~group:A.readable_by
    | _ -> A.readable_by = anonymous()

  (* true if user can write messages *)
  let w = function
    | Some (Authenticated user) -> in_group ~user ~group:A.writable_by
    | _ -> A.writable_by = anonymous()

  (* true if user is a moderator *)
  let m = function
    | Some (Authenticated user) -> in_group ~user ~group:A.moderators
    | _ -> false (* no anonymous moderators *)

  (* gets login name *)
  let name = function
    | Some (Authenticated user) -> 
        let (n, _, _, _) = get_user_data ~user in n
    | _ -> "<anonymous>"

  (* for Sql module calls *)
  let kindof sess = 
    if m sess 
    then Moderator
    else (if w sess && l sess
	  then Author (name sess)
	  else Unknown)

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
  let soL = Int64.to_string (* "string of Long" *)
  let sol = Int32.to_string (* "string of long" *)
  let sod = Printer.CalendarPrinter.to_string (* "string of date" *)

  (* HTML FRAGMENTS: <form> CONTENTS *)

  let new_thread_form = fun (subject,txt) ->
    [h2 [pcdata "Start a new thread with a fresh message:"];
     p [pcdata "Subject: ";
        string_input subject;
        br();
        textarea txt 5 80 (pcdata "")];
     p [submit_input "submit"]]

  let new_message_form = fun (txt) ->
    [h2 [pcdata "Post a new message in this thread:"];
     p [textarea txt 5 80 (pcdata "")];
     p [submit_input "submit"]]

  let forumtoggle_form = fun _ ->
    [p [submit_input "toggle"]]

  let thr_toggle_form i = fun (thr_id) ->
    [p [hidden_user_type_input sol thr_id i;
        submit_input "toggle"]]

  let msg_toggle_form i = fun (msg_id) ->
    [p [hidden_user_type_input sol msg_id i;
        submit_input "toggle"]]


  (* HTML FRAGMENTS: <div> BOXES *) 
    
  let forum_data_box sp sess data =
    let (id, title, description, moder,
         n_shown_thr, n_hidden_thr, 
         n_shown_msg, n_hidden_msg) = data in
      div ~a:[a_class ["forum_data"]]
        (h1 [pcdata ("Forum: " ^ title)] ^:
         h2 [pcdata ("Description: " ^ description)] ^:
         h3 (pcdata ("Threads: " ^ soL n_shown_thr) ^:
            (m sess) % 
            pcdata (" (hidden: " ^ soL n_hidden_thr ^ ")") ^?
            pcdata (" Messages: " ^ soL n_shown_msg) ^:
            (m sess) %
            pcdata (" (hidden: " ^ soL n_hidden_msg ^")") ^?
            pcdata (" Moderated: " ^ if moder then "YES" else "NO") ^:
            []) ^:
         (m sess) %
         action_form act_forumtoggle sp forumtoggle_form ^?
         [])

  let thread_data_box sp sess data =
    let (id, subject, author, datetime, hidden, 
         n_shown_msg, n_hidden_msg) = data in
      div ~a:[a_class ["thread_data"]]
        (h1 [pcdata ("Thread: " ^ subject)] ^:
         h2 [pcdata ("created by: " ^ author);
            pcdata (sod datetime)] ^:
         h3 (pcdata ("Messages: " ^ soL n_shown_msg) ^:
            (m sess) % 
            pcdata (" (hidden: " ^ soL n_hidden_msg ^ ")") ^?
            (m sess) % 
            pcdata (" Hidden thread: " ^ if hidden then "YES" else "NO") ^?
            []) ^:
         (m sess) % 
         action_form act_threadtoggle sp (thr_toggle_form id)^?
         [])

  let message_data_box sp sess data =
    let (id, text, author, datetime, hidden) = data in
      div ~a:[a_class ["message_data"]]
        (h4 [pcdata ("posted by: " ^ author);
             pcdata (sod datetime)] ^: 
         (m sess) % 
         p  [pcdata("Hidden message: " ^ if hidden then "YES" else "NO")] ^?
         p  [pcdata text] ^: 
         (m sess) % 
         action_form act_messagetoggle sp (msg_toggle_form id) ^? 
         [])

  let forum_threads_list_box sp sess thr_l =
    let fields = 
      "date/time" ^: "subject" ^: "author" ^: (m sess) % "hidden" ^? [] in
    let tblhead = List.map (fun i -> th [pcdata i]) fields in
    let tbldata = List.map 
      (fun (id, subject, author, datetime, hidden) ->
         td [pcdata (sod datetime)] ^:
         td [a srv_thread sp [pcdata subject] id] ^:
         td [pcdata author] ^:
         (m sess) % 
         td [pcdata (if hidden then "YES" else "NO");
             action_form act_threadtoggle sp (thr_toggle_form id)] ^?
         [])
      thr_l in
    let tr' = function (x::xs) -> tr x xs | [] -> assert false in
      div ~a:[a_class ["forum_threads_list"]]
        [table (tr' tblhead) (List.map tr' tbldata)]

(*---
  let thread_messages_list_box sp sess msg_l =
    let fields = "date/time" ^: "author" ^: (m sess) % "hidden" ^? [] in
    let tblhead = List.map (fun i -> th [pcdata i]) fields in
    let tbldata = List.map 
      (fun (id, author, datetime, hidden) -> 
         td [pcdata (sod datetime)] ^:
         td [a srv_message sp [pcdata author] id] ^:
         (m sess) % 
         td [pcdata (if hidden then "YES" else "NO");
             action_form act_messagetoggle sp (msg_toggle_form id)] ^?
         [])
      msg_l in
    let tr' = function (x::xs) -> tr x xs | [] -> assert false in
      div ~a:[a_class ["thread_messages_list"]]
        [table (tr' tblhead) (List.map tr' tbldata)]
  ---*)

  let thread_messageswtext_list_box sp sess msg_l =
    div ~a:[a_class ["thread_messageswtext_list"]]
      (List.map (fun m -> message_data_box sp sess m) msg_l)

  let main_link_box sp = 
    div ~a:[a_class ["main_link"]]
      [a srv_forum sp [pcdata ("Back to \"" ^A.title^ "\" Forum homepage")] ()]

  let exit_link_box sp = 
    div ~a:[a_class ["exit_link"]]
      [A.exit_link sp]

  let feedback_box feedback =
    div ~a:[a_class ["feedback"]]
      [p [pcdata feedback]]


  (* HTML FRAGMENTS: <html> PAGES *)
            
  let mk_forum_page sp sess feedback frm_data thr_l =
    html
      (head (title (pcdata (A.title^" Forum"))) [])
      (body (A.mk_log_form sp sess ^:
             feedback_box feedback ^:
             forum_data_box sp sess frm_data ^:
             forum_threads_list_box sp sess thr_l ^:
             (w sess) % 
             post_form srv_newthread sp new_thread_form () ^?
             exit_link_box sp ^:
             []))

  let mk_thread_page sp sess feedback thr_data msg_l =
    html
      (head (title (pcdata (A.title^" Forum"))) [])
      (body (A.mk_log_form sp sess ^:
             feedback_box feedback ^:
             thread_data_box sp sess thr_data ^:
(*--- 
	     thread_messages_list_box sp sess msg_l ^:
  ---*)
             thread_messageswtext_list_box sp sess msg_l ^:
             (w sess) % 
             post_form srv_newmessage sp new_message_form () ^?
             main_link_box sp ^:
             []))

(*---
  let mk_message_page sp sess msg_data =
    html
      (head (title (pcdata (A.title^" Forum"))) [])
      (body [A.mk_log_form sp sess;
             message_data_box sp sess msg_data;
             main_link_box sp])
  ---*)

  let mk_exception_page sp from exc =
    html 
      (head (title (pcdata "Error")) [])
      (body
         [div ~a:[a_class ["exception"]]
            [h1 [pcdata "Uncaught exception:"];
             p [pcdata ((Printexc.to_string exc)^" in function: "^from)]; 
             exit_link_box sp]])


  (* SERVICE IMPLEMENTATION *)

  let lwt_page_with_exception_handling sp failmsg f1 f2 =
  catch      
    (fun () -> f1 () >>= f2)
    (fun exc -> return (mk_exception_page sp failmsg exc))

  let page_newthread sess = fun sp () (subject,txt) ->
    let whoami = kindof sess in
    let author = name sess in
    let db_interact () = 
      return (ignore(new_thread_and_message ~frm_id ~author ~subject ~txt);
              (forum_get_data ~frm_id ~whoami,
               forum_get_threads_list 
		 ~frm_id ~offset:0l ~limit:A.max_rows ~whoami))
    and gen_html = fun (frm_data,thr_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
           | (_,_,_,true,_,_,_,_) -> 
	       "sent; it is going to be submitted to the moderators' \
                approvation. Should it not appear in the list, please \
                do not send it again."
           | (_,_,_,false,_,_,_,_) -> "published.") in
        return (mk_forum_page sp sess feedback frm_data thr_l)
    in (lwt_page_with_exception_handling 
          sp "page_newthread" db_interact gen_html)


  let page_newmessage sess thr_id = fun sp () (txt) ->
    let whoami = kindof sess in
    let author = name sess in
    let db_interact () = 
      return (ignore(new_message ~thr_id ~author ~txt);
              (forum_get_data ~frm_id ~whoami,
               thread_get_data ~thr_id ~whoami,
(*---
	       thread_get_messages_list 
                 ~thr_id ~offset:0l ~limit:A.max_rows ~whoami 
  ---*)
               thread_get_messages_with_text_list 
		 ~thr_id ~offset:0l ~limit:A.max_rows ~whoami))
    and gen_html = fun (frm_data,thr_data,msg_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
           | (_,_,_,true,_,_,_,_) ->
	       "sent; it is going to be submitted to the moderators' \
                approvation. Should it not appear in the list, please \
                do not send it again."
           | (_,_,_,false,_,_,_,_) -> "published.") in
      return (mk_thread_page sp sess feedback thr_data msg_l)
    in (lwt_page_with_exception_handling 
          sp "page_newmessage" db_interact gen_html)


  let page_forum' sess = fun sp (offset,limit) () -> 
    let whoami = kindof sess in
    let register_aux () =
      (* here we register the aux service in the session table of a
	 logged user with permission to write, but ONLY IF
	 A.writable_by <> anonymous(): if everyone can write, service
	 has been registered once in the global table. *)
      if w sess && A.writable_by <> anonymous()
      then register_service_for_session sp srv_newthread (page_newthread sess)
      else () in
    let db_interact () = 
      return (register_aux();
              (forum_get_data ~frm_id ~whoami,
               forum_get_threads_list ~frm_id ~offset ~limit ~whoami))
    and gen_html = fun (frm_data,thr_l) -> 
      return (mk_forum_page sp sess "" frm_data thr_l)
    in (lwt_page_with_exception_handling 
          sp "page_forum" db_interact gen_html)

  let page_forum sess = fun sp () () -> 
    page_forum' sess sp (0l, A.max_rows) ()

  let page_thread' sess = fun sp (thr_id,(offset,limit)) () ->
    let whoami = kindof sess in
    let register_aux () =
      if w sess
      (* compare this to register_aux in page_forum': here we ALWAYS
	 register the aux service in the session table of a logged
	 user with permission to write, as the aux service needs a
	 parameter (thr_id) known only at this point. *)
      then register_service_for_session sp srv_newmessage (page_newmessage 
							     sess thr_id)
      else () in
    let db_interact () = 
      return (register_aux ();
              (thread_get_data ~thr_id ~whoami,
             (*thread_get_messages_list 
		 ~thr_id ~offset:0l ~limit:A.max_rows ~whoami*)
               thread_get_messages_with_text_list 
		 ~thr_id ~offset ~limit ~whoami))
    and gen_html = fun (thr_data,msg_l) ->
      return (mk_thread_page sp sess "" thr_data msg_l)
    in (lwt_page_with_exception_handling 
          sp "page_thread" db_interact gen_html)

  let page_thread sess = fun sp thr_id () ->
    page_thread' sess sp (thr_id,(0l,A.max_rows)) ()

(*---
  let page_message sess = fun sp msg_id () ->
    let whoami = kindof sess in
    let db_interact () =
      return (message_get_data ~msg_id)
    and gen_html = fun (msg_data) ->
      return (mk_message_page sp sess msg_data)
    in (lwt_page_with_exception_handling 
          sp "page_message" db_interact gen_html)
  ---*)

  let login_actions sp sess =
    register_service_for_session sp srv_forum (page_forum sess);
    register_service_for_session sp srv_forum' (page_forum' sess);
    register_service_for_session sp srv_thread (page_thread sess);
    register_service_for_session sp srv_thread' (page_thread' sess);
(*---
    register_service_for_session sp srv_message (page_message sess);
  ---*)
    if m sess then (
      register_action_for_session sp act_forumtoggle 
	(fun sp _ -> return (forum_toggle_moderated ~frm_id)); 
      register_action_for_session sp act_threadtoggle 
	(fun sp thr_id -> return (thread_toggle_hidden ~thr_id)); 
      register_action_for_session sp act_messagetoggle 
	(fun sp msg_id -> return (message_toggle_hidden ~msg_id))
    )
      
  let logout_actions sp = ()
    
  let _ =
    register_service srv_forum (page_forum None);
    register_service srv_forum' (page_forum' None);
    register_service srv_thread (page_thread None);
    register_service srv_thread' (page_thread' None);
(*---
    register_service srv_message (page_message None);
  ---*)
    if A.writable_by = anonymous() then (
      (* see comment to register_aux in page_thread' *)
      register_service srv_newthread (page_newthread None)
    )

end
