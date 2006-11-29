(** Tool for forum creation. *)

open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open MoreXhtml
open Lwt

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
  let frm_id = Sql.Persist.get (Sql.Persist.create 
				  A.identifier 
				  (fun () -> Sql.new_forum 
                                     ~title:A.title 
                                     ~descr:A.descr 
                                     ~moderated:A.moderated))

  exception Unauthorized


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
    | Some (Users.Authenticated _) -> true
    | _ -> false

  (* true if user can read messages *)
  let r = function
    | Some (Users.Authenticated user) -> 
	Users.in_group ~user ~group:A.readable_by
    | _ -> A.readable_by = Users.anonymous()

  (* true if user can write messages *)
  let w = function
    | Some (Users.Authenticated user) -> 
	Users.in_group ~user ~group:A.writable_by
    | _ -> A.writable_by = Users.anonymous()

  (* true if user is a moderator *)
  let m = function
    | Some (Users.Authenticated user) -> 
	Users.in_group ~user ~group:A.moderators
    | _ -> false (* no anonymous moderators *)

  (* gets login name *)
  let name = function
    | Some (Users.Authenticated user) -> 
        let (n, _, _, _) = Users.get_user_data ~user in n
    | _ -> "<anonymous>"

  (* for Sql module calls *)
  let kindof sess = 
    if m sess 
    then Sql.Moderator
    else (if w sess && l sess
	  then Sql.Author (name sess)
	  else Sql.Unknown)

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
            [h1 [pcdata "Exception:"];
             p [pcdata ((Printexc.to_string exc)^" in function: "^from)]; 
             exit_link_box sp]])

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

  let page_newthread sess = fun sp () (subject,txt) ->
    let prepare () = 
      if not (w sess) then 
	raise Unauthorized
      else 
	let whoami = kindof sess in
	let author = name sess in
	  Sql.new_thread_and_message ~frm_id ~author ~subject ~txt;
	  (Sql.forum_get_data ~frm_id ~whoami,
	   Sql.forum_get_threads_list 
	     ~frm_id ~offset:0l ~limit:A.max_rows ~whoami)
    and gen_html = fun (frm_data,thr_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
           | (_,_,_,true,_,_,_,_) -> 
	       "sent; it is going to be submitted to the moderators' \
                approvation. Should it not appear in the list, please \
                do not send it again."
           | (_,_,_,false,_,_,_,_) -> "published.") in
        mk_forum_page sp sess feedback frm_data thr_l
    in lwt_page_with_exception_handling 
	 sp sess "page_newthread" prepare gen_html


  let page_newmessage sess thr_id = fun sp () (txt) ->
    let prepare () = 
      if not (w sess) then
	raise Unauthorized
      else
	let whoami = kindof sess in
	let author = name sess in
	  Sql.new_message ~thr_id ~author ~txt;
	  (Sql.forum_get_data ~frm_id ~whoami,
	   Sql.thread_get_data ~thr_id ~whoami,
	   (*---
             Sql.thread_get_messages_list 
             ~thr_id ~offset:0l ~limit:A.max_rows ~whoami
	     ---*)
	   Sql.thread_get_messages_with_text_list 
	     ~thr_id ~offset:0l ~limit:A.max_rows ~whoami)
    and gen_html = fun (frm_data,thr_data,msg_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
           | (_,_,_,true,_,_,_,_) ->
	       "sent; it is going to be submitted to the moderators' \
                approvation. Should it not appear in the list, please \
                do not send it again."
           | (_,_,_,false,_,_,_,_) -> "published.") in
	mk_thread_page sp sess feedback thr_data msg_l
    in lwt_page_with_exception_handling 
	 sp sess "page_newmessage" prepare gen_html

  let page_forum' sess = fun sp (offset,limit) () -> 
    let prepare () = 
      if not (r sess) then
	raise Unauthorized
      else
	let whoami = kindof sess in
	  if w sess && A.writable_by <> Users.anonymous() then
	    register_service_for_session sp srv_newthread (page_newthread sess)
	  else (); (* user can't write, OR service is public because
		      everyone can write *)
	  (Sql.forum_get_data ~frm_id ~whoami,
	   Sql.forum_get_threads_list ~frm_id ~offset ~limit ~whoami)
    and gen_html = fun (frm_data,thr_l) -> 
      mk_forum_page sp sess "" frm_data thr_l
    in lwt_page_with_exception_handling 
	 sp sess "page_forum" prepare gen_html

  let page_forum sess = fun sp () () -> 
    page_forum' sess sp (0l, A.max_rows) ()

  let page_thread' sess = fun sp (thr_id,(offset,limit)) () ->
    let prepare () = 
      if not (r sess) then
	raise Unauthorized
      else
	let whoami = kindof sess in
	  if w sess
	  then register_service_for_session sp srv_newmessage (page_newmessage 
								 sess thr_id)
	  else ();
	  (Sql.thread_get_data ~thr_id ~whoami,
(*---
           Sql.thread_get_messages_list 
	     ~thr_id ~offset:0l ~limit:A.max_rows ~whoami
  ---*)
	   Sql.thread_get_messages_with_text_list 
	     ~thr_id ~offset ~limit ~whoami)
    and gen_html = fun (thr_data,msg_l) ->
      mk_thread_page sp sess "" thr_data msg_l
    in lwt_page_with_exception_handling 
         sp sess "page_thread" prepare gen_html

  let page_thread sess = fun sp thr_id () ->
    page_thread' sess sp (thr_id,(0l,A.max_rows)) ()

(*---
  let page_message sess = fun sp msg_id () ->
    let whoami = kindof sess in
    let prepare () = Sql.message_get_data ~msg_id
    and gen_html = fun (msg_data) ->
       (mk_message_page sp sess msg_data)
    in (lwt_page_with_exception_handling 
          sp sess "page_message" prepare gen_html)
  ---*)

  let forum_toggle = fun sp _ -> 
    Preemptive.detach (fun i -> Sql.forum_toggle_moderated ~frm_id:i) frm_id

  let thread_toggle = fun sp thr_id -> 
    Preemptive.detach (fun i -> Sql.thread_toggle_hidden ~thr_id:i) thr_id

  let message_toggle = fun sp msg_id -> 
    Preemptive.detach (fun i -> Sql.message_toggle_hidden ~msg_id:i) msg_id

  let login_actions sp sess =
    register_service_for_session sp srv_forum (page_forum sess);
    register_service_for_session sp srv_forum' (page_forum' sess);
    register_service_for_session sp srv_thread (page_thread sess);
    register_service_for_session sp srv_thread' (page_thread' sess);
(*---
    register_service_for_session sp srv_message (page_message sess);
  ---*)
    if m sess then (
      register_action_for_session sp act_forumtoggle forum_toggle;
      register_action_for_session sp act_threadtoggle thread_toggle;
      register_action_for_session sp act_messagetoggle message_toggle
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
    if A.writable_by = Users.anonymous() then (
      (* see comment to register_aux in page_forum' *)
      register_service srv_newthread (page_newthread None)
    )

end
