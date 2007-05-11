(** Tool for forum creation. *)

open XHTML.M
open Eliom
open Eliom.Xhtml
open Lwt
open Ocsimorelib

exception Unauthorized

type forum_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     moderated: bool;
     readable_by: Users.user;
     writable_by: Users.user;
     moderators: Users.user;
     url: string list;
     max_rows: int32;
   }

class type forum = object
  method srv_forum : 
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
  method box_forum :
      Users.user option ->
        Eliom.server_params -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_forum' :
      Users.user option ->
        Eliom.server_params ->
          int32 * int32 -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_threads_list :
      Users.user option ->
        Eliom.server_params -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_threads_list' :
      Users.user option ->
        Eliom.server_params ->
          int32 * int32 -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_newmessage :
      Users.user option ->
        int32 ->
          Eliom.server_params -> string -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_newthread :
      Users.user option ->
        Eliom.server_params ->
          string * string -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_thread' :
      Users.user option ->
        Eliom.server_params ->
          int32 * (int32 * int32) -> XHTML.M.block XHTML.M.elt list Lwt.t
  method page_forum :
      Eliom.server_params -> unit -> unit -> Eliom.Xhtml.page Lwt.t
  method page_forum' :
      Eliom.server_params ->
        int32 * int32 -> unit -> Eliom.Xhtml.page Lwt.t
  method page_newmessage :
      Eliom.server_params -> int32 -> string -> Eliom.Xhtml.page Lwt.t
  method page_newthread :
      Eliom.server_params ->
        unit -> string * string -> Eliom.Xhtml.page Lwt.t
  method page_thread :
      Eliom.server_params -> int32 -> unit -> Eliom.Xhtml.page Lwt.t
  method page_thread' :
      Eliom.server_params ->
        int32 * (int32 * int32) -> unit -> Eliom.Xhtml.page Lwt.t

end



class makeforum
    ~(foruminfo : forum_in)
    ~(sessionmanager : SessionManager.sessionmanager)
    ~(container : 
        Eliom.server_params -> Users.user option -> title:string -> 
          XHTML.M.block XHTML.M.elt list -> html Lwt.t)
    =

  (* SERVICES *)

  (* shows the forum main page, with threads list *)
  let srv_forum = new_service 
    ~url:(foruminfo.url @ [""]) 
    ~get_params:unit
    ()
  in

  (* as above, within a given interval *)
  let srv_forum' = new_service 
    ~url:(foruminfo.url @ [""]) 
    ~get_params:(int32 "offset" ** int32 "limit")
    ()
  in

  (* shows a thread detail, with messages list *)
  let srv_thread = new_service
    ~url:(foruminfo.url @ ["thread"])
    ~get_params:(int32 "thr_id") 
    ()
  in


  (* creates the new forum once and gets its id *)
  let frm_id = 
    Lwt_unix.run
      (Sql.Persist.lwtcreate 
         foruminfo.identifier 
         (fun () -> Sql.new_forum 
             ~title:foruminfo.title 
             ~descr:foruminfo.descr 
             ~moderated:foruminfo.moderated) >>=
       (fun a -> return (Sql.Persist.get a)))
  in

  (* as above, within a given interval *)
  let srv_thread' = new_service
    ~url:(foruminfo.url @ ["thread"])
    ~get_params:(int32 "thr_id" ** (int32 "offset" ** int32 "limit")) 
    ()
  in

(*---
  (* shows a message detail *)
  let srv_message = new_service
    ~url:(foruminfo.url @ ["message"])
    ~get_params:(int32 "msg_id") 
    ()
  in
  ---*)

  (* inserts the new thread and redisplays the threads list *)
  let srv_newthread = new_post_coservice 
      ~fallback:srv_forum
      ~post_params:(string "subject" ** string "text")
      ()
  in

  (* inserts the new message and redisplays the messages list *)
  let srv_newmessage = new_post_coservice
      ~fallback:srv_thread
      ~post_params:(string "text")
      ()
  in


  (* ACTIONS *)

  (* toggle the forum moderation status *)
  let act_forumtoggle = new_post_coservice'
      ~post_params:unit
      ()
  in

  (* toggle the hidden flag of a thread *)
  let act_threadtoggle = new_post_coservice'
      ~post_params:(int32 "thr_id")
      ()
  in

  (* toggle the hidden flag of a message *)
  let act_messagetoggle = new_post_coservice'
      ~post_params:(int32 "msg_id")
      ()
  in

      (* USEFUL STUFF *)
      
  (* true if user is logged on *)
  let l = function
    | Some _ -> true
    | _ -> false
  in      
  
  (* true if user can read messages *)
  let r = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.readable_by
    | _ -> foruminfo.readable_by = Users.anonymous()
  in

  (* true if user can write messages *)
  let w = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.writable_by
    | _ -> foruminfo.writable_by = Users.anonymous()
  in

  (* true if user is a moderator *)
  let m = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.moderators
    | _ -> false (* no anonymous moderators *)
  in      

  (* gets login name *)
  let name = function
    | Some user -> 
        let (n, _, _, _) = Users.get_user_data ~user in n
    | _ -> "<anonymous>"
  in

  (* for Sql module calls *)
  let kindof sess = 
    if m sess 
    then Sql.Moderator
    else (if w sess && l sess
    then Sql.Author (name sess)
    else Sql.Unknown)
  in

  let prepare_threads_list sess (offset, limit) () = 
    if not (r sess) then
      fail Unauthorized
    else
      let role = kindof sess in
      Sql.forum_get_data ~frm_id ~role >>=
      (fun a -> 
	Sql.forum_get_threads_list ~frm_id ~offset ~limit ~role >>=
        (fun b -> return (a,b)))
  in


  object (me)


  (* HTML FRAGMENTS: <form> CONTENTS *)

  method private new_thread_form = fun (subject,txt) ->
    [h2 [pcdata "Start a new thread with a fresh message:"];
     p [pcdata "Subject: ";
        string_input subject;
        br();
        textarea txt 5 80 (pcdata "")];
     p [submit_input "submit"]]


  method private new_message_form = fun (txt) ->
    [h2 [pcdata "Post a new message in this thread:"];
     p [textarea txt 5 80 (pcdata "")];
     p [submit_input "submit"]]


  method private forumtoggle_form = fun _ ->
    [p [submit_input "toggle"]]


  method private thr_toggle_form i = fun (thr_id) ->
    [p [hidden_user_type_input sol thr_id i;
        submit_input "toggle"]]


  method private msg_toggle_form i = fun (msg_id) ->
    [p [hidden_user_type_input sol msg_id i;
        submit_input "toggle"]]



  (* HTML FRAGMENTS: <div> BOXES *) 
  
  method private forum_data_box sp sess data =
    let (id, title, description, moder,
         n_shown_thr, n_hidden_thr, 
         n_shown_msg, n_hidden_msg) = data in
    div ~a:[a_class ["forum_data"]]
      (h1 [pcdata ("Forum: " ^ title)] ^:
       h2 [pcdata ("Description: " ^ description)] ^:
       h3 (pcdata ("Threads: " ^ soL n_shown_thr) ^:
       (m sess || w sess) % 
       pcdata (" (hidden: " ^ soL n_hidden_thr ^ ")") ^?
       pcdata (" Messages: " ^ soL n_shown_msg) ^:
       (m sess || w sess) %
       pcdata (" (hidden: " ^ soL n_hidden_msg ^")") ^?
       pcdata (" Moderated: " ^ if moder then "YES" else "NO") ^:
       []) ^:
       (m sess) %
       post_form act_forumtoggle sp me#forumtoggle_form () ^?
       [])


  method private thread_data_box sp sess data =
    let (id, subject, author, datetime, hidden, 
         n_shown_msg, n_hidden_msg) = data in
    div ~a:[a_class ["thread_data"]]
    (h1 [pcdata ("Thread: " ^ subject)] ^:
     h2 [pcdata ("created by: " ^ author);
     pcdata (sod datetime)] ^:
     h3 (pcdata ("Messages: " ^ soL n_shown_msg) ^:
     (m sess || w sess) % 
     pcdata (" (hidden: " ^ soL n_hidden_msg ^ ")") ^?
     (m sess || w sess) % 
     pcdata (" Hidden thread: " ^ if hidden then "YES" else "NO") ^?
     []) ^:
     (m sess) % 
     post_form act_threadtoggle sp (me#thr_toggle_form id) () ^?
   [])


  method private message_data_box sp sess data =
    let (id, text, author, datetime, hidden) = data in
    div ~a:[a_class ["message_data"]]
      (h4 [pcdata ("posted by: " ^ author);
       pcdata (sod datetime)] ^: 
       (m sess || w sess) % 
       p [pcdata("Hidden message: " ^ if hidden then "YES" else "NO")] ^?
       pre[pcdata text] ^: 
       (m sess) % 
       post_form act_messagetoggle sp (me#msg_toggle_form id) () ^? 
       [])


  method private forum_threads_list_box sp sess thr_l =
    let fields = "date/time" ^: "subject" ^: "author" ^: 
                           (m sess || w sess) % "hidden" ^? [] in
    let tblhead = List.map (fun i -> th [pcdata i]) fields in
    let tbldata = List.map 
        (fun (id, subject, author, datetime, hidden) ->
          td [pcdata (sod datetime)] ^:
          td [a srv_thread sp [pcdata subject] id] ^:
                        td [pcdata author] ^:
                (m sess || w sess) % 
                td (pcdata (if hidden then "YES" else "NO") ^:
    (m sess) %
    post_form act_threadtoggle sp (me#thr_toggle_form id) () ^?
    []) ^?
        [])
        thr_l in
    let tr' = function (x::xs) -> tr x xs | [] -> assert false in
    div ~a:[a_class ["forum_threads_list"]]
      [table (tr' tblhead) (List.map tr' tbldata)]


(*---
   let thread_messages_list_box sp sess msg_l =
   let fields = "date/time" ^: "author" ^: 
   (m sess || w sess) % "hidden" ^? [] in
   let tblhead = List.map (fun i -> th [pcdata i]) fields in
   let tbldata = List.map 
   (fun (id, author, datetime, hidden) -> 
   td [pcdata (sod datetime)] ^:
   td [a srv_message sp [pcdata author] id] ^:
   (m sess) % 
   td [pcdata (if hidden then "YES" else "NO");
   post_form act_messagetoggle sp (me#msg_toggle_form id) ()] ^?
   [])
   msg_l in
   let tr' = function (x::xs) -> tr x xs | [] -> assert false in
   div ~a:[a_class ["thread_messages_list"]]
   [table (tr' tblhead) (List.map tr' tbldata)]
 
   ---*)

  method private thread_messageswtext_list_box sp sess msg_l =
    div ~a:[a_class ["thread_messageswtext_list"]]
      (List.map (fun m -> me#message_data_box sp sess m) msg_l)


  method private main_link_box sp = 
    div ~a:[a_class ["main_link"]]
      [a srv_forum sp [pcdata ("Back to \"" ^foruminfo.title^ "\" Forum homepage")] ()]


  method private feedback_box feedback =
    div ~a:[a_class ["feedback"]]
      [p [pcdata feedback]]


(* HTML FRAGMENTS: <html> PAGES *)

    method private mk_forum_box sp sess feedback frm_data thr_l =
      (me#feedback_box feedback ^:
       me#forum_data_box sp sess frm_data ^:
       me#forum_threads_list_box sp sess thr_l ^:
       (w sess) % 
       post_form srv_newthread sp me#new_thread_form () ^?
       [])


    method private mk_thread_box sp sess feedback thr_id thr_data msg_l =
      (me#feedback_box feedback ^:
       me#thread_data_box sp sess thr_data ^:
(*--- 
   thread_messages_list_box sp sess msg_l ^:
   ---*)
       me#thread_messageswtext_list_box sp sess msg_l ^:
       (w sess) % 
       post_form srv_newmessage sp me#new_message_form thr_id ^?
       [])


(*---
   method private mk_message_box sp sess msg_data =
   [me#message_data_box sp sess msg_data;
   me#main_link_box sp])
   ---*)

  method private mk_exception_box sp from exc =
    div ~a:[a_class ["exception"]]
      [h1 [pcdata "Exception:"];
       p [pcdata ((Printexc.to_string exc)^" in function: "^from)]
     ]


  method private mk_unauthorized_box sp sess =
    div ~a:[a_class ["unauthorized"]]
      [h1 [pcdata "Restricted area:"];
       p [pcdata "Please log in with an authorized account."]]


(* SERVICES & ACTIONS IMPLEMENTATION *)

  method private lwt_box_with_exception_handling :
      'a. Eliom.server_params -> Users.user option -> 
        string -> (unit -> 'a Lwt.t) -> 
          ('a -> [< Xhtmltypes.body_content > `Div ] elt list) -> 
          [< Xhtmltypes.body_content > `Div ] elt list Lwt.t =
          fun sp sess failmsg f1 f2 ->
            catch      
              (function () -> f1 () >>= fun x -> return (f2 x))
              (function
                | Unauthorized -> return [me#mk_unauthorized_box sp sess]
                | exc -> return [me#mk_exception_box sp failmsg exc])
  
  method box_newthread = fun sess sp (subject,txt) ->
    let prepare () = 
      if not (w sess) then 
	fail Unauthorized
      else 
	let role = kindof sess in
	let author = name sess in
	Sql.new_thread_and_message ~frm_id ~author ~subject ~txt >>=
        (fun _ ->
	  Sql.forum_get_data ~frm_id ~role >>=
          (fun a ->
	    Sql.forum_get_threads_list 
	      ~frm_id ~offset:0l ~limit:foruminfo.max_rows ~role >>=
            (fun b -> return (a,b))))
    and gen_html = fun (frm_data,thr_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
        | (_,_,_,true,_,_,_,_) -> 
	    "sent; it is going to be submitted to the moderators' \
              approval. Should it not appear in the list, please \
              do not send it again."
      | (_,_,_,false,_,_,_,_) -> "published.")
    in
    me#mk_forum_box sp sess feedback frm_data thr_l
  in me#lwt_box_with_exception_handling 
    sp sess "page_newthread" prepare gen_html

   method page_newthread sp () (subject,txt) =
     get_persistent_data SessionManager.user_table sp >>=
     (fun sess ->
       me#box_newthread sess sp (subject,txt) >>=
       container
         sp
         sess
         ~title:(foruminfo.title^" Forum"))



  method box_newmessage sess thr_id sp txt =
    let prepare () = 
      if not (w sess) then
	fail Unauthorized
      else
	let role = kindof sess in
	let author = name sess in
	Sql.new_message ~frm_id ~thr_id ~author ~txt >>=
        (fun _ ->
          Sql.forum_get_data ~frm_id ~role >>=
          (fun a ->
	    Sql.thread_get_data ~frm_id ~thr_id ~role >>=
            (fun b ->
(*---
   Sql.thread_get_messages_list 
   ~frm_id ~thr_id ~offset:0l ~limit:foruminfo.max_rows ~role
   ---*)
	      Sql.thread_get_messages_with_text_list 
	        ~frm_id ~thr_id ~offset:0l 
                ~limit:foruminfo.max_rows ~role >>=
              (fun c -> return (a,b,c)))))
    and gen_html = fun (frm_data,thr_data,msg_l) ->
      let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
        | (_,_,_,true,_,_,_,_) ->
	    "sent; it is going to be submitted to the moderators' \
              approvation. Should it not appear in the list, please \
              do not send it again."
      | (_,_,_,false,_,_,_,_) -> "published.")
    in
    me#mk_thread_box sp sess feedback thr_id thr_data msg_l
  in me#lwt_box_with_exception_handling 
       sp sess "page_newmessage" prepare gen_html


  method page_newmessage sp thr_id txt =
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      me#box_newmessage sess thr_id sp txt >>=
      container
        sp sess
        ~title:(foruminfo.title^" Forum"))

  method box_forum' sess sp (offset, limit) =
    let gen_html (frm_data, thr_l) =
      me#mk_forum_box sp sess "" frm_data thr_l
    in me#lwt_box_with_exception_handling 
      sp sess "block_forum'" 
      (prepare_threads_list sess (offset, limit))
      gen_html

  method box_threads_list' sess sp (offset,limit) =
    let gen_html (frm_data, thr_l) =
      [me#forum_threads_list_box sp sess thr_l]
    in me#lwt_box_with_exception_handling 
      sp sess "block_threads_list'" 
      (prepare_threads_list sess (offset, limit)) gen_html

  method page_forum' sp (offset,limit) () =
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      me#box_forum' sess sp (offset,limit) >>=
      container
        sp
        sess
        ~title:(foruminfo.title^" Forum"))

  method box_threads_list sess sp = 
    me#box_threads_list' sess sp (0l, foruminfo.max_rows)

  method page_forum = fun sp () () -> 
    me#page_forum' sp (0l, foruminfo.max_rows) ()

  method box_forum sess sp = 
    me#box_forum' sess sp (0l, foruminfo.max_rows)

  
  method box_thread' sess sp (thr_id, (offset, limit)) =
    let prepare () = 
      if not (r sess) then
	fail Unauthorized
      else
	let role = kindof sess in
	Sql.thread_get_data ~frm_id ~thr_id ~role >>=
        (fun a ->
(*---
   Sql.thread_get_messages_list 
   ~frm_id ~thr_id ~offset:0l ~limit:foruminfo.max_rows ~role,
   ---*)
	  Sql.thread_get_messages_with_text_list 
	    ~frm_id ~thr_id ~offset ~limit ~role >>=
          (fun b -> return (a,b))) >>= function
	    | ((_,_,_,_,true,_,_),[]) -> 
	        (* Raises an exc if someone's trying to see a hidden thread 
	           with no messages by herself *)
	        fail Unauthorized
	    | (thr_data,msg_l) -> return (thr_data,msg_l)
                    
    and gen_html (thr_data, msg_l) =
      me#mk_thread_box sp sess "" thr_id thr_data msg_l
    in me#lwt_box_with_exception_handling 
      sp sess "page_thread" prepare gen_html

  method page_thread' sp (thr_id,(offset,limit)) () =
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      me#box_thread' sess sp (thr_id,(offset,limit)) >>=
      container
        sp sess
        ~title:(foruminfo.title^" Forum"))
  
  method page_thread = fun sp thr_id () ->
    me#page_thread' sp (thr_id,(0l,foruminfo.max_rows)) ()

  
(*---
   let page_message sess = fun sp msg_id () ->
   let role = kindof sess in
   let prepare () = Sql.message_get_data ~frm_id ~msg_id
   and gen_html = fun (msg_data) ->
   (mk_message_page sp sess msg_data)
   in (me#lwt_page_with_exception_handling 
   sp sess "page_message" prepare gen_html)
   in
   ---*)

  method private forum_toggle = fun sp () () -> 
    Sql.forum_toggle_moderated ~frm_id >>=
    fun () -> return []


  method private thread_toggle = fun sp () thr_id -> 
    Sql.thread_toggle_hidden ~frm_id ~thr_id >>= 
    fun () -> return []


  method private message_toggle = fun sp () msg_id -> 
    Sql.message_toggle_hidden ~frm_id ~msg_id >>= 
    fun () -> return []


    method srv_forum : 
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
        = srv_forum

    method private login_actions sp sess =
      return
        (if m sess then (
          Actions.register_for_session sp act_forumtoggle me#forum_toggle;
          Actions.register_for_session sp act_threadtoggle me#thread_toggle;
          Actions.register_for_session sp act_messagetoggle me#message_toggle);

         if w sess && foruminfo.writable_by <> Users.anonymous () then
	   register_for_session sp srv_newthread me#page_newthread
         else (); (* user can't write, OR service is public because
		     everyone can write *)

	 if w sess
	 then register_for_session sp srv_newmessage me#page_newmessage
	 else ();

        )
          
    method private logout_actions sp = return ()
        
    initializer
      register srv_forum me#page_forum;
      register srv_forum' me#page_forum';
      register srv_thread me#page_thread;
      register srv_thread' me#page_thread';
      sessionmanager#add_login_actions me#login_actions;
      sessionmanager#add_logout_actions me#logout_actions;
(*---
   register srv_message page_message;
   ---*)
      if foruminfo.writable_by = Users.anonymous() then (
        (* see comment to register_aux in page_forum' *)
        register srv_newthread me#page_newthread
       )


end

let newforum
    ~(foruminfo : forum_in)      
    ~(sessionmanager : SessionManager.sessionmanager)
    ~(container : 
        Eliom.server_params -> Users.user option -> title:string -> 
          XHTML.M.block XHTML.M.elt list -> html Lwt.t)
    : forum Lwt.t
    = 
  let o = new makeforum ~foruminfo ~sessionmanager ~container in
  return (o :> forum)
  

