(** Tool for forum creation. *)

(* open XHTML.M *)
open Eliom
open Eliomduce.Xhtml
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
     max_rows: int;
		 arborescent: bool;
   }

class forum
    ~(foruminfo : forum_in)
    ~(sessionmanager : SessionManager.sessionmanager)
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
    ~get_params:(int "offset" ** int "limit")
    ()
	in

  (* shows a thread detail, with messages list *)
  let srv_thread = new_service
    ~url:(foruminfo.url @ ["thread"])
    ~get_params:(id "thr_id") 
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
    ~get_params:(id "thr_id" ** (int "offset" ** int "limit")) 
    ()
  in

	let srv_reply = new_service
		~url:(foruminfo.url @ ["reply"])
		~get_params:(id "thr_id" ** id "parent_id")
		()
	in

	let srv_reply' = new_service
		~url:(foruminfo.url @ ["reply"])
		~get_params:(id "thr_id" ** (id "parent_id" ** (int "offset" ** int "limit")))
		()
	in
  (* inserts the new thread and redisplays the threads list *)
  let srv_newthread = new_post_coservice 
      ~fallback:srv_forum
      ~post_params:(bool "is_article" ** (string "subject" ** string "text"))
      ()
  in

  (* inserts the new message and redisplays the messages list *)
  let srv_newmessage = new_post_coservice
      ~fallback:srv_thread'
      ~post_params:(string "text")
      ()
  in

	let srv_replymessage = new_post_coservice
		~fallback:srv_thread
		~post_params:(id "parent_id" ** string "text")
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
      ~post_params:(id "thr_id")
      ()
  in

  (* toggle the hidden flag of a message *)
  let act_messagetoggle = new_post_coservice'
      ~post_params:(id "msg_id")
      ()
  in

  object (me)

      (* USEFUL STUFF *)
      
  (* true if user is logged on *)
  method is_logged_on = function
    | Some _ -> true
    | _ -> false

  (* true if user can read messages *)
  method can_read = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.readable_by
    | _ -> foruminfo.readable_by = Users.anonymous()

  (* true if user can write messages *)
  method can_write = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.writable_by
    | _ -> foruminfo.writable_by = Users.anonymous()

  (* true if user is a moderator *)
  method can_moderate = function
    | Some user -> 
	Users.in_group ~user ~group:foruminfo.moderators
    | _ -> false (* no anonymous moderators *)

  (* gets login name *)
  method private name = function
    | Some user -> 
        let (n, _, _, _) = Users.get_user_data ~user in n
    | _ -> "<anonymous>"

  (* for Sql module calls *)
  method private kindof sess = 
    if me#can_moderate sess 
    then Sql.Moderator
    else (if me#can_write sess && me#is_logged_on sess
    then Sql.Author (me#name sess)
    else Sql.Unknown)

  method private prepare_threads_list sess (offset, limit) () = 
    if not (me#can_read sess) then
      fail Unauthorized
    else
      let role = me#kindof sess in
      Sql.forum_get_data ~frm_id ~role >>=
      (fun a -> 
	Sql.forum_get_threads_list ~frm_id ~offset ~limit ~role >>=
        (fun b -> return (a,b)))

	method container (sp: Eliom.server_params) (user: Users.user option) ~title:(t: string) (contents: {{ Xhtml1_strict.blocks }}): {{ Xhtml1_strict.html }} Lwt.t =
	begin
		return {{ <html>[
			<head>[<title>{: t :}]
			<body>{: contents :}
		] }}
	end


  (* HTML FRAGMENTS: <form> CONTENTS *)

  method new_thread_form = fun (is_article,(subject,txt)) ->
  {{ [
  	<h2>"Start a new thread:"
		<p>[
			{: bool_checkbox ~checked:true ~name:is_article () :}
			' This message is an article'
		]
		<p>[
		'Subject: '
		{: string_input ~input_type:{:"text":} ~name:subject () :}
		<br>[]
		{: textarea ~name:txt ~rows:5 ~cols:80 ~value:{{ "Write your message here" }}  () :}
	]
	<p>[{: string_input ~input_type:{:"submit":} ~value:"Submit" () :}]
  ] }}

  method private new_message_form = fun (txt) ->
  {{ [
  	<h2>"Post a new message in this thread:"
	<p>[{: textarea ~name:txt ~rows:5 ~cols:80 ~value:{{ "Write your message here" }} () :}]
	<p>[{: string_input ~input_type:{:"submit":} ~value:"Submit" () :}]
  ] }}

  method private reply_form = fun parent_id (p_id,txt) ->
  {{ [
  	<h2>"Reply to this message:"
	<p>[{: textarea ~name:txt ~rows:5 ~cols:80 ~value:{{ "Write your message here" }} () :}]
	<p>[{: user_type_input ~input_type:{:"hidden":} ~name:p_id ~value:parent_id Sql.string_of_db_int :}]
	<p>[
		{: string_input ~input_type:{:"submit":} ~value:"Submit" () :}
	]] }}

  method private forumtoggle_form = fun _ ->
  {{ [
  	<p>[{: string_input ~input_type:{:"submit":} ~value:"Toggle" () :}]
  ] }}

  method private thr_toggle_form i = fun thr_id ->
  {{ [
  	<p>[
		{: user_type_input ~input_type:{:"hidden":} ~name:thr_id ~value:i Sql.string_of_db_int :}
		{: string_input ~input_type:{:"submit":} ~value:"Toggle" () :}
	]
  ] }}

  method private msg_toggle_form i = fun (msg_id) ->
  {{ [
  	<p>[
		{: user_type_input ~input_type:{:"hidden":} ~name:msg_id ~value:i Sql.string_of_db_int :}
		{: string_input ~input_type:{:"submit":} ~value:"Toggle" () :}
	]
  ] }}

  method private forum_data_box sp sess data =
  let (id, title, description, moder, n_shown_thr, n_hidden_thr,
       n_shown_msg, n_hidden_msg) = data in
  {{ <div class="forum_data">([
	<h1>{: Format.sprintf "Forum: %s" title :}
	<h2>{: Format.sprintf "Description: %s" description :}
	<h3>{: Format.sprintf "Threads: %d" n_shown_thr :}] @
	{: if me#can_moderate sess || me#can_write sess then
		{{ {: Format.sprintf " (hidden: %d)" n_hidden_thr :} }}
	   else
	   	{{ [] }} :} @
	{: Format.sprintf "Moderated: %s" (if moder then "YES" else "NO") :} @
	{: if me#can_moderate sess then
		{{ [{: post_form act_forumtoggle sp me#forumtoggle_form () :}] }}
	   else
	   	{{ [] }} :}
  ) }}

  method private thread_data_box sp sess data =
  let (id, subject, author, _, datetime, hidden, n_shown_msg, n_hidden_msg) = data
  in
  {{ <div class="thread_data">[
		<h1>{: Format.sprintf "Thread: %s" subject :}
		<h2>{: Format.sprintf "Created by: %s %s" author (sod datetime) :}
		<h3>{: Format.sprintf "Messages: %d%s" n_shown_msg
	       (if me#can_moderate sess || me#can_write sess then
			Format.sprintf " (hidden: %d) Hidden thread: %s" n_hidden_msg (if hidden then "YES" else "NO")
	        else
	   		"") :}
  	!{: if me#can_moderate sess then
		{{ [{: post_form act_threadtoggle sp (me#thr_toggle_form id) () :}] }}
  	   else
	   	{{ [] }} :}
  ] }}

	method private article_box sp sess data =
  let (_, _, _, article, _, _, _, _) = data in
	{{ <div class="thread_article">{:
		match article with
			None -> {{ [] }}
		| Some a -> {{ [<pre>{: a :}] }} :}
	}}

  method private message_data_box sp sess thr_id data offset limit =
  let (id, text, author, datetime, hidden, _) = data in
  {{ <div class="message_data">[
		<h4>{: Format.sprintf "posted by: %s %s" author (sod datetime) :}
	!{: if me#can_moderate sess || me#can_write sess then
		{{ [<p>{: Format.sprintf "Hidden message: %s" (if hidden then "YES" else "NO") :}] }}
	   else
		{{ [] }} :}
	<pre>{: text :}
	{: a srv_reply' sp {{ "Reply to this message" }} (thr_id, (id, (offset, limit))) :}
	!{: if me#can_moderate sess then 
		{{ [ {:	post_form act_messagetoggle sp (me#msg_toggle_form id) () :} ] }}
	   else
		{{ [] }} :}
	] }}

  method private forum_threads_list_box sp sess thr_l =
  let fields = "date/time" ^: "subject" ^: "author" ^: (me#can_moderate sess || me#can_write sess) % "hidden" ^? [] in
  let tblhead = List.map (fun i -> {{ <th>{: i :} }}) fields in
  let tbldata = List.map (fun (id, subject, author, datetime, hidden) ->
  {{ [
	<td>{: sod datetime :}
	<td>[{: a srv_thread sp {{ {: subject :} }} id :}]
	<td>{: author :}
     	!{: if me#can_moderate sess || me#can_write sess then 
		{{ [<td>[
			!{: if hidden then "YES" else "NO" :}
			!{: if me#can_moderate sess then
				{{ [ {: post_form act_threadtoggle sp (me#thr_toggle_form id) () :} ] }}
	    		    else
				{{ [] }} :}
		]] }}
	    else
		{{ [] }} :}
	] }})
     thr_l in
    (* let tr' = function (x::xs) -> {{ <tr>{: x::xs :} }} | [] -> assert false in *)
		{{ <div class="forum_threads_list">[
				<table>[<tr>[{: List.hd tblhead :} !{: List.tl tblhead :}] !{: List.map (fun x -> {{ <tr>{: x :} }}) tbldata :}]
			] }}

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

	method private thread_messageswtext_box sp sess thr_id offset limit msgs =
	let rec tree_to_li msg_t: {{ Xhtml1_strict.li }} list =
	begin
		match msg_t with
		| Sql.Node (m, ch) ->
			[{{ <li>[
				{: me#message_data_box sp sess thr_id m offset limit :}
				!{: forest_to_ul ch :}] }}]
	end
	and forest_to_ul msg_l: {{ Xhtml1_strict.flows }} =
	begin
		match msg_l with
		| [] -> {{ [] }}
		| l -> let nl = List.flatten (List.map tree_to_li l) in
			(match nl with
			| [] -> {{ "empty" }}
			| h::t -> {{ [<ul>[{: h :} !{: t :}]] }})
	end in
	begin
		match msgs with
		| Sql.List msg_l -> 	
			{{ <div class="thread_messageswtext_list">{: List.map
				(fun m -> {{ {: me#message_data_box sp sess thr_id m offset limit :} }})
				msg_l :}
			}}
		| Sql.Forest msg_f ->
			{{ <div class="thread_messageswtext_list">{: forest_to_ul msg_f :} }}
	end

  method private main_link_box sp = 
	{{ <div class="main_link">[
			{: a srv_forum sp {{ {: Format.sprintf "Back to \"%s\" forum homepage" foruminfo.title :} }} () :}] }}

	method private thread_navigation_box sp sess thr_id thr_data offset limit =
  let (id, subject, author, article, datetime, hidden, n_shown_msg, n_hidden_msg) = thr_data in
	begin
		if (n_shown_msg > foruminfo.max_rows) then
			{{ [<p>[
					!{: if offset > 0 then
							{{ [ {: a srv_thread' sp {{ "Previous" }} (thr_id,(offset - limit, limit)) :} ] }}
						else
							{{ "Previous" }}
					:}
					' '
					!{: if offset + limit < n_shown_msg then
							{{ [ {: a srv_thread' sp {{ "Next" }} (thr_id,(offset + limit, limit)) :} ] }} 
						else
							{{ "Next" }}
					:}
					]]
			}}
		else
			{{ [] }}
	end

  method private feedback_box feedback =
	{{ <div class="feedback">[<p>{: feedback :}] }}

(* HTML FRAGMENTS: <html> PAGES *)

    method private mk_forum_box sp sess feedback frm_data thr_l =
			{{ ([
      	{: me#feedback_box feedback :}
				{: me#forum_data_box sp sess frm_data :}
       	{: me#forum_threads_list_box sp sess thr_l :}] @
       	{: if (me#can_write sess) then
       	{{ [{: post_form srv_newthread sp me#new_thread_form () :}] }}
				else
       	{{ [] }} :}) }}

    method private mk_thread_box sp sess feedback thr_id thr_data offset limit msgs =
			{{ [
				{: me#feedback_box feedback :}
       	{: me#thread_data_box sp sess thr_data :}
				{: me#article_box sp sess thr_data :}
       	{: me#thread_messageswtext_box sp sess thr_id offset limit msgs :}
				!{: me#thread_navigation_box sp sess thr_id thr_data offset limit :}
				!{: if (me#can_write sess) then
				{{ [{: post_form srv_newmessage sp me#new_message_form (thr_id, (offset, limit)) :}] }}
				else
				{{ [] }} :}
			]	}}

    method private mk_reply_box sp sess feedback thr_id thr_data parent_id offset limit msg_t =
			{{ [
				{: me#feedback_box feedback :}
       	{: me#thread_data_box sp sess thr_data :}
(*--- 
   thread_messages_list_box sp sess msg_l ^:
   ---*)
       	{: me#thread_messageswtext_box sp sess thr_id offset limit msg_t :}
				!{: if (me#can_write sess) then
				{{ [{: post_form srv_replymessage sp (me#reply_form parent_id) thr_id :}] }}
				else
				{{ [] }} :}
			]	}}
(*---
   method private mk_message_box sp sess msg_data =
   [me#message_data_box sp sess msg_data;
   me#main_link_box sp])
   ---*)

  method private mk_exception_box sp from exc =
	{{ <div class="exception">[
			<h1>"Exception"
			<p>{: Format.sprintf "%s in function: %s" (Printexc.to_string exc) from :}
			] }}

  method private mk_unauthorized_box sp sess =
	{{ <div class="unauthorized">[
			<h1>"Restricted area"
			<p>"Please log in with an authorized account."
		 ] }}

(* SERVICES & ACTIONS IMPLEMENTATION *)

  method private lwt_box_with_exception_handling :
      'a. Eliom.server_params -> Users.user option -> 
        string -> (unit -> 'a Lwt.t) -> 
          ('a -> (* [< Xhtmltypes.body_content > `Div ] elt list *) 'b) -> 
          'b Lwt.t =
          fun sp sess failmsg f1 f2 ->
            catch      
              (function () -> f1 () >>= fun x -> return (f2 x))
              (function
                | Unauthorized -> return {{ [ {: me#mk_unauthorized_box sp sess :} ] }}
                | exc -> return {{ [ {: me#mk_exception_box sp failmsg exc :} ] }})
  
  method box_newthread = fun sp sess (is_article,(subject,txt)) ->
    let prepare () = 
      if not (me#can_write sess) then 
	fail Unauthorized
      else 
	let role = me#kindof sess in
	let author = me#name sess in
	(if is_article then
		Sql.new_thread_and_article ~frm_id ~author ~subject ~txt
	else
		Sql.new_thread_and_message ~frm_id ~author ~subject ~txt) >>=
        (fun _ ->
	  Sql.forum_get_data ~frm_id ~role >>=
          (fun a ->
	    Sql.forum_get_threads_list 
	      ~frm_id ~offset:0 ~limit:foruminfo.max_rows ~role >>=
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

   method page_newthread sp () (is_article,(subject,txt)) =
     get_persistent_data SessionManager.user_table sp >>=
     (fun sess ->
       me#box_newthread sp sess (is_article,(subject,txt)) >>=
       me#container
         sp
         sess
         ~title:(foruminfo.title^" Forum"))


  method box_newmessage sp sess (thr_id, (offset, limit)) txt =
    let prepare () = 
      if not (me#can_write sess) then
	fail Unauthorized
      else
	let role = me#kindof sess in
	let author = me#name sess in
	Sql.new_message ~frm_id ~thr_id ~author ~txt () >>=
	fun id -> Sql.forum_get_data ~frm_id ~role >>=
	fun a -> Sql.thread_get_data ~frm_id ~thr_id ~role >>=
	fun b ->
		(if foruminfo.arborescent then
			Sql.thread_get_messages_with_text_forest ~frm_id ~thr_id ~offset ~limit ~role () 
		else
			Sql.thread_get_messages_with_text ~frm_id ~thr_id ~offset ~limit ~role ()) >>=
	fun c -> return (a,b,c)
	and gen_html = fun (frm_data,thr_data,msgs) ->
  let feedback = "Your message has been " ^
        (match frm_data with (* get moderation status *)
        | (_,_,_,true,_,_,_,_) ->
	    "sent; it is going to be submitted to the moderators' \
              approvation. Should it not appear in the list, please \
              do not send it again."
      | (_,_,_,false,_,_,_,_) -> "published.")
    in
    me#mk_thread_box sp sess feedback thr_id thr_data offset limit msgs
  in me#lwt_box_with_exception_handling 
       sp sess "page_newmessage" prepare gen_html

  method page_newmessage sp tol txt =
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      me#box_newmessage sp sess tol txt >>=
      me#container
        sp sess ~title:foruminfo.title)

  method box_replymessage sp sess thr_id (parent_id,txt) =
  let prepare () = 
  if not (me#can_write sess) then
		fail Unauthorized
  else
	let role = me#kindof sess in
	let author = me#name sess in
	Sql.new_message ~frm_id ~thr_id ~parent_id ~author ~txt () >>=
  fun id -> Sql.forum_get_data ~frm_id ~role >>=
  fun a -> Sql.thread_get_data ~frm_id ~thr_id ~role >>=
  fun b -> 
		(if foruminfo.arborescent then
			Sql.thread_get_messages_with_text_forest ~frm_id ~thr_id ~bottom:id
		~offset:0 ~limit:(Sql.int_of_db_int id) ~role ()
		else
			Sql.thread_get_messages_with_text ~frm_id ~thr_id ~bottom:id
			~offset:0 ~limit:(Sql.int_of_db_int id) ~role ()) >>=
	fun c -> return (a,b,c)
  and gen_html = fun (frm_data,thr_data,msgs) ->
	let (_,_,_,_,_,_,new_id,_) = thr_data in
	let offset = new_id - (new_id mod foruminfo.max_rows) in
  let feedback = "Your reply has been " ^
  	(match frm_data with (* get moderation status *)
		| (_,_,_,true,_,_,_,_) ->
	    "sent; it is going to be submitted to the moderators' \
              approvation. Should it not appear in the list, please \
              do not send it again."
    | (_,_,_,false,_,_,_,_) -> "published.") in
  me#mk_thread_box sp sess feedback thr_id thr_data offset foruminfo.max_rows msgs in
	me#lwt_box_with_exception_handling sp sess "page_newmessage" prepare gen_html

	method page_replymessage sp thr_id (parent_id,txt) =
		get_persistent_data SessionManager.user_table sp >>=
		fun sess -> me#box_replymessage sp sess thr_id (parent_id,txt) >>=
			me#container sp sess ~title:foruminfo.title

  method box_forum' sp sess (offset, limit) =
    let gen_html (frm_data, thr_l) =
      me#mk_forum_box sp sess "" frm_data thr_l
    in me#lwt_box_with_exception_handling 
      sp sess "block_forum'" 
      (me#prepare_threads_list sess (offset, limit))
      gen_html

  method box_threads_list' sp sess (offset,limit) =
    let gen_html (frm_data, thr_l) =
      {{ [ {: me#forum_threads_list_box sp sess thr_l :} ] }}
    in me#lwt_box_with_exception_handling 
      sp sess "block_threads_list'" 
      (me#prepare_threads_list sess (offset, limit)) gen_html

  method page_forum' sp (offset,limit) () =
    get_persistent_data SessionManager.user_table sp >>=
    (fun sess ->
      me#box_forum' sp sess (offset,limit) >>=
      me#container
        sp
        sess
        ~title:(foruminfo.title^" Forum"))

  method box_threads_list sp sess = 
    me#box_threads_list' sp sess (0, foruminfo.max_rows)

  method page_forum = fun sp () () -> 
    me#page_forum' sp (0, foruminfo.max_rows) ()

  method box_forum sp sess = 
    me#box_forum' sp sess (0, foruminfo.max_rows)

	method box_thread' sp sess (thr_id, (offset, limit)) =
	let prepare () = 
		if not (me#can_read sess) then
			fail Unauthorized
		else
		let role = me#kindof sess in
			Sql.thread_get_data ~frm_id ~thr_id ~role >>=
			fun a -> 
				(if foruminfo.arborescent then
					Sql.thread_get_messages_with_text_forest ~frm_id ~thr_id ~offset ~limit ~role ()
				else
					Sql.thread_get_messages_with_text ~frm_id ~thr_id ~offset ~limit ~role ()) >>=
			fun b -> return (a,b) >>=
			function
  		| ((_,_,_,_,_,true,_,_),Sql.List [])
  		| ((_,_,_,_,_,true,_,_),Sql.Forest []) -> fail Unauthorized
			(* Raises an exc if someone's trying to see a hidden thread with no messages by herself *)
			| (thr_data,msgs) -> return (thr_data,msgs)
	and gen_html (thr_data, msg_l) =
		me#mk_thread_box sp sess "" thr_id thr_data offset limit msg_l
	in me#lwt_box_with_exception_handling sp sess "page_thread" prepare gen_html

  method page_thread' sp (thr_id,(offset,limit)) () =
    get_persistent_data SessionManager.user_table sp >>=
		fun sess -> me#box_thread' sp sess (thr_id,(offset,limit)) >>=
    me#container sp sess ~title:(foruminfo.title^" Forum")
  
  method page_thread = fun sp thr_id () ->
    me#page_thread' sp (thr_id, (0, foruminfo.max_rows)) ()

  method box_reply' sp sess (thr_id,(parent_id,(offset, limit))) =
    let prepare () = 
      if not (me#can_read sess) then
	fail Unauthorized
      else
	let role = me#kindof sess in
	Sql.thread_get_data ~frm_id ~thr_id ~role >>=
        (fun a ->
(*---
   Sql.thread_get_messages_list 
   ~frm_id ~thr_id ~offset:0l ~limit:foruminfo.max_rows ~role,
   ---*)
	  (if foruminfo.arborescent then
			Sql.thread_get_messages_with_text_forest
	    ~frm_id ~thr_id ~offset ~limit ~role ~bottom:parent_id ()
		else
			Sql.thread_get_messages_with_text
	    ~frm_id ~thr_id ~offset ~limit ~role ~bottom:parent_id ()) >>=
          (fun b -> return (a,b))) >>= function
	    | ((_,_,_,_,_,true,_,_),Sql.List []) 
	    | ((_,_,_,_,_,true,_,_),Sql.Forest []) -> 
	        (* Raises an exc if someone's trying to see a hidden thread 
	           with no messages by herself *)
	        fail Unauthorized
	    | (thr_data,msgs) -> return (thr_data,msgs)
                    
    and gen_html (thr_data, msgs) =
      me#mk_reply_box sp sess "" thr_id thr_data parent_id offset limit msgs
    in me#lwt_box_with_exception_handling 
      sp sess "page_thread" prepare gen_html

	method page_reply' = fun sp (thr_id,(parent_id,(offset,limit))) () ->
		get_persistent_data SessionManager.user_table sp >>=
		(fun sess ->
			me#box_reply' sp sess (thr_id,(parent_id,(offset,limit))) >>=
			me#container
				sp sess
				~title:(foruminfo.title ^ " Forum"))

	method page_reply = fun sp (thr_id,parent_id) () ->
		me#page_reply' sp (thr_id,(parent_id,(0,foruminfo.max_rows))) ()

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
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      Eliom.service
        = srv_forum

    method srv_newthread : 
      (unit, bool * (string * string), Eliom.post_service_kind,
       [ `WithoutSuffix ], unit, [`One of bool] Eliom.param_name * ([`One of string] Eliom.param_name * [`One of string] Eliom.param_name), [ `Registrable ])
      Eliom.service
        = srv_newthread

    method private login_actions sp sess =
      return
        (if me#can_moderate sess then 
				begin
          Actions.register_for_session sp act_forumtoggle me#forum_toggle;
          Actions.register_for_session sp act_threadtoggle me#thread_toggle;
          Actions.register_for_session sp act_messagetoggle me#message_toggle
				end;
        if me#can_write sess && foruminfo.writable_by <> Users.anonymous () then
	   			register_for_session sp srv_newthread me#page_newthread
        else (); (* user can't write, OR service is public because everyone can write *)

	 if me#can_write sess && foruminfo.writable_by <> Users.anonymous ()
	 then 
	 begin
		register_for_session sp srv_newmessage me#page_newmessage;
		register_for_session sp srv_replymessage me#page_replymessage
	 end;

        )
          
    method private logout_actions sp = return ()
      
		method register =
		begin
      register srv_forum me#page_forum;
      register srv_forum' me#page_forum';
      register srv_thread me#page_thread;
      register srv_thread' me#page_thread';
			register srv_reply me#page_reply;
			register srv_reply' me#page_reply';
      sessionmanager#add_login_actions me#login_actions;
      sessionmanager#add_logout_actions me#logout_actions;
      if foruminfo.writable_by = Users.anonymous() then
			begin
        (* see comment to register_aux in page_forum' *)
        register srv_newthread me#page_newthread;
				register srv_newmessage me#page_newmessage;
				register srv_replymessage me#page_replymessage;
			end
		end

end
