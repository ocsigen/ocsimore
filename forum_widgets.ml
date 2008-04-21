open Lwt
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Session_manager
open Ocsimorelib
open CalendarLib
open Widget

type message_data =
{
	id: Forum_sql.forum;
	text: string;
	author: string;
	datetime: Calendar.t;
	hidden: bool;
};;

class message_toggle_action ~(parent: sessionmanager) = 
object (self)
  inherit [Forum_sql.forum * int32] parametrized_widget parent
    
  val div_class = "thread_toggle"
    
  method apply ~sp (frm_id, msg_id) =
    Lwt_pool.use Sql.pool (fun db -> 
      Forum_sql.message_toggle_hidden db ~frm_id ~msg_id >>= fun () -> 
      return {{ <div class={: div_class :}>[
		  <p>"Thread toggled."		
	        ] }})
end;;

class message_list_widget ~(parent: sessionmanager) = 
object (self)
  inherit [message_data] list_widget parent
  inherit [Forum_sql.forum * int32 * int64 option * int64 option] parametrized_widget parent
    
  val div_class = "message_list"
    
  method private retrieve_data (forum_id, thr_id, offset, limit) =
    Lwt_pool.use Sql.pool (fun db -> 
      Forum.get_role db parent forum_id >>= fun role -> 
      Forum_sql.thread_get_messages_with_text
        db ~thr_id ~role ?offset ?limit () >>= fun results ->
      Lwt_util.map
        (fun (i, t, a, d, h, _) ->
	   return { id = i; 
                    text = t; 
                    author = a; 
                    datetime = d; 
                    hidden = h })
        results
          >>= fun children -> 
      return (self#set_children children))
	
  method apply ~sp (forum_id, thread_id, offset, limit) =
    self#retrieve_data (forum_id, thread_id, offset, limit) >>= fun () -> 
    return self#get_children >>= fun subjects -> 
    Lwt_util.map 
      (fun s -> 
         return 
           {{
	      <div class="message_data">
		[
		  <h4>['posted by: ' !{: s.author :} ' ' !{: sod s.datetime :}]
		  <pre>{: s.text :}
		]
	    }}) subjects >>= fun rows -> 
        return
	  {{
	     <div class={: div_class :}>[
	       !{:
		   match rows with
		     | [] -> {{ [<p>"This thread does not contain any messages."] }}
		     | l -> {{ {: l :} }}
			 :}
	     ]
	   }}
end;;

class message_navigation_widget
  ~(parent: sessionmanager)
  ~(srv_thread:(int32 * (int32 * int64 option), 
                unit, 
                get_service_kind,
                [`WithoutSuffix], 
                [`One of int32] param_name * 
                  ([`One of int32] param_name * [`Opt of int64] param_name), 
                unit, 
                [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32 * int64 option * int64 option] parametrized_widget parent
    
  val div_class = "message_navigation"
  val mutable nr_messages = 0L
    
  method private retrieve_data (forum_id, thr_id, offset, limit) =
    Lwt_pool.use Sql.pool (fun db -> 
    Forum.get_role db parent forum_id >>= fun role -> 
    Forum_sql.thread_get_nr_messages db ~thr_id ~role >>= fun nr_m -> 
    nr_messages <- nr_m;
    Ocsigen_messages.debug2 "[message_navigation_widget] retrieve_data: end";
    return ())

  method apply ~sp (forum_id, thread_id, offset, limit) =
    self#retrieve_data (forum_id, thread_id, offset, limit) >>= fun () -> 
    return {{
	      <div class={: div_class :}>
		{:
		   match limit with
		     | None -> {{ [] }}
		     | Some l -> 
                         {{
			    [<table>[
				<tr>[
				  <td>{: if offset = None || 
                                         offset = Some 0L then
					   {{ "First" }}
				       else
					 {{[{: a srv_thread sp {{"First"}} 
                                               ((Forum_sql.get_id forum_id), 
                                                (thread_id, None))
                                               :}]}}
					   :}
				  <td>{: match offset with
					 | None -> {{ "Previous" }}
					 | Some o -> if o <= l then
					     {{[{: a srv_thread sp
                                                   {{"Previous"}}
                                                   ((Forum_sql.get_id forum_id),
                                                    (thread_id, None))
                                                   :}]}}
					   else
					     {{[{: a srv_thread sp
                                                   {{"Previous"}}
                                                   ((Forum_sql.get_id forum_id),
                                                    (thread_id, Some (Int64.sub o l)))
                                                   :}]}}
					       :}
				  <td>{: let o = match offset with
                                         | None -> 0L
                                         | Some x -> x 
                                       in
                                         if Int64.compare 
                                           (Int64.add o l)
                                           nr_messages >= 0 
                                         then
					   {{ "Next" }}
					 else
					   {{[{: a srv_thread sp {{"Next"}} 
                                                 ((Forum_sql.get_id forum_id), 
                                                  (thread_id, Some (Int64.add o l))):}]}}
					     :}
				  <td>{: let o = match offset with
                                         | None -> 0L
                                         | Some x -> x 
                                       in
					 if l >= nr_messages || 
                                           Int64.compare 
                                           (Int64.add o l)
                                           nr_messages >= 0
                                         then
					     {{ "Last" }}
					 else
					   {{[{: a srv_thread sp {{"Last"}} 
                                                 ((Forum_sql.get_id forum_id),
                                                  (thread_id, Some
                                                     (Int64.sub 
                                                        nr_messages
                                                        (Int64.rem
                                                           nr_messages l))))
                                                 :}]}}
					     :}
				]
			      ]]
			  }}
		           :}
	    }}
end;;

class message_forest_widget
  ~(parent: sessionmanager)
  ~(srv_reply_message:(int32 * 
                         (int32 * (int32 option * int32)), 
                       unit,
                       get_service_kind,
                       [`WithoutSuffix],
                       [`One of int32] param_name *
                         ([`One of int32] param_name * 
                            ([`Opt of int32] param_name *
                               [`One of int32] param_name)), 
                       unit,
                       [`Registrable]) service)
  ~(srv_message_toggle:(int32 * 
                          (int32 * int32 option),
                        int32,
                        post_service_kind, 
                        [`WithoutSuffix], 
                        [`One of int32] param_name * 
                          ([`One of int32] param_name *
                             [`Opt of int32] param_name), 
                        [`One of int32] param_name,
                        [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32 * int32 option] parametrized_widget parent
    
  val div_class = "message_forest"
  val mutable children: message_data tree list = []
  val mutable role = Forum_sql.Unknown 
    
  method set_children c = children <- c
    
  method get_children = children
    
  method private toggle_form hidden id (msg_id) =
    {{ [<p>['Message is hidden: '
              !{: if hidden then "YES" else "NO" :} ' ' 
              {: string_input ~input_type:{: "submit" :} ~value:"Toggle" () :}
              {: int32_input
                 ~input_type:{: "hidden" :} 
                 ~name:msg_id
                 ~value:(Forum_sql.get_id id) () :}
           ]] }}
      
  method private retrieve_data (forum_id, thr_id, bottom) =
    Lwt_pool.use Sql.pool (fun db -> 
    Forum.get_role db parent forum_id >>= fun r -> 
    role <- r;
    Forum_sql.thread_get_messages_with_text_forest db ~thr_id ~role ?bottom () 
        >>= fun results -> 
      lwt_forest_map
	(fun (i, t, a, d, h, _, _, _) ->
	   return { id = i; 
                    text = t; 
                    author = a; 
                    datetime = d; 
                    hidden = h })
	results >>= fun children -> 
      return (self#set_children children))

  method apply ~sp (forum_id, thread_id, bottom) =
    let rec listize_forest
        (f: Xhtmltypes_duce._div tree list): Xhtmltypes_duce.ul list Lwt.t =
      Lwt_util.map (function 
		      | Node (p, ch) -> listize_forest ch >>= fun rest -> 
                          return {{ <ul>[ 
				      <li>[{: p :}]
				        !{: List.map (fun r -> {{ <li>[{: r :}] }}) rest :}
			            ] }}) f
    in
      self#retrieve_data (forum_id, thread_id, bottom) >>= fun () -> 
      return self#get_children >>= fun subjects -> 
      lwt_forest_map 
        (fun s -> 
           return {{
		     <div class="message_data">
		       [
			 <h4>['posted by: ' !{: s.author :} ' ' !{: sod s.datetime :}]
			 <pre>{: s.text :}
			   !{: match role with
			       | Forum_sql.Moderator -> 
                                   {{ [{: post_form ~service:srv_message_toggle
                                          ~sp (self#toggle_form s.hidden s.id)
                                          ((Forum_sql.get_id forum_id), (thread_id, None)) :}] }}
			       | _ -> {{ [] }}
			           :}
			   {: a srv_reply_message sp
                              {{ "Reply to this message" }}
                              ((Forum_sql.get_id forum_id), 
                               (thread_id, (None, 
                                            (Forum_sql.get_id s.id)))) :}
		       ]
	           }}) subjects >>= fun forest -> 
        listize_forest forest >>= fun div_contents -> 
        return {{
		  <div class={: div_class :}>{: div_contents :}
	        }}
end;;

class message_form_widget
  ~(parent: sessionmanager)
  ~(srv_add_message: (int32 * (int32 * int32 option), 
                      string * (int32 option * bool), 
                      post_service_kind,
                      [`WithoutSuffix], 
                      [`One of int32] param_name *
                        ([`One of int32] param_name * 
                           [`Opt of int32] param_name), 
                      [`One of string] param_name * 
                        ([`Opt of int32] param_name * 
                           [`One of bool] param_name), 
                      [`Registrable]) service) =
object (self)
	inherit [Forum_sql.forum * int32 * int32 option * int32 option] 
          parametrized_widget parent

	val div_class = "message_form"
	val mutable my_parent_id = None

	method private form (message, (parent_id, sticky)) =
	{: [
		<h2>{: 
			match my_parent_id with
			| None -> "Post a new message in this thread:"
			| Some _ -> "Reply to this message:" 
		:}
		!{: match my_parent_id with
			None -> {{ [] }}
		|	Some p -> 
                          {{ [<p>[ {: int32_input ~input_type:{: "hidden" :} 
                                      ~name:parent_id
                                      ~value:p () :}]] }} :}
		<p>[{: bool_checkbox ~name:sticky () :} ' Sticky message']
		<p>[{: textarea ~name:message ~rows:5 ~cols:80
                       ~value:{{ "Your message here" }} () :}]
		<p>[{: string_input ~input_type:{: "submit" :} 
                       ~value:"OK" () :}]
	] :}

	method apply ~(sp:server_params) 
          (forum_id, thread_id, parent_id, offset) =
	  Ocsigen_messages.debug2 "[forumWidget] message_form_widget#apply"; 
          my_parent_id <- parent_id;
	  return {{
		<div class={: div_class :}>[
			{: (post_form ~service:srv_add_message ~sp self#form)
                           ((Forum_sql.get_id forum_id), (thread_id, offset)) :}
		] }}
end;;

class message_add_action ~(parent: sessionmanager) = 
object (self)
  inherit [Forum_sql.forum * 
             int32 * 
             int32 option * 
             string * 
             bool] parametrized_widget parent
	
  val div_class = "message_add"
    
  method apply ~sp (forum_id, thr_id, parent_id, txt, sticky) =
    let author_id = parent#get_user_id in
    Lwt_pool.use Sql.pool 
      (fun db -> 
         Forum_sql.new_message
           db ~thr_id ?parent_id ~author_id ~txt ~sticky () >>= fun _ -> 
         return {{
		   <div class={: div_class :}>[
		     <p>"Your message has been added (possibly subject to moderation)."
		   ]
	         }})
end;;

class latest_messages_widget ~(parent: sessionmanager) =
object (self)
	inherit [int64] parametrized_widget parent

	val div_class = "latest_messages"
	val mutable messages = []
	
	method private set_messages m = messages <- m

	method private retrieve_data limit =
          Lwt_pool.use Sql.pool (fun db -> 
            Forum_sql.get_forums_list db >>= fun forums -> 
            let frm_ids = List.map (fun (id, _, _, _, _) -> id) forums in
            Forum_sql.get_latest_messages db ~frm_ids ~limit () >>= fun res ->
            Lwt.return (self#set_messages res))

	method apply ~sp limit =
	self#retrieve_data limit >>=
	fun () -> Lwt_util.map (fun (id, msg, author) ->
		return {{ <tr>[<td>{: msg :} <td>{: author :}] }} ) messages >>=
	fun tbl -> return {{
		<div class={: div_class :}>{:
			match tbl with
			| [] -> {{ [<p>"There are no messages for the moment."] }}
			| l -> {{ [<table>[
				<tr>[<th>"Message" <th>"Author"]
				!{: l :}
			]] }}
		:}
	}}
end;;

type thread_data =
    {
      id: Forum_sql.forum;
      subject: string;
      author: string;
      datetime: Calendar.t
    };;

class thread_widget
  ~(parent: sessionmanager)
  ~(srv_thread_toggle: (int32 * (int32 * int32 option), 
                        unit, 
                        post_service_kind,
                        [`WithoutSuffix],
                        [`One of int32] param_name * 
                          ([`One of int32] param_name * 
                             [`Opt of int32] param_name), 
                        unit, 
                        [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32] parametrized_widget parent
    
  val div_class = "thread"
  val mutable subject = ""
  val mutable author = ""
  val mutable article = None
  val mutable datetime = Calendar.now ()
  val mutable hidden = false
  val mutable shown_messages = 0L
  val mutable hidden_messages = 0L
  val mutable role = Forum_sql.Unknown 
    
  method set_subject s = subject <- s
  method set_author a = author <- a
  method set_article a = article <- (Some a)
  method set_datetime d = datetime <- d
  method set_hidden h = hidden <- h
  method set_shown_messages sm = shown_messages <- sm
  method set_hidden_messages hm = hidden_messages <- hm
    
  method get_subject = subject
  method get_author = author
  method get_article = article
  method get_datetime = datetime
  method get_hidden = hidden
  method get_shown_messages = shown_messages
  method get_hidden_messages = hidden_messages
    
  method private toggle_form () =
    {{ [<p>['Thread is hidden: '
              !{: if self#get_hidden then "YES" else "NO" :}
              ' ' 
              {: string_input ~input_type:{: "submit" :} ~value:"Toggle" () :}
           ]] }}

  method private retrieve_data (forum_id, thr_id) =
    Lwt_pool.use Sql.pool (fun db -> 
    Forum.get_role db parent forum_id >>= fun r ->
    role <- r;
    Forum_sql.thread_get_data db ~thr_id ~role
      >>= fun (i, s, a, ar, d, h, sm, hm) ->
    self#set_subject s;
    self#set_author a;
    (match ar with
       | None -> ()
       | Some x -> self#set_article x);
    self#set_datetime d;
    self#set_hidden h;
    self#set_shown_messages sm;
    return (self#set_hidden_messages hm))

  method apply ~sp (forum_id, thread_id) =
    self#retrieve_data (forum_id, thread_id) >>= fun () -> 
    return
      {{ <div class={: div_class :}>[
	   <h1>{: self#get_subject :}
	     !{:
		 match role with
		   | Forum_sql.Moderator -> 
                       {{ [{: post_form ~service:srv_thread_toggle
                              ~sp self#toggle_form
                              ((Forum_sql.get_id forum_id), 
                               (thread_id, None)) :}] }}
		   | _ -> {{ [] }}
		       :}
	   <h2>{: Printf.sprintf
                  "Created by: %s %s" self#get_author (sod self#get_datetime)
                  :}
	   <div class="article">{:
			           match self#get_article with
			             | None -> {{ [] }}
			             | Some a -> {{ [<pre>{: a :}] }}
		                         :}
	 ] }}
end

class thread_toggle_action ~(parent: sessionmanager) = 
object (self)
  inherit [Forum_sql.forum * int32] parametrized_widget parent
    
  val div_class = "thread_toggle"
    
  method apply ~sp (frm_id, thr_id) =
    Lwt_pool.use Sql.pool (fun db -> 
    Forum_sql.thread_toggle_hidden db ~frm_id ~thr_id >>= fun () -> 
    return {{ <div class={: div_class :}>[
		<p>"Thread toggled."		
	      ] }})
end;;

class thread_list_widget
  ~(parent: sessionmanager)
  ~(srv_thread:(int32 * (int32 * int32 option), 
                unit, 
                get_service_kind, 
                [`WithoutSuffix],
                [`One of int32] param_name * 
                  ([`One of int32] param_name * [`Opt of int32] param_name),
                unit,
                [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum] parametrized_widget parent
  inherit [thread_data] list_widget parent
    
  val div_class = "thread_list"
    
  method private retrieve_data frm_id =
    Lwt_pool.use Sql.pool (fun db -> 
    Forum.get_role db parent frm_id >>= fun role -> 
    Forum_sql.forum_get_threads_list db ~frm_id ~role () >>= fun result -> 
    Lwt_util.map (fun (i, s, a, d, _) ->
                    return { id = Forum_sql.of_id i; 
                             subject = s; 
                             author = a; 
                             datetime = d }
	         ) result >>= fun children -> 
    return (self#set_children children))

  method apply ~sp (forum_id) =
    catch (fun () -> self#retrieve_data forum_id >>= fun () -> 
             return self#get_children >>= fun subjects -> 
             Ocsigen_messages.debug2
               (Printf.sprintf "[thread_list] apply: %d items" 
                  (List.length subjects));
	       Lwt_util.map 
                 (fun s -> 
                    return {{ <tr>[
			        <td>{: sod s.datetime :}
			        <td>[{: a ~service:srv_thread ~sp
                                        {{ {: s.subject :} }} 
                                        (Forum_sql.get_id forum_id, 
                                         (Forum_sql.get_id s.id, None)) :}]
			        <td>{: s.author:}
		              ] }}) subjects >>= fun rows -> 
                 return {{
		           <div class={: div_class :}>
                             {:
			        match rows with
			          | [] -> {{ [<p>"This forum does not contain any threads."] }}
			          | l -> {{ [<table>[
				                <tr>[<th>"Time" <th>"Subject" <th>"Author"]
				                  !{: l :}
			                      ]] }}
		                      :} }}) 
      (function 
	 | Not_found -> 
             return
               {{ <div class={: div_class :}>
                    [<p>"This forum is not available."] }}
	 | e -> return {{ <div class={: div_class :}>[
		            <p>{: Printf.sprintf "Error: %s"
                                  (Printexc.to_string e) :}
	                  ] }})
end

class thread_form_widget
  ~(parent: sessionmanager) 
  ~(srv_add_thread: (int32, 
                     bool * (string * string), 
                     post_service_kind,
                     [`WithoutSuffix], 
                     [`One of int32] param_name,
                     [`One of bool] param_name * 
                       ([`One of string] param_name * 
                          [`One of string] param_name), 
                     [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum] parametrized_widget parent
    
  val div_class = "thread_form"
    
  method private form (is_article, (subject, txt)) =
    {{ [
	 <h2>"Start a new thread"
	 <table>[
	   <tr>[<td>[{: bool_checkbox ~checked:true ~name:is_article () :} ' This message is an article']]
	   <tr>[<td>['Subject: ' {: string_input ~input_type:{: "text" :} ~name:subject () :}]]
	   <tr>[<td>[{: textarea ~name:txt ~rows:5 ~cols:80 ~value:{{ "Your message here" }} () :}]]
	   <tr>[<td>[{: string_input ~input_type:{: "submit" :} ~value:"Submit" () :}]]
	 ]
       ] }}
      
  method apply ~sp forum_id =
    Ocsigen_messages.debug2 "[forumWidget] thread_form#apply";
    return {{
	      <div class={: div_class :}>[
		{: (post_form ~service:srv_add_thread ~sp self#form) 
                   (Forum_sql.get_id forum_id) :}
	      ] }}
end;; 

class thread_add_action ~(parent: sessionmanager) =
object (self)
	inherit [Forum_sql.forum * bool * string * string] parametrized_widget parent
	
	val div_class = "thread_add"

	method apply ~sp (frm_id, is_article, sbj, txt) =
	  let author_id = parent#get_user_id 
	  and subject = (if sbj = "" then "No subject" else sbj) in
          Lwt_pool.use Sql.pool (fun db -> 
            (if is_article then
		Forum_sql.new_thread_and_article db ~frm_id ~author_id ~subject ~txt
	else
		Forum_sql.new_thread_and_message db ~frm_id ~author_id ~subject ~txt) >>=
	fun _ -> return {{
		<div class={: div_class :}>[
			<p>"The new thread has been created (possibly subject to moderation)."
		]
	}})
end;;

type forum_data =
    {
      id: Forum_sql.forum;
      name: string;
      description: string;
      moderated: bool;
      arborescent: bool;
    };;

class forums_list_widget
  ~(parent: sessionmanager)
  ~(srv_forum: (int32,
                unit, 
                get_service_kind, 
                [`WithoutSuffix], 
                [`One of int32] param_name, 
                unit,
                [`Registrable]) service) =
object (self)

  inherit [unit] parametrized_widget parent
  inherit [forum_data] list_widget parent

  val div_class = "forums_list"

  method private retrieve_data () =
    Lwt_pool.use Sql.pool 
      (fun db -> 
         Forum_sql.get_forums_list db >>= fun result -> 
         Lwt_util.map (fun (i, n, d, m, a) ->
	                 Lwt.return
                           { id = i; 
                             name = n; 
                             description = d; 
                             moderated = m; 
                             arborescent = a }
	              ) result 
           >>= fun children -> 
         Lwt.return (self#set_children children))

  method apply ~sp () =
    self#retrieve_data () >>= fun () -> 
    return self#get_children >>= fun subjects -> 
    Lwt_util.map
      (fun s -> 
         return {{ <tr>[
		     <td>[{: a ~service:srv_forum
                             ~sp {{ {: s.name :} }} 
                             (Forum_sql.get_id s.id) :}]
		     <td>{: s.description :}
		     <td>{: if s.moderated then "Yes" else "No" :}
		   ] }}) subjects >>= fun rows -> 
        return {{
		  <div class={: div_class :}>
                    {:
		       match rows with
			 | [] -> {{ [<p>"There are no forums available."] }}
			 | l -> {{ [<table>[
				       <tr>[<th>"Name" <th>"Description" <th>"Moderated"]
				         !{: l :}
			             ]] }}
		             :}
	        }}
end;;

class forum_form_widget ~(parent: sessionmanager) ~(srv_add_forum: (unit, string * (string * (string * (bool * bool))), post_service_kind, [`WithoutSuffix], unit, [`One of string] param_name * ([`One of string] param_name * ([`One of string] param_name * ([`One of bool] param_name * [`One of bool] param_name))), [`Registrable]) service) =
object (self)
	inherit [unit] parametrized_widget parent

	val div_class = "forum_form"

	method private form (name, (url, (descr, (moderated, arborescent)))) =
	{{ [
		<h2>"Start a new forum"
		<table>[
			<tr>[<td>['Name: ' {: string_input ~input_type:{: "text" :} ~name () :}]]
			<tr>[<td>['URL: ' {: string_input ~input_type:{: "text" :} ~name:url () :}]]
			<tr>[<td>['Description: ' {: string_input ~input_type:{: "text" :} ~name:descr () :}]]
			<tr>[<td>[{: bool_checkbox ~checked:true ~name:moderated () :} ' This forum is moderated']]
			<tr>[<td>[{: bool_checkbox ~checked:true ~name:arborescent () :} ' This forum is arborescent']]
			<tr>[<td>[{: string_input ~input_type:{: "submit" :} ~value:"Submit" () :}]]
		]
	] }}

	method apply ~sp forum_id =
	return {{
		<div class={: div_class :}>[
			{: (post_form ~service:srv_add_forum ~sp self#form) () :}
		] }}
end;;

class forum_add_action ~(parent: sessionmanager) =
object (self)
	inherit [string * string * string * bool * bool] parametrized_widget parent
	
	val div_class = "forum_add"

	method apply ~sp (name, url, descr, moderated, arborescent) =
          Lwt_pool.use Sql.pool (fun db -> 
          Forum.create_forum
            db ~title:name ~descr ~moderated ~arborescent () >>=
	    fun _ ->
		return {{
		<div class={: div_class :}>[
			<p>"The new thread has been created."
		]
	}})
end;;

(* 

class message_widget ~(parent: sessionmanager) ~(srv_message_toggle: unit) =
object (self)
  inherit [Forum_sql.forum * int] parametrized_widget parent
    
  val div_class = "message"
  val mutable subject = ""
  val mutable author = ""
  val mutable text = ""
  val mutable datetime = Calendar.now ()
  val mutable hidden = false
  val mutable sticky = false
    
  method set_subject s = subject <- s
  method set_author a = author <- a
  method set_text t = text <- t
  method set_datetime d = datetime <- d
  method set_hidden h = hidden <- h
  method set_sticky s = sticky <- s
    
  method apply ~sp (forum_id, message_id) =
    self#retrieve_data (forum_id, message_id) >>= fun () -> 
    Forum.get_role db parent forum_id >>= fun role -> 
    return
      {{ <div class={: div_class :}>[
	   <h4>{: Format.sprintf "posted by: %s %s" author (sod datetime) :}
	     !{:
		 match role with
		   | Forum_sql.Moderator -> 
                       {{ [<p>{: Format.sprintf
                                 "Message is hidden: %s; sticky: %s" 
                                 (if hidden then "YES" else "NO")
                                 (if sticky then "YES" else "NO") :}] }}
		   | _ -> {{[] }}
		       :}
	   <pre>{: text :}
	     (* TODO: reply to this message *)
	 ] }}
end;;

*)
