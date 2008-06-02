open Lwt
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Session_manager
open Ocsimore_lib
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

class message_toggle_action = 
object (self)
  inherit [Forum_sql.forum * int32] parametrized_unit_div_widget
    
  val xhtml_class = "thread_toggle"
    
  method apply ~sp ~sd ~data:(frm_id, msg_id) =
    Forum_sql.message_toggle_hidden ~frm_id ~msg_id >>= fun () -> 
    Lwt.return {{ <div class={: xhtml_class :}>[
                    <p>"Thread toggled."                
                  ] }}
end

class message_list_widget = 
object (self)
  inherit [message_data] list_widget
  inherit [Forum_sql.forum * int32 * int64 option * int64 option, 
           message_data list] parametrized_div_widget
    
  val xhtml_class = "message_list"
    
  method private retrieve_data ~sp ~sd (forum_id, thr_id, offset, limit) =
    Forum.get_role sp sd forum_id >>= fun role -> 
    Forum_sql.thread_get_messages_with_text
      ~thr_id ~role ?offset ?limit () >>= fun results ->
    Lwt_util.map
      (fun (i, t, a, d, h, _) ->
         return { id = i; 
                  text = t; 
                  author = a; 
                  datetime = d; 
                  hidden = h })
      results
        
  method apply ~sp ~sd ~data:(forum_id, thread_id, offset, limit) =
    self#retrieve_data sp sd
      (forum_id, thread_id, offset, limit) >>= fun subjects -> 
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
             <div class={: xhtml_class :}>[
               !{:
                   match rows with
                     | [] -> {{ [<p>"This thread does not contain any messages."] }}
                     | l -> {{ {: l :} }}
                         :}
             ]
           }}
end;;

class message_navigation_widget
  ~(srv_thread:(int32 * (int32 * int64 option), 
                unit, 
                get_service_kind,
                [`WithoutSuffix], 
                [`One of int32] param_name * 
                  ([`One of int32] param_name * [`Radio of int64] param_name), 
                unit, 
                [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32 * int64 option * int64 option, int64] 
    parametrized_div_widget
    
  val xhtml_class = "message_navigation"
    
  method private retrieve_data ~sp ~sd (forum_id, thr_id, offset, limit) =
    Forum.get_role sp sd forum_id >>= fun role -> 
    Forum_sql.thread_get_nr_messages ~thr_id ~role

  method apply ~sp ~sd ~data:(forum_id, thread_id, offset, limit) =
    self#retrieve_data sp sd (forum_id, thread_id, offset, limit) 
    >>= fun nr_messages -> 
    return {{
              <div class={: xhtml_class :}>
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
  ~(srv_reply_message:(int32 * 
                         (int32 * (int32 option * int32)), 
                       unit,
                       get_service_kind,
                       [`WithoutSuffix],
                       [`One of int32] param_name *
                         ([`One of int32] param_name * 
                            ([`Radio of int32] param_name *
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
                             [`Radio of int32] param_name), 
                        [`One of int32] param_name,
                        [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32 * int32 option, 
           message_data Ocsimore_lib.tree list * Forum_sql.role] 
    parametrized_div_widget
    
  val xhtml_class = "message_forest"
    
  method private toggle_form hidden id (msg_id) =
    {{ [<p>['Message is hidden: '
              !{: if hidden then "YES" else "NO" :} ' ' 
              {: string_input ~input_type:{: "submit" :} ~value:"Toggle" () :}
              {: int32_input
                 ~input_type:{: "hidden" :} 
                 ~name:msg_id
                 ~value:(Forum_sql.get_id id) () :}
           ]] }}
      
  method private retrieve_data ~sp ~sd (forum_id, thr_id, bottom) =
    Forum.get_role sp sd forum_id >>= fun role -> 
    Forum_sql.thread_get_messages_with_text_forest ~thr_id ~role ?bottom () 
        >>= fun results -> 
      lwt_forest_map
        (fun (i, t, a, d, h, _, _, _) ->
           return { id = i; 
                    text = t; 
                    author = a; 
                    datetime = d; 
                    hidden = h })
        results >>= fun children ->
      Lwt.return (children, role)

  method apply ~sp ~sd ~data:(forum_id, thread_id, bottom) =
    let rec listize_forest
        (f: Xhtmltypes_duce._div tree list): Xhtmltypes_duce.ul list Lwt.t =
      Lwt_util.map (function 
                      | Node (p, ch) -> listize_forest ch >>= fun rest -> 
                          return {{ <ul>[ 
                                      <li>[{: p :}]
                                        !{: List.map (fun r -> {{ <li>[{: r :}] }}) rest :}
                                    ] }}) f
    in
      self#retrieve_data sp sd (forum_id, thread_id, bottom) 
      >>= fun (subjects, role) -> 
      lwt_forest_map 
        (fun s -> 
           return {{
                     <div class="message_data">
                       [
                         <h4>['posted by: ' !{: s.author :} ' ' 
                                !{: sod s.datetime :}]
                         <pre>{: s.text :}
                           !{: match role with
                               | Forum_sql.Moderator -> 
                                   {{ [{: post_form ~service:srv_message_toggle
                                          ~sp (self#toggle_form s.hidden s.id)
                                          ((Forum_sql.get_id forum_id), 
                                           (thread_id, None)) :}] }}
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
                  <div class={: xhtml_class :}>{: div_contents :}
                }}
end;;

class message_form_widget
  ~(srv_add_message: (int32 * (int32 * int32 option), 
                      string * (int32 option * bool), 
                      post_service_kind,
                      [`WithoutSuffix], 
                      [`One of int32] param_name *
                        ([`One of int32] param_name * 
                           [`Radio of int32] param_name), 
                      [`One of string] param_name * 
                        ([`Radio of int32] param_name * 
                           [`One of bool] param_name), 
                      [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32 * int32 option * int32 option] 
    parametrized_unit_div_widget
    
  val xhtml_class = "message_form"
    
  method private form my_parent_id (message, (parent_id, sticky)) =
    {: [
       <h2>{: 
              match my_parent_id with
                | None -> "Post a new message in this thread:"
                | Some _ -> "Reply to this message:" 
                    :}
         !{: match my_parent_id with
               None -> {{ [] }}
             |        Some p -> 
                  {{ [<p>[ {: int32_input ~input_type:{: "hidden" :} 
                              ~name:parent_id
                              ~value:p () :}]] }} :}
       <p>[{: bool_checkbox ~name:sticky () :} ' Sticky message']
       <p>[{: textarea ~name:message ~rows:5 ~cols:80
              ~value:{{ "Your message here" }} () :}]
       <p>[{: string_input ~input_type:{: "submit" :} 
              ~value:"OK" () :}]
     ] :}
      
  method apply ~(sp:server_params) ~sd
    ~data:(forum_id, thread_id, parent_id, offset) =
    Ocsigen_messages.debug2 "[forumWidget] message_form_widget#apply"; 
    return {{
              <div class={: xhtml_class :}>[
                {: (post_form
                      ~service:srv_add_message
                      ~sp 
                      (self#form parent_id))
                   ((Forum_sql.get_id forum_id), (thread_id, offset)) :}
              ] }}
end

class message_add_action = 
object (self)
  inherit [Forum_sql.forum * 
             int32 * 
             int32 option * 
             string * 
             bool] parametrized_unit_div_widget
        
  val xhtml_class = "message_add"
    
  method apply ~sp ~sd ~data:(forum_id, thr_id, parent_id, txt, sticky) =
    Users.get_user_id sp sd >>= fun author_id ->
    Forum_sql.new_message
      ~thr_id ?parent_id ~author_id ~txt ~sticky () >>= fun _ -> 
    Lwt.return {{
                  <div class={: xhtml_class :}>[
                    <p>"Your message has been added (possibly subject to moderation)."
                  ]
                }}
end

class latest_messages_widget =
object (self)
  inherit [int64, (Forum_sql.forum * string * string) list]
    parametrized_div_widget
    
  val xhtml_class = "latest_messages"
    
  method private retrieve_data ~sp ~sd limit =
    Forum_sql.get_forums_list () >>= fun forums -> 
    let frm_ids = List.map (fun (id, _, _, _, _) -> id) forums in
    Forum_sql.get_latest_messages ~frm_ids ~limit ()

  method apply ~sp ~sd ~data:limit =
    self#retrieve_data sp sd limit >>= fun messages -> 
    Lwt_util.map 
      (fun (id, msg, author) ->
         return {{ <tr>[<td>{: msg :} <td>{: author :}] }} ) messages
      >>= fun tbl -> 
    Lwt.return {{
                  <div class={: xhtml_class :}>{:
                        match tbl with
                        | [] -> {{ [<p>"There are no messages for the moment."] }}
                        | l -> {{ [<table>[
                                <tr>[<th>"Message" <th>"Author"]
                                !{: l :}
                        ]] }}
                :}
        }}
end

type thread_data =
    {
      id: Forum_sql.forum;
      subject: string;
      author: string;
      datetime: Calendar.t
    };;

class thread_widget
  ~(srv_thread_toggle: (int32 * (int32 * int32 option), 
                        unit, 
                        post_service_kind,
                        [`WithoutSuffix],
                        [`One of int32] param_name * 
                          ([`One of int32] param_name * 
                             [`Radio of int32] param_name), 
                        unit, 
                        [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum * int32,
           ((int32 * string * string * string option * 
               Calendar.t * bool * int64 * int64) * Forum_sql.role)
          ] parametrized_div_widget
    
  val xhtml_class = "thread"
    
  method private toggle_form hidden () =
    {{ [<p>['Thread is hidden: '
              !{: if hidden then "YES" else "NO" :}
              ' ' 
              {: string_input ~input_type:{: "submit" :} ~value:"Toggle" () :}
           ]] }}

  method private retrieve_data ~sp ~sd (forum_id, thr_id) =
    Forum.get_role sp sd forum_id >>= fun role ->
    Forum_sql.thread_get_data ~thr_id ~role >>= fun r ->
    Lwt.return (r, role)

  method apply ~sp ~sd ~data:(forum_id, thread_id) =
    self#retrieve_data sp sd (forum_id, thread_id) 
    >>= fun ((_, subject, author, article, datetime, 
              hidden, shown_messages, hidden_messages), role) -> 
    return
      {{ <div class={: xhtml_class :}>[
           <h1>{: subject :}
             !{:
                 match role with
                   | Forum_sql.Moderator -> 
                       {{ [{: post_form ~service:srv_thread_toggle
                              ~sp (self#toggle_form hidden)
                              ((Forum_sql.get_id forum_id), 
                               (thread_id, None)) :}] }}
                   | _ -> {{ [] }}
                       :}
           <h2>{: Printf.sprintf
                  "Created by: %s %s" author (sod datetime)
                  :}
           <div class="article">{:
                                   match article with
                                     | None -> {{ [] }}
                                     | Some a -> {{ [<pre>{: a :}] }}
                                         :}
         ] }}
end

class thread_toggle_action = 
object (self)
  inherit [Forum_sql.forum * int32] parametrized_unit_div_widget
    
  val xhtml_class = "thread_toggle"
    
  method apply ~sp ~sd ~data:(frm_id, thr_id) =
    Forum_sql.thread_toggle_hidden ~frm_id ~thr_id >>= fun () -> 
    return {{ <div class={: xhtml_class :}>[
                <p>"Thread toggled."                
              ] }}
end;;

class thread_list_widget
  ~(srv_thread:(int32 * (int32 * int32 option), 
                unit, 
                get_service_kind, 
                [`WithoutSuffix],
                [`One of int32] param_name * 
                  ([`One of int32] param_name * [`Radio of int32] param_name),
                unit,
                [`Registrable]) service) =
object (self)
  inherit [Forum_sql.forum, 
           thread_data list * Forum_sql.role] parametrized_div_widget
  inherit [thread_data] list_widget
    
  val xhtml_class = "thread_list"
    
  method private retrieve_data ~sp ~sd frm_id =
    Forum.get_role sp sd frm_id >>= fun role -> 
    Forum_sql.forum_get_threads_list ~frm_id ~role () >>= fun result -> 
    Lwt_util.map (fun (i, s, a, d, _) ->
                    return { id = Forum_sql.of_id i; 
                             subject = s; 
                             author = a; 
                             datetime = d }
                 ) result >>= fun children -> 
    return (children, role)

  method apply ~sp ~sd ~data:(forum_id) =
    catch 
      (fun () -> 
         self#retrieve_data sp sd forum_id >>= fun (subjects, role) -> 
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
                       <div class={: xhtml_class :}>
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
               {{ <div class={: xhtml_class :}>
                    [<p>"This forum is not available."] }}
         | e -> return {{ <div class={: xhtml_class :}>[
                            <p>{: Printf.sprintf "Error: %s"
                                  (Printexc.to_string e) :}
                          ] }})
end

class thread_form_widget
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
  inherit [Forum_sql.forum] parametrized_unit_div_widget
    
  val xhtml_class = "thread_form"
    
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
      
  method apply ~sp ~sd ~data:forum_id =
    Ocsigen_messages.debug2 "[forumWidget] thread_form#apply";
    return {{
              <div class={: xhtml_class :}>[
                {: (post_form ~service:srv_add_thread ~sp self#form) 
                   (Forum_sql.get_id forum_id) :}
              ] }}
end;; 

class thread_add_action =
object (self)
        inherit [Forum_sql.forum * bool * string * string] 
          parametrized_unit_div_widget
        
        val xhtml_class = "thread_add"

        method apply ~sp ~sd ~data:(frm_id, is_article, sbj, txt) =
          Users.get_user_id sp sd >>= fun author_id ->
          let subject = (if sbj = "" then "No subject" else sbj) in
          (if is_article 
           then
             Forum_sql.new_thread_and_article ~frm_id ~author_id ~subject ~txt
           else
             Forum_sql.new_thread_and_message ~frm_id ~author_id ~subject ~txt) >>=
        fun _ -> return {{
                <div class={: xhtml_class :}>[
                        <p>"The new thread has been created (possibly subject to moderation)."
                ]
        }}
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
  ~(srv_forum: (int32,
                unit, 
                get_service_kind, 
                [`WithoutSuffix], 
                [`One of int32] param_name, 
                unit,
                [`Registrable]) service) =
object (self)

  inherit [unit, forum_data list] parametrized_div_widget
  inherit [forum_data] list_widget

  val xhtml_class = "forums_list"

  method private retrieve_data ~sp ~sd () =
    Forum_sql.get_forums_list () >>= fun result -> 
    Lwt_util.map (fun (i, n, d, m, a) ->
                    Lwt.return
                      { id = i; 
                        name = n; 
                        description = d; 
                        moderated = m; 
                        arborescent = a }
                 ) result

  method apply ~sp ~sd ~data:() =
    self#retrieve_data sp sd () >>= fun subjects -> 
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
                  <div class={: xhtml_class :}>
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

class forum_form_widget
  ~(srv_add_forum: (unit, string * 
                      (string * (string * (bool * bool))), 
                    post_service_kind, 
                    [`WithoutSuffix],
                    unit,
                    [`One of string] param_name * 
                      ([`One of string] param_name * 
                         ([`One of string] param_name * 
                            ([`One of bool] param_name * 
                               [`One of bool] param_name))), 
                    [`Registrable]) service) =
object (self)
  inherit [unit] parametrized_unit_div_widget
    
  val xhtml_class = "forum_form"
    
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
      
  method apply ~sp ~sd ~data:forum_id =
    return {{
              <div class={: xhtml_class :}>[
                {: (post_form ~service:srv_add_forum ~sp self#form) () :}
              ] }}
end
  
class forum_add_action =
object (self)
        inherit [string * string * string * bool * bool] 
          parametrized_unit_div_widget
        
        val xhtml_class = "forum_add"

        method apply ~sp ~sd ~data:(name, url, descr, moderated, arborescent) =
          Forum.create_forum
            ~title:name ~descr ~moderated ~arborescent () >>=
            fun _ ->
              return {{
                        <div class={: xhtml_class :}>[
                          <p>"The new thread has been created."
                        ]
                      }}
end;;

(* 

class message_widget ~(srv_message_toggle: unit) =
object (self)
  inherit [Forum_sql.forum * int] parametrized_div_widget
    
  val xhtml_class = "message"
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
    
  method apply ~sp ~sd ~data:(forum_id, message_id) =
    self#retrieve_data ~sp ~sd (forum_id, message_id) >>= fun () -> 
    Forum.get_role sp sd forum_id >>= fun role -> 
    return
      {{ <div class={: xhtml_class :}>[
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
