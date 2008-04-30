(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)


let (>>=) = Lwt.bind
let ( ** ) = Eliom_parameters.prod

type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.userdata option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

let retrieve_wikibox_content ids =
  Wiki_sql.get_wikibox_data ~wikibox:ids () >>= fun result ->
  match result with
    | None -> Lwt.fail Not_found
    | Some (com, a, cont, d) -> Lwt.return cont

let retrieve_full_wikibox_data ((wiki_id, _) as ids) =
  Wiki_sql.get_wikibox_data ~wikibox:ids () >>= fun result ->
  match result with
    | None -> Lwt.fail Not_found
    | Some (com, a, cont, d) ->
        Lwt.catch
          (fun () -> 
             Users.get_user_by_name a >>= fun user ->
             Lwt.return (Some user))
          (function
             | Users.NoSuchUser -> Lwt.return None
             | e -> Lwt.fail e) >>= fun user ->
         Lwt.return
           ({ wiki_id = wiki_id;
              content = cont; 
              author = user; 
              datetime = d;
              comment = com })


let display_error_box message = 
  {{ <p class="errormsg">[ !{: message :} ] }}

class noneditable_wikibox =
object (self)

  val wikibox_class = "noned_wikibox"
    
  method private display_error_box = display_error_box

  method private retrieve_data a =
    retrieve_wikibox_content a

  method noneditable_wikibox ~sp ~sd ~data =
    Wiki.get_role ~sp ~sd data >>= fun role ->
    match role with
      | Wiki.Admin
      | Wiki.Author
      | Wiki.Lurker -> 
          self#retrieve_data data >>= fun content -> 
          Lwt.return
            {{ <div class={: wikibox_class :}>
                 {: content :}
             }}
      | Wiki.Nonauthorized ->
          Lwt.return
            {{ <div class={: wikibox_class :}>[
                 {: self#display_error_box 
                    "You are not allowed to see this content." :}
               ]
             }}


end;;


class editable_wikibox () =
  (* The registration must be done during site loading, nor before! *)
  
  let action_edit_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_edit"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      (fun sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.Edit_box g)])
  in
    
  let action_wikibox_history =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_history"
      ~get_params:(((Eliom_parameters.int32 "wikiid") ** 
                      (Eliom_parameters.int32 "boxid")) **
                     (Eliom_parameters.opt (Eliom_parameters.int "first") ** 
                        Eliom_parameters.opt (Eliom_parameters.int "last")))
      (fun sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.History g)])
  in
    
  let action_old_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_old_version"
      ~get_params:(((Eliom_parameters.int32 "wikiid") ** 
                      (Eliom_parameters.int32 "boxid")) **
                     (Eliom_parameters.int32 "version"))
      (fun sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.Oldversion g)])
  in
    
  let action_send_wikibox =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~keep_get_na_params:false
      ~name:"wiki_send"
      ~post_params:
      ((((Eliom_parameters.int32 "wikiid") ** 
           (Eliom_parameters.int32 "boxid")) ** 
          Eliom_parameters.string "content") **
         (Eliom_parameters.opt (Eliom_parameters.string "addreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "addwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "addadmin") **
            Eliom_parameters.opt (Eliom_parameters.string "delreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "delwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "deladmin")
         ))
      (fun sp () p -> 
         let sd = Ocsimore_common.create_sd () in
         Wiki.save_wikibox sp sd p)
  in
(*  fun <other parameters if any> -> *)

object (self)
  
   val ne_class = "wikibox"
   val editform_class = "wikibox editform"
   val history_class = "wikibox history"
   val editable_class = "wikibox editable"
   val oldwikibox_class = "wikibox editable oldversion"
   val box_button_class = "boxbutton"

   method private display_error_message = function
     | Some Wiki.Operation_not_allowed ->
         {{ [ {{ self#display_error_box "Operation not allowed" }} ] }}
     | Some (Wiki.Action_failed e) ->
         {{ [ {{ self#display_error_box "Action failed" }} ] }}
     | None ->
         {{ [] }}

   method private display_error_box = display_error_box

   method retrieve_wikibox_content = retrieve_wikibox_content

   method display_edit_box
     ~sp
     ~sd
     ?(rows=40)
     ?(cols=80)
     ~classe
     ((wiki_id, message_id) as ids) content =
     let draw_form r
         (((wikiidname, boxidname), contentname),
          (addrn, (addwn, (addan, (delrn, (delwn, delan)))))) =
       let f =
         {{ [
              {: Eliom_duce.Xhtml.int32_input
                 ~input_type:{: "hidden" :} 
                 ~name:wikiidname
                 ~value:wiki_id () :}
                {: Eliom_duce.Xhtml.int32_input
                   ~input_type:{: "hidden" :} 
                   ~name:boxidname
                   ~value:message_id () :}
                {: Eliom_duce.Xhtml.textarea
                   ~name:contentname
                   ~rows
                   ~cols
                   ~value:{{ {: content :} }} () :}
              <br>[]
            ]
          }}
       in
       match r with
         | Some (r, w, a) ->
             {{ [<p>[!f 
                       'Users who can read this wiki box: ' 
                       !{: r :}
                     <br>[]
                       'Add readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addrn
                          () :}
                     <br>[]
                       'Remove readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delrn
                          () :}
                     <br>[]
                       'Users who can modify this wiki box: ' 
                       !{: w :}
                     <br>[]
                       'Add writers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addwn
                          () :}
                     <br>[]
                       'Remove writers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delwn
                          () :}
                     <br>[]
                       'Users who can administrate this wiki box: ' 
                       !{: a :}
                     <br>[]
                       'Add readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addan
                          () :}
                     <br>[]
                       'Remove readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delan
                          () :}
                     <br>[]
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "submit" :} 
                          ~value:"Submit" () :}
                    ]
                ]
              }}
         | _ -> {{ [<p>[!f
                          {: Eliom_duce.Xhtml.string_input
                             ~input_type:{: "submit" :} 
                             ~value:"Submit" () :}
                       ]] }}
     in
     Wiki.get_role ~sp ~sd ids >>= fun role ->
     (match role with
       | Wiki.Admin ->
         Wiki_sql.get_readers wiki_id message_id >>= fun readers ->
         Wiki_sql.get_writers wiki_id message_id >>= fun writers ->
         Wiki_sql.get_admins wiki_id message_id >>= fun admins ->
         Lwt.return
           (Some
              ((List.fold_left 
                  (fun s r -> s^" "^Users.get_group_name r)
                  ""
                  readers),
               (List.fold_left 
                  (fun s r -> s^" "^Users.get_group_name r)
                  ""
                  writers),
               (List.fold_left 
                  (fun s r -> s^" "^Users.get_group_name r)
                  ""
                  admins)))
       | _ -> Lwt.return None) >>= fun rightowners ->
     Lwt.return
       {{ <div class={: editform_class ^ classe :}>
            [<p class={: box_button_class :}>[ 
              {: Eliom_duce.Xhtml.a
                   ~a:{{ { class={: box_button_class :} } }}
                   ~service:Eliom_services.cancel_action
                   ~sp
                   {{ "view" }} () :} 
                <span class={: box_button_class :}>[ 'edit' ]
                {: Eliom_duce.Xhtml.a 
                   ~a:{{ { class={: box_button_class :} } }}
                   ~service:action_wikibox_history
                   ~sp {{ "history" }} (ids, (None, None)) :}
              ]
                <div>[
                  {:
                     Eliom_duce.Xhtml.post_form
                     action_send_wikibox
                     sp (draw_form rightowners) ()
                     :}]
            ]
        }}

   method display_noneditable_box ?error ~classe content =
     let err = self#display_error_message error in
     Lwt.return
       {{ <div class={: ne_class ^ classe :}>[ !err !{: content :} ] }}

   method display_editable_box ~sp
     ?error ~classe ((wiki_id, message_id) as ids) content =
     let err = self#display_error_message error in
     Lwt.return
       {{ <div class={: editable_class ^ classe :}>
            [<p class={: box_button_class :}>
                [ 
                  <span class={: box_button_class :}>[ 'view' ]
                  {: Eliom_duce.Xhtml.a 
                     ~a:{{ { class={: box_button_class :} } }}
                     ~service:action_edit_wikibox
                     ~sp {{ "edit" }} ids :}
                  {: Eliom_duce.Xhtml.a 
                     ~a:{{ { class={: box_button_class :} } }}
                     ~service:action_wikibox_history
                     ~sp {{ "history" }} (ids, (None, None)) :}
                ]
             <div>[ !err !{: content :} ] ] }}


   method retrieve_old_wikibox_content ~sp ids version =
     Wiki_sql.get_wikibox_data ~version ~wikibox:ids ()
     >>= fun result ->
     match result with
       | None -> Lwt.fail Not_found
       | Some (com, a, cont, d) -> Lwt.return cont

   method display_old_wikibox ~sp ~classe ids (content : string) version =
     let title = "Version "^Int32.to_string version in
     Lwt.return
       {{ <div class={: oldwikibox_class ^ classe :}>
            [<p class={: box_button_class :}>
                [ !{: title :}
                    {: Eliom_duce.Xhtml.a
                       ~a:{{ { class={: box_button_class :} } }}
                       ~service:Eliom_services.cancel_action
                       ~sp
                       {{ "view" }} () :} 
                    {: Eliom_duce.Xhtml.a 
                       ~a:{{ { class={: box_button_class :} } }}
                       ~service:action_edit_wikibox
                       ~sp 
                       {{ "edit" }} ids :}
                    {: Eliom_duce.Xhtml.a 
                       ~a:{{ { class={: box_button_class :} } }}
                       ~service:action_wikibox_history
                       ~sp 
                       {{ "history" }} (ids, (None, None)) :}
                ]
             <div>{: content :} ] }}


   method private retrieve_history ~sp (wiki_id, message_id) ?first ?last () =
     Wiki_sql.get_history wiki_id message_id

   method display_history ~sp ids l =
     Lwt.return
       {{ map
            {: 
               List.map 
               (fun (version, comment, author, date) -> 
                  {{ [ 
                       !{: Int32.to_string version :}
                       ' '
                       !{: CalendarLib.Printer.Calendar.to_string date :}
                       ' '
                       <em>[ 'by ' !{: author :} ]
                       ' '
                       {: 
                          Eliom_duce.Xhtml.a 
                          ~service:action_old_wikibox
                          ~sp
                          {{ "view" }}
                          (ids, version)
                          :}
                       <br>[] 
                     ]
                   }})
               l
               :}
          with i -> i
        }}
(*       {{ map
            {: List.map 
               (fun (version, comment, author, date) -> 
                  {{ {: Int32.to_string version :} }}) 
               l
               :}
          with i -> 
            [ {: Eliom_duce.Xhtml.a 
                 ~service:action_old_wikibox
                 ~sp
                 i
                 (ids, Int32.of_string {: i :})
(*VVV How to get version without converting back!!! *)
                 :}
              <br>[] ]
        }}
*)

   method display_history_box ~sp ~classe ids ?first ?last l =
     self#display_history ~sp ids l >>= fun c ->
     Lwt.return
       {{ <div class={: history_class ^ classe :}>
            [<p class={: box_button_class :}>[ 
                {: Eliom_duce.Xhtml.a
                   ~a:{{ { class={: box_button_class :} } }}
                   ~service:Eliom_services.cancel_action
                   ~sp
                   {{ "view" }} () :}
                {: Eliom_duce.Xhtml.a 
                   ~a:{{ { class={: box_button_class :} } }}
                   ~service:action_edit_wikibox
                   ~sp {{ "edit" }} ids :}
                <span class={: box_button_class :}>[ 'history' ]
              ]
                <div>[<p>c] ] }}


   method editable_wikibox
     ~sp
     ~sd 
     ~data
     ?rows
     ?cols
     ?(classe=[])
     ()
     =
     let rec find_action = function
       | [] -> None
       | (Wiki.Wiki_action_info e)::_ -> Some e
       | _::l -> find_action l
     in
     let action = find_action (Eliom_sessions.get_exn sp) in
     let classe = Ocsimorelib.build_class_attr classe in
     Wiki.get_role ~sp ~sd data >>= fun role ->
     (match role with
        | Wiki.Admin
        | Wiki.Author ->
            (match action with
               | Some (Wiki.Edit_box i) when i = data ->
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_edit_box ~sp ~sd ~classe 
                     ?cols ?rows data content
               | Some (Wiki.History (i, (first, last))) when i = data ->
                   self#retrieve_history
                     ~sp data ?first ?last () >>= fun l ->
                   self#display_history_box ~sp ~classe data ?first ?last l
(*VVV et en cas d'erreur ? *)
               | Some (Wiki.Oldversion (i, version)) when i = data ->
                   self#retrieve_old_wikibox_content ~sp data version
                   >>= fun content ->
                   self#display_old_wikibox ~sp ~classe data content version
(*VVV et en cas d'erreur ? *)
               | Some (Wiki.Error (i, error)) when i = data ->
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_editable_box ~sp ~error ~classe data content
               | _ -> 
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_editable_box ~sp ~classe data content
            )
        | Wiki.Lurker -> 
            (match action with
               | Some (Wiki.Edit_box i)
               | Some (Wiki.History (i, _))
               | Some (Wiki.Oldversion (i, _)) when i = data ->
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_noneditable_box
                     ~error:(Wiki.Operation_not_allowed)
                     ~classe 
                     content
               | Some (Wiki.Error (i, error)) when i = data ->
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_noneditable_box ~error ~classe content
               | _ -> 
                   self#retrieve_wikibox_content data >>= fun content ->
                   self#display_noneditable_box ~classe content)
       | Wiki.Nonauthorized ->
           Lwt.return
             {{ <div class={: ne_class :}>[
                  {: self#display_error_box 
                     "You are not allowed to see this content." :}
                ]
              }}
     )
              
end
