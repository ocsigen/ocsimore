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

(*
let retrieve_full_wikibox_data ((wiki_id, _) as ids) =
  self#bind_or_display_error
    (Wiki_sql.get_wikibox_data ~wikibox:ids ())
    (fun result ->
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
                      comment = com }))
*)

exception Unknown_box of (int32 * int32)

class noneditable_wikibox =
object (self)

  inherit Widget.widget_with_error_box as papa

  val ne_class = "noned_wikibox"

  method display_error_box ?classe ?message ?exn () =
    match exn with
      | Some (Unknown_box (w, i)) ->
          papa#display_error_box
            ?classe
            ~message:("The box "^Int32.to_string i^
                        " does not exist in wiki "^Int32.to_string w^".")
            ?exn
            ()
      | _ -> papa#display_error_box ?classe ?message ?exn ()
       
  method pretty_print_wikisyntax ?subbox ~(ancestors : Wiki_syntax.ancestors)
    ~sp ~sd wiki_id content =
    Lwt.return (Ocamlduce.Utf8.make content)

  method private retrieve_wikibox_content ids =
    Wiki_sql.get_wikibox_data ~wikibox:ids () >>= fun result ->
    match result with
      | None -> Lwt.fail (Unknown_box ids)
      | Some (com, a, cont, d) -> Lwt.return cont

  method display_noneditable_box ~classe content =
    let classe = Ocsimore_lib.build_class_attr (ne_class::classe) in
    Lwt.return
      {{ <div class={: classe :}>content }}

  method noneditable_wikibox
    ?(subbox : Xhtmltypes_duce.flows option)
    ~ancestors
    ~sp ~sd ?(classe=[]) ~data =
    Wiki.get_role ~sp ~sd data >>= fun role ->
    match role with
      | Wiki.Admin
      | Wiki.Author
      | Wiki.Lurker -> 
          self#bind_or_display_error
            ~classe
            (self#retrieve_wikibox_content data)
            (self#pretty_print_wikisyntax ?subbox ~ancestors ~sp ~sd (fst data))
            (self#display_noneditable_box)
      | Wiki.Nonauthorized ->
          Lwt.return
            (self#display_error_box 
               ~classe:(ne_class::classe)
               ~message:"You are not allowed to see this content."
               ())


end;;


class editable_wikibox () =
  (* The registration must be done during site loading, nor before! *)
  
  let service_edit_wikibox =
    Eliom_services.new_service
      ~path:["ocsimore";"wiki_edit"]
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      ()
  in
    
  let action_edit_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_edit"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      (fun sp g () -> 
         Lwt.return [Wiki.Wiki_action_info (Wiki.Edit_box g)])
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
    
  let action_src_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_src"
      ~get_params:(((Eliom_parameters.int32 "wikiid") ** 
                      (Eliom_parameters.int32 "boxid")) **
                     (Eliom_parameters.int32 "version"))
      (fun sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.Src g)])
  in
    
  let action_send_wikibox =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~keep_get_na_params:false
      ~name:"wiki_send"
      ~post_params:
      (Eliom_parameters.string "actionname" **
         (((Eliom_parameters.int32 "wikiid" ** 
              Eliom_parameters.int32 "boxid") ** 
             Eliom_parameters.string "content") **
            (Eliom_parameters.opt (Eliom_parameters.string "addreaders") **
               Eliom_parameters.opt (Eliom_parameters.string "addwriters") **
               Eliom_parameters.opt (Eliom_parameters.string "addadmin") **
               Eliom_parameters.opt (Eliom_parameters.string "delreaders") **
               Eliom_parameters.opt (Eliom_parameters.string "delwriters") **
               Eliom_parameters.opt (Eliom_parameters.string "deladmin")
            )))
      (fun sp () (actionname, ((((wikiid, _) as a, content), b) as p)) -> 
         if actionname = "save"
         then
           let sd = Ocsimore_common.get_sd sp in
           Wiki_filter.preparse_extension (sp, sd) wikiid content
           >>= fun content ->
           Wiki.save_wikibox sp sd ((a, content), b)
         else
           Lwt.return [Wiki.Wiki_action_info (Wiki.Preview p)]
      )
  in

(*  fun <other parameters if any> -> *)
object (self)
  
  inherit Widget.widget_with_error_box as error_class
  inherit noneditable_wikibox

  val editform_class = "wikibox editform"
  val history_class = "wikibox history"
  val editable_class = "wikibox editable"
  val oldwikibox_class = "wikibox editable oldversion"
  val srcwikibox_class = "wikibox editable src"
  val box_button_class = "boxbutton"
  val box_title_class = "boxtitle"
  val preview_class = "preview"

  method private create_error_message = function
    | Wiki.Operation_not_allowed -> "Operation not allowed"
    | Wiki.Action_failed e -> "Action failed"

  val preapply_edit = 
    fun ids -> 
      (Eliom_services.preapply action_edit_wikibox ids
         :
         (Eliom_services.get_service_kind, [ `Unregistrable ]) 
         Eliom_duce_tools.one_page)

   val preapply_history =
     fun ids ->
       Eliom_services.preapply action_wikibox_history (ids, (None, None))

   val preapply_view =
     fun ids -> Eliom_services.cancel_action

   method private box_menu ?service ids =
     let history = preapply_history ids in
     let edit = preapply_edit ids in
     let view = preapply_view ids in
     let service = 
       match service with
         | None -> None
         | Some current -> 
             if current == preapply_view
             then Some view
             else
               if current == preapply_edit
               then Some edit
               else Some history
     in
     Eliom_duce_tools.menu
       ~classe:[box_button_class]
       (history, {{ "history" }})
       [
         (edit, {{ "edit" }});
         (view, {{ "view" }});
       ]
       ?service

   method display_menu_box ~classe ?service ~sp ids content =
     let classe = Ocsimore_lib.build_class_attr classe in
     Lwt.return
       {{ <div class={: classe :}>
            [ {: self#box_menu ?service ids ~sp :}
              <div>content ] }}
     
   method display_edit_form
     ~sp
     ~sd
     ?(rows=20)
     ?(cols=80)
     ~previewonly
     ?(rights = (None, (None, (None, (None, (None, None))))))
     ((wiki_id, message_id) as ids) 
     (content : string)
     =
     let draw_form r
         (actionname,
          (((wikiidname, boxidname), contentname),
           (addrn, (addwn, (addan, (delrn, (delwn, delan))))))) =
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
                   ~value:(Ocamlduce.Utf8.make content) () :}
              <br>[]
            ]
          }}
       in
       match r with
         | Some (r, w, a) ->
             let (ar, (aw, (aa, (dr, (dw, da))))) = rights in 
             {{ [<p>[!f 
                       'Users who can read this wiki box: ' 
                       !{: r :}
                     <br>[]
                       'Add readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addrn
                          ?value:ar
                          () :}
                     <br>[]
                       'Remove readers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delrn
                          ?value:dr
                          () :}
                     <br>[]
                       'Users who can modify this wiki box: ' 
                       !{: w :}
                     <br>[]
                       'Add writers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addwn
                          ?value:aw
                          () :}
                     <br>[]
                       'Remove writers: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delwn
                          ?value:dw
                          () :}
                     <br>[]
                       'Users who can administrate this wiki box: ' 
                       !{: a :}
                     <br>[]
                       'Add box administrators: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:addan
                          ?value:aa
                          () :}
                     <br>[]
                       'Remove box administrators: '
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "text" :}
                          ~name:delan
                          ?value:da
                          () :}
                     <br>[]
                       !{: 
                        let prev =
                          Eliom_duce.Xhtml.string_button
                            ~name:actionname
                            ~value:"preview" {{ "Preview" }}
                        in
                        if previewonly
                        then [prev]
                        else
                          [prev;
                           Eliom_duce.Xhtml.string_button
                             ~name:actionname
                             ~value:"save" {{ "Save" }}
                          ] :}
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
           Wiki.get_readers ~sp ~sd ids >>= fun readers ->
           Wiki.get_writers ~sp ~sd ids >>= fun writers ->
           Wiki.get_admins ~sp ~sd ids >>= fun admins ->
           List.fold_left 
             (fun s r -> 
                s >>= fun s -> 
                Users.get_user_name_by_id r >>= fun s2 ->
                Lwt.return (s^" "^s2))
             (Lwt.return "")
             readers >>= fun r ->
           List.fold_left 
             (fun s r -> 
                s >>= fun s -> 
                Users.get_user_name_by_id r >>= fun s2 ->
                Lwt.return (s^" "^s2))
             (Lwt.return "")
             writers >>= fun w ->
           List.fold_left 
             (fun s r -> 
                s >>= fun s -> 
                Users.get_user_name_by_id r >>= fun s2 ->
                Lwt.return (s^" "^s2))
             (Lwt.return "")
             admins >>= fun a ->
           Lwt.return (Some (r, w, a))
       | _ -> Lwt.return None) >>= fun rightowners ->
     Lwt.return
       {{[
           {:
              Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikibox
              ~sp (draw_form rightowners) ()
              :}]
        }}

   method display_edit_box
     ~sp
     ((w, b) as ids)
     ~classe
     content
     =
     let title = "Wiki "^Int32.to_string w^", box "^Int32.to_string b in
     self#display_menu_box
       ~classe:(editform_class::classe)
       ~service:preapply_edit
       ~sp
       ids
       {{ [ <p class={: box_title_class :}>{: title :}
              !content ] }}

   method display_editable_box ~sp ids ~classe content =
     self#display_menu_box 
       ~classe:(editable_class::classe)
       ~service:preapply_view
       ~sp
       ids
       content

   method retrieve_old_wikibox_content ~sp ids version =
     Wiki_sql.get_wikibox_data ~version ~wikibox:ids ()
     >>= fun result ->
     match result with
       | None -> Lwt.fail Not_found
       | Some (com, a, cont, d) -> Lwt.return cont

   method display_old_wikibox ~sp ((w, b) as ids) version ~classe content =
     let title = "Wiki "^Int32.to_string w^", box "^Int32.to_string b^
       ", version "^Int32.to_string version in
     self#display_menu_box
       ~classe:(oldwikibox_class::classe)
       ~sp
       ids
       {{ [ <p class={: box_title_class :}>{: title :}
              !content ] }}

   method display_src_wikibox ~sp ids version ~classe content =
     let title = "Version "^Int32.to_string version in
     self#display_menu_box
       ~classe:(srcwikibox_class::classe)
       ~sp
       ids
       {{ [ <p class={: box_title_class :}>{: title :}
            !content ] }}

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
                       '. '
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
                         ' ('
                       {: 
                          Eliom_duce.Xhtml.a 
                          ~service:action_src_wikibox
                          ~sp
                          {{ "source" }}
                          (ids, version)
                          :}
                          ')'
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

   method display_history_box ~sp ids ~classe content =
     self#display_menu_box
       ~classe:(history_class::classe)
       ~service:preapply_history
       ~sp
       ids
       content


   method editable_wikibox
     ~sp
     ~sd 
     ~data
     ?rows
     ?cols
     ?(classe=[])
     ?subbox (* a box may be a container for another box *)
     ~ancestors
     ()
     =
     let rec find_action = function
       | [] -> None
       | (Wiki.Wiki_action_info e)::_ -> Some e
       | _::l -> find_action l
     in
     let (wiki_id, _) = data in
     let action = find_action (Eliom_sessions.get_exn sp) in 
     Wiki.get_role ~sp ~sd data >>= fun role ->
     (match role with
        | Wiki.Admin
        | Wiki.Author ->
            (match action with
               | Some (Wiki.Edit_box i) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_wikibox_content data)
                     (self#display_edit_form ~sp ~sd ?cols ?rows 
                        ~previewonly:true data)
                     (self#display_edit_box ~sp data)
               | Some (Wiki.Preview ((i, content), rights)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (Lwt.return content)
                     (fun c ->
                        self#pretty_print_wikisyntax
                          ?subbox 
                          ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                          ~sp ~sd wiki_id c >>= fun pp ->
                        self#display_noneditable_box ~classe:[preview_class] pp
                        >>= fun preview ->
                        self#display_edit_form ~sp ~sd ?cols ?rows ~rights
                          ~previewonly:false data c >>= fun form ->
                        Lwt.return {{ [<p class={: box_title_class :}>"Preview"
                                       preview
                                       !form ] }}
                     )
                     (self#display_edit_box ~sp data)
               | Some (Wiki.History (i, (first, last))) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_history ~sp data ?first ?last ())
                     (self#display_history ~sp data)
                     (self#display_history_box ~sp data)
               | Some (Wiki.Oldversion (i, version)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_old_wikibox_content ~sp data version)
                     (self#pretty_print_wikisyntax ?subbox
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ~sp ~sd wiki_id)
                     (self#display_old_wikibox ~sp data version)
               | Some (Wiki.Src (i, version)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_old_wikibox_content ~sp data version)
                     (fun c -> 
                        let c = Ocamlduce.Utf8.make c in
                        Lwt.return {{ [ <pre>c ] }})
                     (self#display_src_wikibox ~sp data version)
               | Some (Wiki.Error (i, error)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     ~error:(self#create_error_message error)
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_editable_box ~sp data)
               | _ -> 
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_editable_box ~sp data)
            )
        | Wiki.Lurker -> 
            (match action with
               | Some (Wiki.Edit_box i)
               | Some (Wiki.History (i, _))
               | Some (Wiki.Oldversion (i, _)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     ~error:(self#create_error_message
                               Wiki.Operation_not_allowed)
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_noneditable_box)
               | Some (Wiki.Error (i, error)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     ~error:(self#create_error_message
                               Wiki.Operation_not_allowed)
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_noneditable_box)
               | _ -> 
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_noneditable_box)
            )
       | Wiki.Nonauthorized ->
           Lwt.return
             (self#display_error_box 
                ~classe:(ne_class::classe)
                ~message:"You are not allowed to see this content."
                ())
     )


   initializer
     begin
       Wiki_syntax.add_block_extension "wikibox"
         (fun wiki_id (sp, sd, (subbox, ancestors)) args c -> 
            try
              let wiki = 
                try Int32.of_string (List.assoc "wiki" args) 
                with Not_found -> wiki_id 
              in
              try
                let box = Int32.of_string (List.assoc "box" args) in
                if Wiki_syntax.in_ancestors (wiki, box) ancestors
                then
                  let b =
                    self#display_error_box
                      ~message:"Wiki error: loop of wikiboxes" ()
                  in
                  Lwt.return {{ [ b ] }}
                else begin
                  (match c with
                     | None -> Lwt.return None
                     | Some c -> 
                         Wiki_syntax.xml_of_wiki
                           ~sp ~sd ~ancestors wiki_id c >>= fun r ->
                         Lwt.return (Some r)) >>= fun subbox ->
                  self#editable_wikibox 
                    ?rows:(Ocsimore_lib.int_of_string_opt
                             (Ocsimore_lib.list_assoc_opt "rows" args))
                    ?cols:(Ocsimore_lib.int_of_string_opt
                             (Ocsimore_lib.list_assoc_opt "cols" args))
                    ?classe:(try Some [List.assoc "class" args] 
                             with Not_found -> None) 
                    ~data:(wiki, box)
                    ~sp
                    ~sd
                    ?subbox
                    ~ancestors:(Wiki_syntax.add_ancestor (wiki, box) ancestors)
                    () >>= fun b ->
                  Lwt.return {{ [ b ] }}
                end
              with Not_found ->
                Lwt.return {{ [ <code>"<<wikibox>>" ] }}
            with
              | Failure _ ->
                  let b =
                    self#display_error_box
                      ~message:"Wiki error: error in wikibox extension" ()
                  in
                  Lwt.return {{ [ b ] }});
       
       Wiki_filter.add_preparser_extension "wikibox"
         (fun wiki_id (sp, sd) args c -> 
            (try
              let wiki = 
                try
                  Int32.of_string (List.assoc "wiki" args)
                with Not_found -> wiki_id
              in
              try
                ignore (List.assoc "box" args);
                Lwt.return None
              with Not_found ->
                Wiki.get_wiki_by_id wiki >>= fun wiki ->
                Users.get_user_name ~sp ~sd >>= fun name ->
                Wiki.new_wikibox 
                  wiki
                  name
                  "new wikibox" 
                  "**//new wikibox//**"
(*VVV readers, writers, admins? *)
                  () >>= fun box ->
                Lwt.return (Some 
                              (Wiki_syntax.string_of_extension 
                                 "wikibox" 
                                 (("box", Int32.to_string box)::args)
                                 c))
            with Failure _ -> Lwt.return None)

         );

       Eliom_duce.Xhtml.register
         service_edit_wikibox
         (fun sp ((w, b) as g) () -> 
            let sd = Ocsimore_common.get_sd sp in
            self#editable_wikibox ~sp ~sd ~data:g  
              ~ancestors:Wiki_syntax.no_ancestors
              () >>= fun subbox ->
            self#editable_wikibox ~sp ~sd
              ~ancestors:Wiki_syntax.no_ancestors
              ~data:(w, 1l) ~subbox:{{ [ subbox ] }} () >>= fun page ->
            Lwt.return
              {{
                 <html>[
                   <head>[
                     <title>"Ocsimore administration"
                       {: Eliom_duce.Xhtml.css_link 
                          (Eliom_duce.Xhtml.make_uri
                             (Eliom_services.static_dir sp) 
                             sp ["example.css"]) () :}
(*VVV quel css ? quel layout de page ? *)
                   ]
                   <body>[ page ]
                 ]
               }}

         )

     end
              
end
