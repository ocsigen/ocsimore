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
    (Wiki_cache.get_wikibox_data ~wikibox:ids ())
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
exception Not_css_editor

class noneditable_wikibox =
object (self)

  inherit Widget.widget_with_error_box as papa

  val ne_class = "noned_wikibox"

  method container ?css content =
    let css = match css with
      | None -> {{ [] }}
      | Some c -> c
    in
    {{
       <html>[
         <head>[
           <title>"Ocsimore wiki"
             !css
         ]
         <body>content
       ]
     }}


  method display_error_box ?classe ?message ?exn () =
    match exn with
      | Some (Unknown_box (w, i)) ->
          papa#display_error_box
            ?classe
            ~message:("The box "^Int32.to_string i^
                        " does not exist in wiki "^Int32.to_string w^".")
            ?exn
            ()
      | Some Not_css_editor ->
          papa#display_error_box
            ?classe
            ~message:("You are not allowed to modify the stylesheet for that page.")
            ?exn
            ()
      | _ -> papa#display_error_box ?classe ?message ?exn ()
       
  method pretty_print_wikisyntax ?subbox ~(ancestors : Wiki_syntax.ancestors)
    ~sp ~sd wiki_id content =
    Lwt.return (Ocamlduce.Utf8.make content)

  method private retrieve_wikibox_content ids =
    Wiki_cache.get_wikibox_data ~wikibox:ids () >>= fun result ->
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

type menu_item =
  | Edit
  | Edit_perm
  | Edit_css
  | History
  | View


class editable_wikibox () =
  (* The registration must be done during site loading, nor before! *)
  
  let service_edit_wikibox =
    Eliom_services.new_service
      ~path:["ocsimore"; "wiki_edit"]
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      ()
  in
    
  let service_edit_css =
    Eliom_services.new_service'
      ~name:"css_edit"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.string "page"))
      ()
  in
    
  let service_edit_wikicss =
    Eliom_services.new_service'
      ~name:"wiki_css_edit"
      ~get_params:(Eliom_parameters.int32 "wikiid")
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
    
  let action_edit_wikibox_permissions =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_edit_perm"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      (fun sp g () ->
         let sd = Ocsimore_common.get_sd sp in
         Wiki.get_role sp sd g >>= fun role ->
         if role = Wiki.Admin
         then
           Lwt.return [Ocsimore_common.Session_data sd;
                       Wiki.Wiki_action_info (Wiki.Edit_perm g)]
         else Lwt.return [Ocsimore_common.Session_data sd])
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
             Eliom_parameters.string "content")))
      (fun sp () (actionname, (((wikiid, boxid) as a, content) as p)) -> 
         if actionname = "save"
         then
           let sd = Ocsimore_common.get_sd sp in
           Wiki_filter.preparse_extension (sp, sd, boxid) wikiid content
           >>= fun content ->
           Wiki.save_wikibox sp sd (a, content)
         else
           Lwt.return [Wiki.Wiki_action_info (Wiki.Preview p)]
      )
  in

  let action_send_wikibox_permissions =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~keep_get_na_params:false
      ~name:"wiki_send_permissions"
      ~post_params:
      ((Eliom_parameters.int32 "wikiid" ** 
              Eliom_parameters.int32 "boxid") ** 
         (Eliom_parameters.string "addreaders" **
            (Eliom_parameters.string "addwriters" **
               (Eliom_parameters.string "addrightadm" **
                  (Eliom_parameters.string "addwbcr" **
                     (Eliom_parameters.string "delreaders" **
                        (Eliom_parameters.string "delwriters" **
                           (Eliom_parameters.string "delrightadm" **
                              Eliom_parameters.string "delwbcr")
            )))))))
      (fun sp () p -> 
         let sd = Ocsimore_common.get_sd sp in
         Wiki.save_wikibox_permissions sp sd p
      )
  in

  let action_send_css =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~keep_get_na_params:false
      ~name:"css_send"
      ~post_params:
      ((Eliom_parameters.int32 "wikiid" ** 
              Eliom_parameters.string "page") ** 
             Eliom_parameters.string "content")
      (fun sp () ((wiki, page), content) -> 
         Wiki_cache.set_css_for_page ~wiki ~page content >>= fun () ->
         Lwt.return []
      )
  in

  let action_send_wiki_css =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~keep_get_na_params:false
      ~name:"wiki_css_send"
      ~post_params:
      (Eliom_parameters.int32 "wikiid" ** Eliom_parameters.string "content")
      (fun sp () (wiki, content) -> 
         Wiki_cache.set_css_for_wiki ~wiki content >>= fun () ->
         Lwt.return []
      )
  in


  (* Registering the service for css *)
  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service
      ~path:["ocsimore"; "wikicss"]
      ~get_params:(Eliom_parameters.int32 "wiki")
      (fun sp wiki () -> 
         Lwt.catch
           (fun () -> Wiki_cache.get_css_for_wiki wiki)
           (function
              | Not_found -> Lwt.fail Eliom_common.Eliom_404
              | e -> Lwt.fail e
           )
      )
  in
  
  
  (* Registering the service for css *)
  let pagecss_service =
    Eliom_predefmod.CssText.register_new_service
      ~path:["ocsimore"; "pagecss"]
      ~get_params:(Eliom_parameters.suffix 
                     (Eliom_parameters.prod
                        (Eliom_parameters.int32 "wiki")
                        (Eliom_parameters.all_suffix_string "page")))
      (fun sp (wiki, page) () -> 
         Lwt.catch
           (fun () -> Wiki_cache.get_css_for_page wiki page)
           (function
              | Not_found -> Lwt.fail Eliom_common.Eliom_404
              | e -> Lwt.fail e
           )
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
  val css_class = "editcss"

  method private create_error_message = function
    | Wiki.Operation_not_allowed -> "Operation not allowed"
    | Wiki.Action_failed e -> "Action failed"

   method private box_menu ~sp ?(perm = false) ?cssmenu ?service 
     ?(title = "")
     ((wiki, _) as ids) =
     let history = Eliom_services.preapply action_wikibox_history 
       (ids, (None, None)) 
     in
     let edit = Eliom_services.preapply action_edit_wikibox ids in
     let edit_perm = Eliom_services.preapply 
       action_edit_wikibox_permissions ids 
     in
     let view = Eliom_services.void_action in
     let edit_css =
       match cssmenu with
         | Some (Some page) -> 
             let cssedit = Eliom_services.preapply 
               service_edit_css (wiki, page) 
             in
             (Some (cssedit, (cssedit, {{ "edit page css" }})))
         | Some None -> 
             let cssedit = Eliom_services.preapply 
               service_edit_wikicss wiki
             in
             (Some (cssedit, (cssedit, {{ "edit wiki css" }})))
         | None -> None
     in
     let service = 
       match service with
         | None -> None
         | Some current -> 
             if current == View
             then Some view
             else
               if current == Edit
               then Some edit
               else 
                 if current == Edit_perm
                 then Some edit_perm
                 else 
                   match edit_css with
                     | Some (edit_css, _) -> 
                         if current == Edit_css
                         then Some edit_css
                         else Some history
                     | None -> Some history
     in
     let l = 
       [
         (edit, {{ "edit" }});
         (view, {{ "view" }});
       ]
     in
     let l = 
       if perm 
       then (edit_perm, {{ "edit permissions" }})::l
       else l
     in
     let l = match edit_css with
       | Some (_, mi) -> mi::l
       | None -> l
     in
     let title = Ocamlduce.Utf8.make title in
     {{ [ {: Eliom_duce_tools.menu
             ~sp
             ~classe:[box_button_class]
             (history, {{ "history" }})
             l
             ?service
             :}
          <p class={: box_title_class :}>title
        ]
      }}

   method display_menu_box 
     ~classe ?service ?cssmenu ?title ~sp ~sd ids content =
     let classe = Ocsimore_lib.build_class_attr classe in
     Wiki.get_role ~sp ~sd ids >>= fun role ->
     let perm = role = Wiki.Admin in
     Lwt.return
       {{ <div class={: classe :}>[
            !{: self#box_menu ~sp ~perm ?cssmenu ?service ?title ids :}
            <div>content ]}}
     
   method display_edit_form
     ~sp
     ~sd
     ?(rows=20)
     ?(cols=80)
     ~previewonly
     (wiki_id, message_id)
     (content : string)
     =
     let draw_form (actionname, (((wikiidname, boxidname), contentname))) =
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
       {{ [<p>[!f
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
              ]] }}
     in
     Lwt.return
       {{[
           {:
              Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikibox
              ~sp draw_form ()
              :}]
        }}

   method display_edit_perm_form
     ~sp
     ~(sd : Ocsimore_common.session_data)
     ((wiki_id, message_id) as ids)
     =
     Wiki.get_readers ids >>= fun readers ->
     Wiki.get_writers ids >>= fun writers ->
     Wiki.get_rights_adm ids >>= fun rights_adm ->
     Wiki.get_wikiboxes_creators ids >>= fun creators ->
     Ocsimore_lib.lwt_bind_opt
       readers
       (List.fold_left 
          (fun s r -> 
             s >>= fun s -> 
             Users.get_user_name_by_id r >>= fun s2 ->
             Lwt.return (s^" "^s2))
          (Lwt.return ""))
     >>= fun r ->
     Ocsimore_lib.lwt_bind_opt
       writers
       (List.fold_left 
          (fun s r -> 
             s >>= fun s -> 
             Users.get_user_name_by_id r >>= fun s2 ->
             Lwt.return (s^" "^s2))
          (Lwt.return ""))
     >>= fun w ->
     Ocsimore_lib.lwt_bind_opt
       rights_adm
       (List.fold_left 
          (fun s r -> 
             s >>= fun s -> 
             Users.get_user_name_by_id r >>= fun s2 ->
             Lwt.return (s^" "^s2))
          (Lwt.return ""))
     >>= fun a ->
     Ocsimore_lib.lwt_bind_opt
       creators 
       (List.fold_left
          (fun s r -> 
             s >>= fun s -> 
             Users.get_user_name_by_id r >>= fun s2 ->
             Lwt.return (s^" "^s2))
          (Lwt.return ""))
     >>= fun c ->
     let r = Ocsimore_lib.string_of_string_opt r in
     let w = Ocsimore_lib.string_of_string_opt w in
     let a = Ocsimore_lib.string_of_string_opt a in
     let c = Ocsimore_lib.string_of_string_opt c in
     
     let draw_form
         ((wikiidname, boxidname),
          (addrn, (addwn, (addan, (addc, 
                                   (delrn, (delwn, (delan, delc)))))))) =
           {{ [<p>[
                  {: Eliom_duce.Xhtml.int32_input
                     ~input_type:{: "hidden" :} 
                     ~name:wikiidname
                     ~value:wiki_id () :}
                    {: Eliom_duce.Xhtml.int32_input
                       ~input_type:{: "hidden" :} 
                       ~name:boxidname
                       ~value:message_id () :}
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
                    'Users who can change rights of this wiki box: ' 
                      !{: a :}
                      <br>[]
                      'Add: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:addan
                         () :}
                      <br>[]
                      'Remove: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:delan
                         () :}
                      <br>[]
                      'Users who can create wikiboxes inside this wiki box: ' 
                      !{: c :}
                      <br>[]
                      'Add: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:addc
                         () :}
                      <br>[]
                      'Remove: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:delc
                         () :}
                      <br>[]
                      {: 
                         Eliom_duce.Xhtml.button
                         ~button_type:{: "submit" :}
                         {{ "Save" }}
                         :}
                ]
              ]
            }}
           
     in
     Lwt.return
       {{[
           {:
              Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikibox_permissions
              ~sp draw_form ()
              :}]
        }}



   method display_edit_box
     ~sp
     ~sd
     ((w, b) as ids)
     ~classe
     ?cssmenu
     content
     =
     let title = "Edit - Wiki "^Int32.to_string w^", box "^Int32.to_string b in
     self#display_menu_box
       ~classe:(editform_class::classe)
       ~service:Edit
       ~title
       ~sp
       ~sd
       ?cssmenu 
       ids
       content

   method display_edit_perm
     ~sp
     ~sd
     ((w, b) as ids)
     ~classe
     ?cssmenu 
     content
     =
     let title = "Permissions - Wiki "^Int32.to_string w^
       ", box "^Int32.to_string b 
     in
     self#display_menu_box
       ~classe:(editform_class::classe)
       ~service:Edit_perm
       ~title
       ~sp
       ~sd
       ?cssmenu 
       ids
       content

   method display_editable_box ~sp ~sd ids ~classe ?cssmenu content =
     self#display_menu_box 
       ~classe:(editable_class::classe)
       ~service:View
       ~sp
       ~sd
       ?cssmenu 
       ids
       content

   method retrieve_old_wikibox_content ~sp ~sd ids version =
     Wiki_cache.get_wikibox_data ~version ~wikibox:ids ()
     >>= fun result ->
     match result with
       | None -> Lwt.fail Not_found
       | Some (com, a, cont, d) -> Lwt.return cont

   method display_old_wikibox ~sp ~sd 
     ((w, b) as ids) version ~classe ?cssmenu content =
     let title = "Old version - Wiki "^Int32.to_string w^", box "^Int32.to_string b^
       ", version "^Int32.to_string version in
     self#display_menu_box
       ~classe:(oldwikibox_class::classe)
       ~title
       ~sp
       ~sd
       ?cssmenu 
       ids
       content

   method display_src_wikibox ~sp ~sd ((w, b) as ids)
     version ~classe ?cssmenu content =
     let title = "Source - Wiki "^Int32.to_string w^", box "^Int32.to_string b^
       ", version "^Int32.to_string version in
     self#display_menu_box
       ~classe:(srcwikibox_class::classe)
       ~sp ~sd
       ?cssmenu 
       ~title
       ids
       content

   method private retrieve_history ~sp (wiki_id, message_id) ?first ?last () =
     Wiki_sql.get_history wiki_id message_id

   method display_history ~sp ids l =
     Lwt_util.map
       (fun (version, comment, author, date) -> 
          Users.get_user_name_by_id author >>= fun author ->
          Lwt.return
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
            >>= fun l ->
     Lwt.return {{ map {: l :} with i -> i
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

   method display_history_box ~sp ~sd ((w, b) as ids) ~classe ?cssmenu content =
     let title = 
       "History - Wiki "^Int32.to_string w^", box "^Int32.to_string b
     in
     self#display_menu_box
       ~classe:(history_class::classe)
       ~service:History
       ~title
       ~sp ~sd
       ?cssmenu 
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
     ?cssmenu (* display css menu for that page *)
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
                     (self#display_edit_box ~sp ~sd ?cssmenu data)
               | Some (Wiki.Edit_perm i) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (Lwt.return data)
                     (self#display_edit_perm_form ~sp ~sd)
                     (self#display_edit_perm ~sp ~sd ?cssmenu data)
               | Some (Wiki.Preview (i, content)) when i = data ->
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
                        self#display_edit_form ~sp ~sd ?cols ?rows
                          ~previewonly:false data c >>= fun form ->
                        Lwt.return {{ [<p class={: box_title_class :}>"Preview"
                                       preview
                                       !form ] }}
                     )
                     (self#display_edit_box ~sp ~sd ?cssmenu data)
               | Some (Wiki.History (i, (first, last))) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_history ~sp data ?first ?last ())
                     (self#display_history ~sp data)
                     (self#display_history_box ~sp ~sd ?cssmenu data)
               | Some (Wiki.Oldversion (i, version)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_old_wikibox_content ~sp ~sd data version)
                     (self#pretty_print_wikisyntax ?subbox
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ~sp ~sd wiki_id)
                     (self#display_old_wikibox ~sp ~sd ?cssmenu data version)
               | Some (Wiki.Src (i, version)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_old_wikibox_content ~sp ~sd data version)
                     (fun c -> 
                        let c = Ocamlduce.Utf8.make c in
                        Lwt.return {{ [ <pre>c ] }})
                     (self#display_src_wikibox ~sp ~sd ?cssmenu data version)
               | Some (Wiki.Error (i, error)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     ~error:(self#create_error_message error)
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_editable_box ~sp ~sd ?cssmenu data)
               | _ -> 
                   self#bind_or_display_error
                     ~classe
                     (self#retrieve_wikibox_content data)
                     (self#pretty_print_wikisyntax
                        ~ancestors:(Wiki_syntax.add_ancestor data ancestors)
                        ?subbox ~sp ~sd wiki_id)
                     (self#display_editable_box ~sp ~sd ?cssmenu data)
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




   method display_edit_css_form
     ~sp
     ~sd
     ?(rows=20)
     ?(cols=80)
     ~data:(wiki_id, page)
     (content : string)
     =
     let draw_form ((wikiidname, pagename), contentname) =
       {{ [<p>[
              {: Eliom_duce.Xhtml.int32_input
                 ~input_type:{: "hidden" :} 
                 ~name:wikiidname
                 ~value:wiki_id () :}
                {: Eliom_duce.Xhtml.string_input
                   ~input_type:{: "hidden" :} 
                   ~name:pagename
                   ~value:page () :}
                {: Eliom_duce.Xhtml.textarea
                   ~name:contentname
                   ~rows
                   ~cols
                   ~value:(Ocamlduce.Utf8.make content) () :}
              <br>[]
                 {: 
                    Eliom_duce.Xhtml.button
                    ~button_type:{{ "submit" }}
                      {{ "Save" }}
                    :}
              ]] }}
     in
     Lwt.return
       {{[
           {:
              Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_css
              ~sp draw_form ()
              :}]
        }}


   method display_edit_css_box
     ~sp
     ~sd
     ((w, _) as ids)
     page
     ~classe
     ?cssmenu 
     content
     =
     let title = "CSS for wiki "^Int32.to_string w^
       (if page = "" 
        then ", main page" 
        else ", page "^page)
     in
     self#display_menu_box
       ~classe:(css_class::editable_class::classe)
       ~service:Edit_css
       ~title
       ~sp
       ~sd
       ?cssmenu
       ids
       content

   method edit_css_box
     ~sp
     ~sd 
     ~data
     ?rows
     ?cols
     ?(classe=[])
     ()
     =
     let (wiki, page) = data in
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors () >>= fun c ->
     Wiki_cache.get_box_for_page wiki page >>= fun box ->
     self#bind_or_display_error
       ~classe
       (if c
        then
          Lwt.catch
            (fun () -> 
               Wiki_cache.get_css_for_page wiki page (* The css exists *)
            )
            (function 
               | Not_found -> Lwt.return ""
               | e -> Lwt.fail e
            )
        else Lwt.fail Not_css_editor)
       (self#display_edit_css_form ~sp ~sd ?rows ?cols ~data)
       (self#display_edit_css_box ~sp ~sd ~cssmenu:(Some page)
          (wiki, box) page)



   method display_edit_wikicss_box
     ~sp
     ~sd
     ((w, _) as ids)
     ~classe
     ?cssmenu
     content
     =
     let title = "CSS for wiki "^Int32.to_string w^
       " (global stylesheet)"
     in
     self#display_menu_box
       ~classe:(css_class::editable_class::classe)
       ~service:Edit_css
       ~title
       ~sp
       ~sd
       ?cssmenu
       ids
       content

       
   method display_edit_wikicss_form
     ~sp
     ~sd
     ?(rows=20)
     ?(cols=80)
     ~wiki
     (content : string)
     =
     let draw_form (wikiidname, contentname) =
       {{ [<p>[
              {: Eliom_duce.Xhtml.int32_input
                 ~input_type:{: "hidden" :} 
                 ~name:wikiidname
                 ~value:wiki () :}
                {: Eliom_duce.Xhtml.textarea
                   ~name:contentname
                   ~rows
                   ~cols
                   ~value:(Ocamlduce.Utf8.make content) () :}
              <br>[]
                 {: 
                    Eliom_duce.Xhtml.button
                    ~button_type:{{ "submit" }}
                      {{ "Save" }}
                    :}
              ]] }}
     in
     Lwt.return
       {{[
           {:
              Eliom_duce.Xhtml.post_form
              ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wiki_css
              ~sp draw_form ()
              :}]
        }}



   method edit_wikicss_box
     ~sp
     ~sd 
     ~wiki
     ?rows
     ?cols
     ?(classe=[])
     ()
     =
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors () >>= fun c ->
     self#bind_or_display_error
       ~classe
       (if c
        then
          Lwt.catch
            (fun () -> 
               Wiki_cache.get_css_for_wiki wiki (* The css exists *)
            )
            (function 
               | Not_found -> Lwt.return ""
               | e -> Lwt.fail e
            )
        else Lwt.fail Not_css_editor)
       (self#display_edit_wikicss_form ~sp ~sd ?rows ?cols ~wiki)
       (self#display_edit_wikicss_box ~sp ~sd
          ?cssmenu:(Some None)
          (wiki, 
           Wiki.wikipage_container_id))
       

   method get_css_header ~sp ~wiki ?page () =
     let css =
       Eliom_duce.Xhtml.css_link 
         (Eliom_duce.Xhtml.make_uri
            (Eliom_services.static_dir sp) 
            sp ["ocsiwikistyle.css"]) ()
(*VVV CSS? *)
     in
     Lwt.catch
       (fun () ->
          Wiki_cache.get_css_for_wiki wiki >>= fun _ ->
            Lwt.return 
              {{ [ css
                     {:
                        Eliom_duce.Xhtml.css_link 
                        (Eliom_duce.Xhtml.make_uri wikicss_service sp wiki)
                        ()
(*VVV encoding? *)
                        :}
                 ]}}
       )
       (function
          | Not_found -> Lwt.return {{ [ css ] }}
          | e -> Lwt.fail e)
     >>= fun css ->
       Lwt.catch
         (fun () ->
            match page with
              | None -> Lwt.return css
              | Some page ->
                  Wiki_cache.get_css_for_page wiki page >>= fun _ ->
                    Lwt.return 
                      {{ [ !css
                             {:
                                Eliom_duce.Xhtml.css_link 
                                (Eliom_duce.Xhtml.make_uri 
                                   pagecss_service sp (wiki, page))
                                ()
(*VVV encoding? *)
                                :}
                         ]}}
         )
         (function
            | Not_found -> Lwt.return css
            | e -> Lwt.fail e)




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
                           ?subbox
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
         (fun wiki_id (sp, sd, father) args c -> 
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
                Users.get_user_id ~sp ~sd >>= fun userid ->
                let ids = (wiki_id, father) in
                Wiki.can_create_wikibox ~sp ~sd wiki father userid >>= fun b ->
                if b
                then begin
                  Wiki.get_readers ~wiki ids >>= fun readers ->
                  Wiki.get_writers ~wiki ids >>= fun writers ->
                  Wiki.get_rights_adm ~wiki ids >>= fun rights_adm ->
                  Wiki.get_wikiboxes_creators ~wiki ids 
                    >>= fun wikiboxes_creators ->
                  Wiki.new_wikibox 
                    ~wiki
                    ~author:userid
                    ~comment:"new wikibox" 
                    ~content:"**//new wikibox//**"
                    ?readers
                    ?writers
                    ?rights_adm
                    ?wikiboxes_creators
                    () >>= fun box ->
                  Lwt.return (Some 
                                (Wiki_syntax.string_of_extension 
                                   "wikibox" 
                                   (("box", Int32.to_string box)::args)
                                   c))
                end
                else Lwt.return None
            with Failure _ -> Lwt.return None)

         );

       Wiki_syntax.add_link_extension "link"
        (fun wiki_id (sp, sd, (subbox, ancestors)) args c -> 
           let href = 
             try 
               List.assoc "page" args
             with Not_found -> ""
           in
           let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
           let https = 
             try 
               let a = List.assoc "protocol" args in
               if a = "http"
               then Some false
               else 
                 if a = "https"
                 then Some true
                 else None
             with Not_found -> None
           in
           let wiki_id = 
             try 
               Int32.of_string (List.assoc "wiki" args)
             with 
               | Failure _
               | Not_found -> wiki_id
           in
           let content = match c with
             | Some c -> c
             | None -> href
           in
           let href =
             Ocsigen_lib.remove_slash_at_end
               (Ocsigen_lib.remove_slash_at_beginning
                  (Ocsigen_lib.remove_dotdot (Neturl.split_path href)))
           in
           ((Eliom_duce.Xhtml.make_uri
               ?https
               ?fragment
               ~service:(Wiki_syntax.find_servpage wiki_id)
               ~sp
               href
            ),
            args,
            Lwt.return (Ocamlduce.Utf8.make content))
        );

       Wiki_syntax.add_link_extension "nonattachedlink"
        (fun wiki_id (sp, sd, (subbox, ancestors)) args c -> 
           let href = 
             try 
               List.assoc "page" args
             with Not_found -> ""
           in
           let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
           let https = 
             try 
               let a = List.assoc "protocol" args in
               if a = "http"
               then Some false
               else 
                 if a = "https"
                 then Some true
                 else None
             with Not_found -> None
           in
           let wiki_id = 
             try 
               Int32.of_string (List.assoc "wiki" args)
             with 
               | Failure _
               | Not_found -> wiki_id
           in
           let content = match c with
             | Some c -> c
             | None -> href
           in
           ((Eliom_duce.Xhtml.make_uri
               ?https
               ?fragment
               ~service:(Wiki_syntax.find_naservpage wiki_id)
               ~sp
               href
            ),
            args,
            Lwt.return (Ocamlduce.Utf8.make content))
        );



       Eliom_duce.Xhtml.register
         service_edit_wikibox
         (fun sp ((w, b) as g) () -> 
            let sd = Ocsimore_common.get_sd sp in
            self#editable_wikibox ~sp ~sd ~data:g  
              ~rows:30
              ~ancestors:Wiki_syntax.no_ancestors
              () >>= fun subbox ->
            self#editable_wikibox ~sp ~sd
              ~ancestors:Wiki_syntax.no_ancestors
              ~data:(w, Wiki.wikiadmin_container_id)
              ?cssmenu:(Some None)
              ~subbox:{{ [ subbox ] }} () >>= fun page ->
            self#get_css_header ~sp ~wiki:w ?page:None () >>= fun css ->
            Lwt.return (self#container ~css {{ [ page ] }})

         );

       Eliom_duce.Xhtml.register
         service_edit_css
         (fun sp ((wiki, page) as g) () -> 
            let sd = Ocsimore_common.get_sd sp in
            self#edit_css_box ~sp ~sd ~rows:30 ~data:g () >>= fun subbox ->
            self#editable_wikibox ~sp ~sd
              ~ancestors:Wiki_syntax.no_ancestors
              ~data:(wiki, Wiki.wikipage_container_id)
              ?cssmenu:(Some None)
              ~subbox:{{ [ subbox ] }} () >>= fun pagecontent ->
            self#get_css_header ~sp ~wiki ?page:(Some page) () >>= fun css ->
            Lwt.return (self#container ~css {{ [ pagecontent ] }})

         );

       Eliom_duce.Xhtml.register
         service_edit_wikicss
         (fun sp wiki () -> 
            let sd = Ocsimore_common.get_sd sp in
            self#edit_wikicss_box ~sp ~sd ~rows:30 ~wiki ()
(*              >>= fun subbox ->
            self#editable_wikibox ~sp ~sd
              ~ancestors:Wiki_syntax.no_ancestors
              ~data:(wiki, Wiki.wikiadmin_container_id)
              ?cssmenu:(Some None)
              ~subbox:{{ [ subbox ] }} () 
*)
            >>= fun pagecontent ->
            self#get_css_header ~sp ~wiki ?page:None () >>= fun css ->
            Lwt.return (self#container ~css {{ [ pagecontent ] }})

         )

     end
              
end
