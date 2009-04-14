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
   @author Boris Yakobowski
*)

open Wiki_widgets_interface
open Wiki_sql.Types

let (>>=) = Lwt.bind


class noneditable_wikibox : Wiki_widgets_interface.noneditable_wikibox =
object (self)

  inherit Widget.widget_with_error_box as error_box

  val ne_class = "noned_wikibox"

  method container ?css content =
    let css = match css with
      | None -> {{ [] }}
      | Some c -> c
    in
    {{
       <html xmlns="http://www.w3.org/1999/xhtml">[
         <head>[
           <title>"Ocsimore wiki"
             !css
         ]
         <body>content
       ]
     }}


  method display_error_box ?classe ?message ?exn () =
    match exn with
      | Some (Wiki.Unknown_box (w, i)) ->
          error_box#display_error_box
            ?classe
            ~message:("The box "^Int32.to_string i^
                        " does not exist in wiki "^ wiki_id_s w^".")
            ?exn
            ()
      | Some Wiki_services.Not_css_editor ->
          error_box#display_error_box
            ?classe
            ~message:("You are not allowed to modify the stylesheet for that page.")
            ?exn
            ()
      | _ -> error_box#display_error_box ?classe ?message ?exn ()


  method display_wikiboxcontent ~wiki ~bi (content_type, content, _ver) =
    match content_type with
      | Wiki_sql.Deleted -> Lwt.return {{ [<em>"//Deleted//"] }}
      | Wiki_sql.Css  -> Lwt.return {{ [<pre>{:Ocamlduce.Utf8.make content :}]}}
      | Wiki_sql.WikiCreole -> Wiki_syntax.xml_of_wiki wiki bi content

  method display_raw_wikiboxcontent (content_type, content, _ver) =
    match content_type with
      | Wiki_sql.Deleted -> Lwt.return {{ [<em>"//Deleted//"] }}
      | Wiki_sql.Css | Wiki_sql.WikiCreole ->
          Lwt.return {{ [<pre>{:Ocamlduce.Utf8.make content :}] }})
    >>= fun c ->
    Lwt.return (classe, c)
      | Wiki_sql.Css, None -> Lwt.return {{ [<em>"/* Deleted Css */"] }}
      | Wiki_sql.WikiCreole, None -> Lwt.return {{ [<em>"//Deleted//"] }}
      | Wiki_sql.Css, Some content  ->
          Lwt.return {{ [<pre>{:Ocamlduce.Utf8.make content :}]}}

  method display_basic_box ~classe content =
    let classe = Ocsimore_lib.build_class_attr (ne_class::classe) in
    Lwt.return {{ <div class={: classe :}>content }}

  method display_noneditable_wikibox ~bi ?(classe=[]) ~data () =
    Wiki.get_role ~sp:bi.bi_sp ~sd:bi.bi_sd data
    >>= function
      | Wiki.Admin
      | Wiki.Author
      | Wiki.Lurker ->
          self#bind_or_display_error
            (Wiki.retrieve_wikibox_aux data)
            (self#display_wikiboxcontent ~wiki:(fst data) ~bi ~classe)
            (self#display_basic_box)
      | Wiki.Nonauthorized ->
          Lwt.return
            (self#display_error_box
               ~classe:(ne_class::classe)
               ~message:"You are not allowed to see this content."
               ())
end



class editable_wikibox
  (service_edit_wikibox,
   service_edit_wikipage_css,
   service_edit_wiki_css,
   action_edit_wikibox,
   action_delete_wikibox,
   action_edit_wikibox_permissions,
   action_wikibox_history,
   action_old_wikibox,
   action_src_wikibox,
   action_send_wikibox,
   action_send_wikibox_permissions,
   action_send_wikipage_css,
   action_send_wiki_css,
   pagecss_service,
   action_create_page
  ) : Wiki_widgets_interface.editable_wikibox =
object (self)

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
    | Wiki_services.Operation_not_allowed -> "Operation not allowed"
    | Wiki_services.Action_failed _ -> "Action failed"

  method private box_menu ~sp ?(perm = false) ?cssmenu ?service ?(title = "") ((wiki, _) as ids) =
    let history = Eliom_services.preapply action_wikibox_history ids
    and edit = Eliom_services.preapply action_edit_wikibox ids
    and delete = Eliom_services.preapply action_delete_wikibox ids
    and edit_perm = Eliom_services.preapply action_edit_wikibox_permissions ids
    in
    let view = Eliom_services.void_coservice' in
    let edit_css =
      match cssmenu with
        | Some (Some page) ->
            let cssedit = Eliom_services.preapply
              service_edit_wikipage_css (wiki, page)
            in
            (Some (cssedit, (cssedit, {{ "edit page css" }})))
        | Some None ->
            let cssedit = Eliom_services.preapply service_edit_wiki_css wiki
            in
            (Some (cssedit, (cssedit, {{ "edit wiki css" }})))
        | None -> None
    in
    let service = match service with
      | None -> None
      | Some View -> Some view
      | Some Edit -> Some edit
      | Some Edit_perm -> Some edit_perm
      | Some Edit_css ->
          (match edit_css with
             | Some (edit_css, _) -> Some edit_css
             | None -> None
          )
      | Some History -> Some history
    in
    let l = [
      (delete, {{ "delete" }});
      (edit, {{ "edit" }});
      (view, {{ "view" }});
    ] in
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
    {{ [ {: Eliom_duce_tools.menu ~sp ~classe:[box_button_class]
            (history, {{ "history" }}) l ?service :}
         <p class={: box_title_class :}>title
       ]
     }}

  method display_menu_box ~classe ?service ?cssmenu ?title ~bi ids content =
    let sp = bi.bi_sp in
    let sd = bi.bi_sd in
    let classe = Ocsimore_lib.build_class_attr classe in
    Wiki.get_role ~sp ~sd ids
    >>= fun role ->
    let perm = role = Wiki.Admin in
    Lwt.return
      {{ <div class={: classe :}>[
         !{: self#box_menu ~sp ~perm ?cssmenu ?service ?title ids :}
         <div>content ]}}

  (* Wikibox in editing mode *)
  method display_wikitext_edit_form ~bi ?(rows=25) ?(cols=80) ~previewonly (wiki_id, message_id as wikibox) (content, boxversion) =
    let sp = bi.bi_sp in
    Wiki.modified_wikibox wikibox boxversion >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            {{ [ <em>['Warning: ']
                 'the content of this wikibox has been updated since you started \
                 editing it. The preview and the code below reflect your \
                 modifications, not the current saved version.'
                 <br>[] <br>[]
             ]
             }},
            {{ [ <br>[]
                 <b> [ <em> ['Warning: ']
                         'If you save your changes, you will overwrite the updated
                           version of the page currently in the wiki.'
                     ]
                 <br>[]
               ] }} )

       | None -> Lwt.return (boxversion, {{ [] }}, {{ [] }})
    ) >>= fun (curversion, warning1, warning2)  ->
    let draw_form (actionname, (((wikiidname, (boxidname, wikiboxversion)), contentname))) =
      let f =
        {{ [
             {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
                ~name:wikiidname ~value:wiki_id wiki_id_s :}
             {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                ~name:boxidname ~value:message_id () :}
             {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                ~name:wikiboxversion ~value:curversion () :}
             {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
                ~value:(Ocamlduce.Utf8.make content) () :}
             <br>[]
           ]
         }}
      in
      {{ [
           <p>[!warning1 !f !warning2
               !{: let prev = Eliom_duce.Xhtml.string_button
                   ~name:actionname ~value:"preview" {{ "Preview" }}
                 in
                 if previewonly then [prev]
                 else
                   [prev;
                    Eliom_duce.Xhtml.string_button ~name:actionname
                      ~value:"save" {{ "Save" }}
                   ] :}
              ]] }}
    in
    Lwt.return
      (Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
         ~service:action_send_wikibox ~sp draw_form ())

  (* Wikibox in editing mode, with an help box on the syntax of the wiki *)
      (* XXX!! content_type *)
  method display_full_edit_form ~bi ?rows ?cols ~previewonly (wiki_id, message_id) (content_type, content, boxversion) =
    Wiki_services.get_admin_wiki ()
    >>= fun { wiki_id = admin_wiki } ->
    Wiki_sql.get_box_for_page ~wiki:admin_wiki ~page:"wikisyntax-help"
    >>= fun { Wiki_sql.wikipage_dest_wiki = wiki;
              wikipage_wikibox = wiki_help_box } ->
    self#bind_or_display_error
      (Wiki.retrieve_wikibox_aux (wiki, wiki_help_box))
      (self#display_wikiboxcontent ~wiki:admin_wiki ~bi ~classe:["wikihelp"])
      (self#display_basic_box)
    >>= fun b ->
      let content = match content, content_type with
        | None, Wiki_sql.Css -> "/* Deleted CSS */"
        | None, Wiki_sql.WikiCreole -> "<<|  Deleted >>"
        | Some content, _ -> content
      in
    self#display_wikitext_edit_form ~bi ?rows ?cols ~previewonly
        (wiki_id, message_id) (content, boxversion)
   >>= fun f ->
   Lwt.return (classe, {{ [ b f ] }})


  (* Edition of the permissions of a wiki *)
  method display_edit_perm_form ~classe ~bi ((wiki_id, message_id) as ids) =
    let sp = bi.bi_sp in
    let aux u =
      Ocsimore_lib.lwt_bind_opt u
        (List.fold_left
           (fun s r ->
              s >>= fun s ->
              Users.get_user_name_by_id r
              >>= fun s2 ->
              Lwt.return (s^" "^s2))
           (Lwt.return ""))
    in
    Wiki.get_readers ids >>= fun readers ->
    Wiki.get_writers ids >>= fun writers ->
    Wiki.get_rights_adm ids >>= fun rights_adm ->
    Wiki.get_wikiboxes_creators ids >>= fun creators ->
    aux readers >>= fun r ->
    aux writers >>= fun w ->
    aux rights_adm >>= fun a ->
    aux creators >>= fun c ->
    let r = Ocsimore_lib.string_of_string_opt r in
    let w = Ocsimore_lib.string_of_string_opt w in
    let a = Ocsimore_lib.string_of_string_opt a in
    let c = Ocsimore_lib.string_of_string_opt c in

    let string_input arg =
      Eliom_duce.Xhtml.string_input ~input_type:{: "text" :} ~name:arg () in
    let draw_form ((wid, bid), (addrn, (addwn, (addan, (addc, (delrn, (delwn, (delan, delc)))))))) =
      {{ [<p>[
            {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
               ~name:wid ~value:wiki_id wiki_id_s :}
            {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
               ~name:bid ~value:message_id () :}
            'Users who can read this wiki box: ' !{: r :}  <br>[]
            'Add readers: '    {: string_input addrn :} <br>[]
            'Remove readers: ' {: string_input delrn :} <br>[]
            'Users who can modify this wiki box: ' !{: w :} <br>[]
            'Add writers: '    {: string_input addwn :} <br>[]
            'Remove writers: ' {: string_input delwn :} <br>[]
            'Users who can change rights of this wiki box: ' !{: a :} <br>[]
            'Add: '            {: string_input addan :} <br>[]
            'Remove: '         {: string_input delan :} <br>[]
            'Users who can create wikiboxes inside this wiki box: ' !{: c:} <br>[]
            'Add: '            {: string_input addc :}  <br>[]
            'Remove: '         {: string_input delc :}  <br>[]
            {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :} {{ "Save" }} :}
            ]
          ]
        }}
    in
    Lwt.return
      (classe,
       {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikibox_permissions
              ~sp draw_form () :}] }})

  (* Auxiliary method to factorize some code *)
  method private display_menu_box_aux ?title ?service cl (wid, bid) ~bi ~classe ?cssmenu content =
    self#display_menu_box ~classe:(cl::classe) ?service ?title
      ~bi ?cssmenu (wid, bid) content

  method private display_edit_box (w, b as wb) =
    let title = Printf.sprintf "Edit - Wiki %s, box %ld" (wiki_id_s w) b in
    self#display_menu_box_aux ~title ~service:Edit editform_class wb

  method private display_edit_perm (w, b as wb) =
    let title = Printf.sprintf "Permissions - Wiki %s, box %ld" (wiki_id_s w) b in
    self#display_menu_box_aux ~title ~service:Edit_perm editform_class wb

  method private display_history_box (w, b as wb) =
    let title = Printf.sprintf "History - Wiki %s, box %ld" (wiki_id_s w) b in
    self#display_menu_box_aux ~title ~service:History history_class wb

  method private display_editable_box =
    self#display_menu_box_aux ~service:View editable_class

  method private display_old_wikibox (w, b as wb) version =
    let title = Printf.sprintf "Old version - Wiki %s, box %ld, version %ld"
      (wiki_id_s w) b version
    in
    self#display_menu_box_aux ~title oldwikibox_class wb

  method private display_src_wikibox (w, b as wb) version =
    let title = Printf.sprintf "Source - Wiki %s, box %ld, version %ld"
      (wiki_id_s w) b version
    in
    self#display_menu_box_aux ~title srcwikibox_class wb

  method private display_edit_css_box ((w, _) as wb) page =
    let title = Printf.sprintf "CSS for wiki %s, %s"
      (wiki_id_s w) (if page = "" then "main page" else "page " ^ page)
    in
    self#display_menu_box_aux ~title ~service:Edit_css css_class wb

  method private display_edit_wikicss_box ((w, _) as wb) =
    let title = Printf.sprintf "CSS for wiki %s (global stylesheet)"
      (wiki_id_s w)
    in
    self#display_menu_box_aux ~title ~service:Edit_css css_class wb

  method display_history ~classe ~bi ids l =
    let sp = bi.bi_sp in
    Lwt_util.map
      (fun (version, _comment, author, date) ->
         Users.get_user_fullname_by_id author
         >>= fun author ->
           Lwt.return
             {{ [ !{: Int32.to_string version :}'. '
                  !{: CalendarLib.Printer.Calendar.to_string date :}' '
                  <em>[ 'by ' !{: author :} ]' '
                  {:  Eliom_duce.Xhtml.a ~sp
                     ~service:action_old_wikibox
                     {{ "view" }} (ids, version) :}
                  ' ''('
                  {: Eliom_duce.Xhtml.a ~sp
                     ~service:action_src_wikibox
                     {{ "source" }} (ids, version) :}
                  ')'
                  <br>[]
                ]
              }})
      l
    >>= fun l ->
    Lwt.return (classe, {{ map {: l :} with i -> i }})

  (* Display a wikibox, plus some potentials actions on the wikibox, which
     are extracted from sp. This function returns the wikibox, as well
     as a boolean indicating whether the page is correctly displayed, or
     whether the user has not even rights to see the box (in which case
     a 403 is returned in Wiki.ml)
  *)
  method editable_wikibox_aux ~bi ~data ?rows ?cols ?(classe=[]) ?cssmenu () =
    let sp = bi.bi_sp in
    let sd = bi.bi_sd in
    let rec find_action = function
      | [] -> None
      | (Wiki_services.Wiki_action_info e)::_ -> Some e
      | _ :: l -> find_action l
    in
    let (wiki_id, _) = data in
    let action = find_action (Eliom_sessions.get_exn sp) in
    Wiki.get_role ~sp ~sd data
    >>= function
      | Wiki.Admin
      | Wiki.Author ->
          (match action with
             | Some (Wiki_services.Edit_box i) when i = data ->
                 self#bind_or_display_error
                   (Wiki.retrieve_wikibox_aux data)
                   (self#display_full_edit_form ~bi ?cols ?rows
                      ~previewonly:true data)
                   (self#display_edit_box ~bi ?cssmenu data)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.Edit_perm i) when i = data ->
                 self#bind_or_display_error
                   (Lwt.return data)
                   (self#display_edit_perm_form ~bi ~classe)
                   (self#display_edit_perm ~bi ?cssmenu data)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.PreviewWikitext (i, (content, version))) when i = data ->
                 self#bind_or_display_error
                   (Lwt.return (Wiki_sql.WikiCreole, Some content, version))
                   (fun cv ->
                      self#display_wikiboxcontent ~classe:[] ~wiki:wiki_id
                        ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi) cv
                      >>= fun (classe, pp) ->
                      self#display_basic_box ~classe:(preview_class::classe) pp
                      >>= fun preview ->
                      self#display_full_edit_form
                        ~bi ?cols ?rows ~previewonly:false data cv
                      >>= fun (_, form) ->
                      Lwt.return
                        (classe,
                         {{ [ <p class={: box_title_class :}>"Preview"
                                preview
                                !form ] }})
                   )
                   (self#display_edit_box ~bi ?cssmenu data)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.History i) when i = data ->
                 self#bind_or_display_error
                   (Wiki_sql.get_history data)
                   (self#display_history ~classe ~bi data)
                   (self#display_history_box ~bi ?cssmenu data)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.Oldversion (i, version)) when i = data ->
                 self#bind_or_display_error
                   (Wiki.retrieve_wikibox_aux ~version data)
                   (self#display_wikiboxcontent
                      ~classe
                      ~wiki:wiki_id
                      ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi)
                   )
                   (self#display_old_wikibox ~bi ?cssmenu data version)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.Src (i, version)) when i = data ->
                 self#bind_or_display_error
                   (Wiki.retrieve_wikibox_aux ~version data)
                   (self#display_raw_wikiboxcontent ~classe)
                   (self#display_src_wikibox ~bi ?cssmenu data version)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.Error (i, error)) when i = data ->
                 self#bind_or_display_error
                   ~error:(self#create_error_message error)
                   (Wiki.retrieve_wikibox_aux data)
                   (self#display_wikiboxcontent ~classe ~wiki:wiki_id
                      ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi)
                   )
                   (self#display_editable_box ~bi ?cssmenu data)
                 >>= fun r ->
                 Lwt.return (r, true)

             | Some (Wiki_services.Delete_Box i) when i = data -> assert false

             | _ ->
                   (* No action, or the action does not concern this page.
                      We simply display the wikipage itself *)
                   self#bind_or_display_error
                     (Wiki.retrieve_wikibox_aux data)
                     (self#display_wikiboxcontent ~classe ~wiki:wiki_id
                        ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi)
                     )
                     (self#display_editable_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)
            )

        | Wiki.Lurker ->
            (match action with
               | Some (Wiki_services.Edit_box i)
               | Some (Wiki_services.History i)
               | Some (Wiki_services.Oldversion (i, _))
               | Some (Wiki_services.Error (i, _)) when i = data ->
                   self#bind_or_display_error
                     ~error:(self#create_error_message
                               Wiki_services.Operation_not_allowed)
                     (Wiki.retrieve_wikibox_aux data)
                     (self#display_wikiboxcontent ~classe ~wiki:wiki_id
                        ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi)
                     )
                     (self#display_basic_box)
                   (* Returning true would also be meaningful: the action
                      is forbidden but the wikibox itself is shown *)
                   >>= fun r ->
                   Lwt.return (r, false)

               | _ ->
                   (* As for Author/default *)
                   self#bind_or_display_error
                     (Wiki.retrieve_wikibox_aux data)
                     (self#display_wikiboxcontent ~classe ~wiki:wiki_id
                        ~bi:(Wiki_widgets_interface.add_ancestor_bi data bi)
                     )
                     (self#display_basic_box)
                   >>= fun r ->
                   Lwt.return (r, true)
            )

        | Wiki.Nonauthorized ->
            Lwt.return
              (self#display_error_box
                 ~classe:(ne_class::classe)
                 ~message:"You are not allowed to see this content."
                 (),
               false)

   method editable_wikibox ~bi ~data ?rows ?cols ?(classe=[]) ?cssmenu () =
     self#editable_wikibox_aux ~bi ~data ?rows ?cols ~classe ?cssmenu ()
     >>= fun (r, _allowed) -> Lwt.return r


   method display_edit_css_form ~classe ~bi ?(rows=25) ?(cols=80) ~data:(wiki_id, page) content =
     let sp = bi.bi_sp in
     let draw_form ((wikiidname, pagename), contentname) =
       {{ [<p>[
            {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
               ~name:wikiidname ~value:wiki_id wiki_id_s :}
            {: Eliom_duce.Xhtml.string_input ~input_type:{: "hidden" :}
               ~name:pagename ~value:page () :}
            {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
               ~value:(Ocamlduce.Utf8.make content) () :}
            <br>[]
            {: Eliom_duce.Xhtml.button ~button_type:{{ "submit" }} {{ "Save" }} :}
            ]] }}
     in
     Lwt.return
       (classe,
        {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
               ~service:action_send_wikipage_css
               ~sp draw_form () :}] }})

   method edit_css_box ~bi ~data ?rows ?cols ?(classe=[]) ()  =
     let sp = bi.bi_sp in
     let sd = bi.bi_sd in
     let (wiki, page) = data in
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors () >>= fun c ->
     Wiki_sql.get_box_for_page wiki page
     >>= fun { Wiki_sql.wikipage_dest_wiki = wiki'; wikipage_wikibox = box} ->
     self#bind_or_display_error
       (if c then
          Wiki_sql.get_css_for_wikipage wiki page >>= function
            | Some css -> Lwt.return css
            | None -> Lwt.return ""
        else Lwt.fail Wiki_services.Not_css_editor)
       (self#display_edit_css_form ~classe ~bi ?rows ?cols ~data)
       (self#display_edit_css_box ~bi ~cssmenu:(Some page) (wiki', box) page)

   method display_edit_wikicss_form ~classe ~bi ?(rows=25) ?(cols=80) ~wiki (content : string) =
     let sp = bi.bi_sp in
     let draw_form (wikiidname, contentname) =
       {{ [<p>[
            {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
               ~name:wikiidname ~value:wiki wiki_id_s :}
            {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
               ~value:(Ocamlduce.Utf8.make content) () :}
            <br>[]
            {: Eliom_duce.Xhtml.button ~button_type:{{ "submit" }} {{ "Save" }} :}
            ]] }}
     in
     Lwt.return
       (classe,
        {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
               ~service:action_send_wiki_css ~sp draw_form () :}] }})

   method edit_wikicss_box ~bi ~wiki ?rows ?cols ?(classe=[]) () =
     let sp = bi.bi_sp in
     let sd = bi.bi_sd in
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors ()
     >>= fun c ->
     self#bind_or_display_error
       (if c then
          Wiki_sql.get_css_for_wiki wiki >>= function
            | None -> Lwt.return ""
            | Some css -> Lwt.return css
        else Lwt.fail Wiki_services.Not_css_editor)
       (self#display_edit_wikicss_form ~classe ~bi ?rows ?cols ~wiki)
       (self#display_edit_wikicss_box ~bi ?cssmenu:(Some None)
          (wiki, wiki_info.wiki_container))

   method get_css_header ~bi ~wiki ?(admin=false) ?page () =
     let sp = bi.bi_sp in
     let css_url_service service args = Eliom_duce.Xhtml.css_link
       (Eliom_duce.Xhtml.make_uri service sp args) () in
     let css_url path = css_url_service (Eliom_services.static_dir sp) path in

     if admin then
       Lwt.return
         {{ [ {: css_url [Ocsimore_lib.ocsimore_admin_dir; "ocsiwikistyle.css"] :}
              {: css_url [Ocsimore_lib.ocsimore_admin_dir; "ocsiwikiadmin.css"] :} ] }}
     else
       let css = css_url [Ocsimore_lib.ocsimore_admin_dir; "ocsiwikistyle.css"] in
       (match Wiki_services.find_servwikicss wiki with
          | None -> Lwt.return {{ [ css ] }}
          | Some wikicss_service ->
              Wiki_sql.get_css_for_wiki wiki
              >>= function
                | Some _ -> Lwt.return (* encoding? *)
                    {{ [ css  {: css_url_service wikicss_service ():} ]}}
                | None -> Lwt.return {{ [ css ] }}
       )
       >>= fun css ->
       match page with
         | None -> Lwt.return css
         | Some page ->
             Wiki_sql.get_css_for_wikipage ~wiki ~page >>= function
               | None -> Lwt.return css
               | Some _ -> Lwt.return
                   {{ [ !css
                        {: css_url_service pagecss_service (wiki, page)
                           (* encoding? *) :}
                      ]}}


(* Displaying of an entire page. We essentially render the page,
   and then include it inside its container *)
   method display_page ~bi ~wiki ~page =
     let sp = bi.bi_sp
     and sd = bi.bi_sd in
     Wiki_sql.get_wiki_info_by_id wiki
     >>= fun wiki_info ->
     Lwt.catch
       (fun () ->
          (* We render the wikibox for the page *)
          Wiki_sql.get_box_for_page wiki page
          >>= fun { Wiki_sql.wikipage_dest_wiki = wiki';
                    wikipage_wikibox = box; wikipage_title = title } ->
          let bi = {
            Wiki_widgets_interface.bi_sp = sp;
            bi_sd = sd;
            bi_ancestors = Wiki_widgets_interface.no_ancestors;
            bi_subbox = None;
          } in
          self#editable_wikibox_aux ~bi ~data:(wiki', box) ~cssmenu:(Some page) ()
          >>= fun (subbox, allowed) ->
          Lwt.return ({{ [ subbox ] }},
                      (if allowed then Wiki_widgets_interface.Page_displayable
                       else            Wiki_widgets_interface.Page_403),
                      title)
       )
       (function
          | Not_found ->
              (* No page. We create a default page, which will be
                 inserted into the container *)
              Users.get_user_id ~sp ~sd
              >>= fun userid ->
              let draw_form (wikiidname, pagename) =
                {{ [<p>[
                       {: Eliom_duce.Xhtml.user_type_input
                          ~input_type:{: "hidden" :}
                          ~name:wikiidname ~value:wiki wiki_id_s :}
                       {: Eliom_duce.Xhtml.string_input ~name:pagename
                          ~input_type:{: "hidden" :} ~value:page () :}
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "submit" :} ~value:"Create it!" () :}
                     ]] }}
              in
              Wiki.page_creators_group wiki
              >>= fun creators ->
              Users.in_group ~sp ~sd ~user:userid ~group:creators ()
              >>= fun c ->
              let form =
                if c then
                  {{ [ {: Eliom_duce.Xhtml.post_form ~service:action_create_page
                          ~sp draw_form () :} ] }}
                else {{ [] }}
              and err_msg = !Language.messages.Language.page_does_not_exist
              in
              Lwt.return
                ({{ [ <p>{:err_msg:} !form ] }},
                 Wiki_widgets_interface.Page_404,
                 None)
          | e -> Lwt.fail e
       )
       >>= fun (subbox, err_code, title) ->
       Wiki_widgets_interface.set_page_displayable sd err_code;

       (* We render the container *)
       let bi = {
         Wiki_widgets_interface.bi_sp = sp;
         bi_sd = sd;
         bi_ancestors = Wiki_widgets_interface.no_ancestors;
         bi_subbox = Some subbox;
       } in
       self#editable_wikibox ~bi ~data:(wiki, wiki_info.wiki_container)
         ~cssmenu:None ()

       >>= fun pagecontent ->
       self#get_css_header ~bi ~wiki ~admin:false ~page ()

       >>= fun css ->
       let title = Ocamlduce.Utf8.make
         (match title with
            | Some title -> title
            | None -> wiki_info.wiki_descr)
       and code = match err_code with
         | Wiki_widgets_interface.Page_displayable -> 200
         | Wiki_widgets_interface.Page_404 -> 404
         | Wiki_widgets_interface.Page_403 -> 403
       in
       Lwt.return
         ({{ <html xmlns="http://www.w3.org/1999/xhtml">[
               <head>[
                 <title>title
                 !css
               ]
               <body>[ pagecontent ]
             ] }},
          code)


(* Displaying of an entire page. We essentially render the page,
   and then include it inside its container *)
   method send_page ~bi ~wiki ~page =
     let sp = bi.bi_sp
     and sd = bi.bi_sd in
     Wiki_sql.get_wiki_info_by_id wiki
     >>= fun wiki_info ->
     (* if there is a static page, we serve it: *)
     Lwt.catch
       (fun () ->
          match wiki_info.wiki_staticdir with
            | Some d ->
                Ocsigen_messages.debug
                  (fun () -> Printf.sprintf "Trying static file '%s' at path '%s'"
                     page d);
                Wiki_services.send_static_file sp sd wiki_info d page
            | None -> Lwt.fail Eliom_common.Eliom_404)
       (function
          | Eliom_common.Eliom_404 ->
              self#display_page ~bi ~wiki ~page
              >>= fun (html, code) ->
              Eliom_duce.Xhtml.send ~sp ~code html
          | e -> Lwt.fail e)


initializer

(* BY: Helper functions, which factorizes a bot of code. Some of them
   (eg. extract_wiki_id) are very mysterious:
   - I believe there is always a field "wiki" present, so the
   exception handler is useless
   - Why do we need to extract this value since we have a default ?
*)
let extract_wiki_id args default =
  try s_wiki_id (List.assoc "wiki" args)
  with Failure _ | Not_found -> default
and extract_https args =
  try match List.assoc "protocol" args with
    | "http" -> Some false
    | "https" -> Some true
    | _ -> None
  with Not_found -> None
in

Wiki_syntax.add_block_extension "wikibox"
  (fun wiki_id bi args c ->
     try
       let wiki = extract_wiki_id args wiki_id in
       try
         let box = Int32.of_string (List.assoc "box" args) in
         if Wiki_widgets_interface.in_ancestors (wiki, box) bi.bi_ancestors then
           Lwt.return {{ [ {: self#display_error_box
                              ~message:"Wiki error: loop of wikiboxes" () :} ] }}
         else
           (match c with
              | None -> Lwt.return None
              | Some c ->
                  Wiki_syntax.xml_of_wiki wiki_id bi c
                  >>= fun r -> Lwt.return (Some r)
           ) >>=fun subbox ->
           self#editable_wikibox
             ?rows:(Ocsimore_lib.int_of_string_opt
                      (Ocsimore_lib.list_assoc_opt "rows" args))
             ?cols:(Ocsimore_lib.int_of_string_opt
                      (Ocsimore_lib.list_assoc_opt "cols" args))
             ?classe:(try Some [List.assoc "class" args]
                      with Not_found -> None)
             ~data:(wiki, box)
             ~bi:{bi with
                    bi_ancestors = Wiki_widgets_interface.add_ancestor
                        (wiki, box) bi.bi_ancestors;
                    bi_subbox = subbox}
             ()
           >>= fun b ->
           Lwt.return {{ [ b ] }}
       with Not_found ->
         Lwt.return {{ [ <code>"<<wikibox>>" ] }}
     with
       | Failure _ ->
           Lwt.return {{ [ {: self#display_error_box
               ~message:"Wiki error: error in wikibox extension" () :} ] }}
  );

Wiki_filter.add_preparser_extension "wikibox"
  (fun wid (sp, sd, father) args c ->
     (try
        let wid = extract_wiki_id args wid in
        try
          ignore (List.assoc "box" args);
          Lwt.return None
        with Not_found ->
          Wiki_sql.get_wiki_info_by_id wid
          >>= fun wiki ->
          Users.get_user_id ~sp ~sd
          >>= fun userid ->
          let ids = (wid, father) in
          Wiki.can_create_wikibox ~sp ~sd wiki father userid >>= function
            | true ->
                Wiki.get_readers ids >>= fun readers ->
                Wiki.get_writers ids >>= fun writers ->
                Wiki.get_rights_adm ids >>= fun rights_adm ->
                Wiki.get_wikiboxes_creators ids >>= fun wikiboxes_creators ->
                Wiki.new_wikibox
                  ~wiki:wid
                  ~author:userid
                  ~comment:(Printf.sprintf "Subbox of wikibox %s, wiki %ld"
                              (wiki_id_s wid) father)
                  ~content:"**//new wikibox//**"
                  ~content_type:Wiki_sql.WikiCreole
                  ?readers
                  ?writers
                  ?rights_adm
                  ?wikiboxes_creators
                  ()
                  >>= fun box ->
                  Lwt.return
                    (Some (Wiki_syntax.string_of_extension "wikibox"
                             (("box", Int32.to_string box)::args) c))
            | false -> Lwt.return None
   with Failure _ -> Lwt.return None)
  );

Wiki_syntax.add_link_extension "link"
  (fun wiki_id bi args c ->
     let sp = bi.bi_sp in
     let href = Ocsimore_lib.list_assoc_default "page" args "" in
     let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
     let https = extract_https args in
     let wiki_id = extract_wiki_id args wiki_id in
     let content =
       match c with
         | Some c -> Wiki_syntax.a_content_of_wiki wiki_id bi c
         | None -> Lwt.return (Ocamlduce.Utf8.make href)
     in
     (* class and id attributes will be taken by Wiki_syntax.a_elem *)
     ((if Wiki_syntax.is_absolute_link href then
         href
       else
         match Wiki_services.find_servpage wiki_id with
           | Some s ->
               let href = Ocsigen_lib.remove_slash_at_beginning
                 (Neturl.split_path href)
               in Eliom_duce.Xhtml.make_uri ?https ?fragment ~service:s ~sp href
           | None -> href
      ),
      args,
      content)
  );

Wiki_syntax.add_link_extension "nonattachedlink"
  (fun wiki_id bi args c ->
     let sp = bi.bi_sp in
     let href = Ocsimore_lib.list_assoc_default "page" args "" in
     let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
     let https = extract_https args in
     let wiki_id = extract_wiki_id args wiki_id in
     let content =
       match c with
         | Some c -> Wiki_syntax.a_content_of_wiki wiki_id bi c
         | None -> Lwt.return (Ocamlduce.Utf8.make href)
     in
     (Eliom_duce.Xhtml.make_uri ?https ?fragment
        ~service:(Wiki_services.find_naservpage wiki_id) ~sp href,
      args,
      content)
  );

Wiki_syntax.add_link_extension "cancellink"
  (fun wiki_id bi args c ->
     let content =
       match c with
         | Some c -> Wiki_syntax.a_content_of_wiki wiki_id bi c
         | None -> Lwt.return (Ocamlduce.Utf8.make "Cancel")
     in
     (Eliom_duce.Xhtml.make_uri ~service:Eliom_services.void_coservice'
        ~sp:bi.bi_sp (),
      args,
      content)
  );


Wiki_syntax.add_a_content_extension "object"
  (fun wiki_id bi args _c ->
     let type_ = Ocsimore_lib.list_assoc_default "type" args "" in
     let href = Ocsimore_lib.list_assoc_default "data" args "" in
     let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
     let wiki_id = extract_wiki_id args wiki_id in
     let https = extract_https args in
     let atts = Wiki_syntax.parse_common_attribs args in
     let url =
       if Wiki_syntax.is_absolute_link href then
         href
       else
         match Wiki_services.find_servpage wiki_id with
           | Some s ->
               let href = Ocsigen_lib.remove_slash_at_beginning
                 (Neturl.split_path href)
               in
               Eliom_duce.Xhtml.make_uri ?https ?fragment ~service:s
                 ~sp:bi.bi_sp href
           | None -> href
     in
     Lwt.return
       {{ [<object
              ({data = {: Ocamlduce.Utf8.make url :}
                type = {: Ocamlduce.Utf8.make type_ :}}
               ++ atts)>[] ] }});

Wiki_syntax.add_a_content_extension "img"
  (fun wiki_id bi args c ->
     let href = Ocsimore_lib.list_assoc_default "name" args "" in
     let https = extract_https args in
     let wiki_id = extract_wiki_id args wiki_id in
     let alt = match c with Some c -> c | None -> href in
     let atts = Wiki_syntax.parse_common_attribs args in
     let url =
       if Wiki_syntax.is_absolute_link href then
         href
       else
         match Wiki_services.find_servpage wiki_id with
           | Some s ->
               let href =
                 Ocsigen_lib.remove_slash_at_beginning
                    (Neturl.split_path href)
               in
               Eliom_duce.Xhtml.make_uri ?https ~service:s
                 ~sp:bi.bi_sp href
           | _ -> href
     in
     Lwt.return
       {{ [<img ({ src={: Ocamlduce.Utf8.make url :}
                   alt={: Ocamlduce.Utf8.make alt :}}
                   ++ atts )>[] ] }});
end
