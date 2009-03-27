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


class virtual noneditable_wikibox =
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
                        " does not exist in wiki "^ Wiki_sql.wiki_id_s w^".")
            ?exn
            ()
      | Some Wiki.Not_css_editor ->
          error_box#display_error_box
            ?classe
            ~message:("You are not allowed to modify the stylesheet for that page.")
            ?exn
            ()
      | _ -> error_box#display_error_box ?classe ?message ?exn ()

  method virtual pretty_print_wikisyntax :
    wiki:Wiki_sql.wiki ->
    bi:Wiki_syntax.box_info ->
    string -> Xhtmltypes_duce.flows Lwt.t


  method display_basic_box ~classe content =
    let classe = Ocsimore_lib.build_class_attr (ne_class::classe) in
    Lwt.return
      {{ <div class={: classe :}>content }}

  method display_noneditable_wikibox ~bi ?(classe=[]) ~data () =
    Wiki.get_role ~sp:bi.Wiki_syntax.bi_sp ~sd:bi.Wiki_syntax.bi_sd data
    >>= function
      | Wiki.Admin
      | Wiki.Author
      | Wiki.Lurker ->
          self#bind_or_display_error
            ~classe
            (Wiki.retrieve_wikibox_current_wikitext data)
            (self#pretty_print_wikisyntax ~wiki:(fst data) ~bi)
            (self#display_basic_box)
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


let eliom_wiki_args = Wiki_sql.eliom_wiki "wikiid"

let eliom_wikibox_args = eliom_wiki_args ** (Eliom_parameters.int32 "boxid")

let eliom_wikipage_args =
  (Wiki_sql.eliom_wiki "wikiid") ** (Eliom_parameters.string "page")


class virtual editable_wikibox ?sp () =
  (* The registration of the services below must be done during site loading,
     not before! *)

  let service_edit_wikibox = Eliom_services.new_service ?sp
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "wiki_edit"]
    ~get_params:eliom_wikibox_args ()
  in

  let service_edit_wikipage_css = Eliom_services.new_coservice'
    ~name:"css_edit" ~get_params:eliom_wikipage_args ()
  in

  let service_edit_wiki_css = Eliom_services.new_coservice'
    ~name:"wiki_css_edit" ~get_params:eliom_wiki_args ()
  in

  let action_edit_wikibox = Eliom_predefmod.Actions.register_new_coservice'
    ~name:"wiki_edit" ~get_params:eliom_wikibox_args
      (fun _sp wbox () ->
         Lwt.return [Wiki.Wiki_action_info (Wiki.Edit_box wbox)])
  in

  let action_delete_wikibox = Eliom_predefmod.Any.register_new_coservice'
    ~name:"wiki_delete" ~get_params:eliom_wikibox_args
    (fun sp wikibox () ->
       let sd = Ocsimore_common.get_sd sp in
       Wiki.save_wikibox ~enough_rights:Wiki.user_can_save_wikibox
         ~sp ~sd ~wikibox ~content:"" ~content_type:Wiki_sql.Deleted
    )
  in

  let action_edit_wikibox_permissions =
    Eliom_predefmod.Actions.register_new_coservice'
      ~name:"wiki_edit_perm" ~get_params:eliom_wikibox_args
      (fun sp wbox () ->
         let sd = Ocsimore_common.get_sd sp in
         Wiki.get_role sp sd wbox
         >>= fun role ->
           if role = Wiki.Admin then
             Lwt.return [Ocsimore_common.Session_data sd;
                         Wiki.Wiki_action_info (Wiki.Edit_perm wbox)]
           else Lwt.return [Ocsimore_common.Session_data sd])
  in

  let action_wikibox_history = Eliom_predefmod.Actions.register_new_coservice'
    ~name:"wiki_history" ~get_params:eliom_wikibox_args
    (fun _sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.History g)])
  in

  let action_old_wikibox = Eliom_predefmod.Actions.register_new_coservice'
    ~name:"wiki_old_version"
    ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
    (fun _sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.Oldversion g)])
  in

  let action_src_wikibox = Eliom_predefmod.Actions.register_new_coservice'
      ~name:"wiki_src"
      ~get_params:(eliom_wikibox_args ** (Eliom_parameters.int32 "version"))
      (fun _sp g () -> Lwt.return [Wiki.Wiki_action_info (Wiki.Src g)])
  in

  let action_send_wikibox = Eliom_predefmod.Any.register_new_post_coservice'
    ?sp ~keep_get_na_params:false ~name:"wiki_send"
    ~post_params:
    (Eliom_parameters.string "actionname" **
       (((Wiki_sql.eliom_wiki "wikiid" **
            (Eliom_parameters.int32 "boxid" **
               Eliom_parameters.int32 "boxversion")
         ) **
           Eliom_parameters.string "content")))
    (fun sp () (actionname, (((wiki_id, (box_id, boxversion)), content))) ->
       let wikibox = (wiki_id, box_id) in
       (* We always show a preview before saving. Moreover, we check that the
          wikibox that the wikibox has not been modified in parallel of our
          modifications. If this is the case, we also show a warning *)
       Wiki.modified_wikibox wikibox boxversion
       >>= fun modified ->
       if actionname = "save" then
         match modified with
           | None ->
               let sd = Ocsimore_common.get_sd sp in
               Wiki_filter.preparse_extension (sp, sd, box_id) wiki_id content
               >>= fun content ->
               Wiki.save_wikibox ~enough_rights:Wiki.user_can_save_wikibox
                 ~sp ~sd ~wikibox ~content ~content_type:Wiki_sql.Wiki
           | Some _ ->
               Eliom_predefmod.Action.send ~sp
                 [Wiki.Wiki_action_info
                    (Wiki.Preview (wikibox, (content, boxversion)))]
       else
         Eliom_predefmod.Action.send ~sp
           [Wiki.Wiki_action_info
              (Wiki.Preview (wikibox, (content, boxversion)))]
      )
  in

  let action_send_wikibox_permissions =
    Eliom_predefmod.Any.register_new_post_coservice' ?sp
      ~keep_get_na_params:true
      ~name:"wiki_send_permissions"
      ~post_params:
      (eliom_wikibox_args **
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
         Wiki.save_wikibox_permissions sp sd p >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp
         Eliom_services.void_hidden_coservice'
      )
  in

  let action_send_wikipage_css =
    Eliom_predefmod.Redirection.register_new_post_coservice' ?sp
      ~keep_get_na_params:false ~name:"css_send"
      ~post_params:(eliom_wikipage_args ** Eliom_parameters.string "content")
      (fun sp () ((wiki, page), content) ->
         let sd = Ocsimore_common.get_sd sp in
         Users.get_user_data sp sd
         >>= fun user ->
         Wiki_sql.set_css_for_page ~wiki ~page content ~author:user.Users.id
         >>= fun () ->
         Lwt.return Eliom_services.void_coservice'
      )
  in

  let action_send_wiki_css =
    Eliom_predefmod.Redirection.register_new_post_coservice' ?sp
      ~keep_get_na_params:false
      ~name:"wiki_css_send"
      ~post_params:
      (Wiki_sql.eliom_wiki "wikiid" ** Eliom_parameters.string "content")
      (fun _sp () (wiki, content) ->
         Wiki_sql.set_css_for_wiki ~wiki content >>= fun () ->
         Lwt.return Eliom_services.void_coservice'
      )
  in

  (* The functions below register the services for the css of wikis
     and wikipages.  The css at the level of wikis are registered in
     Wiki.ml *)

  (* do not use this service, but the one below for css <link>s inside page *)
  let _ = Eliom_predefmod.CssText.register_new_service ?sp
    ~path:[Ocsimore_lib.ocsimore_admin_dir; "pagecss"]
    ~get_params:(Eliom_parameters.suffix eliom_wikipage_args)
    (fun _sp -> Wiki.wikipagecss_service_handler)
  in

  (* This is a non attached coservice, so that the css is in the same
     directory as the page. Important for relative links inside the css. *)
  let pagecss_service = Eliom_predefmod.CssText.register_new_coservice' ?sp
    ~name:"pagecss" ~get_params:eliom_wikipage_args
    (fun _sp -> Wiki.wikipagecss_service_handler)
  in

  let  _ = Eliom_predefmod.CssText.register_new_service ?sp
      ~path:[Ocsimore_lib.ocsimore_admin_dir; "wikicss"]
      ~get_params:(Wiki_sql.eliom_wiki "wiki")
      (fun _sp -> Wiki.wikicss_service_handler)
  in


fun (wikiadmin_container_id, wiki_help_box) ->
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
    | Wiki.Operation_not_allowed -> "Operation not allowed"
    | Wiki.Action_failed _ -> "Action failed"

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
            let cssedit = Eliom_services.preapply
              service_edit_wiki_css wiki
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
     let sp = bi.Wiki_syntax.bi_sp in
     let sd = bi.Wiki_syntax.bi_sd in
     let classe = Ocsimore_lib.build_class_attr classe in
     Wiki.get_role ~sp ~sd ids >>= fun role ->
     let perm = role = Wiki.Admin in
     Lwt.return
       {{ <div class={: classe :}>[
            !{: self#box_menu ~sp ~perm ?cssmenu ?service ?title ids :}
            <div>content ]}}

   (* Wikibox in editing mode *)
   method display_edit_form ~bi ?(rows=25) ?(cols=80) ~previewonly (wiki_id, message_id as wikibox) (content, boxversion) =
     let sp = bi.Wiki_syntax.bi_sp in
     Wiki.modified_wikibox wikibox boxversion >>= (function
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
                 ~name:wikiidname ~value:wiki_id Wiki_sql.wiki_id_s :}
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
   method display_full_edit_form ~bi ?rows ?cols ~previewonly (wiki_id, message_id) (content, boxversion) =
     Wiki.get_admin_wiki () >>= fun admin_wiki ->
     self#bind_or_display_error
       ~classe:["wikihelp"]
       (Wiki.retrieve_wikibox_current_wikitext (admin_wiki, wiki_help_box))
       (self#pretty_print_wikisyntax ~wiki:admin_wiki ~bi)
       (self#display_basic_box)
     >>= fun b ->
       self#display_edit_form ~bi ?rows ?cols ~previewonly
         (wiki_id, message_id) (content, boxversion)
     >>= fun f ->
     Lwt.return {{ [ b f ] }}


   (* Edition of the permissions of a wiki *)
   method display_edit_perm_form ~bi ((wiki_id, message_id) as ids) =
     let sp = bi.Wiki_syntax.bi_sp in
     let aux u =
       Ocsimore_lib.lwt_bind_opt
         u
         (List.fold_left
            (fun s r ->
               s >>= fun s ->
               Users.get_user_name_by_id r >>= fun s2 ->
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
               ~name:wid ~value:wiki_id Wiki_sql.wiki_id_s :}
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
       {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikibox_permissions ~sp draw_form () :}] }}

   (* Auxiliary method to factorize some code *)
   method private display_menu_box_aux ?title ?service cl (wid, bid) ~bi ~classe ?cssmenu content =
     self#display_menu_box ~classe:(cl::classe) ?service ?title
       ~bi ?cssmenu (wid, bid) content

   method private display_edit_box (w, b as wb) =
     let title = Printf.sprintf "Edit - Wiki %s, box %ld"
       (Wiki_sql.wiki_id_s w) b
     in
     self#display_menu_box_aux ~title ~service:Edit editform_class wb

   method private display_edit_perm (w, b as wb) =
     let title = Printf.sprintf "Permissions - Wiki %s, box %ld"
       (Wiki_sql.wiki_id_s w) b
     in
     self#display_menu_box_aux ~title ~service:Edit_perm editform_class wb

   method private display_history_box (w, b as wb) =
     let title = Printf.sprintf "History - Wiki %s, box %ld"
       (Wiki_sql.wiki_id_s w) b
     in
     self#display_menu_box_aux ~title ~service:History history_class wb

   method private display_editable_box =
     self#display_menu_box_aux ~service:View editable_class

   method private display_old_wikibox (w, b as wb) version =
     let title = Printf.sprintf "Old version - Wiki %s, box %ld, version %ld"
       (Wiki_sql.wiki_id_s w) b version
     in
     self#display_menu_box_aux ~title oldwikibox_class wb

   method private display_src_wikibox (w, b as wb) version =
     let title = Printf.sprintf "Source - Wiki %s, box %ld, version %ld"
       (Wiki_sql.wiki_id_s w) b version
     in
     self#display_menu_box_aux ~title srcwikibox_class wb

   method private display_edit_css_box ((w, _) as wb) page =
     let title = Printf.sprintf "CSS for wiki %s, %s"
       (Wiki_sql.wiki_id_s w) (if page = "" then "main page" else "page " ^ page)
     in
     self#display_menu_box_aux ~title ~service:Edit_css css_class wb

   method private display_edit_wikicss_box ((w, _) as wb) =
     let title = Printf.sprintf "CSS for wiki %s (global stylesheet)"
       (Wiki_sql.wiki_id_s w)
     in
     self#display_menu_box_aux ~title ~service:Edit_css css_class wb

   method display_history ~bi ids l =
     let sp = bi.Wiki_syntax.bi_sp in
     Lwt_util.map
       (fun (version, _comment, author, date) ->
          Users.get_user_fullname_by_id author >>= fun author ->
          Lwt.return
            {{ [ !{: Int32.to_string version :}'. '
                   !{: CalendarLib.Printer.Calendar.to_string date :}' '
                 <em>[ 'by ' !{: author :} ]' '
                 {:  Eliom_duce.Xhtml.a ~service:action_old_wikibox
                      ~sp {{ "view" }} (ids, version) :}
                   ' ''(' {: Eliom_duce.Xhtml.a ~service:action_src_wikibox
                             ~sp {{ "source" }} (ids, version) :} ')'
                 <br>[]
               ]
             }})
       l
     >>= fun l ->
     Lwt.return {{ map {: l :} with i -> i }}


   (* Display a wikibox, plus some potentials actions on the wikibox, which
      are extracted from sp. This function returns the wikibox, as well
      as a boolean indicating whether the page is correctly displayed, or
      whether the user has not even rights to see the box (in which case
      a 403 is returned in Wiki.ml)
   *)
   method editable_wikibox_aux ~bi ~data ?rows ?cols ?(classe=[])
     ?cssmenu () =
     let sp = bi.Wiki_syntax.bi_sp in
     let sd = bi.Wiki_syntax.bi_sd in
     let rec find_action = function
       | [] -> None
       | (Wiki.Wiki_action_info e)::_ -> Some e
       | _ :: l -> find_action l
     in
     let (wiki_id, _) = data in
     let action = find_action (Eliom_sessions.get_exn sp) in
     Wiki.get_role ~sp ~sd data >>= fun role ->
     (match role with
        | Wiki.Admin
        | Wiki.Author ->
            (match action with
               | Some (Wiki.Edit_box i) when i = data ->
                   self#bind_or_display_error ~classe
                     (Wiki.retrieve_wikibox_current_wikitext_and_version data)
                     (self#display_full_edit_form ~bi ?cols ?rows
                        ~previewonly:true data)
                     (self#display_edit_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Edit_perm i) when i = data ->
                   self#bind_or_display_error ~classe
                     (Lwt.return data)
                     (self#display_edit_perm_form ~bi)
                     (self#display_edit_perm ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Preview (i, (content, version))) when i = data ->
                   self#bind_or_display_error ~classe
                     (Lwt.return (content, version))
                     (fun ((c, _v) as cv) ->
                        self#pretty_print_wikisyntax
                          wiki_id
                          {bi with
                             Wiki_syntax.bi_ancestors = 
                              Wiki_syntax.add_ancestor 
                                data bi.Wiki_syntax.bi_ancestors
                          }
                          c
                        >>= fun pp ->
                        self#display_basic_box ~classe:[preview_class] pp
                        >>= fun preview ->
                        self#display_full_edit_form
                          ~bi ?cols ?rows ~previewonly:false data cv
                        >>= fun form ->
                        Lwt.return {{ [<p class={: box_title_class :}>"Preview"
                                       preview
                                       !form ] }}
                     )
                     (self#display_edit_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.History i) when i = data ->
                   self#bind_or_display_error ~classe
                     (Wiki_sql.get_history data)
                     (self#display_history ~bi data)
                     (self#display_history_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Oldversion (i, version)) when i = data ->
                   self#bind_or_display_error ~classe
                     (Wiki.retrieve_wikibox_wikitext_at_version version data)
                     (self#pretty_print_wikisyntax
                        ~wiki:wiki_id
                        ~bi:{bi with
                               Wiki_syntax.bi_ancestors =
                            Wiki_syntax.add_ancestor 
                              data bi.Wiki_syntax.bi_ancestors }
                     )
                     (self#display_old_wikibox ~bi ?cssmenu data version)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Src (i, version)) when i = data ->
                   self#bind_or_display_error ~classe
                     (Wiki.retrieve_wikibox_wikitext_at_version version data)
                     (fun c ->
                        let c = Ocamlduce.Utf8.make c in
                        Lwt.return {{ [ <pre>c ] }})
                     (self#display_src_wikibox ~bi ?cssmenu data version)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Error (i, error)) when i = data ->
                   self#bind_or_display_error ~classe
                     ~error:(self#create_error_message error)
                     (Wiki.retrieve_wikibox_current_wikitext data)
                     (self#pretty_print_wikisyntax
                        ~wiki:wiki_id
                        ~bi:{bi with
                               Wiki_syntax.bi_ancestors =
                            Wiki_syntax.add_ancestor
                              data bi.Wiki_syntax.bi_ancestors}
                     )
                     (self#display_editable_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)

               | Some (Wiki.Delete_Box i) when i = data -> assert false

               | _ ->
                   (* No action, or the action does not concern this page.
                      We simply display the wikipage itself *)
                   self#bind_or_display_error ~classe
                     (Wiki.retrieve_wikibox_current_wikitext data)
                     (self#pretty_print_wikisyntax
                        ~wiki:wiki_id
                        ~bi:{bi with
                           Wiki_syntax.bi_ancestors =
                            Wiki_syntax.add_ancestor
                              data bi.Wiki_syntax.bi_ancestors}
                     )
                     (self#display_editable_box ~bi ?cssmenu data)
                   >>= fun r ->
                   Lwt.return (r, true)
            )

        | Wiki.Lurker ->
            (match action with
               | Some (Wiki.Edit_box i)
               | Some (Wiki.History i)
               | Some (Wiki.Oldversion (i, _))
               | Some (Wiki.Error (i, _)) when i = data ->
                   self#bind_or_display_error
                     ~classe
                     ~error:(self#create_error_message
                               Wiki.Operation_not_allowed)
                     (Wiki.retrieve_wikibox_current_wikitext data)
                     (self#pretty_print_wikisyntax
                        ~wiki:wiki_id
                        ~bi:{bi with
                           Wiki_syntax.bi_ancestors =
                            Wiki_syntax.add_ancestor
                              data bi.Wiki_syntax.bi_ancestors}
                     )
                     (self#display_basic_box)
                   (* Returning true would also be meaningful: the action
                      is forbidden but the wikibox itself is shown *)
                   >>= fun r ->
                   Lwt.return (r, false)

               | _ ->
                   (* As for Author/default *)
                   self#bind_or_display_error
                     ~classe
                     (Wiki.retrieve_wikibox_current_wikitext data)
                     (self#pretty_print_wikisyntax
                        ~wiki:wiki_id
                        ~bi:{bi with
                           Wiki_syntax.bi_ancestors =
                            Wiki_syntax.add_ancestor 
                              data bi.Wiki_syntax.bi_ancestors}
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
     )

   method editable_wikibox ~bi ~data ?rows ?cols ?(classe=[]) ?cssmenu () =
     self#editable_wikibox_aux ~bi ~data ?rows ?cols ~classe ?cssmenu ()
     >>= fun (r, _allowed) -> Lwt.return r


   method display_edit_css_form ~bi ?(rows=25) ?(cols=80) ~data:(wiki_id, page) content =
     let sp = bi.Wiki_syntax.bi_sp in
     let draw_form ((wikiidname, pagename), contentname) =
       {{ [<p>[
            {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
               ~name:wikiidname ~value:wiki_id Wiki_sql.wiki_id_s :}
            {: Eliom_duce.Xhtml.string_input ~input_type:{: "hidden" :}
               ~name:pagename ~value:page () :}
            {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
               ~value:(Ocamlduce.Utf8.make content) () :}
            <br>[]
            {: Eliom_duce.Xhtml.button ~button_type:{{ "submit" }} {{ "Save" }} :}
            ]] }}
     in
     Lwt.return
       {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wikipage_css ~sp draw_form () :}] }}

   method edit_css_box ~bi ~data ?rows ?cols ?(classe=[]) ()  =
     let sp = bi.Wiki_syntax.bi_sp in
     let sd = bi.Wiki_syntax.bi_sd in
     let (wiki, page) = data in
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors () >>= fun c ->
     Wiki_sql.get_box_for_page wiki page
     >>= fun { Wiki_sql.wikipage_dest_wiki = wiki'; wikipage_wikibox = box} ->
     self#bind_or_display_error ~classe
       (if c then
          Wiki_sql.get_css_for_page wiki page >>= function
            | Some css -> Lwt.return css
            | None -> Lwt.return ""
        else Lwt.fail Wiki.Not_css_editor)
       (self#display_edit_css_form ~bi ?rows ?cols ~data)
       (self#display_edit_css_box ~bi ~cssmenu:(Some page) (wiki', box) page)

   method display_edit_wikicss_form ~bi ?(rows=25) ?(cols=80) ~wiki (content : string) =
     let sp = bi.Wiki_syntax.bi_sp in
     let draw_form (wikiidname, contentname) =
       {{ [<p>[
            {: Eliom_duce.Xhtml.user_type_input ~input_type:{: "hidden" :}
               ~name:wikiidname ~value:wiki Wiki_sql.wiki_id_s :}
            {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
               ~value:(Ocamlduce.Utf8.make content) () :}
            <br>[]
            {: Eliom_duce.Xhtml.button ~button_type:{{ "submit" }} {{ "Save" }} :}
            ]] }}
     in
     Lwt.return
       {{[ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
              ~service:action_send_wiki_css ~sp draw_form () :}] }}

   method edit_wikicss_box ~bi ~wiki ?rows ?cols ?(classe=[]) () =
     let sp = bi.Wiki_syntax.bi_sp in
     let sd = bi.Wiki_syntax.bi_sd in
     Users.get_user_id ~sp ~sd >>= fun userid ->
     Wiki.css_editors_group wiki >>= fun editors ->
     Wiki_sql.get_wiki_by_id wiki >>= fun wiki_info ->
     Users.in_group ~sp ~sd ~user:userid ~group:editors ()
     >>= fun c ->
     self#bind_or_display_error ~classe
       (if c then
          Lwt.catch
            (fun () -> Wiki_sql.get_css_for_wiki wiki (* The css exists *))
            (function
               | Not_found -> Lwt.return ""
               | e -> Lwt.fail e)
        else Lwt.fail Wiki.Not_css_editor)
       (self#display_edit_wikicss_form ~bi ?rows ?cols ~wiki)
       (self#display_edit_wikicss_box ~bi ?cssmenu:(Some None)
          (wiki, wiki_info.Wiki_sql.container_id))

   method get_css_header ~bi ~wiki ?(admin=false) ?page () =
     let sp = bi.Wiki_syntax.bi_sp in
     let css_url_service service args = Eliom_duce.Xhtml.css_link
       (Eliom_duce.Xhtml.make_uri service sp args) () in
     let css_url path = css_url_service (Eliom_services.static_dir sp) path in

     if admin then
       Lwt.return
         {{ [ {: css_url ["ocsiwikistyle.css"] :}
              {: css_url ["ocsiwikiadmin.css"] :} ] }}
     else
       let css = css_url ["ocsiwikistyle.css"] in
       (match Wiki_syntax.find_servwikicss wiki with
          | None -> Lwt.return {{ [ css ] }}
          | Some wikicss_service ->
              Lwt.catch
                (fun () ->
                   Wiki_sql.get_css_for_wiki wiki
                   >>= fun _ -> Lwt.return
                     {{ [ css
                          {: css_url_service wikicss_service () (* encoding? *) :}
                        ]}})
                (function
                   | Not_found -> Lwt.return {{ [ css ] }}
                   | e -> Lwt.fail e)
       )
       >>= fun css ->
       Lwt.catch
         (fun () ->
            match page with
              | None -> Lwt.return css
              | Some page ->
                  Wiki_sql.get_css_for_page wiki page
                  >>= fun _ -> Lwt.return
                    {{ [ !css
                         {: css_url_service pagecss_service (wiki, page)
                            (*VVV encoding? *) :}
                       ]}}
         )
         (function
            | Not_found -> Lwt.return css
            | e -> Lwt.fail e)


   initializer
     begin
       (* BY: Helper functions, which factorizes a bot of code. Some of them
          (eg. extract_wiki_id) are very mysterious:
          - I believe there is always a field "wiki" present, so the
            exception handler is useless
          - Why do we need to extract this value since we have a default ?
       *)
       let extract_wiki_id args default =
         try Wiki_sql.s_wiki_id (List.assoc "wiki" args)
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
                if Wiki_syntax.in_ancestors(wiki, box) bi.Wiki_syntax.bi_ancestors
                then
                  Lwt.return {{ [ {: self#display_error_box
                              ~message:"Wiki error: loop of wikiboxes" () :} ] }}
                else begin
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
                           Wiki_syntax.bi_ancestors =
                        Wiki_syntax.add_ancestor
                          (wiki, box) bi.Wiki_syntax.bi_ancestors;
                           Wiki_syntax.bi_subbox = subbox}
                    ()
                  >>= fun b ->
                  Lwt.return {{ [ b ] }}
                end
              with Not_found ->
                Lwt.return {{ [ <code>"<<wikibox>>" ] }}
            with
              | Failure _ ->
                  Lwt.return {{ [ {: self#display_error_box
                    ~message:"Wiki error: error in wikibox extension" () :} ] }}
         );

       Wiki_filter.add_preparser_extension "wikibox"
         (fun wiki_id (sp, sd, father) args c ->
            (try
              let wiki = extract_wiki_id args wiki_id in
              try
                ignore (List.assoc "box" args);
                Lwt.return None
              with Not_found ->
                Wiki_sql.get_wiki_by_id wiki >>= fun wiki ->
                Users.get_user_id ~sp ~sd >>= fun userid ->
                let ids = (wiki_id, father) in
                Wiki.can_create_wikibox ~sp ~sd wiki father userid >>= fun b ->
                if b
                then begin
                  Wiki.get_readers ids >>= fun readers ->
                  Wiki.get_writers ids >>= fun writers ->
                  Wiki.get_rights_adm ids >>= fun rights_adm ->
                  Wiki.get_wikiboxes_creators ids >>= fun wikiboxes_creators ->
                  Wiki.new_wikibox
                    ~wiki
                    ~author:userid
                    ~comment:"new wikibox"
                    ~content:"**//new wikibox//**"
                    ~content_type:Wiki_sql.Wiki
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
        (fun wiki_id bi args c ->
           let sp = bi.Wiki_syntax.bi_sp in
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
           ((if Wiki_syntax.is_absolute_link href
             then href
             else
               match Wiki_syntax.find_servpage wiki_id with
                 | Some s ->
                     let href =
                       Ocsigen_lib.remove_slash_at_beginning
                         (Ocsigen_lib.remove_dotdot (Neturl.split_path href))
                     in
                     Eliom_duce.Xhtml.make_uri
                       ?https
                       ?fragment
                       ~service:s
                       ~sp
                       href
                 | None -> href
            ),
            args,
            content)
        );

       Wiki_syntax.add_link_extension "nonattachedlink"
        (fun wiki_id bi args c ->
           let sp = bi.Wiki_syntax.bi_sp in
           let href = Ocsimore_lib.list_assoc_default "page" args "" in
           let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
           let https = extract_https args in
           let wiki_id = extract_wiki_id args wiki_id in
           let content =
               match c with
                 | Some c -> Wiki_syntax.a_content_of_wiki wiki_id bi c
                 | None -> Lwt.return (Ocamlduce.Utf8.make href)
           in
           ((Eliom_duce.Xhtml.make_uri ?https ?fragment
               ~service:(Wiki_syntax.find_naservpage wiki_id) ~sp href
            ),
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
           ((Eliom_duce.Xhtml.make_uri ~service:Eliom_services.void_coservice'
               ~sp:bi.Wiki_syntax.bi_sp ()
            ),
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
             if Wiki_syntax.is_absolute_link href
             then href
             else
               match Wiki_syntax.find_servpage wiki_id with
                 | Some s ->
                     let href =
                       Ocsigen_lib.remove_slash_at_beginning
                         (Ocsigen_lib.remove_dotdot (Neturl.split_path href))
                     in
                     Eliom_duce.Xhtml.make_uri ?https ?fragment ~service:s
                       ~sp:bi.Wiki_syntax.bi_sp href
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
             if Wiki_syntax.is_absolute_link href
             then href
             else
               match Wiki_syntax.find_servpage wiki_id with
                 | Some s ->
                     let href =
                       Ocsigen_lib.remove_slash_at_beginning
                         (Ocsigen_lib.remove_dotdot (Neturl.split_path href))
                     in
                     Eliom_duce.Xhtml.make_uri ?https ~service:s
                       ~sp:bi.Wiki_syntax.bi_sp href
                 | _ -> href
           in
           Lwt.return
             {{ [<img ({ src={: Ocamlduce.Utf8.make url :}
                         alt={: Ocamlduce.Utf8.make alt :}}
                         ++ atts )>[] ] }});

       Eliom_duce.Xhtml.register service_edit_wikibox
         (fun sp ((w, _b) as g) () ->
            let sd = Ocsimore_common.get_sd sp in
            let bi = { Wiki_syntax.bi_sp = sp;
                       bi_sd = sd;
                       bi_ancestors = Wiki_syntax.no_ancestors;
                       bi_subbox = None;
                       bi_page = None;
                     }
            in
            self#editable_wikibox ~bi ~data:g ~rows:30 ()
            >>= fun subbox ->
            Wiki.get_admin_wiki () >>= fun admin_wiki ->
            let bi = { bi with Wiki_syntax.bi_subbox = Some {{ [ subbox ] }} } in
            self#editable_wikibox ~bi ~data:(admin_wiki, wikiadmin_container_id)
              ?cssmenu:(Some None) ()
            >>= fun page ->
            self#get_css_header ~admin:true ~bi ~wiki:w ?page:None ()
            >>= fun css ->
            Lwt.return (self#container ~css {{ [ page ] }})
         );

       Eliom_duce.Xhtml.register service_edit_wikipage_css
         (fun sp ((wiki, page) as g) () ->
            let sd = Ocsimore_common.get_sd sp in
            let bi = { Wiki_syntax.bi_sp = sp;
                       bi_sd = sd;
                       bi_ancestors = Wiki_syntax.no_ancestors;
                       bi_subbox = None;
                       bi_page = None;
                     }
            in
            Wiki_sql.get_wiki_by_id wiki
            >>= fun wiki_info ->
              self#edit_css_box ~bi ~rows:30 ~data:g ()
            >>= fun subbox ->
            let bi = { bi with Wiki_syntax.bi_subbox = Some {{ [ subbox ] }} } in
            self#editable_wikibox ~bi ?cssmenu:(Some None)
              ~data:(wiki, wiki_info.Wiki_sql.container_id) ()
            >>= fun pagecontent ->
            self#get_css_header ~bi ~wiki ?page:(Some page) ()
            >>= fun css ->
            Lwt.return (self#container ~css {{ [ pagecontent ] }})
         );

       Eliom_duce.Xhtml.register service_edit_wiki_css
         (fun sp wiki () ->
            let sd = Ocsimore_common.get_sd sp in
            let bi = { Wiki_syntax.bi_sp = sp;
                       bi_sd = sd;
                       bi_ancestors = Wiki_syntax.no_ancestors;
                       bi_subbox = None;
                       bi_page = None;
                     }
            in
            self#edit_wikicss_box ~bi ~rows:30 ~wiki ()
            >>= fun pagecontent ->
            self#get_css_header ~admin:true ~bi ~wiki ?page:None ()
            >>= fun css ->
            Lwt.return (self#container ~css {{ [ pagecontent ] }})
         )

     end

end


class creole_wikibox ?sp () adminwikiinfo = object
  inherit editable_wikibox ?sp () adminwikiinfo

  method pretty_print_wikisyntax ~wiki ~bi = Wiki_syntax.xml_of_wiki wiki bi

end
