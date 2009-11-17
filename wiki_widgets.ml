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
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Language

open User_sql.Types
open Wiki_widgets_interface
open Wiki_types

let (>>=) = Lwt.bind


class wikibox_error_box =
object

  inherit Widget.widget_with_error_box as error_box

  method display_error_box ?classes ?message ?exn () =
    match exn with
      | Some (Wiki_data.Unknown_box (wb, ver)) ->
          error_box#display_error_box ?classes ?exn
            ~message:(Printf.sprintf
                        "The box %ld does not exist%s."
                        (sql_of_wikibox wb)
                        (match ver with
                           | None -> ""
                           | Some v -> Printf.sprintf " with version %ld" v))
            ()

      | Some (Wiki_sql.Unknown_Css wb) ->
          error_box#display_error_box ?classes ?exn
            ~message:(Printf.sprintf
                        "The CSS %ld does not exist." (sql_of_wikibox wb))
            ()

    | Some Wiki_data.Page_already_exists _wb ->
          error_box#display_error_box ?classes ?exn
            ~message:("This page has already been created \
                       (reload the page to see it).") ()

      | _ -> error_box#display_error_box ?classes ?message ?exn ()

end


module MakeWikiWidget(P : Ocsimore_page.PageSig)(X : Wiki_services.WikiServices) = struct


let wiki_css_header =
  P.Header.create_header
    (fun sp ->
       {{ [ {: Eliom_duce.Xhtml.css_link
               (P.static_file_uri sp ["ocsiwikistyle.css"]) () :}
          ] }})

let add_wiki_css_header sp =
  P.Header.require_header wiki_css_header ~sp;


class wikibox_aux (error_box : Widget.widget_with_error_box)
  : Wiki_widgets_interface.wikibox_aux =
object (self)

  method display_wikiboxcontent ~bi ~classes (wiki_syntax, content, _ver as wb) =
    add_wiki_css_header bi.bi_sp;
    let wiki_parser = Wiki_models.get_flows_wiki_parser wiki_syntax in
    match content with
      | Some content ->
          wiki_parser bi content >>= fun x -> 
          Lwt.return (classes, x)
      | _ -> self#display_raw_wikiboxcontent ~classes wb

  method display_raw_wikiboxcontent ~classes (_content_type, content, _ver) =
    (match content with
       | Some content ->
           Lwt.return {{ [<pre>{:Ocamlduce.Utf8.make content :}] }}
       | None -> Lwt.return {{ [<em>"/* Deleted content */"] }}
    ) >>= fun x ->
    Lwt.return (classes, x)

  method display_basic_box ~classes content =
    let classe = Ocsimore_lib.build_class_attr classes in
    Lwt.return
      {{ <div class={: classe :}>content }}

end


class frozen_wikibox (error_box : Widget.widget_with_error_box)
  : Wiki_widgets_interface.frozen_wikibox =
object (self)

  inherit wikibox_aux error_box

  val frozen_wb_class = "frozen_wikibox"

  method display_frozen_wikibox ~bi ?(classes=[]) ~wikibox =
    Lwt.catch
      (fun () ->
         let exn =
           match Wiki_services.get_wikibox_error ~sp:bi.bi_sp with
             | None -> None
             | Some (wb', e) -> if Some wikibox = wb' then Some e else None
         in
         error_box#bind_or_display_error
           ?exn
           (Wiki_data.wikibox_content bi.bi_rights bi.bi_sp wikibox)
           (self#display_wikiboxcontent ~bi ~classes:(frozen_wb_class::classes))
           (self#display_basic_box)
      )
      (function
         | Ocsimore_common.Permission_denied ->
              Lwt.return
                (error_box#display_error_box
                   ~classes:(frozen_wb_class::classes)
                   ~message:"You are not allowed to see this content."
                   ())
         | e -> Lwt.fail e)
end;;


(** Displaying of a wikibox with viewing and/or editing rights. Takes
    as argument all the services needed to save modifications
    or navigate through viewing options *)
class dynamic_wikibox (error_box : Widget.widget_with_error_box)
  (user_widgets: User_widgets.user_widget_class)
  : Wiki_widgets_interface.interactive_wikibox =
object (self)

  inherit frozen_wikibox error_box

  val wikibox_class = "wikibox"
  val interactive_class = "interactive" (* means "with menu" *)

  val view_class = "view"
  val editform_class = "editform"
  val history_class = "history"
  val css_history_class = "history"
  val oldwikibox_class = "oldversion"
  val srcwikibox_class = "src"
  val box_title_class = "boxtitle"
  val preview_class = "preview"
  val css_class = "editcss"
  val wikipage_properties_class = "wikipageproperties"


  method private box_menu ~bi ?(special_box=RegularBox) ?active_item ?(title = "") html_id_wikibox wb =
    match bi.bi_menu_style with
      | `None -> Lwt.return {{ [] }}
      | `Pencil | `Linear as menu_style ->
    let sp = bi.bi_sp
    and preapply = Eliom_services.preapply in
    let history = preapply X.action_wikibox_history wb
    and edit = preapply X.action_edit_wikibox wb
    and delete = preapply X.action_delete_wikibox wb
    and edit_wikibox_perm = preapply X.action_edit_wikibox_permissions wb
    and view = Eliom_services.void_coservice' in
    (match special_box with
       | WikiPageBox (w, page) ->
           (bi.bi_rights#can_create_wikipagecss sp (w, page) >>= function
              | true -> let edit =
                  preapply X.action_edit_css_list (wb, (w, Some page)) in
                Lwt.return (Some (edit, {{ "wikipage css" }}))
              | false -> Lwt.return None
           )
       | WikiContainerBox w ->
           (bi.bi_rights#can_create_wikicss sp w >>= function
              | true ->
                  let edit = preapply X.action_edit_css_list (wb, (w, None)) in
                  Lwt.return (Some (edit, {{ "wiki css" }}))
              | false -> Lwt.return None
           )
       | RegularBox -> Lwt.return None
    ) >>= fun css ->
    (match special_box with
       | RegularBox | WikiContainerBox _ -> Lwt.return None
       | WikiPageBox wp ->
           bi.bi_rights#can_admin_wikipage sp wp >>= function
             | true ->
                 let edit_wp =
                   preapply X.action_edit_wikipage_properties (wb, wp) in
                 Lwt.return (Some (edit_wp, {{ "edit wikipage options" }}))
             | false -> Lwt.return None
    ) >>= fun wp_prop ->
    (match special_box with
       | RegularBox | WikiPageBox _ -> Lwt.return None
       | WikiContainerBox w ->
           bi.bi_rights#can_set_wiki_permissions sp w >>= fun b1 ->
           bi.bi_rights#can_edit_metadata sp w >>= fun b2 ->
           match b1 || b2 with
             | true ->
                 let edit_p = preapply X.action_edit_wiki_options (wb, w) in
                 Lwt.return (Some (edit_p, {{ "edit wiki permissions or options" }}))
             | false -> Lwt.return None
    ) >>= fun edit_wiki_perms ->
    (* We choose the button to highlight, which is indicated by the
       [active_item] argument *)
    let service = match active_item with
      | None -> None
      | Some Menu_View -> Some view
      | Some Menu_Edit -> Some edit
      | Some Menu_EditWikiboxPerms -> Some edit_wikibox_perm
      | Some Menu_EditWikiOptions ->
          (match edit_wiki_perms with
             | Some (edit_wiki_perms, _) -> Some edit_wiki_perms
             | None -> None)
      | Some Menu_History -> Some history
      | Some Menu_Css ->
          (match css with
             | Some (css, _) -> Some css
             | None -> None)
      | Some Menu_WikipageProperties ->
          (match wp_prop with
             | Some (s, _) -> Some s
             | None -> None)
    in
    Wiki_sql.wikibox_wiki wb >>= fun wiki ->
    bi.bi_rights#can_delete_wikiboxes ~sp wiki >>= fun wbdel ->
    bi.bi_rights#can_write_wikibox ~sp wb >>= fun wbwr ->
    bi.bi_rights#can_view_history ~sp wb >>= fun wbhist ->
    bi.bi_rights#can_set_wikibox_specific_permissions ~sp wb >>= fun  wbperm ->
    let menuedit =
      if wbwr
      then Some (edit, {{ "edit" }})
      else None
    in
    let menuperm =
      if wbperm
      then Some (edit_wikibox_perm, {{ "edit permissions" }})
      else None
    in
    let menuhist =
      if wbhist
      then Some (history, {{ "history" }})
      else None
    in
    let l = Ocsimore_lib.concat_list_opt
      [menuedit; menuperm; menuhist; wp_prop; edit_wiki_perms; css]
      []
    in
    let menudel = 
      if wbdel
      then (
        P.add_obrowser_header sp;
        let link = Eliom_duce.Xhtml.make_string_uri ~service:delete ~sp () in
        {{ [<a class="jslink"
               onclick={: Wiki_client_calls.delete_wikibox
                            link html_id_wikibox :}>
               "delete"
           ] }})
      else {{[]}}
    in
    let title = Ocamlduce.Utf8.make title in
    match l, wbdel with
      | [], false -> Lwt.return {{[]}} (* empty list => no menu *)
      | _ ->
          Wiki.wiki_admin_page_link sp ["crayon.png"] >>= fun img ->
          let menu = Eliom_duce_tools.menu ~sp ~classe:["wikiboxmenu"]
            (view, {{ "view"}}) l ?service in
          let menu = match menu with {{ <ul (attrs)>[ (li::_)* ] }} ->
            {{ <ul (attrs)>[!li <li>menudel] }} in
          match menu_style with
            | `Pencil ->
                Lwt.return
                  {{ [ <div class="pencilmenucontainer">[
                         <img class="pencilmenuimg" src={: img :} alt="edit">[]
                           <div class="pencilmenu">[
                             <div class="wikiboxmenutitle">title
                             menu
                           ]
                       ]
                     ]  }}
            | `Linear ->
                Lwt.return
                  {{ [  <div class="wikiboxlinearmenu">[
                          <div class="wikiboxmenutitle">title
                            menu
                        ]
                     ]  }}

  method display_menu_box ~bi ~classes ?active_item ?special_box ?title ~wb content =
    let id = Eliom_obrowser.fresh_id () in
    self#box_menu ~bi ?special_box ?active_item ?title id wb >>= fun menu ->
    let classes = Ocsimore_lib.build_class_attr
      (if menu = {{ [] }} then classes else interactive_class::classes) in
    Lwt.return
      {{ <div id={: id :} class={: classes :}>[
           !menu
           <div>content ]}}

  method draw_edit_form ~rows ~cols wb warning1 warning2 curversion content
    previewonly
    (actionname, ((wbname, versionname), contentname)) =
    {{ [ <p>[!warning1
               {: Ocsimore_common.input_opaque_int32 ~value:wb wbname :}
               {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                  ~name:versionname ~value:curversion () :}
               {: Eliom_duce.Xhtml.textarea
                  ~a:{{ { class="wikitextarea" } }}
                  ~name:contentname ~rows ~cols
                  ~value:content () :}
               <br>[]
               !warning2
               {: Eliom_duce.Xhtml.string_button
                  ~name:actionname ~value:"preview" {{ "Preview" }} :}
               !{: if previewonly then []
                   else
                     [Eliom_duce.Xhtml.string_button ~name:actionname
                        ~value:"save" {{ "Save" }} ] :}
              ]] }}


  (* Wikitext in editing mode *)
  method display_wikitext_edit_form
    ~bi ~classes ?(rows=25) ?(cols=80) ~previewonly ~wb (content, version) =
    let content = match content with
      | None -> "<<|  Deleted >>"
      | Some content -> content
    and sp = bi.bi_sp in
    Wiki.modified_wikibox wb version >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            {{ [ <em>['Warning: ']
                 !{: Ocamlduce.Utf8.make !messages.wikitext_edition_conflict1 :}
                 <br>[] <br>[]
             ]
             }},
            {{ [ <br>[]
                 <b> [ <em> ['Warning: ']
                       !{: Ocamlduce.Utf8.make
                           !messages.wikitext_edition_conflict1 :}
                     ]
                 <br>[]
               ] }} )

       | None -> Lwt.return (version, {{ [] }}, {{ [] }})
    ) >>= fun (curversion, warning1, warning2)  ->
    Lwt.return
      (classes,
       Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
         ~service:X.action_send_wikiboxtext ~sp 
         (self#draw_edit_form ~rows ~cols wb warning1 warning2 curversion
            content previewonly) ())

  (* Wikitext in editing mode, with an help box on the syntax of the wiki *)
  method display_wikitext_edit_form_help ~bi ~classes ?rows ?cols ~previewonly ~wb data=
    Wiki.get_admin_wiki () >>= fun { wiki_id = admin_wiki } ->
    Wiki_sql.get_wikipage_info ~wiki:admin_wiki
      ~page:Wiki_widgets_interface.wikisyntax_help_name
    >>= fun { wikipage_wikibox = wb_help } ->
    error_box#bind_or_display_error
      (Wiki_data.wikibox_content bi.bi_rights bi.bi_sp wb_help)
      (self#display_wikiboxcontent ~bi ~classes:["wikihelp"])
      (self#display_basic_box)
    >>= fun b ->
    self#display_wikitext_edit_form ~bi ~classes:[] ?rows ?cols
      ~previewonly ~wb data
    >>= fun (_, f) ->
    Lwt.return (classes, {{ [ b f ] }})


  (* Css in editing mode *)
  method display_css_edit_form ~bi ~classes ?(rows=25) ?(cols=80) ~wb ~wbcss ~wikipage (content, boxversion) =
    let content = match content with
      | None -> "/* Deleted CSS */"
      | Some content -> content
    and sp = bi.bi_sp in
    Wiki.modified_wikibox wbcss boxversion >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            {{ [ <em>['Warning: ']
                 !{: Ocamlduce.Utf8.make !messages.css_edition_conflict :}
                 <br>[] <br>[]
             ]
             }} )

       | None -> Lwt.return (boxversion, {{ [] }} )
    ) >>= fun (curversion, warning)  ->
    let draw_form ((wbname, (((wikiname, wikipagename),wbcssname), versionname)), contentname) =
      {{ [ <p>[!warning
               {: Ocsimore_common.input_opaque_int32 ~value:wb wbname :}
               {: Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssname :}
               {: Ocsimore_common.input_opaque_int32 ~value:(fst wikipage)
                  wikiname :}
               !{: match snd wikipage with
                   | None -> []
                   | Some page ->
                       [ Eliom_duce.Xhtml.string_input ~name:wikipagename
                           ~input_type:{: "hidden" :} ~value:page () ]
                :}
               {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                  ~name:versionname ~value:curversion () :}
               {: Eliom_duce.Xhtml.textarea ~name:contentname ~rows ~cols
                  ~value:content () :}
               <br>[]
               {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                  {{ "Save" }} :}
              ] ] }}
    in
    Lwt.return
      (classes,
       {{ [ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
               ~service:X.action_send_css ~sp draw_form () :} ] }})


  (** Edition of the permissions of a wikibox *)
  (** As usual, the function supposes that the user has enough rights
      to change the permissions, as this will be checked by the service
      (and the user should not have access to the page otherwise). We
      also suppose that boxrights is set to true for the wiki *)
  method display_edit_wikibox_perm_form ~bi ~classes wb =
    Wiki_sql.get_wikibox_info wb
    >>= fun { wikibox_wiki = wiki;
              wikibox_id = uid; wikibox_special_rights = sr } ->
    User.GenericRights.map_awr
      (fun it -> User_sql.user_to_string
         (it.User.GenericRights.field Wiki.wiki_wikiboxes_grps $ wiki)
      )
    >>= fun defaults ->
    user_widgets#form_edit_awr ~sp:bi.bi_sp ~grps:Wiki.wikibox_grps ~arg:wb
      ~defaults ()
    >>= fun form ->
    let msg1 = Ocamlduce.Utf8.make
      "Check this box if you want permissions specific to the wikibox. \
       Otherwise, permissions are inherited from the wiki"
    and msg2 = Ocamlduce.Utf8.make "Below are the current permissions for the
      wikibox. Add users in the fields to change them"
    in
    let form (nsr, (narg, args)) = {{ [
              <p>[ !msg1
                   {: Eliom_duce.Xhtml.bool_checkbox
                      ~a:{{ { id="checkwikiboxpermissions"
                              onclick={: Wiki_client_calls.toggle_wikibox_permissions () :}
                            } }}
                      ~checked:sr ~name:nsr():}]
              <div id="wikiboxpermissions" style={: "display: " ^
                                 if sr then "block" else "none" :} >[
                <p>[ !msg2 ]
                <p>[ {: Wiki.helpers_wikibox_permissions.User.GroupsForms.awr_form_arg uid narg :}
                     !{: form args :} ]
              ]
              <p>[ {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                        {{"Save"}} :}
              ]
    ] }} in
    let form = Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
      ~service:X.action_send_wikibox_permissions ~sp:bi.bi_sp form
      ()
    in
    P.add_obrowser_header bi.bi_sp;
    Lwt.return (classes, {{ [ form ] }})


  (** Form to edit the description and container of a wiki *)
  method private display_edit_wiki_metadata ~sp ~classes ?wb wiki =
    Wiki_sql.get_wiki_info_by_id wiki >>= fun { wiki_descr = descr;
                                                wiki_container = container } ->
      let form (wbname, (wikiname, (descrname, containername))) =
      {{ [ <h2>"Wiki properties"
           <p>[ {: Ocsimore_common.input_opaque_int32 ~value:wiki wikiname :}
                  !{: match wb with None -> [] | Some wb ->
                      [Ocsimore_common.input_opaque_int32 ~value:wb wbname] :}
                  'Description: '
                  {: Eliom_duce.Xhtml.string_input ~name:descrname
                     ~input_type:{: "text" :} ~value:descr () :} <br>[]
                  'Container wikibox: '
                  {: Ocsimore_common.input_opaque_int32_opt ~hidden:false
                     ~value:container containername :}
                  {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{"Save"}} :}
              ] ] }}
      in
      let form = Eliom_duce.Xhtml.post_form ~sp
        ~a:{{ { accept-charset="utf-8" } }}
        ~service:X.action_send_wiki_metadata form
        ()
      in Lwt.return (classes, {{ [form] }})


  method private display_edit_wiki_option_form ~sp ~classes ?wb ~options ~perms wiki =
    (if options then
       self#display_edit_wiki_metadata ~sp ~classes ?wb wiki
     else Lwt.return ([], {{ [] }})
    ) >>= fun (cl1, f1) ->
    (if perms then
       self#display_edit_wiki_perm_form ~sp ~classes ?wb wiki
     else
       Lwt.return ([], {{ [] }})
    ) >>= fun (cl2, f2) ->
    Lwt.return (cl1 @ cl2, {{ [!f1 !f2] }})


  (** Form for the permissions of a wiki; The [wb] argument is the wikibox
      which will be overridden with an error message if the save fails *)
  method display_edit_wiki_perm_form ~sp ~classes ?wb wiki =
    let form wiki =
      let aux g text = user_widgets#form_edit_group ~sp
        ~group:(g $ wiki) ~text ~show_edit:true in
    aux Wiki.wiki_admins "Administer the wiki" () >>= fun f1 ->
    aux Wiki.wiki_subwikiboxes_creators "Create subwikiboxes" () >>= fun f2 ->
    aux Wiki.wiki_wikipages_creators "Create wikipages" () >>= fun f3 ->
    aux Wiki.wiki_wikiboxes_creators "Create wikiboxes" () >>= fun f4 ->
    aux Wiki.wiki_css_creators "Create CSS" () >>= fun f5 ->
    aux Wiki.wiki_wikiboxes_deletors "Delete wikiboxes" () >>= fun f6 ->
    aux Wiki.wiki_files_readers "Read static files" () >>= fun f8 ->
    aux Wiki.wiki_wikiboxes_src_viewers "View wikiboxes source" () >>= fun f9 ->
    aux Wiki.wiki_wikiboxes_oldversion_viewers "View wikiboxes old versions" () >>= fun f10 ->
    aux Wiki.wiki_metadata_editors "Edit the metadata of the wiki" () >>= fun f11 ->
    user_widgets#form_edit_awr ~sp
      ~grps:Wiki.wiki_wikiboxes_grps ~arg:wiki () >>= fun f7 ->

    let msg = Ocamlduce.Utf8.make
      ("Permissions for wiki " ^ string_of_wiki wiki)
    and msg2 = Ocamlduce.Utf8.make "(inherited permissions are not shown)"
    and msg_wikiboxes = Ocamlduce.Utf8.make "Global permissions for wikiboxes:"
    in
    Lwt.return (
      fun (narg, (n1, (n2, (n3, (n4, (n5, (n6, (n7, (n8, (n9, (n10, n11))))))))))) ->
        ({{ [
             <h2>msg
             <p>[<em>msg2]
             <p>[ {: Wiki.h_wiki_admins.User.GroupsForms.grp_form_arg wiki narg :}
                  !{: f1 n1 :} <br>[]
                  !{: f2 n2 :} <br>[]
                  !{: f3 n3 :} <br>[]
                  !{: f4 n4 :} <br>[]
                  !{: f5 n5 :} <br>[]
                  !{: f6 n6 :} <br>[]
                  !{: f8 n8 :} <br>[]
                  !{: f9 n9 :} <br>[]
                  !{: f10 n10 :} <br>[]
                  !{: f11 n11 :} <br>[]
                  <b>msg_wikiboxes <br>[]
                  !{: f7 n7 :}]
             <p>[ {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{"Save"}} :} ]
           ] }} : Eliom_duce.Blocks.form_content_elt_list))
    in
    form wiki >>= fun form ->
    let form (wbname, nargs) =
      {{ [ <p>[ !{: match wb with
                     | None -> []
                     | Some wb -> [Ocsimore_common.input_opaque_int32
                                         ~value:wb wbname] :} ]
                !{: form nargs :}
         ]}}
    in
    let form = Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
      ~service:X.action_send_wiki_permissions ~sp form
      ()
    in
    Lwt.return (classes, {{ [ form ] }})

  (* Auxiliary method to factorize some code *)
  method private menu_box_aux ?title ?active_item cl wb ~bi ~classes ?special_box content =
    self#display_menu_box ~classes:(cl::classes) ?active_item ?title ~bi
      ?special_box ~wb content

  method private menu_edit_wikitext wb =
    let title = Printf.sprintf "Edit - Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_Edit editform_class wb

  method private menu_edit_wikibox_perms wb =
    let title = Printf.sprintf "Permissions - Wikibox %s"
      (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_EditWikiboxPerms editform_class wb
  method private menu_edit_css_perms wb (wiki, page) =
    let title = Printf.sprintf "CSS Permissions - wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page) in
    self#menu_box_aux ~title ~active_item:Menu_Css editform_class wb

  method private menu_edit_wiki_options wb wiki =
    let title = Printf.sprintf "Options - Wiki %s" (string_of_wiki wiki) in
    self#menu_box_aux ~title ~active_item:Menu_EditWikiOptions editform_class wb

  method private menu_wikitext_history wb =
    let title = Printf.sprintf "History - Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_History history_class wb

  method private menu_css_history wb (wiki, page) =
    let title = Printf.sprintf "CSS history, wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page) in
    self#menu_box_aux ~title ~active_item:Menu_Css css_history_class wb

  method private menu_edit_wikipage_properties wb (wiki, page) =
    let title = Printf.sprintf "Page properties, wiki %s, page %s"
      (string_of_wiki wiki) page in
    self#menu_box_aux ~title ~active_item:Menu_WikipageProperties
      wikipage_properties_class wb

  method private menu_view wb =
    let title = Printf.sprintf "Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_View view_class wb

  method private menu_old_wikitext wb version =
    let title = Printf.sprintf "Old version - Wikibox %s, version %ld"
      (string_of_wikibox wb) version in
    self#menu_box_aux ~title oldwikibox_class wb

  method private menu_old_css wb (wiki, page) =
    let title = Printf.sprintf "Old css version, wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page) in
    self#menu_box_aux ~title oldwikibox_class wb

  method private menu_src_wikitext wb version =
    let title = Printf.sprintf "Source - Wikibox %s, version %ld"
      (string_of_wikibox wb) version in
    self#menu_box_aux ~title srcwikibox_class wb

  method private menu_edit_css wb (wiki, page) =
    let title = Printf.sprintf "CSS for wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page) in
    self#menu_box_aux ~title ~active_item:Menu_Css css_class wb

  method private menu_edit_css_list wb (wiki, page) =
    let title = Printf.sprintf "All CSS for wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page) in
    self#menu_box_aux ~title ~active_item:Menu_Css css_class wb

  method private css_wikibox_text = function
    | None -> "global stylesheet"
    | Some "" -> "main page"
    | Some page ->  "page " ^ page


  method display_wikitext_history ~bi ~classes ~wb l =
    let sp = bi.bi_sp in
    Lwt_util.map
      (fun (version, _comment, author, date) ->
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >>= fun { user_fullname = author } ->
         Lwt.return
           {{ [ !{: Int32.to_string version :}'. '
                !{: CalendarLib.Printer.Calendar.to_string date :}' '
                <em>[ 'by ' !{: Ocamlduce.Utf8.make author :} ]' '
                {:  Eliom_duce.Xhtml.a ~sp ~service:X.action_old_wikibox
                   {{ "view" }} (wb, version) :}
                ' ''('
                {: Eliom_duce.Xhtml.a ~sp ~service:X.action_src_wikibox
                   {{ "source" }} (wb, version) :}
                ')'
                <br>[]
              ]
            }})
      l
    >>= fun l ->
    Lwt.return (classes, {{ map {: l :} with i -> i }})

  method display_css_history ~bi ~classes ~wb ~wbcss ~wikipage l =
    let sp = bi.bi_sp in
    Lwt_util.map
      (fun (version, _comment, author, date) ->
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >>= fun { user_fullname = author } ->
           Lwt.return
             {{ [ !{: Int32.to_string version :}'. '
                  !{: CalendarLib.Printer.Calendar.to_string date :}' '
                  <em>[ 'by ' !{: Ocamlduce.Utf8.make author :} ]' '
                  {:  Eliom_duce.Xhtml.a ~sp ~service:X.action_old_wikiboxcss
                     {{ "view" }} (wb, ((wikipage, wbcss), version)) :}
                  <br>[]
                ]
              }})
      l
    >>= fun l ->
    Lwt.return (classes, {{ map {: l :} with i -> i }})

   method private display_edit_wikipage_properties ~bi ~classes ~(wb:wikibox) wp =
     let (wiki, page) = wp in
     Wiki_sql.get_wikipage_info wiki page >>= fun wp ->
     let draw_form (wbname, ((wikiidname, pagename), (titlename, ((wbidname, pathname))))) =
       {{ [<p>[
              {: Ocsimore_common.input_opaque_int32
                 ~value:wiki wikiidname :}
              {: Ocsimore_common.input_opaque_int32
                 ~value:wb wbname :}
              {: Eliom_duce.Xhtml.string_input ~name:pagename
                 ~input_type:{: "hidden" :} ~value:page () :}
              !"Title of the wikipage: "
              {: Eliom_duce.Xhtml.string_input ~name:titlename
                 ~input_type:{: "text" :}
                 ~value:(match wp.wikipage_title
                         with None -> "" | Some s -> s) () :}
              !"(if blank, the title of the wiki will be used" <br>[]

              'Wikibox to which the wikipage points to. Leave blank
                to delete the wikipage. '
              {: Ocsimore_common.input_opaque_int32_opt ~hidden:false
                 ~value:(Some wp.wikipage_wikibox) wbidname :} <br>[]

              !"Path of the wikipage inside the wiki"
              {: Eliom_duce.Xhtml.string_input ~name:pathname
                 ~input_type:{: "text" :} ~value:wp.wikipage_page () :}
              !"Notice that changing this path will "<em>"not"
              !" update links to this wikipage." <br>[]

              {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{"Save"}} :}

            ]] }}
     in
     Lwt.return
      (classes,
       {{ [ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
              ~service:X.action_send_wikipage_properties ~sp:bi.bi_sp
              draw_form () :} ] }})

  method private display_edit_css_list ~bi ~classes ~(wb:wikibox) wikipage =
    let wiki, page = wikipage and sp = bi.bi_sp in
    (match page with
      | None -> Wiki_sql.get_css_for_wiki ~wiki
      | Some page -> Wiki_sql.get_css_for_wikipage ~wiki ~page
    ) >>= fun l ->
    (match page with
       | None -> bi.bi_rights#can_create_wikicss sp wiki
       | Some page -> bi.bi_rights#can_create_wikipagecss sp (wiki, page)
    ) >>= fun can_create ->
    let select_media name media =
      let l = [ "braille"; "embossed"; "handheld"; "print";
                "projection"; "screen"; "speech"; "tty"; "tv"] in
      let in_media m = List.mem m media in
      let l = List.map
        (fun s -> Eliom_duce.Xhtml.Option ( {{ {} }}, s, None, in_media s)) l
      in
      Eliom_duce.Xhtml.string_multiple_select ~name
        ~a:{{ { style="vertical-align: top" size="5"} }}
        (Eliom_duce.Xhtml.Option ({{ {} }}, "all",
                                  Some {{ "all media" }}, in_media "all")) l
    in
    let aux ((wbcss, media, rank), (_, ver)) =
      bi.bi_rights#can_view_history ~sp wbcss >>= fun csshist ->
      bi.bi_rights#can_write_wikibox ~sp wbcss >>= fun csswr ->
      bi.bi_rights#can_set_wikibox_specific_permissions
        ~sp wbcss >>= fun cssperm ->
      let wbcss_ = ((wiki, page), wbcss) in
      let v1 = if csshist then
        {{ [ {{ Eliom_duce.Xhtml.a ~sp ~service:X.action_css_history
                  {{ "History" }} (wb, wbcss_) }} ] }}
      else {{ [] }}
      and v2 = if csswr then
        {{ [ {{ Eliom_duce.Xhtml.a ~sp ~service:X.action_edit_css
                  {{ "Edit" }} (wb, (wbcss_, None)) }} ] }}
      else {{ [] }}
      and v3 = if cssperm then
        {{ [ {{ Eliom_duce.Xhtml.a ~sp ~service:X.action_css_permissions
                {{ "Permissions" }} (wb, wbcss_)  }} ] }}
      else {{ [] }}
      and v4 =
        {{ [ {{ Eliom_duce.Xhtml.a ~sp ~service:X.action_old_wikiboxcss
                  {{ "View" }} (wb, (wbcss_, ver)) }} ] }}
      in
      let fupdate (wbn, (((((wikin, wpn), wbcssn), newwbcssn), median), rankn))=
        {{ [ <div class="eliom_inline">
               [{: Ocsimore_common.input_opaque_int32 ~value:wb wbn :}
                {: Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssn :}
                {: Ocsimore_common.input_opaque_int32 ~value:wiki wikin :}
                'CSS '
                {: Eliom_duce.Xhtml.int32_input ~input_type:{: "text" :}
                   ~a:{{ { size = "2" } }} ~value:rank ~name:rankn () :}
                '(Id ' !{: Opaque.int32_t_to_string wbcss :} ') '
                {: Ocsimore_common.input_opaque_int32 ~value:wbcss newwbcssn :}
                !{: match page with
                    | None -> []
                    | Some page ->
                        [ Eliom_duce.Xhtml.string_input ~name:wpn
                            ~input_type:{: "hidden" :} ~value:page () ]
                           :}
                {: select_media median media :}
                ' '
                {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                   {{ " Update media and CSS order" }} :}
                ' '
               ] ] }}
      and fdelete (wbn, (((((wikin, wpn), wbcssn), _newwbcssn), _median), rankn)) =
        {{ [ <div class="eliom_inline">
               [{: Ocsimore_common.input_opaque_int32 ~value:wb wbn :}
                {: Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssn :}
                {: Ocsimore_common.input_opaque_int32 ~value:wiki wikin :}
                {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                   ~value:rank ~name:rankn () :}
                !{: match page with
                    | None -> []
                    | Some page ->
                        [ Eliom_duce.Xhtml.string_input ~name:wpn
                            ~input_type:{: "hidden" :} ~value:page () ]
                           :}
                {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                   {{ " Remove CSS" }} :}
               ] ] }}
      in
      if can_create then Lwt.return
        ({{ <div>[
             {{ Eliom_duce.Xhtml.post_form ~keep_get_na_params:true
                  ~a:{{ { accept-charset="utf-8" class="eliom_inline"} }}
                  ~service:X.action_send_css_options ~sp fupdate () }}
             {{ Eliom_duce.Xhtml.post_form ~keep_get_na_params:true
                  ~a:{{ { accept-charset="utf-8" class="eliom_inline"} }}
                  ~service:X.action_send_css_options ~sp fdelete () }}
             ' '
             !v4 ' / ' !v1 ' / ' !v2 ' / ' !v3
           ] }} : Xhtmltypes_duce.block)
      else
        Lwt.return ({{ <p>[ !v4 ' / '!v1 ' / ' !v2 ' / ' !v3 ] }} :
                      Xhtmltypes_duce.block)
    in
    (if l = [] then
      Lwt.return {{ [ 'There are currently no CSS' ] }}
     else
       Lwt_util.fold_left
         (fun (x : Xhtmltypes_duce.flows) e ->
            aux e >>= fun e ->
              Lwt.return {{ [ !x <br>[] {: e :} ] }}) {{ [] }} l
    ) >>= fun forms ->
    (if can_create then
       let mform (wbn, ((wikin, wpn), (median, wbcssn))) =
         {{ [ <p>['Add another CSS: ' <br>[]
                  {: Ocsimore_common.input_opaque_int32 ~value:wb wbn :}
                  {: Ocsimore_common.input_opaque_int32 ~value:wiki wikin :}
                  !{: match page with
                      | None -> []
                      | Some page ->
                          [ Eliom_duce.Xhtml.string_input ~name:wpn
                              ~input_type:{: "hidden" :} ~value:page () ]
                            :}
                  'Media: '{: select_media median ["all"] :} <br>[]
                  'Id: '
                  {: Ocsimore_common.input_opaque_int32_opt ~hidden:false
                     wbcssn :}
                  'Type an existing CSS id, or leave blank to create a
                    new CSS' <br>[]
                  {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{ "Add" }} :}
                 ] ] }}
       in Lwt.return
            {{ [ !forms <br>[]
                   {: Eliom_duce.Xhtml.post_form ~sp
                      ~a:{{ { accept-charset="utf-8"} }}
                      ~service:X.action_create_css mform () :}
               ] }}
     else
       Lwt.return {{ [] }}
    ) >>= fun r ->
    let classes_ = Ocsimore_lib.build_class_attr classes in
    Lwt.return (classes, {{ [ <div class={: classes_ :}> r ] }})


  method display_interactive_wikibox_aux
    ~bi ?(classes=[]) ?rows ?cols ?special_box wb =
    let classes = wikibox_class::classes in
    let sp = bi.bi_sp in
    let override = Wiki_services.get_override_wikibox ~sp in
    let exn = match Wiki_services.get_wikibox_error ~sp with
      | None -> None
      | Some (wb', e) -> if Some wb = wb' then Some e else None
    in
    match override with
      | Some (wb', override) when wb = wb' ->
          self#display_overriden_interactive_wikibox ~bi ~classes ?rows ?cols
            ?special_box ~wb_loc:wb ~override ?exn ()
      | _ ->
          error_box#bind_or_display_error
            ?exn
            (Wiki_data.wikibox_content bi.bi_rights sp wb)
            (self#display_wikiboxcontent ~classes
               ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
            (self#menu_view ~bi ?special_box wb)
          >>= fun r ->
          Lwt.return (r, true)

  method display_overriden_interactive_wikibox
    ~bi ?(classes=[]) ?rows ?cols ?special_box ~wb_loc ~override ?exn () =
    let sp = bi.bi_sp in
    let display_error () = 
      Lwt.return (error_box#display_error_box
                    ~classes:(frozen_wb_class::classes)
                    ~message:"You are not allowed to do that."
                    (),
                  false)
    in
    match override with
      | EditWikitext wb ->
          (bi.bi_rights#can_write_wikibox ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_content' bi.bi_rights bi.bi_sp wb)
                  (self#display_wikitext_edit_form_help ~bi ?cols ?rows
                     ~previewonly:true ~wb ~classes)
                  (self#menu_edit_wikitext ~bi ?special_box wb_loc)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | EditCss ((wikipage, wbcss), css) ->
          (bi.bi_rights#can_write_wikibox ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (match css with
                     | None -> Wiki_data.wikibox_content' bi.bi_rights sp wbcss
                     | Some (content, version) ->
                     Lwt.return (Some content, version)
                  )
                  (self#display_css_edit_form ~bi ?cols ?rows
                     ~wb:wb_loc ~wbcss ~wikipage ~classes)
                  (self#menu_edit_css ~bi ?special_box wb_loc wikipage)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | EditCssList wikipage ->
          error_box#bind_or_display_error
            ?exn
            (Lwt.return wikipage)
            (self#display_edit_css_list ~bi ~classes ~wb:wb_loc)
            (self#menu_edit_css_list ~bi ?special_box wb_loc wikipage)
          >>= fun r ->
          Lwt.return (r, true)

      | EditWikiboxPerms wb ->
          (bi.bi_rights#can_set_wikibox_specific_permissions ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Lwt.return wb)
                  (self#display_edit_wikibox_perm_form ~bi ~classes)
                  (self#menu_edit_wikibox_perms ~bi ?special_box wb_loc)
                >>= fun r ->
                Lwt.return (r, true)
          | false -> display_error ())

      | EditWikiOptions wiki ->
          (bi.bi_rights#can_set_wiki_permissions ~sp wiki >>= fun b1 ->
           bi.bi_rights#can_edit_metadata ~sp wiki >>= fun b2 ->
           match b1 || b2 with
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Lwt.return wiki)
                  (self#display_edit_wiki_option_form ~sp:bi.bi_sp ~classes
                     ~wb:wb_loc ~options:b2 ~perms:b1)
                  (self#menu_edit_wiki_options ~bi ?special_box wb_loc wiki)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | PreviewWikitext (wb, (content, version)) ->
          (bi.bi_rights#can_write_wikibox ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_content ~sp ~version~rights:bi.bi_rights wb
                   >>= fun (syntax, _, _) ->
                   Lwt.return (syntax, (Some content, version)))
                  (fun (syntax, (content, version as cv)) ->
                     let bi' = { (Wiki_widgets_interface.add_ancestor_bi wb bi)
                                 with bi_menu_style = `None } in
                     self#display_wikiboxcontent ~classes:[] ~bi:bi'
                       (syntax, content, version)
                   >>= fun (_, pp) ->
                   self#display_basic_box ~classes:[preview_class] pp
                   >>= fun preview ->
                   self#display_wikitext_edit_form_help ~classes:[]
                     ~bi ?cols ?rows ~previewonly:false ~wb cv
                   >>= fun (_, form) ->
                   Lwt.return
                     (classes,
                      {{ [ <p class={: box_title_class :}>"Preview"
                             preview
                             !form ] }})
                  )
                  (self#menu_edit_wikitext ~bi ?special_box wb_loc)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | EditWikipageProperties wp ->
          (bi.bi_rights#can_admin_wikipage ~sp wp >>= function
             | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Lwt.return wp)
                  (self#display_edit_wikipage_properties ~bi ~classes ~wb:wb_loc)
                  (self#menu_edit_wikipage_properties ~bi ?special_box wb_loc wp)
                >>= fun r ->
                Lwt.return (r, true)

             | false -> display_error ())

      | History wb ->
          (bi.bi_rights#can_view_history ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_history bi.bi_rights sp wb)
                  (self#display_wikitext_history ~bi ~classes ~wb)
                  (self#menu_wikitext_history ~bi ?special_box wb_loc)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | CssHistory ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_view_history ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_history bi.bi_rights sp wbcss)
                  (self#display_css_history ~bi ~classes ~wb:wb_loc ~wbcss
                     ~wikipage:(wiki,wikipage))
                  (self#menu_css_history ~bi ?special_box wb_loc
                     (wiki, wikipage))
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | CssPermissions ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_set_wikibox_specific_permissions ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Lwt.return wbcss)
                  (self#display_edit_wikibox_perm_form ~bi ~classes)
                  (self#menu_edit_css_perms ~bi ?special_box wb_loc
                     (wiki, wikipage))
                >>= fun r ->
                Lwt.return (r, true)
          | false -> display_error ())

      | Oldversion (wb, version) ->
          (bi.bi_rights#can_view_oldversions ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_content ~sp ~version ~rights:bi.bi_rights wb)
                  (self#display_wikiboxcontent ~classes
                     ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
                  (self#menu_old_wikitext ~bi ?special_box wb_loc version)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | CssOldversion (((wiki, page), wbcss), version) ->
          (bi.bi_rights#can_view_oldversions ~sp wbcss >>= function 
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_content ~sp ~version
                     ~rights:bi.bi_rights wbcss)
                  (self#display_raw_wikiboxcontent ~classes)
                  (self#menu_old_css ~bi ?special_box wb_loc (wiki, page))
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())

      | Src (wb, version)->
          (bi.bi_rights#can_view_oldversions_src ~sp wb >>= function 
            | true ->
                error_box#bind_or_display_error
                  ?exn
                  (Wiki_data.wikibox_content ~sp ~version ~rights:bi.bi_rights wb)
                  (self#display_raw_wikiboxcontent ~classes)
                  (self#menu_src_wikitext ~bi ?special_box wb_loc version)
                >>= fun r ->
                Lwt.return (r, true)
            | false -> display_error ())


   method display_interactive_wikibox
     ~bi ?(classes=[]) ?rows ?cols ?special_box wb =
     add_wiki_css_header bi.bi_sp;
     self#display_interactive_wikibox_aux
       ~bi ?rows ?cols ~classes ?special_box wb
     >>= fun (r, _allowed) -> Lwt.return r


   method css_header ~sp ?page wiki =
     let css_url_service service args media =
       Eliom_duce.Xhtml.css_link
         ?a:(if media = [] then
               None
             else
               let media = Ocamlduce.Utf8.make (String.concat " " media) in
               (Some {{ { media } }}))
         ~uri:(Eliom_duce.Xhtml.make_uri service sp args)
       ()
     in
     (match Wiki_self_services.find_servwikicss wiki with
        | None -> Lwt.return {{ [] }}
        | Some wikicss_service ->
            Wiki_sql.get_css_wikibox_for_wiki wiki >>= fun l ->
            let l' = List.map (fun (wb, media, _) ->
                                 css_url_service wikicss_service wb media) l
            in
            Lwt.return {{ {: l' :} }}
     )
     >>= fun css ->
     match page with
       | None -> Lwt.return css
       | Some page ->
           Wiki_sql.get_css_wikibox_for_wikipage ~wiki ~page >>= fun l ->
           let l' = List.map (fun (wb, media, _) ->
                                css_url_service
                                  X.pagecss_service ((wiki, page), wb) media) l
           in
           Lwt.return {{ [ !css !{: l' :} ] }}

   method private display_container
     ~sp ~wiki ~menu_style ~page:(page, page_list) ~gen_box =
     Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
     let rights = Wiki_models.get_rights wiki_info.wiki_model
     and wb_container = wiki_info.wiki_container in
     gen_box menu_style >>= fun (wbid, subbox, err_code, title) ->
     Wiki_widgets_interface.set_page_displayable sp err_code;

     (* We render the container, if it exists *)
     (match wb_container with
        | None -> Lwt.return {{ [ <div>subbox ] }}

        | Some wb_container ->
            Wiki.default_bi ~sp ~rights ~wikibox:wb_container >>= fun bi ->
            let fsubbox ms =
              if ms = menu_style then
                Lwt.return (Some (wbid, subbox))
              else
                gen_box ms >>= fun (wbid, subbox, _, _) ->
                Lwt.return (Some (wbid, subbox))
            in
            let bi = { bi with  bi_subbox = fsubbox;
                         bi_page = wiki, Some page_list;
                         bi_menu_style = menu_style } in
            self#display_interactive_wikibox ~bi
              ~classes:[Wiki_syntax.class_wikibox wb_container]
              ~special_box:(WikiContainerBox wiki) wb_container
            >>= fun b -> Lwt.return {{ [b] }}

     ) >>= fun pagecontent ->

     self#css_header ~sp ~page wiki >>= fun css ->

     let title = (match title with
                    | Some title -> title
                    | None -> wiki_info.wiki_descr)
     and code = match err_code with
       | Wiki_widgets_interface.Page_displayable -> 200
       | Wiki_widgets_interface.Page_404 -> 404
       | Wiki_widgets_interface.Page_403 -> 403
     in
     P.html_page ~sp ~css ~title pagecontent >>= fun r ->
     Lwt.return (r, code)


   (* Displays the wikibox corresponding to a wikipage. This function, properly
      applied, is suitable for use with [display_container]. *)
   method private display_wikipage_wikibox ~sp ~wiki ~page:(page, page_list) =
     Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
     let rights = Wiki_models.get_rights wiki_info.wiki_model
     and wb_container = wiki_info.wiki_container in
     Lwt.return
     (fun menu_style -> Lwt.catch
       (fun () ->
          (* We render the wikibox for the page *)
          Wiki_sql.get_wikipage_info wiki page
          >>= fun { wikipage_wikibox = box; wikipage_title = title } ->
          Wiki.default_bi ~sp ~wikibox:box ~rights >>= fun bi ->
          let bi = { bi with bi_page = wiki, Some page_list;
                             bi_menu_style = menu_style } in
          self#display_interactive_wikibox_aux ~bi
            ~special_box:(WikiPageBox (wiki, page)) box
          >>= fun (subbox, allowed) ->
          Lwt.return (Some box,
                      {{ [ subbox ] }},
                      (if allowed
                       then Wiki_widgets_interface.Page_displayable
                       else Wiki_widgets_interface.Page_403),
                      title)
       )
       (function
          | Not_found ->
              (* No wikibox. We create a default page, to insert into the
                 container *)
              let draw_form (wbname, (wikiidname, pagename)) =
                {{ [<p>[
                       {: Ocsimore_common.input_opaque_int32
                          ~value:wiki wikiidname :}
                       (* Used to know where to display errors (only possible
                          if there is a container, otherwise we don't know
                          what to override *)
                       !{: match wb_container with
                           | None -> []
                           | Some container ->
                               [ Ocsimore_common.input_opaque_int32
                                   ~value:container wbname ]
                        :}
                       {: Eliom_duce.Xhtml.string_input ~name:pagename
                          ~input_type:{: "hidden" :} ~value:page () :}
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "submit" :} ~value:"Create it!" () :}
                     ]] }}
              in
              rights#can_create_wikipages sp wiki >>= fun c ->
              let form =
                if c then
                  {{ [ {: Eliom_duce.Xhtml.post_form
                          ~service:X.action_create_page
                          ~sp draw_form () :} ] }}
                else {{ [] }}
              and err_msg = !Language.messages.Language.page_does_not_exist
              in
              Lwt.return
                (None,
                 {{ [ <p>{:err_msg:} !form ] }},
                 Wiki_widgets_interface.Page_404,
                 None)
          | e -> Lwt.fail e
       )
     )


   (* Displaying of an entire page. We just pass the proper rendering
   function to [display_container] *)
   method display_wikipage ~sp ~wiki ~menu_style ~page =
     self#display_wikipage_wikibox ~sp ~wiki ~page
     >>= fun gen_box ->
     self#display_container ~sp ~wiki ~menu_style ~page ~gen_box


   method display_all_wikis ~sp =
     (* Lists of all wikis *)
     let l = ref [] in
     Wiki_sql.iter_wikis (fun w -> Lwt.return (l := w :: !l)) >>= fun () ->
     let l = List.sort
      (fun w1 w2 -> compare w1.wiki_title w2.wiki_title) !l in

     let line w =
       let img = P.icon ~sp in
       let t = Ocamlduce.Utf8.make w.wiki_title
       and d = Ocamlduce.Utf8.make w.wiki_descr
       and id = Opaque.int32_t_to_string w.wiki_id
       and edit = Eliom_duce.Xhtml.a ~service:X.edit_wiki ~sp
         (img "imgedit.png" "Edit wiki options") w.wiki_id
       and edit_perm = Eliom_duce.Xhtml.a
         ~service:X.edit_wiki_permissions_ocsisite ~sp
         (img "imgeditperms.png" "Edit permissions") w.wiki_id
       and page =
         match Wiki_self_services.find_servpage w.wiki_id with
           | None -> {{ [] }}
           | Some service ->
               {{ [ {: Eliom_duce.Xhtml.a ~service ~sp
                       (img "imgview.png" "View wiki root wikipage") [] :} ] }}
       in
       {{ <tr>[<td class="wikiid">{: id :}
               <td class="wikiname">[<b>t]
               <td class="wikidescr">d
               <td>[edit]
               <td>[edit_perm]
               <td>page ] }}
     in
     let l = List.fold_left (fun (s : {{ [Xhtmltypes_duce.tr*] }}) arg ->
                                {{ [ !s {: line arg:} ] }}) {{ [] }} l in
     Lwt.return {{ [ <h1>"Existing Ocsimore wikis"
                     <table class="table_admin">
                       [ <tr>[<th>"Id" <th>"Wiki" <th>"Description" ] !l]
                   ] }}

end

class inline_wikibox (error_box : Widget.widget_with_error_box) (user_widgets: User_widgets.user_widget_class)
  : Wiki_widgets_interface.interactive_wikibox =
object (self)

  inherit dynamic_wikibox error_box user_widgets

  method draw_edit_form ~rows:_ ~cols:_ wb warning1 warning2 curversion content
    previewonly
    (actionname, ((wbname, versionname), contentname)) =
    {{ [ <p>[!warning1
               {: Ocsimore_common.input_opaque_int32 ~value:wb wbname :}
               {: Eliom_duce.Xhtml.int32_input ~input_type:{: "hidden" :}
                  ~name:versionname ~value:curversion () :}
               {: Eliom_duce.Xhtml.string_input
                    ~a:{{ { class="wikitextarea" } }}
                    ~input_type:{: "text" :} ~name:contentname
                    ~value:content () :}
               <br>[]
               !warning2
               {: Eliom_duce.Xhtml.string_button
                  ~name:actionname ~value:"preview" {{ "Preview" }} :}
               !{: if previewonly then []
                   else
                     [Eliom_duce.Xhtml.string_button ~name:actionname
                        ~value:"save" {{ "Save" }} ] :}
              ]] }}


  method display_wikitext_edit_form_help ~bi ~classes ?rows ?cols ~previewonly ~wb data=
    self#display_wikitext_edit_form ~bi ~classes:[] ?rows ?cols
      ~previewonly ~wb data
    >>= fun (_, f) ->
    Lwt.return (classes, {{ [ f ] }})


end
end
