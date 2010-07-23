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

open User_sql.Types
open Wiki_widgets_interface
open Wiki_types

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)


class wikibox_error_box =
object

  inherit Widget.widget_with_error_box as error_box

  method display_error_box ?classes ?message ?exc () =
    match exc with
      | Some (Wiki_data.Unknown_box (wb, ver)) ->
          error_box#display_error_box ?classes ?exc
            ~message:(Printf.sprintf
                        "The box %ld does not exist%s."
                        (sql_of_wikibox wb)
                        (match ver with
                           | None -> ""
                           | Some v -> Printf.sprintf " with version %ld" v))
            ()

      | Some (Wiki_sql.Unknown_Css wb) ->
          error_box#display_error_box ?classes ?exc
            ~message:(Printf.sprintf
                        "The CSS %ld does not exist." (sql_of_wikibox wb))
            ()

    | Some Wiki_data.Page_already_exists _wb ->
          error_box#display_error_box ?classes ?exc
            ~message:("This page has already been created \
                       (select the current url and hit enter to see it).") ()

      | _ -> error_box#display_error_box ?classes ?message ?exc ()

end



let wiki_css_header =
  Page_site.Header.create_header
    (fun sp ->
       [Eliom_predefmod.Xhtml.css_link
          (Page_site.static_file_uri sp ["ocsiwikistyle.css"])
          ()
       ]
    )

let add_wiki_css_header sp =
  Page_site.Header.require_header wiki_css_header ~sp;


class wikibox_aux (error_box : Widget.widget_with_error_box)
  : Wiki_widgets_interface.wikibox_aux =
object (self)

  method display_wikiboxcontent ~bi ~classes (wiki_syntax, content, _ver as wb)=
    add_wiki_css_header bi.bi_sp;
    let wiki_parser = Wiki_models.get_flows_wiki_parser wiki_syntax in
    match content with
      | Some content ->
          wiki_parser bi content >>= fun x ->
          Lwt.return (classes, x)
      | _ -> self#display_raw_wikiboxcontent ~classes wb

  method display_raw_wikiboxcontent ~classes (_content_type, content, _ver) =
    Lwt.return
      (classes,
       (match content with
          | Some content ->
              [XHTML.M.pre [XHTML.M.pcdata content]]
          | None ->
              [XHTML.M.em [XHTML.M.pcdata "/* Deleted content */"]]
       )
      )

  method display_basic_box (classes, content) =
    Lwt.return
      (XHTML.M.div ~a:[XHTML.M.a_class classes] content
       : [`Div] XHTML.M.elt :> Xhtmltypes.block XHTML.M.elt)

  method wrap_error ~sp ~wb r =
    match Wiki_services.get_wikibox_error ~sp with
      | Some (wb', exc) when Some wb = wb' ->
          let err_msg = error_box#display_error_box ~exc () in
          [XHTML.M.div (err_msg :: r)]
      | _ -> r

end


class frozen_wikibox (error_box : Widget.widget_with_error_box)
  : Wiki_widgets_interface.frozen_wikibox =
object (self)

  inherit wikibox_aux error_box

  val frozen_wb_class = "frozen_wikibox"

  method display_frozen_wikibox ~bi ?(classes=[]) ~wikibox =
    Lwt.catch
      (fun () ->
         error_box#bind_or_display_error
           (Wiki_data.wikibox_content bi.bi_rights bi.bi_sp wikibox)
           (self#display_wikiboxcontent ~bi ~classes:(frozen_wb_class::classes))
         >>= self#display_basic_box >|= fun r ->
         self#wrap_error ~sp:bi.bi_sp ~wb:wikibox [r]
      )
      (function
         | Ocsimore_common.Permission_denied ->
              Lwt.return
                [error_box#display_error_box
                   ~classes:(frozen_wb_class::classes)
                   ~message:"You are not allowed to see this content."
                   ()]
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


  method private box_menu
    ~bi
    ?(special_box=RegularBox)
    ?active_item
    ?(title = "")
    wb =

    let preapply = Eliom_services.preapply in
    let sp = bi.bi_sp in

    match bi.bi_menu_style with
      | `None -> Lwt.return []
      | `Pencil | `Linear as menu_style ->

    let history  = preapply Wiki_services.action_wikibox_history wb in
    let edit     = preapply Wiki_services.action_edit_wikibox wb in
    let delete   = preapply Wiki_services.action_delete_wikibox wb in
    let view     = Eliom_services.void_coservice' in
    let edit_wikibox_perm =
      preapply Wiki_services.action_edit_wikibox_permissions wb
    in
    (match special_box with
       | WikiPageBox (w, page) ->
           (bi.bi_rights#can_create_wikipagecss sp (w, page) >|= function
              | true ->
                  let edit =
                    preapply
                      Wiki_services.action_edit_css_list
                      (wb, (w, Some page))
                  in
                  Some (edit, [XHTML.M.pcdata "wikipage css"])
              | false -> None
           )
       | WikiContainerBox w ->
           (bi.bi_rights#can_create_wikicss sp w >|= function
              | true ->
                  let edit =
                    preapply
                      Wiki_services.action_edit_css_list
                      (wb, (w, None))
                  in
                  Some (edit, [XHTML.M.pcdata "wiki css"])
              | false -> None
           )
       | RegularBox -> Lwt.return None
    ) >>= fun css ->
    (match special_box with
       | RegularBox | WikiContainerBox _ -> Lwt.return None
       | WikiPageBox wp ->
           bi.bi_rights#can_admin_wikipage sp wp >|= function
             | true ->
                 let edit_wp =
                   preapply
                     Wiki_services.action_edit_wikipage_properties
                     (wb, wp)
                 in
                 Some (edit_wp, [XHTML.M.pcdata "edit wikipage options"])
             | false -> None
    ) >>= fun wp_prop ->
    (match special_box with
       | RegularBox | WikiPageBox _ -> Lwt.return None
       | WikiContainerBox w ->
           bi.bi_rights#can_set_wiki_permissions sp w >>= fun b1 ->
           bi.bi_rights#can_edit_metadata sp w        >>= fun b2 ->
           match b1 || b2 with
             | true ->
                 let edit_p =
                   preapply
                     Wiki_services.action_edit_wiki_options
                     (wb, w)
                 in
                 Lwt.return
                   (Some
                      (edit_p,
                       [XHTML.M.pcdata "edit wiki permissions or options"])
                   )
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
      then Some (edit, [XHTML.M.pcdata "edit"])
      else None
    in
    let menuperm =
      if wbperm
      then Some (edit_wikibox_perm, [XHTML.M.pcdata "edit permissions"])
      else None
    in
    let menuhist =
      if wbhist
      then Some (history, [XHTML.M.pcdata "history"])
      else None
    in
    let l = Ocsimore_lib.concat_list_opt
      [menuedit; menuperm; menuhist; wp_prop; edit_wiki_perms; css]
      []
    in
    let menudel =
      if wbdel
      then (
(* Failure "TODO: replace obrowser for deletion" *)
        let link =
          Eliom_predefmod.Xhtml.make_string_uri ~service:delete ~sp ()
        in
        [XHTML.M.a
           ~a:[XHTML.M.a_class ["jslink"]]
           [XHTML.M.pcdata "delete"]
        ]
      )
      else []
    in
    match l, wbdel with
      | [], false -> Lwt.return [] (* empty list => no menu *)
      | _ ->
          Wiki.wiki_admin_page_link sp ["crayon.png"] >>= fun img ->
          let menu = Eliom_tools.Xhtml.menu
                       ~classe:["wikiboxmenu"]
                       (view, [XHTML.M.pcdata "view"]) l
                       ?service ~sp
          in
          (* Failure "TODO: generalize menus so that a function call can be used" *)
          (*
          let menu = match menu with
            | {{ <ul (attrs)>[ (li::_)* ] }} ->
                {{ <ul (attrs)>[!li <li>menudel] }}
          in
          *)
          match menu_style with
            | `Pencil ->
                Lwt.return
                  [XHTML.M.div
                     ~a:[XHTML.M.a_class ["pencilmenucontainer"]]
                     [XHTML.M.img
                        ~src:img ~alt:"edit"
                        ~a:[XHTML.M.a_class ["pencilmenuimg"]]
                        ();
                      XHTML.M.div
                        ~a:[XHTML.M.a_class ["pencilmenu"]]
                        [XHTML.M.div
                           ~a:[XHTML.M.a_class ["wikiboxmenutitle"]]
                           [XHTML.M.pcdata title];
                         menu;
                        ]
                     ]
                  ]
            | `Linear ->
                Lwt.return
                  [XHTML.M.div
                     ~a:[XHTML.M.a_class ["wikiboxlinearmenu"]]
                     [XHTML.M.div
                        ~a:[XHTML.M.a_class ["wikiboxmenutitle"]]
                        [XHTML.M.pcdata title];
                      menu;
                     ]
                  ]

  method display_menu_box
                  ~bi
                  ~classes
                  ?active_item ?special_box ?title
                  ~wb content =

    self#box_menu ~bi ?special_box ?active_item ?title wb >>= fun menu ->
    let classes = if menu = [] then classes else interactive_class::classes in
    Lwt.return
      [XHTML.M.div
         ~a:[XHTML.M.a_class classes]
         (  menu
          @ [XHTML.M.div ~a:[XHTML.M.a_class ["boxcontent"]] content]
         )
      ]

  method draw_edit_form
      ~rows ~cols
      wb
      warning1 warning2
      curversion
      content
      previewonly
      (actionname, ((wbname, versionname), contentname)) =
    [XHTML.M.p
       (   warning1
        @ (Ocsimore_common.input_opaque_int32 ~value:wb wbname
        :: Eliom_predefmod.Xhtml.int32_input ~input_type:`Hidden
             ~name:versionname ~value:curversion ()
        :: Eliom_predefmod.Xhtml.textarea
              ~a:[XHTML.M.a_class ["wikitextarea"]]
              ~name:contentname ~rows ~cols
              ~value:content ()
        :: [XHTML.M.br ()])
        @  warning2
        @ (Eliom_predefmod.Xhtml.string_button
             ~name:actionname ~value:"preview"
             [XHTML.M.pcdata "Preview"]
        :: (if previewonly
            then []
            else [Eliom_predefmod.Xhtml.string_button
                    ~name:actionname ~value:"save"
                    [XHTML.M.pcdata "Save"]
                 ]
           ))
       )
    ]


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
            [XHTML.M.em [XHTML.M.pcdata "Warning: "];
             XHTML.M.pcdata
               !Language.messages.Language.wikitext_edition_conflict1;
             XHTML.M.br (); XHTML.M.br ();
            ],
            [XHTML.M.br ();
             XHTML.M.strong
               [XHTML.M.em [XHTML.M.pcdata "Warning: "];
                XHTML.M.pcdata
                  !Language.messages.Language.wikitext_edition_conflict1;
               ];
             XHTML.M.br ();
            ]
           )

       | None -> Lwt.return (version, [], [])
    ) >>= fun (curversion, warning1, warning2)  ->
    Lwt.return
      (classes,
       Eliom_predefmod.Xhtml.post_form
         ~a:[XHTML.M.a_accept_charset "utf-8"]
         ~service:Wiki_services.action_send_wikiboxtext ~sp
         (self#draw_edit_form ~rows ~cols wb warning1 warning2 curversion
            content previewonly) ())

  (* Wikitext in editing mode, with an help box on the syntax of the wiki *)
  method display_wikitext_edit_form_help
      ~bi ~classes
      ?rows ?cols
      ~previewonly
      ~wb data =

    Wiki.get_admin_wiki ()              >>= fun { wiki_id = admin_wiki } ->
    Wiki_sql.get_wikipage_info
      ~wiki:admin_wiki
      ~page:Wiki_widgets_interface.wikisyntax_help_name
                                  >>= fun { wikipage_wikibox = wb_help } ->
    error_box#bind_or_display_error
      (Wiki_data.wikibox_content bi.bi_rights bi.bi_sp wb_help)
      (self#display_wikiboxcontent ~bi ~classes:["wikihelp"])
    >>= self#display_basic_box                                 >>= fun b ->
    self#display_wikitext_edit_form ~bi ~classes:[] ?rows ?cols
      ~previewonly ~wb data                               >|= fun (_, f) ->
    (classes, [b; f])


  (* Css in editing mode *)
  method display_css_edit_form
      ~bi ~classes
      ?(rows=25) ?(cols=80)
      ~wb ~wbcss ~wikipage
      (content, boxversion) =

    let content = match content with
      | None -> "/* Deleted CSS */"
      | Some content -> content
    and sp = bi.bi_sp in
    Wiki.modified_wikibox wbcss boxversion >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            [XHTML.M.em [XHTML.M.pcdata "Warning: "];
             XHTML.M.pcdata !Language.messages.Language.css_edition_conflict;
             XHTML.M.br (); XHTML.M.br ();
            ]
           )

       | None -> Lwt.return (boxversion, [])
    ) >>= fun (curversion, warning)  ->
    let draw_form ( (wbname,
                     (((wikiname, wikipagename),
                       wbcssname),
                      versionname)),
                    contentname) =
      [XHTML.M.p
         (List.flatten
            [warning;
             [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
              Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssname;
              Ocsimore_common.input_opaque_int32 ~value:(fst wikipage) wikiname;
             ];
             (match snd wikipage with
                | None -> []
                | Some page ->
                    [Eliom_predefmod.Xhtml.string_input
                       ~name:wikipagename ~input_type:`Hidden ~value:page ()
                    ]);
             [Eliom_predefmod.Xhtml.int32_input
                ~input_type:`Hidden ~name:versionname ~value:curversion ();
              Eliom_predefmod.Xhtml.textarea
                ~name:contentname ~rows ~cols ~value:content ();
              XHTML.M.br ();
              Eliom_predefmod.Xhtml.button
                ~button_type:`Submit [XHTML.M.pcdata"Save"];
             ];
            ]
         )
      ]
    in
    Lwt.return
      (classes,
       [Eliom_predefmod.Xhtml.post_form
          ~a:[XHTML.M.a_accept_charset "utf-8"]
          ~service:Wiki_services.action_send_css ~sp draw_form ()]
      )


  (** Edition of the permissions of a wikibox *)
  (** As usual, the function supposes that the user has enough rights
      to change the permissions, as this will be checked by the service
      (and the user should not have access to the page otherwise). We
      also suppose that boxrights is set to true for the wiki *)
  method display_edit_wikibox_perm_form ~bi ~classes wb =
    Wiki_sql.get_wikibox_info wb >>= fun { wikibox_special_rights = sr } ->
    let bt_change value textbt =
      let mform (wbname, srname) =
        [XHTML.M.div
           [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
            Ocsimore_lib.hidden_bool_input ~value srname;
            Eliom_predefmod.Xhtml.button
              ~button_type:`Submit [XHTML.M.pcdata textbt];
           ]
        ]
      in
      Eliom_predefmod.Xhtml.post_form
        ~a:[XHTML.M.a_accept_charset "utf-8"]
        ~service:Wiki_services.action_set_wikibox_special_permissions
        ~sp:bi.bi_sp mform ()
    in
    if sr = false then
      let msg = "The permissions for this wikibox are currently inherited from \
                 the wiki. Press this button if you want to use special \
                 permissions."
      and bt = bt_change true "Use specific permissions"
      in
      Lwt.return (classes, [XHTML.M.pcdata msg; bt])
    else
      user_widgets#form_edit_awr ~text_prefix:"Wikibox " ~sp:bi.bi_sp
        ~grps:Wiki.wikibox_grps ~arg:wb ()
      >>= fun (formedit_hd, formedit_tl) ->
      let msg =
        "This wikibox has specific permissions. You can edit them below, or \
         use the button next to revert to the wiki generic permissions."
      in
      let bt = bt_change false "Use generic wiki permissions"
      and msg2 =
        XHTML.M.p
          [XHTML.M.pcdata "If you edit the permissions below, do ";
           XHTML.M.em [XHTML.M.pcdata "not"];
           XHTML.M.pcdata " remove yourself from the admin group, as you will \
                            no longer be allowed to edit permissions \
                            afterwards.";
          ]
      in
      Lwt.return
        (classes,
         [XHTML.M.pcdata msg;
          bt;
          msg2;
          XHTML.M.table
            ~a:[XHTML.M.a_class ["table_admin"]]
            (XHTML.M.tr
              (XHTML.M.th [XHTML.M.pcdata "Role"])
              [XHTML.M.th [XHTML.M.pcdata "Current users in this role"]]
            )
            (formedit_hd :: formedit_tl)
         ]
        )



  (** Form to edit the description and container of a wiki *)
  method private display_edit_wiki_metadata ~sp ~classes ?wb wiki =
    Wiki_sql.get_wiki_info_by_id wiki
        >>= fun { wiki_descr = descr; wiki_container = container } ->
    let form (wbname, (wikiname, (descrname, containername))) =
      [XHTML.M.h2 [XHTML.M.pcdata "Wiki properties"];
       XHTML.M.p
         (List.flatten
            [[Ocsimore_common.input_opaque_int32 ~value:wiki wikiname];
             (match wb with
                | None -> []
                | Some wb ->
                    [Ocsimore_common.input_opaque_int32 ~value:wb wbname]);
             [XHTML.M.pcdata "Description: ";
              Eliom_predefmod.Xhtml.string_input ~name:descrname
                ~input_type:`Text ~value:descr ();
              XHTML.M.br ();
              XHTML.M.pcdata "Container wikibox: ";
              Ocsimore_common.input_opaque_int32_opt ~hidden:false
                ~value:container containername;
              Eliom_predefmod.Xhtml.button ~button_type:`Submit
                   [XHTML.M.pcdata "Save"];
             ];
            ]
         );
      ]
    in
    let form = Eliom_predefmod.Xhtml.post_form ~sp
      ~a:[XHTML.M.a_accept_charset "utf-8"]
      ~service:Wiki_services.action_send_wiki_metadata form
      ()
    in Lwt.return (classes, [form])


  method private display_edit_wiki_option_form
         ~sp
         ~classes ?wb
         ~options ~perms
         wiki =
    (if options then
       self#display_edit_wiki_metadata ~sp ~classes ?wb wiki
     else Lwt.return ([], [])
    ) >>= fun (cl1, f1) ->
    (if perms then
       self#display_edit_wiki_perm_form ~sp ~classes ?wb wiki
     else
       Lwt.return ([], [])
    ) >>= fun (cl2, f2) ->
    Lwt.return (cl1 @ cl2, f1 @ f2)


  (** Form for the permissions of a wiki; The [wb] argument is the wikibox
      which will be overridden with an error message if the save fails *)
  method display_edit_wiki_perm_form ~sp ~classes ?wb wiki =
    let aux g text =
      user_widgets#form_edit_group ~sp ~group:(g $ wiki)
        ~text:[XHTML.M.p
                 ~a:[XHTML.M.a_class ["eliom_inline"]]
                 [XHTML.M.strong [XHTML.M.pcdata text]]
              ]
        ~show_edit:true
    in
    aux Wiki.wiki_admins "Administer the wiki" ()                  >>= fun f1 ->
    aux Wiki.wiki_subwikiboxes_creators "Create subwikiboxes" ()   >>= fun f2 ->
    aux Wiki.wiki_wikipages_creators "Create wikipages" ()         >>= fun f3 ->
    aux Wiki.wiki_wikiboxes_creators "Create wikiboxes" ()         >>= fun f4 ->
    aux Wiki.wiki_css_creators "Create CSS" ()                     >>= fun f5 ->
    aux Wiki.wiki_wikiboxes_deletors "Delete wikiboxes" ()         >>= fun f6 ->
    user_widgets#form_edit_awr ~sp ~text_prefix:"Wikiboxes"
      ~grps:Wiki.wiki_wikiboxes_grps ~arg:wiki ()      >>= fun (f7_hd, f7_tl) ->
    aux Wiki.wiki_files_readers "Read static files" ()             >>= fun f8 ->
    aux Wiki.wiki_wikiboxes_src_viewers "View wikiboxes source" () >>= fun f9 ->
    aux Wiki.wiki_wikiboxes_oldversion_viewers "View wikiboxes old versions" ()
                                                                  >>= fun f10 ->
    aux Wiki.wiki_metadata_editors "Edit the metadata of the wiki" ()
                                                                  >>= fun f11 ->

    let form =
      [XHTML.M.h2
         [XHTML.M.pcdata ("Permissions for wiki " ^ string_of_wiki wiki)];
       XHTML.M.p
         [XHTML.M.em [XHTML.M.pcdata "(inherited permissions are not shown)"]];
       XHTML.M.table
         ~a:[XHTML.M.a_class ["table_admin"]]
         (XHTML.M.tr
            (XHTML.M.th [XHTML.M.pcdata "Role"])
            [XHTML.M.th [XHTML.M.pcdata "Current users in the group"]]
         )
         (   f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: f8 :: f9 :: f10 :: f11 :: f7_hd
          :: f7_tl
         )
      ]
    in
    Lwt.return (classes, form)

  (* Auxiliary method for code factorization *)
  method private menu_box_aux
      ?title ?active_item
      cl wb ~bi ?special_box
      (classes, content) =
    self#display_menu_box ~classes:(cl::classes) ?active_item ?title ~bi
      ?special_box ~wb content

  method private menu_edit_wikitext wb =
    let title = Printf.sprintf "Edit - Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_Edit editform_class wb

  method private menu_edit_wikibox_perms wb =
    let title = Printf.sprintf "Permissions - Wikibox %s"
      (string_of_wikibox wb)
    in
    self#menu_box_aux
      ~title ~active_item:Menu_EditWikiboxPerms editform_class wb

  method private menu_edit_css_perms wb (wiki, page) =
    let title = Printf.sprintf "CSS Permissions - wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page)
    in
    self#menu_box_aux ~title ~active_item:Menu_Css editform_class wb

  method private menu_edit_wiki_options wb wiki =
    let title = Printf.sprintf "Options - Wiki %s" (string_of_wiki wiki) in
    self#menu_box_aux ~title ~active_item:Menu_EditWikiOptions editform_class wb

  method private menu_wikitext_history wb =
    let title = Printf.sprintf "History - Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_History history_class wb

  method private menu_css_history wb (wiki, page) =
    let title = Printf.sprintf "CSS history, wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page)
    in
    self#menu_box_aux ~title ~active_item:Menu_Css css_history_class wb

  method private menu_edit_wikipage_properties wb (wiki, page) =
    let title = Printf.sprintf "Page properties, wiki %s, page %s"
      (string_of_wiki wiki) page
    in
    self#menu_box_aux ~title ~active_item:Menu_WikipageProperties
      wikipage_properties_class wb

  method private menu_view wb =
    let title = Printf.sprintf "Wikibox %s" (string_of_wikibox wb) in
    self#menu_box_aux ~title ~active_item:Menu_View view_class wb

  method private menu_old_wikitext wb version =
    let title = Printf.sprintf "Old version - Wikibox %s, version %ld"
      (string_of_wikibox wb) version
    in
    self#menu_box_aux ~title oldwikibox_class wb

  method private menu_old_css wb (wiki, page) =
    let title = Printf.sprintf "Old css version, wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page)
    in
    self#menu_box_aux ~title oldwikibox_class wb

  method private menu_src_wikitext wb version =
    let title = Printf.sprintf "Source - Wikibox %s, version %ld"
      (string_of_wikibox wb) version
    in
    self#menu_box_aux ~title srcwikibox_class wb

  method private menu_edit_css wb (wiki, page) =
    let title = Printf.sprintf "CSS for wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page)
    in
    self#menu_box_aux ~title ~active_item:Menu_Css css_class wb

  method private menu_edit_css_list wb (wiki, page) =
    let title = Printf.sprintf "All CSS for wiki %s, %s"
      (string_of_wiki wiki) (self#css_wikibox_text page)
    in
    self#menu_box_aux ~title ~active_item:Menu_Css css_class wb

  method private css_wikibox_text = function
    | None -> "global stylesheet"
    | Some "" -> "main page"
    | Some page ->  "page " ^ page


  method display_wikitext_history ~bi ~classes ~wb l =
    let sp = bi.bi_sp in
    Lwt_list.map_s
      (fun (version, _comment, author, date) ->
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >|= fun { user_fullname = author } ->
         [XHTML.M.pcdata (Int32.to_string version);
          XHTML.M.pcdata ". ";
          XHTML.M.pcdata (CalendarLib.Printer.Calendar.to_string date);
          XHTML.M.pcdata " ";
          XHTML.M.em [XHTML.M.pcdata " by "; XHTML.M.pcdata author];
          XHTML.M.pcdata " ";
          Eliom_predefmod.Xhtml.a ~sp ~service:Wiki_services.action_old_wikibox
            [XHTML.M.pcdata "view"] (wb, version);
          XHTML.M.pcdata " (";
          Eliom_predefmod.Xhtml.a ~sp ~service:Wiki_services.action_src_wikibox
            [XHTML.M.pcdata "source"] (wb, version);
          XHTML.M.pcdata ")";
          XHTML.M.br ();
         ]
      )
      l
    >|= List.flatten
    >|= fun l -> (classes, l)

  method display_css_history ~bi ~classes ~wb ~wbcss ~wikipage l =
    let sp = bi.bi_sp in
    Lwt_list.map_s
      (fun (version, _comment, author, date) ->
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >|= fun { user_fullname = author } ->
         [XHTML.M.pcdata (Int32.to_string version);
          XHTML.M.pcdata ". ";
          XHTML.M.pcdata (CalendarLib.Printer.Calendar.to_string date);
          XHTML.M.pcdata " ";
          XHTML.M.em [XHTML.M.pcdata "by "; XHTML.M.pcdata author];
          Eliom_predefmod.Xhtml.a ~sp
            ~service:Wiki_services.action_old_wikiboxcss
            [XHTML.M.pcdata "view"](wb, ((wikipage, wbcss), version));
          XHTML.M.br ();
         ]
      )
      l
    >|= List.flatten
    >|= fun l -> (classes, l)

   method private display_edit_wikipage_properties
                   ~bi ~classes
                   ~(wb:wikibox) wp =

     let (wiki, page) = wp in
     Wiki_sql.get_wikipage_info wiki page >>= fun wp ->
     let draw_form (wbname,
                    ((wikiidname, pagename),
                     (titlename,
                      ((wbidname, pathname))))) =
       [XHTML.M.p
         [Ocsimore_common.input_opaque_int32 ~value:wiki wikiidname;
          Ocsimore_common.input_opaque_int32 ~value:wb wbname;
          Eliom_predefmod.Xhtml.string_input
            ~name:pagename ~input_type:`Hidden ~value:page ();
          XHTML.M.pcdata "Title of the wikipage: ";
          Eliom_predefmod.Xhtml.string_input ~name:titlename
            ~input_type:`Text
            ~value:(match wp.wikipage_title with None -> "" | Some s -> s)
            ();
          XHTML.M.pcdata "(if blank, the title of the wiki will be used";
          XHTML.M.br ();

          XHTML.M.pcdata "Wikibox to which the wikipage points to. Leave blank \
                          to delete the wikipage. ";
          Ocsimore_common.input_opaque_int32_opt ~hidden:false
                 ~value:(Some wp.wikipage_wikibox) wbidname;
          XHTML.M.br ();

          XHTML.M.pcdata "Path of the wikipage inside the wiki";
          Eliom_predefmod.Xhtml.string_input ~name:pathname
                 ~input_type:`Text ~value:wp.wikipage_page ();
          XHTML.M.pcdata "Notice that changing this path will ";
          XHTML.M.em [XHTML.M.pcdata "not"];
          XHTML.M.pcdata " update links to this wikipage.";
          XHTML.M.br ();

          Eliom_predefmod.Xhtml.button
            ~button_type:`Submit [XHTML.M.pcdata "Save"];
         ]
       ]
     in
     Lwt.return
      (classes,
       [Eliom_predefmod.Xhtml.post_form
          ~a:[XHTML.M.a_accept_charset "utf-8"]
          ~service:Wiki_services.action_send_wikipage_properties ~sp:bi.bi_sp
          draw_form ()]
      )

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
      let l = [ `Braille; `Embossed; `Handheld; `Print;
                `Projection; `Screen; `Speech; `TTY; `TV]
      in
      let in_media m = List.mem m media in
      let l =
        List.map
          (fun s -> Eliom_predefmod.Xhtml.Option ([], s, None, in_media s))
          l
      in
      Eliom_predefmod.Xhtml.user_type_multiple_select
        (fun x -> Wiki_types.string_of_media_type [x])
        ~name
        ~a:[XHTML.M.a_style "vertical-align: top"; XHTML.M.a_size 5]
        (Eliom_predefmod.Xhtml.Option ([],
                                       `All,
                                       Some (XHTML.M.pcdata "all media"),
                                       in_media `All)
        )
        l
    in
    let aux ((wbcss, media, rank), (_, ver)) =
      bi.bi_rights#can_view_history ~sp wbcss                 >>= fun csshist ->
      bi.bi_rights#can_write_wikibox ~sp wbcss                >>= fun csswr ->
      bi.bi_rights#can_set_wikibox_specific_permissions ~sp wbcss
                                                              >>= fun cssperm ->
      let wbcss_ = ((wiki, page), wbcss) in
      let v1 = if csshist then
        [Eliom_predefmod.Xhtml.a ~sp ~service:Wiki_services.action_css_history
           [XHTML.M.pcdata "History"] (wb, wbcss_)
        ]
      else []
      and v2 = if csswr then
        [Eliom_predefmod.Xhtml.a ~sp ~service:Wiki_services.action_edit_css
           [XHTML.M.pcdata "Edit"] (wb, (wbcss_, None))
        ]
      else []
      and v3 = if cssperm then
        [Eliom_predefmod.Xhtml.a ~sp
           ~service:Wiki_services.action_css_permissions
           [XHTML.M.pcdata "Permissions"] (wb, wbcss_)
        ]
      else []
      and v4 =
        [Eliom_predefmod.Xhtml.a ~sp
           ~service:Wiki_services.action_old_wikiboxcss
           [XHTML.M.pcdata "View"] (wb, (wbcss_, ver))
        ]
      in
      let fupdate (wbn, (((((wikin, wpn), wbcssn), newwbcssn), median), rankn))=
        [XHTML.M.div
           ~a:[XHTML.M.a_class ["eliom_inline"]]
           (List.flatten
              [[Ocsimore_common.input_opaque_int32 ~value:wb wbn;
                Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssn;
                Ocsimore_common.input_opaque_int32 ~value:wiki wikin;
                XHTML.M.pcdata "CSS ";
                Eliom_predefmod.Xhtml.int32_input ~input_type:`Text
                   ~a:[XHTML.M.a_size 2] ~value:rank ~name:rankn ();
                XHTML.M.pcdata "(Id ";
                XHTML.M.pcdata (Opaque.int32_t_to_string wbcss);
                XHTML.M.pcdata ") ";
                Ocsimore_common.input_opaque_int32 ~value:wbcss newwbcssn];
               (match page with
                  | None -> []
                  | Some page ->
                      [Eliom_predefmod.Xhtml.string_input ~name:wpn
                           ~input_type:`Hidden ~value:page ()
                      ]
               );
               [select_media median media;
                XHTML.M.pcdata " ";
                Eliom_predefmod.Xhtml.button ~button_type:`Submit
                   [XHTML.M.pcdata " Update media and CSS order"];
                XHTML.M.pcdata " "];
              ]
           )
        ]
      and fdelete
              (wbn,
               (((((wikin, wpn),
                   wbcssn),
                  _newwbcssn),
                 _median),
                rankn)) =
        [XHTML.M.div
           ~a:[XHTML.M.a_class ["eliom_inline"]]
           (List.flatten
              [[Ocsimore_common.input_opaque_int32 ~value:wb wbn;
                Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssn;
                Ocsimore_common.input_opaque_int32 ~value:wiki wikin;
                Eliom_predefmod.Xhtml.int32_input ~input_type:`Hidden
                   ~value:rank ~name:rankn ()];
               (match page with
                  | None -> []
                  | Some page ->
                      [Eliom_predefmod.Xhtml.string_input ~name:wpn
                           ~input_type:`Hidden ~value:page ()
                      ]
               );
               [Eliom_predefmod.Xhtml.button ~button_type:`Submit
                   [XHTML.M.pcdata " Remove CSS"];
               ];
              ]
           )
        ]
      in
      if can_create then Lwt.return
        [XHTML.M.div
           (List.flatten
              [[Eliom_predefmod.Xhtml.post_form ~keep_get_na_params:true
                  ~a:[XHTML.M.a_accept_charset "utf-8";
                      XHTML.M.a_class ["eliom_inline"]]
                  ~service:Wiki_services.action_send_css_options ~sp fupdate ();
                Eliom_predefmod.Xhtml.post_form ~keep_get_na_params:true
                  ~a:[XHTML.M.a_accept_charset "utf-8";
                      XHTML.M.a_class ["eliom_inline"]]
                  ~service:Wiki_services.action_send_css_options ~sp fdelete ();
                XHTML.M.pcdata " "];
               v4;
               [XHTML.M.pcdata " / "];
               v1;
               [XHTML.M.pcdata " / "];
               v2;
               [XHTML.M.pcdata " / "];
               v3;
              ]
           )
        ]
      else Lwt.return
        [XHTML.M.p
           (List.flatten
              [v4;
               [XHTML.M.pcdata " / "];
               v1;
               [XHTML.M.pcdata " / "];
               v2;
               [XHTML.M.pcdata " / "];
               v3;
              ]
           )
        ]
    in
    (if l = [] then
      Lwt.return [XHTML.M.pcdata "There are currently no CSS"]
     else
       Lwt_list.fold_left_s
         (fun (x : Xhtmltypes.div_content XHTML.M.elt list) e ->
            aux e >|= fun e -> x @ (XHTML.M.br () :: e)
         )
         []
         l
    ) >>= fun forms ->
    (if can_create then
       let mform (wbn, ((wikin, wpn), (median, wbcssn))) =
         [XHTML.M.p
           (List.flatten
             [[XHTML.M.pcdata "Add another CSS: ";
               XHTML.M.br ();
               Ocsimore_common.input_opaque_int32 ~value:wb wbn;
               Ocsimore_common.input_opaque_int32 ~value:wiki wikin];
              (match page with
                 | None -> []
                 | Some page ->
                     [Eliom_predefmod.Xhtml.string_input ~name:wpn
                        ~input_type:`Hidden ~value:page ()
                     ]
              );
              [XHTML.M.pcdata "Media: ";
               select_media median [`All];
               XHTML.M.br ();
               XHTML.M.pcdata "Id: ";
               Ocsimore_common.input_opaque_int32_opt ~hidden:false wbcssn;
               XHTML.M.pcdata
                 "Type an existing CSS id, or leave blank to create a new CSS";
               XHTML.M.br ();
               Eliom_predefmod.Xhtml.button ~button_type:`Submit
                 [XHTML.M.pcdata "Add"]];
             ]
           )
         ]
       in Lwt.return
            (   forms
             @ [XHTML.M.br ();
                Eliom_predefmod.Xhtml.post_form ~sp
                  ~a:[XHTML.M.a_accept_charset "utf-8"]
                  ~service:Wiki_services.action_create_css mform ();
               ]
            )
     else
       Lwt.return []
    ) >|= fun r ->
    (classes, [XHTML.M.div ~a:[XHTML.M.a_class classes] r])


  method display_interactive_wikibox_aux
      ~bi
      ?(classes=[]) ?rows ?cols
      ?special_box wb =

    let classes = wikibox_class::classes in
    let sp = bi.bi_sp in
    let override = Wiki_services.get_override_wikibox ~sp in
    (match override with
       | Some (wb', override) when wb = wb' ->
           self#display_overriden_interactive_wikibox ~bi ~classes ?rows ?cols
             ?special_box ~wb_loc:wb ~override () >|= fun (b, c) ->
           ([XHTML.M.div ~a:[XHTML.M.a_class ["overridden"]] b], c)

       | _ ->
           Lwt.catch
             (fun () ->
                Wiki_data.wikibox_content bi.bi_rights sp wb >|= fun c ->
                (Lwt.return c, true)
             )
             (fun e -> Lwt.return (Lwt.fail e, false)) >>= fun (c, code) ->
           error_box#bind_or_display_error c
             (self#display_wikiboxcontent ~classes
                ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
           >>= (self#menu_view ~bi ?special_box wb) >|= fun r ->
           (r, code)
    ) >|= fun (r, code) ->
    (self#wrap_error ~sp ~wb r, code)

  method display_overriden_interactive_wikibox
    ~bi ?(classes=[]) ?rows ?cols ?special_box ~wb_loc ~override () =
    let sp = bi.bi_sp in
    let display_error () =
      Lwt.return
        ([error_box#display_error_box
           ~classes:(frozen_wb_class::classes)
           ~message:"You are not allowed to do that."
           ()],
         false)
    and ok r = Lwt.return (r, true)
    in
    let place_second_in_list (x,y) = Lwt.return (x,[y]) in
    match override with
      | EditWikitext wb ->
          (bi.bi_rights#can_write_wikibox ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content' bi.bi_rights bi.bi_sp wb)
                  (fun x ->
                     self#display_wikitext_edit_form_help ~bi ?cols ?rows
                       ~previewonly:true ~wb ~classes x >|= fun (s,b) ->
                     (s, (b :Xhtmltypes.block XHTML.M.elt list :> Xhtmltypes.div_content XHTML.M.elt list))
                  )
                >>= (self#menu_edit_wikitext ~bi ?special_box wb_loc)
                >>= ok
            | false -> display_error ()
          )

      | EditCss ((wikipage, wbcss), css) ->
          (bi.bi_rights#can_write_wikibox ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (match css with
                     | None -> Wiki_data.wikibox_content' bi.bi_rights sp wbcss
                     | Some (content, version) ->
                     Lwt.return (Some content, version)
                  )
                  (self#display_css_edit_form ~bi ?cols ?rows
                     ~wb:wb_loc ~wbcss ~wikipage ~classes)
                >>= (self#menu_edit_css ~bi ?special_box wb_loc wikipage)
                >>= ok
            | false -> display_error ()
          )

      | EditCssList wikipage ->
          (self#display_edit_css_list ~bi ~classes ~wb:wb_loc wikipage)
          >>= (self#menu_edit_css_list ~bi ?special_box wb_loc wikipage)
          >>= ok

      | EditWikiboxPerms wb ->
          (bi.bi_rights#can_set_wikibox_specific_permissions ~sp wb >>= function
            | true ->
                self#display_edit_wikibox_perm_form ~bi ~classes wb
                >>=  (self#menu_edit_wikibox_perms ~bi ?special_box wb_loc)
                >>= ok
          | false -> display_error ()
          )

      | EditWikiOptions wiki ->
          (bi.bi_rights#can_set_wiki_permissions ~sp wiki >>= fun b1 ->
           bi.bi_rights#can_edit_metadata ~sp wiki        >>= fun b2 ->
           match b1 || b2 with
            | true ->
                (self#display_edit_wiki_option_form ~sp:bi.bi_sp ~classes
                   ~wb:wb_loc ~options:b2 ~perms:b1 wiki)
                >>= (self#menu_edit_wiki_options ~bi ?special_box wb_loc wiki)
                >>= ok
            | false -> display_error ()
          )

      | PreviewWikitext (wb, (content, version)) ->
          (bi.bi_rights#can_write_wikibox ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~sp ~version~rights:bi.bi_rights wb
                   >|= fun (syntax, _, _) -> (syntax, (Some content, version)))
                  (fun (syntax, (content, version as cv)) ->
                     let bi' =
                       { (Wiki_widgets_interface.add_ancestor_bi wb bi) with
                         bi_menu_style = `None
                       }
                     in
                     self#display_wikiboxcontent ~classes:[] ~bi:bi'
                       (syntax, content, version)           >>= fun (_, pp) ->
                     self#display_basic_box ([preview_class], pp)
                                                            >>= fun prev ->
                     self#display_wikitext_edit_form_help ~classes:[]
                       ~bi ?cols ?rows ~previewonly:false ~wb cv
                                                            >|= fun (_, form) ->
                     (classes,
                      ((   XHTML.M.p
                             ~a:[XHTML.M.a_class [box_title_class]]
                             [XHTML.M.pcdata "Preview"]
                        :: prev
                        :: form)
                       : Xhtmltypes.block XHTML.M.elt list
                       :> Xhtmltypes.div_content XHTML.M.elt list)
                     )
                  )
                >>= (self#menu_edit_wikitext ~bi ?special_box wb_loc)
                >>= ok
            | false -> display_error ()
          )

      | EditWikipageProperties wp ->
          (bi.bi_rights#can_admin_wikipage ~sp wp >>= function
             | true ->
                self#display_edit_wikipage_properties ~bi ~classes ~wb:wb_loc wp
                >>= (self#menu_edit_wikipage_properties ~bi ?special_box
                       wb_loc wp)
                >>= ok
             | false -> display_error ()
          )

      | History wb ->
          (bi.bi_rights#can_view_history ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_history bi.bi_rights sp wb)
                  (self#display_wikitext_history ~bi ~classes ~wb)
                >>= (self#menu_wikitext_history ~bi ?special_box wb_loc)
                >>= ok
            | false -> display_error ()
          )

      | CssHistory ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_view_history ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_history bi.bi_rights sp wbcss)
                  (self#display_css_history ~bi ~classes ~wb:wb_loc ~wbcss
                     ~wikipage:(wiki,wikipage))
                >>= (self#menu_css_history ~bi ?special_box wb_loc
                        (wiki, wikipage))
                >>= ok
            | false -> display_error ()
          )

      | CssPermissions ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_set_wikibox_specific_permissions ~sp wbcss
                                                                   >>= function
            | true ->
                (self#display_edit_wikibox_perm_form ~bi ~classes wbcss)
                >>= (self#menu_edit_css_perms ~bi ?special_box wb_loc
                       (wiki, wikipage))
                >>= ok
          | false -> display_error ()
          )

      | Oldversion (wb, version) ->
          (bi.bi_rights#can_view_oldversions ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~sp ~version ~rights:bi.bi_rights wb)
                  (self#display_wikiboxcontent ~classes
                     ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
                >>= (self#menu_old_wikitext ~bi ?special_box wb_loc version)
                >>= ok
            | false -> display_error ()
          )

      | CssOldversion (((wiki, page), wbcss), version) ->
          (bi.bi_rights#can_view_oldversions ~sp wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~sp ~version
                     ~rights:bi.bi_rights wbcss)
                  (self#display_raw_wikiboxcontent ~classes)
                >>= (self#menu_old_css ~bi ?special_box wb_loc (wiki, page))
                >>= ok
            | false -> display_error ()
          )

      | Src (wb, version)->
          (bi.bi_rights#can_view_oldversions_src ~sp wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~sp
                     ~version ~rights:bi.bi_rights wb)
                  (self#display_raw_wikiboxcontent ~classes)
                >>= (self#menu_src_wikitext ~bi ?special_box wb_loc version)
                >>= ok
            | false -> display_error ()
          )


   method display_interactive_wikibox
     ~bi ?(classes=[]) ?rows ?cols ?special_box wb =
     add_wiki_css_header bi.bi_sp;
     self#display_interactive_wikibox_aux
       ~bi ?rows ?cols ~classes ?special_box wb
     >|= fst (*fun (r, _allowed) -> Lwt.return r*)


   method css_header ~sp ?page wiki =
     let css_url_service service args media =
       Eliom_predefmod.Xhtml.css_link
         ?a:(if media = []
             then None
             else Some [XHTML.M.a_media media])
         ~uri:(Eliom_predefmod.Xhtml.make_uri ~service ~sp args)
       ()
     in
     (match Wiki_self_services.find_servwikicss wiki with
        | None -> Lwt.return []
        | Some wikicss_service ->
            Wiki_sql.get_css_wikibox_for_wiki wiki >|=
            List.map
              (fun (wb, media, _) -> css_url_service wikicss_service wb media)
     )
     >>= fun css ->
     match page with
       | None -> Lwt.return css
       | Some page ->
           Wiki_sql.get_css_wikibox_for_wikipage ~wiki ~page >>= fun l ->
           let ll =
             List.map
               (fun (wb, media, _) ->
                  css_url_service
                    Wiki_services.pagecss_service
                    ((wiki, page), wb)
                    media
               )
               l
           in
           Lwt.return (css @ ll)

   method private display_container
     ~sp ~wiki ~menu_style ~page:(page, page_list) ~gen_box =
     Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
     let rights = Wiki_models.get_rights wiki_info.wiki_model
     and wb_container = wiki_info.wiki_container in
     gen_box menu_style >>= fun (wbid, subbox, err_code, title) ->
     Wiki_widgets_interface.set_page_displayable sp err_code;

     (* We render the container, if it exists *)
     (match wb_container with
        | None -> Lwt.return [XHTML.M.div subbox]

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
     Page_site.html_page ~sp ~css ~title pagecontent >>= fun r ->
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
                      subbox,
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
                [XHTML.M.p
                  (List.flatten
                     [[Ocsimore_common.input_opaque_int32 ~value:wiki wikiidname
                      ];
                       (* Used to know where to display errors (only possible
                          if there is a container, otherwise we don't know
                          what to override *)
                      (match wb_container with
                         | None -> []
                         | Some container ->
                             [Ocsimore_common.input_opaque_int32
                                   ~value:container wbname]
                      );
                      [Eliom_predefmod.Xhtml.string_input ~name:pagename
                         ~input_type:`Hidden ~value:page ();
                       Eliom_predefmod.Xhtml.string_input
                         ~input_type:`Submit ~value:"Create it!" ();
                      ];
                     ]
                  )
                ]
              in
              rights#can_create_wikipages sp wiki >>= fun c ->
              let form =
                if c then
                  [Eliom_predefmod.Xhtml.post_form ~sp
                     ~service:Wiki_services.action_create_page draw_form ()
                  ]
                else []
              and err_msg = !Language.messages.Language.page_does_not_exist
              in
              Lwt.return
                (None,
                 (XHTML.M.p [XHTML.M.pcdata err_msg] :: form),
                 Wiki_widgets_interface.Page_404,
                 None)
          | e -> Lwt.fail e
       )
     )


   (* Displaying of an entire page. We just pass the proper rendering
   function to [display_container] *)
   method display_wikipage ~sp ~wiki ~menu_style ~page =
     self#display_wikipage_wikibox ~sp ~wiki ~page >>= fun g ->
     self#display_container ~sp ~wiki ~menu_style ~page
       ~gen_box:(fun x ->
                   (g x : (Wiki_types.wikibox option *
                           Xhtmltypes.block XHTML.M.elt list *
                           Wiki_widgets_interface.page_displayable *
                           string option) Lwt.t
                        :> (Wiki_types.wikibox option *
                            Xhtmltypes.div_content XHTML.M.elt list *
                            Wiki_widgets_interface.page_displayable *
                            string option) Lwt.t
                   )
                )


   method display_all_wikis ~sp =
     (* Lists of all wikis *)
     let l = ref [] in
     Wiki_sql.iter_wikis (fun w -> Lwt.return (l := w :: !l)) >>= fun () ->
     let l =
       List.sort (fun w1 w2 -> compare w1.wiki_title w2.wiki_title) !l
     in

     let line w =
       let img path text = [Page_site.icon ~sp ~path ~text] in
       let id = Opaque.int32_t_to_string w.wiki_id in
       let edit =
         Eliom_predefmod.Xhtml.a ~service:Wiki_services.edit_wiki ~sp
           (img "imgedit.png" "Edit wiki options") w.wiki_id
       in
       let edit_perm =
         Eliom_predefmod.Xhtml.a ~sp
           ~service:Wiki_services.edit_wiki_permissions_admin
           (img "imgeditperms.png" "View permissions") w.wiki_id
       in
       let page =
         match Wiki_self_services.find_servpage w.wiki_id with
           | None -> []
           | Some service ->
               [Eliom_predefmod.Xhtml.a ~service ~sp
                  (img "imgview.png" "View wiki root wikipage") []
               ]
       in
       (XHTML.M.tr
          (XHTML.M.td ~a:[XHTML.M.a_class ["wikiid"]] [XHTML.M.pcdata id])
          [XHTML.M.td ~a:[XHTML.M.a_class ["wikiname"]]
             [XHTML.M.strong [XHTML.M.pcdata w.wiki_title]];
           XHTML.M.td ~a:[XHTML.M.a_class ["wikidescr"]]
             [XHTML.M.pcdata w.wiki_descr];
           XHTML.M.td [edit];
           XHTML.M.td [edit_perm];
           XHTML.M.td page;
          ]
       )
     in
     let l = List.map line l in
     Lwt.return
       [XHTML.M.h1 [XHTML.M.pcdata "Existing Ocsimore wikis"];
        XHTML.M.table ~a:[XHTML.M.a_class ["table_admin"]]
          (XHTML.M.tr
             (XHTML.M.th [XHTML.M.pcdata "Id"])
             [XHTML.M.th [XHTML.M.pcdata "Wiki"];
              XHTML.M.th [XHTML.M.pcdata "Description"];
             ]
          )
          l;
       ]

end

class inline_wikibox
  (error_box : Widget.widget_with_error_box)
  (user_widgets: User_widgets.user_widget_class)
  : Wiki_widgets_interface.interactive_wikibox =
object (self)

  inherit dynamic_wikibox error_box user_widgets

  method draw_edit_form
         ~rows:_ ~cols:_
         wb warning1 warning2 curversion content
         previewonly
         (actionname, ((wbname, versionname), contentname)) =
    [XHTML.M.p
       (List.flatten
          [warning1;
           [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
            Eliom_predefmod.Xhtml.int32_input ~input_type:`Hidden
              ~name:versionname ~value:curversion ();
            Eliom_predefmod.Xhtml.string_input
              ~a:[XHTML.M.a_class ["wikitextarea"]]
              ~input_type:`Text ~name:contentname ~value:content ();
            XHTML.M.br ()];
           warning2;
           [Eliom_predefmod.Xhtml.string_button
              ~name:actionname ~value:"preview" [XHTML.M.pcdata "Preview"]];
           (if previewonly
            then []
            else [Eliom_predefmod.Xhtml.string_button ~name:actionname
                    ~value:"save" [XHTML.M.pcdata "Save"] ]);
          ]
       )
    ]


  method display_wikitext_edit_form_help
      ~bi
      ~classes ?rows ?cols
      ~previewonly ~wb
      data =
    self#display_wikitext_edit_form ~bi
      ~classes:[] ?rows ?cols ~previewonly ~wb data >|= fun (_, f) ->
    (classes, [f])


end
