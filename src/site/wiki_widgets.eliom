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

{shared{
  open Eliom_lib.Lwt_ops
}}
open Eliom_content
open User_sql.Types
open Wiki_widgets_interface
open Wiki_types

let preview_wikibox_content : (wikibox * string) option Eliom_reference.Volatile.eref =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope None

(* TODO Handle wikiboxes with multiple media (i.e. "screen print" or even "all") carefully! *)
let grouped_by_media wb_list_with_media =
  let rec insert_by_media wb_media = function
      [] -> [wb_media.media, [wb_media.wikibox]]
    | (media', wblist) :: rest when wb_media.media = media' ->
        (wb_media.media, wb_media.wikibox :: wblist) :: rest
    | pair :: rest -> pair :: insert_by_media wb_media rest in
  List.fold_right insert_by_media wb_list_with_media []

{client{
  let opera_hack = "onload_edit_window"
}}

let () =
  Ocsimore_appl.register
    ~service:Wiki_services.Ui.edit_service
    (fun wb () ->
      lwt wiki = Wiki_sql.wikibox_wiki wb in
      lwt wiki_info = Wiki_sql.get_wiki_info_by_id ~id:wiki in
      lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
      let heading = "Editing wikibox "^Wiki_types.string_of_wikibox wb in
      lwt _, opt_content, _version = Wiki_data.wikibox_content ~rights wb in
      let content = match opt_content with | Some c -> c | None -> "DELETED" in
      lwt () = Wiki_services.add_wiki_css_header () in
      lwt headers = Page_site.Header.generate_headers () in
      ignore {unit{
        Js.Unsafe.eval_string ("self.opener." ^ opera_hack ^ "(window)")
      }};
      Lwt.return Html5.D.(
        html
          (head (title (pcdata heading)) headers)
          (body [
            h1 [pcdata heading];
            Raw.textarea ~a:[
              a_id Wiki_services.preview_textarea_id;
              a_class ["wikitextarea"];
              a_rows 25; a_cols 80;
            ] (pcdata content);
            raw_input ~input_type:`Submit ~a:[a_id Wiki_services.save_id; a_value "Save"] ();
            Html5.F.pcdata " ";
            Html5.F.a
              ~service:(Wiki_services.get_wikisyntax_helper ())
              [Html5.F.pcdata "Wiki syntax helper"] ();
            Html5.F.pcdata ". The preview is available directly in the page you come from.";
          ])
      )
    )

let hidden_page_inputs (page_wiki, page_path) (page_wiki_name, (empty_page_path_name, page_path_name)) =
  Ocsimore_common.input_opaque_int32 ~value:page_wiki page_wiki_name ::
  match page_path with
      Some path ->
        page_path_name.Eliom_parameter.it
          (fun name value init -> Html5.D.string_input ~input_type:`Hidden ~name ~value () :: init)
          path
          []
    | None ->
        [Html5.D.user_type_input (fun () -> "")
           ~name:empty_page_path_name
           ~value:()
           ~input_type:`Hidden ()]

class wikibox_error_box = object

  inherit Widget.widget_with_error_box as error_box

  method! display_error_box ?classes ?message ?exc () =
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




class wikibox_aux (error_box : Widget.widget_with_error_box) : Wiki_widgets_interface.wikibox_aux = object (self)

  method display_basic_box : 'a 'b.
    _ * ([< Html5_types.div_content_fun] as 'b) Html5.F.elt list ->
      ([> Html5_types.div ] as 'a) Html5.F.elt Lwt.t =

    fun (classes, content) ->
    Lwt.return (Html5.F.div ~a:[Html5.F.a_class classes] content)

  method display_wikiboxcontent : 'a 'b.
    bi:_ -> classes:_ ->
      ([< Html5_types.flow5 ] as 'a) Html5.F.elt list Wiki_types.wikibox_content ->
        (Wiki_widgets_interface.classes *
           ([> Html5_types.flow5 ] as 'b) Html5.F.elt list) Lwt.t =

    fun ~bi ~classes (wiki_syntax, content, _ver as wb) ->
    lwt () = Wiki_services.add_wiki_css_header () in
    let wiki_parser = Wiki_models.get_flows_wiki_parser wiki_syntax in
    match content with
      | Some content ->
          wiki_parser bi content >|=
            fun x -> (classes, x)
      | _ -> self#display_raw_wikiboxcontent ~classes wb

  method display_raw_wikiboxcontent : 'a 'b.
    classes:_ ->
      'a Html5.F.elt list Wiki_types.wikibox_content ->
        (Wiki_widgets_interface.classes *
           ([> Html5_types.pre | Html5_types.em ] as 'b) Html5.F.elt list) Lwt.t =

    fun ~classes (_content_type, content, _ver) ->
    Lwt.return
      (classes,
       (match content with
          | Some content ->
              [Html5.F.pre [Html5.F.pcdata content]]
          | None ->
              [Html5.F.em [Html5.F.pcdata "/* Deleted content */"]]
       )
      )

  method wrap_error : 'a.
    wb:_ ->
      ([< Html5_types.div_content_fun > `Div ] as 'a) Html5.F.elt list ->
        'a Html5.F.elt list Lwt.t =

    fun ~wb r ->
      Wiki_services.get_wikibox_error () >|= function
        | Some (wb', exc) when Some wb = wb' ->
            let r = (r :> Html5_types.div_content_fun Html5.F.elt list) in
            let err_msg = error_box#display_error_box ~exc () in
            [Html5.F.div (err_msg :: r)]
        | _ -> r

 end


class frozen_wikibox (error_box : Widget.widget_with_error_box) : Wiki_widgets_interface.frozen_wikibox = object (self)

  inherit wikibox_aux error_box

  val frozen_wb_class = "frozen_wikibox"

  method display_frozen_wikibox : 'a.
    bi:_ -> ?classes:_ -> _ ->
      ([> Html5_types.div | Html5_types.p ] as 'a) Html5.F.elt list Lwt.t =

    fun ~bi ?(classes=[]) wikibox ->
      (try_lwt
        lwt res =
         error_box#bind_or_display_error
          (Wiki_data.wikibox_content ~rights:bi.bi_rights wikibox)
          (self#display_wikiboxcontent ~bi ~classes:(frozen_wb_class::classes))
        in
        lwt r = self#display_basic_box res in
        (self#wrap_error ~wb:wikibox [r] :> [ `Div | `P ] Html5.F.elt list Lwt.t)
      with
        | Ocsimore_common.Permission_denied ->
          Lwt.return
            [error_box#display_error_box
                ~classes:(frozen_wb_class::classes)
                ~message:"You are not allowed to see this content."
                ()]
        | e -> Lwt.fail e
       : [ `Div | `P ] Html5.F.elt list Lwt.t
       :> [> `Div | `P ] Html5.F.elt list Lwt.t)

 end

(** Displaying of a wikibox with viewing and/or editing rights. Takes
    as argument all the services needed to save modifications
    or navigate through viewing options *)
class dynamic_wikibox
    (error_box : Widget.widget_with_error_box)
    (user_widgets: User_widgets.user_widget_class)
  : interactive_wikibox =
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

    let preapply = Eliom_service.preapply in

    match bi.bi_menu_style with
      | `None -> Lwt.return []
      | `Pencil | `Linear as menu_style ->

    let history =
      (preapply
         ~service:Wiki_services.action_wikibox_history
         wb
       :> Eliom_tools_common.get_page)
    in
    let edit_onclick wb _id =
      let wiki_page = bi.bi_page in
      {{ fun _ ->
      let onload edit_window _ =
        let get_elt id coerce =
          Js.Opt.get
            (Js.Opt.bind
               (edit_window##document##querySelector(Js.string ("#"^id)))
               coerce)
            (fun () -> raise Not_found)
        in
        let textarea = get_elt %Wiki_services.preview_textarea_id Dom_html.CoerceTo.textarea in
        ignore
          (let send_content content =
             Lwt.ignore_result (
               let scroll = Dom_html.getDocumentScroll () in
               Eliom_client.change_page
                 ~service: %Wiki_services.Ui.preview_service
                 () (%wiki_page, (%wb, content))
               >>= fun () ->
               Dom_html.window##scroll (fst scroll, snd scroll);
               Lwt.return ()
             )
           in
           let timeout = ref None in
           Dom_html.addEventListener
             textarea
             Dom_html.Event.keypress
             (Dom.handler
               (fun _ ->
                  (match !timeout with
                    | Some timeout ->
                        Dom_html.window##clearTimeout(timeout)
                    | None -> ());
                  let callback =
                    Js.wrap_callback
                      (fun () -> send_content (Js.to_string textarea##value))
                  in
                  timeout := Some (Dom_html.window##setTimeout(callback, 1000.0));
                  Js._true))
             Js._false);
        edit_window##onbeforeunload <-
          Dom.handler
            (fun _ ->
               Lwt.ignore_result
                 (Eliom_client.change_page ~service:Eliom_service.void_coservice' () ());
               Js._true);
        ignore
          (Dom_html.addEventListener
             (get_elt %Wiki_services.save_id Dom_html.CoerceTo.input)
             Dom_html.Event.click
             (Dom.handler
                (fun _ ->
                  edit_window##onbeforeunload <- Dom.no_handler;
                  edit_window##close ();
                  Lwt.ignore_result
                    (lwt _version =
                       Eliom_client.call_caml_service
                         ~service: %Wiki_services.API.set_wikibox_content
                         () (%wiki_page, (%wb, Js.to_string textarea##value))
                     in
                     Eliom_client.change_page ~service:Eliom_service.void_coservice' () ());
                  Js._true))
             Js._false);
        Js._true
      in
      (** TODO Disable button *)
      let edit_window =
        Eliom_client.window_open
          ~window_name:(Js.string ("Editing wikibox "^Wiki_types.string_of_wikibox %wb))
          ~window_features:(Js.string "alwaysRaised=yes,width=800,height=600,location=no,dependent=yes")
          ~service:%Wiki_services.Ui.edit_service
          %wb
      in
      (** onload method with new window doesn't works with opera *)
      (*edit_window##onload <- Dom.full_handler onload*)
      (** This hack (for having opera works) needs a non-standard method :( *)
      Js.Unsafe.set Dom_html.window (Js.string opera_hack) onload;
    }} in
    let delete_service = preapply
      ~service:Wiki_services.action_delete_wikibox
      wb
    in
    let delete_onclick _id = {{ fun _ ->
      let answer = Dom_html.window##confirm(Js.string "Do you really want to delete this wikibox?") in
      if Js.to_bool answer then
        Eliom_client.exit_to ~service: %delete_service () ()
      else
        Eliom_lib.debug "Canceled delete"
    }} in
    let view     = (Eliom_service.void_coservice' :> Eliom_tools_common.get_page) in
    let edit_wikibox_perm =
      (preapply
         ~service:Wiki_services.action_edit_wikibox_permissions
         wb
       :> Eliom_tools_common.get_page)
    in
    lwt current_override = Wiki_services.get_override_wikibox () in
    lwt css =
      match special_box with
        | WikiPageBox (w, page) ->
            (bi.bi_rights#can_create_wikipagecss (w, page) >|= function
               | true ->
                   let edit =
                     (preapply
                       ~service:Wiki_services.action_edit_css_list
                       (wb, (w, Some page)) :> Eliom_tools_common.get_page)
                   in
                   let is_current = current_override = Some (wb, EditCssList (w, Some page)) in
                   Some (Eliom_lib.Right (edit, is_current), [Html5.F.pcdata "wikipage css"])
               | false -> None
            )
        | WikiContainerBox w ->
            (bi.bi_rights#can_create_wikicss w >|= function
               | true ->
                   let edit =
                     (preapply
                       ~service:Wiki_services.action_edit_css_list
                       (wb, (w, None)) :> Eliom_tools_common.get_page)
                   in
                   let is_current = current_override = Some (wb, EditCssList (w, None)) in
                   Some (Eliom_lib.Right (edit, is_current), [Html5.F.pcdata "wiki css"])
               | false -> None
            )
        | RegularBox -> Lwt.return None
    in
    lwt wp_prop =
      match special_box with
        | RegularBox | WikiContainerBox _ -> Lwt.return None
        | WikiPageBox wp ->
            bi.bi_rights#can_admin_wikipage wp >|= function
              | true ->
                  let edit_wp =
                    (preapply
                      ~service:Wiki_services.action_edit_wikipage_properties
                      (wb, wp) :> Eliom_tools_common.get_page)
                  in
                  let is_current = current_override = Some (wb, EditWikipageProperties wp) in
                  Some (Eliom_lib.Right (edit_wp, is_current), [Html5.F.pcdata "edit wikipage options"])
              | false -> None
    in
    lwt edit_wiki_perms =
      match special_box with
        | RegularBox | WikiPageBox _ ->
            Lwt.return None
        | WikiContainerBox w ->
            lwt b1 = bi.bi_rights#can_set_wiki_permissions w in
            lwt b2 = bi.bi_rights#can_edit_metadata w in
            match b1 || b2 with
              | true ->
                  let edit_p =
                    (preapply
                       ~service:Wiki_services.action_edit_wiki_options
                       (wb, w) :> Eliom_tools_common.get_page)
                  in
                  let is_current = current_override = Some (wb, EditWikiOptions w) in
                  Lwt.return
                    (Some
                       (Eliom_lib.Right ((edit_p :> Eliom_tools_common.get_page), is_current),
                        [Html5.F.pcdata "edit wiki permissions or options"]))
              | false -> Lwt.return None
    in
    Wiki_sql.wikibox_wiki wb >>= fun wiki ->
    bi.bi_rights#can_delete_wikiboxes wiki >>= fun wbdel ->
    bi.bi_rights#can_write_wikibox wb >>= fun wbwr ->
    bi.bi_rights#can_view_history wb >>= fun wbhist ->
    bi.bi_rights#can_set_wikibox_specific_permissions wb >>= fun  wbperm ->
    let menuedit =
      if wbwr
      then
        Some (Eliom_lib.Left (edit_onclick wb), [Html5.F.pcdata "edit"])
      else None
    in
    let menuperm =
      if wbperm
      then
        let is_current = current_override = Some (wb, EditWikiboxPerms wb) in
        Some (Eliom_lib.Right (edit_wikibox_perm, is_current), [Html5.F.pcdata "edit permissions"])
      else None
    in
    let menuhist =
      if wbhist
      then
        let is_current = current_override = Some (wb, History wb) in
        Some (Eliom_lib.Right (history, is_current), [Html5.F.pcdata "history"])
      else None
    in
    let menudel =
      if wbdel
      then Some (Eliom_lib.Left delete_onclick, [Html5.F.span [Html5.F.pcdata "delete"]])
      else None
    in
    let menuview =
      let is_current = match current_override with None -> true | Some (wb', _) when wb <> wb' -> true | _ -> false in
      Eliom_lib.Right (view, is_current), [Html5.F.pcdata "view"]
    in
    let menu_entries = Ocsimore_lib.concat_list_opt
      [menuedit; menudel; menuperm; menuhist; wp_prop; edit_wiki_perms; css]
      []
    in
    match menu_entries, wbdel with
      | [], false -> Lwt.return [] (* empty list => no menu *)
      | _ ->
          let img = Page_site.static_file_uri ~path:["crayon.png"] in
          let menu =
            let is_first =
              let first = ref true in
              fun () ->
                if !first then
                  (first := false; ["eliomtools_first"])
                else []
            in
            Html5.F.ul ~a:[Html5.D.a_class ["wikiboxmenu"; "eliomtools_menu"]]
              (List.map
                 (function (Eliom_lib.Left onclick, content) ->
                      let id = Html5.Id.new_elt_id () in
                      Html5.F.(
                        li ~a:[a_class (is_first ())] [
                          Html5.Id.create_named_elt ~id (Html5.D.Raw.a ~a:[a_onclick (onclick id)] content)
                        ])
                  | (Eliom_lib.Right (service, is_current), content) ->
                      if is_current then
                        Html5.F.(li ~a:[a_class ("eliomtools_current" :: is_first ())]
                                 content)
                      else
                        Html5.F.(li ~a:[a_class (is_first ())]
                                 [Html5.D.a ~service content ()]))
                 (menuview :: menu_entries))
          in
          match menu_style with
            | `Pencil ->
                Lwt.return
                  [Html5.F.div
                     ~a:[Html5.F.a_class ["pencilmenucontainer"]]
                     [Html5.F.img
                        ~src:img ~alt:"edit"
                        ~a:[Html5.F.a_class ["pencilmenuimg"]]
                        ();
                      Html5.F.div
                        ~a:[Html5.F.a_class ["pencilmenu"]]
                        [Html5.F.div
                           ~a:[Html5.F.a_class ["wikiboxmenutitle"]]
                           [Html5.F.pcdata title];
                         menu;
                        ]
                     ]
                  ]
            | `Linear ->
                Lwt.return
                  [Html5.F.div
                     ~a:[Html5.F.a_class ["wikiboxlinearmenu"]]
                     [Html5.F.div
                        ~a:[Html5.F.a_class ["wikiboxmenutitle"]]
                        [Html5.F.pcdata title];
                      menu;
                     ]
                  ]

  method display_menu_box : 'a 'b.
    bi:_ -> classes:_ ->
      ?active_item:_ -> ?special_box:_ ->
        ?title:_ -> wb:_ ->
          ([< Html5_types.div_content_fun > `Div ] as 'a) Html5.F.elt list ->
            'a Html5.F.elt list Lwt.t =

    fun ~bi ~classes
        ?active_item ?special_box
        ?title ~wb
        content ->

    self#box_menu ~bi ?special_box ?active_item ?title wb >>= fun menu ->
    if menu = [] then
      Lwt.return content
        (* [Html5.F.div ~a:[Html5.F.a_class classes] content] *)
    else
      Lwt.return
        [Html5.F.div
            ~a:[Html5.F.a_class (interactive_class::classes)]
            (  menu
               @ [Html5.F.div ~a:[Html5.F.a_class ["boxcontent"]] content]
            )
        ]

  method draw_edit_form
      ~page
      ~rows ~cols
      wb
      warning1
      warning2
      curversion
      content
      previewonly
      (actionname, (page_wiki_path_names, ((wbname, versionname), contentname))) =
    [Html5.F.(h4 ~a:[a_class ["override_heading"]] [pcdata (Printf.sprintf "Edit wikibox %s" (Wiki_types.string_of_wikibox wb))]);
     Html5.F.p
       (  warning1
        @ Ocsimore_common.input_opaque_int32 ~value:wb wbname
        :: hidden_page_inputs page page_wiki_path_names
        @ Html5.D.int32_input ~input_type:`Hidden
             ~name:versionname ~value:curversion ()
        :: Html5.D.textarea
              ~a:[Html5.F.a_class ["wikitextarea"]]
              ~name:contentname
              ~value:content ()
        :: Html5.F.br ()
        :: warning2
        @ Html5.D.string_button
             ~name:actionname ~value:"preview"
             [Html5.F.pcdata "Preview"]
        :: if previewonly
            then []
            else [Html5.D.string_button ~name:actionname ~value:"save"
                    [Html5.F.pcdata "Save"]]
           )
    ]


  (* Wikitext in editing mode *)
  method display_wikitext_edit_form : 'a.
    bi:_ -> classes:_ ->
      ?rows:_ -> ?cols:_ ->
        previewonly:_ -> wb:_ -> _ ->
           (classes * ([> Html5_types.form ] as 'a) Html5.F.elt) Lwt.t =

    fun ~bi ~classes ?(rows=25) ?(cols=80) ~previewonly ~wb (content, version) ->
    let content = match content with
      | None -> "<<|  Deleted >>"
      | Some content -> content
    in
    Wiki.modified_wikibox ~wikibox:wb ~boxversion:version >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            [Html5.F.em [Html5.F.pcdata "Warning: "];
             Html5.F.pcdata
               !Language.messages.Language.wikitext_edition_conflict1;
             Html5.F.br (); Html5.F.br ();
            ],
            [Html5.F.br ();
             Html5.F.strong
               [Html5.F.em [Html5.F.pcdata "Warning: "];
                Html5.F.pcdata
                  !Language.messages.Language.wikitext_edition_conflict1;
               ];
             Html5.F.br ();
            ]
           )

       | None -> Lwt.return (version, [], [])
    ) >>= fun (curversion, warning1, warning2)  ->
    Lwt.return
      (classes,
       Html5.D.post_form
         ~a:[Html5.F.a_accept_charset ["utf-8"]]
         ~service:Wiki_services.action_send_wikiboxtext
         (self#draw_edit_form ~page:bi.Wiki_widgets_interface.bi_page ~rows ~cols wb warning1 warning2 curversion content previewonly) ())

  (* Wikitext in editing mode, with an help box on the syntax of the wiki *)
  method display_wikitext_edit_form_help : 'a.
    bi:_ -> classes:_ ->
      ?rows:_ -> ?cols:_ ->
        previewonly:_ -> wb:_-> _ ->
          (classes * ([> Html5_types.form | Html5_types.div ] as 'a) Html5.F.elt list) Lwt.t =

     fun ~bi ~classes
         ?rows ?cols
         ~previewonly
         ~wb data ->

    Wiki.get_admin_wiki ()              >>= fun { wiki_id = admin_wiki; _ } ->
    Wiki_sql.get_wikipage_info
      ~wiki:admin_wiki
      ~page:Wiki_widgets_interface.wikisyntax_help_name
                                  >>= fun { wikipage_wikibox = wb_help; _ } ->
    error_box#bind_or_display_error
      (Wiki_data.wikibox_content ~rights:bi.bi_rights wb_help)
      (self#display_wikiboxcontent ~bi ~classes:["wikihelp"])
    >>= self#display_basic_box                                 >>= fun b ->
    self#display_wikitext_edit_form ~bi ~classes:[] ?rows ?cols
      ~previewonly ~wb data                               >|= fun (_, f) ->
    (classes, [b; f])


  (* Css in editing mode *)
  method display_css_edit_form : 'a.
    bi:_ -> classes:_ ->
      ?rows:_ -> ?cols:_ ->
        wb:_ -> wbcss:_ ->
          wikipage:_ -> _ ->
            (Wiki_widgets_interface.classes *
               ([> Html5_types.form ] as 'a) Html5.F.elt list) Lwt.t =

    fun ~bi ~classes ?(rows=25) ?(cols=80)
        ~wb ~wbcss ~wikipage (content, boxversion) ->

    let content = match content with
      | None -> "/* Deleted CSS */"
      | Some content -> content
    in
    Wiki.modified_wikibox ~wikibox:wbcss ~boxversion >>=
    (function
       | Some curversion -> Lwt.return
           (curversion,
            [Html5.F.em [Html5.F.pcdata "Warning: "];
             Html5.F.pcdata !Language.messages.Language.css_edition_conflict;
             Html5.F.br (); Html5.F.br ();
            ]
           )

       | None -> Lwt.return (boxversion, [])
    ) >>= fun (curversion, warning)  ->
    let draw_form ( (wbname,
                     (((wikiname, wikipagename),
                       wbcssname),
                      versionname)),
                    contentname) =
      [Html5.F.p ~a:[Html5.F.a_class ["fullsize"]]
         (List.flatten
            [warning;
             [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
              Ocsimore_common.input_opaque_int32 ~value:wbcss wbcssname;
              Ocsimore_common.input_opaque_int32 ~value:(fst wikipage) wikiname;
             ];
             (match snd wikipage with
                | None -> []
                | Some page ->
                    [Html5.D.string_input
                       ~name:wikipagename ~input_type:`Hidden ~value:page ()
                    ]);
             [Html5.D.int32_input ~input_type:`Hidden ~name:versionname ~value:curversion ();
              Html5.D.textarea
                ~a:[Html5.F.a_class ["wikitextarea"]]
                ~name:contentname
                ~value:content ();
              Html5.F.br ();
              Html5.D.button ~button_type:`Submit [Html5.F.pcdata"Save"];
             ];
            ]
         )
      ]
    in
    Lwt.return
      (classes,
       [Html5.D.post_form
          ~a:[Html5.F.a_accept_charset ["utf-8"]; Html5.F.a_class ["fullsize"]]
          ~service:Wiki_services.action_send_css draw_form ()]
      )


  (** Edition of the permissions of a wikibox *)
  (** As usual, the function supposes that the user has enough rights
      to change the permissions, as this will be checked by the service
      (and the user should not have access to the page otherwise). We
      also suppose that boxrights is set to true for the wiki *)
  method display_edit_wikibox_perm_form : 'a.
    bi:_ -> classes:_ ->
      _ ->
        (Wiki_widgets_interface.classes *
           ([> `Form | `P | `PCDATA | `Table ] as 'a) Html5.F.elt list) Lwt.t =

    fun ~bi ~classes wb ->
    Wiki_sql.get_wikibox_info wb >>= fun { wikibox_special_rights = sr; _ } ->
    let bt_change value textbt =
      let mform (wbname, srname) =
        [Html5.F.div
           [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
            Ocsimore_lib.hidden_bool_input ~value srname;
            Html5.D.button
              ~button_type:`Submit [Html5.F.pcdata textbt];
           ]
        ]
      in
      Html5.D.post_form
        ~a:[Html5.F.a_accept_charset ["utf-8"]]
        ~service:Wiki_services.action_set_wikibox_special_permissions
        mform ()
    in
    if sr = false then
      let msg = "The permissions for this wikibox are currently inherited from \
                 the wiki. Press this button if you want to use special \
                 permissions."
      and bt = bt_change true "Use specific permissions"
      in
      Lwt.return (classes, [Html5.F.pcdata msg; bt])
    else
      user_widgets#form_edit_awr ~text_prefix:"Wikibox "
        ~grps:Wiki.wikibox_grps ~arg:wb ()
      >>= fun (formedit_hd, formedit_tl) ->
      let msg =
        "This wikibox has specific permissions. You can edit them below, or \
         use the button next to revert to the wiki generic permissions."
      in
      let bt = bt_change false "Use generic wiki permissions"
      and msg2 =
        Html5.F.p
          [Html5.F.pcdata "If you edit the permissions below, do ";
           Html5.F.em [Html5.F.pcdata "not"];
           Html5.F.pcdata " remove yourself from the admin group, as you will \
                            no longer be allowed to edit permissions \
                            afterwards.";
          ]
      in
      Lwt.return
        (classes,
         [Html5.F.pcdata msg;
          bt;
          msg2;
          Html5.F.table
            ~a:[Html5.F.a_class ["table_admin"]]
            (Html5.F.tr
              [Html5.F.th [Html5.F.pcdata "Role"];
               Html5.F.th [Html5.F.pcdata "Current users in this role"]]
            )
            (formedit_hd :: formedit_tl)
         ]
        )



  (** Form to edit the description and container of a wiki *)
  method private display_edit_wiki_metadata ~classes ?wb wiki =
    Wiki_sql.get_wiki_info_by_id ~id:wiki
        >>= fun { wiki_descr = descr; wiki_container = container; _ } ->
    let form (wbname, (wikiname, (descrname, containername))) =
      [ Html5.F.p
         (List.flatten
            [[Ocsimore_common.input_opaque_int32 ~value:wiki wikiname];
             (match wb with
                | None -> []
                | Some wb ->
                    [Ocsimore_common.input_opaque_int32 ~value:wb wbname]);
             [Html5.F.pcdata "Description: ";
              Html5.D.string_input ~name:descrname
                ~input_type:`Text ~value:descr ();
              Html5.F.br ();
              Html5.F.pcdata "Container wikibox: ";
              Ocsimore_common.input_opaque_int32_opt ~hidden:false
                ~value:container containername;
              Html5.D.button ~button_type:`Submit
                   [Html5.F.pcdata "Save"];
             ];
            ]
         );
      ]
    in
    let form = Html5.D.post_form
      ~a:[Html5.F.a_accept_charset ["utf-8"]]
      ~service:Wiki_services.action_send_wiki_metadata form
      ()
    in Lwt.return (classes, [form])


  method private display_edit_wiki_option_form
         ~classes ?wb
         ~options ~perms
         wiki =
    (if options then
       self#display_edit_wiki_metadata ~classes ?wb wiki
     else Lwt.return ([], [])
    ) >>= fun (cl1, f1) ->
    (if perms then
       self#display_edit_wiki_perm_form ~classes ?wb wiki
     else
       Lwt.return ([], [])
    ) >>= fun (cl2, f2) ->
    Lwt.return (cl1 @ cl2, f1 @ f2)


  (** Form for the permissions of a wiki; The [wb] argument is the wikibox
      which will be overridden with an error message if the save fails *)
  method display_edit_wiki_perm_form : 'a.
    classes:_ -> ?wb:_ -> _ ->
      (Wiki_widgets_interface.classes *
         ([> `H2 | `P | `Table ] as 'a) Html5.F.elt list) Lwt.t =

    fun ~classes ?wb wiki ->
    let aux g text =
      user_widgets#form_edit_group ~group:(g $ wiki)
        ~text:[Html5.F.p
                 ~a:[Html5.F.a_class ["eliom_inline"]]
                 [Html5.F.strong [Html5.F.pcdata text]]
              ]
    in
    aux Wiki.wiki_admins "Administer the wiki" ()                  >>= fun f1 ->
    aux Wiki.wiki_subwikiboxes_creators "Create subwikiboxes" ()   >>= fun f2 ->
    aux Wiki.wiki_wikipages_creators "Create wikipages" ()         >>= fun f3 ->
    aux Wiki.wiki_wikiboxes_creators "Create wikiboxes" ()         >>= fun f4 ->
    aux Wiki.wiki_css_creators "Create CSS" ()                     >>= fun f5 ->
    aux Wiki.wiki_wikiboxes_deletors "Delete wikiboxes" ()         >>= fun f6 ->
    user_widgets#form_edit_awr ~text_prefix:"Wikiboxes"
      ~grps:Wiki.wiki_wikiboxes_grps ~arg:wiki ()      >>= fun (f7_hd, f7_tl) ->
    aux Wiki.wiki_files_readers "Read static files" ()             >>= fun f8 ->
    aux Wiki.wiki_wikiboxes_src_viewers "View wikiboxes source" () >>= fun f9 ->
    aux Wiki.wiki_wikiboxes_oldversion_viewers "View wikiboxes old versions" ()
                                                                  >>= fun f10 ->
    aux Wiki.wiki_metadata_editors "Edit the metadata of the wiki" ()
                                                                  >>= fun f11 ->
    aux Wiki.wiki_files_uploaders "Upload files" ()               >>= fun f12 ->

    let form =
      [
       Html5.F.p
         [Html5.F.em [Html5.F.pcdata "(inherited permissions are not shown)"]];
       Html5.F.table
         ~a:[Html5.F.a_class ["table_admin"]]
         (Html5.F.tr
            [Html5.F.th [Html5.F.pcdata "Role"];
             Html5.F.th [Html5.F.pcdata "Current users in the group"]]
         )
         (   f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: f8 :: f9 :: f10 :: f11 :: f12
             :: f7_hd :: f7_tl
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


  method display_wikitext_history : 'a 'b.
    bi:_ -> classes:_ -> wb:_ -> _ ->
      (classes * ([> `PCDATA | `Em | `Br | `A of ([> `PCDATA] as 'b) ] as 'a) Html5.F.elt list) Lwt.t =

    fun ~bi ~classes ~wb l ->
    Lwt_list.map_s
      (fun sql_data ->
        let version = Sql.get sql_data#version
        and author = Sql.get sql_data#author
        and date = Sql.get sql_data#datetime in
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >|= fun { user_fullname = author; _ } ->
         [Html5.F.pcdata (Int32.to_string version);
          Html5.F.pcdata ". ";
          Html5.F.pcdata (CalendarLib.Printer.Calendar.to_string date);
          Html5.F.pcdata " ";
          Html5.F.em [Html5.F.pcdata " by "; Html5.F.pcdata author];
          Html5.F.pcdata " ";
          Html5.D.a ~service:Wiki_services.action_old_wikibox
            [Html5.F.pcdata "view"] (wb, version);
          Html5.F.pcdata " (";
          Html5.D.a ~service:Wiki_services.action_src_wikibox
            [Html5.F.pcdata "source"] (wb, version);
          Html5.F.pcdata ")";
          Html5.F.br ();
         ]
      )
      l
    >|= List.flatten
    >|= fun l -> (classes, l)

  method display_css_history : 'a 'b.
    bi:_ -> classes:_ -> wb:_ -> wbcss:_ ->
      wikipage:_ -> _ ->
        (Wiki_widgets_interface.classes *
           ([> `PCDATA | `Em | `Br | `A of ([> `PCDATA ]as 'b) ] as 'a) Html5.F.elt list) Lwt.t =

    fun ~bi ~classes ~wb ~wbcss ~wikipage l ->
    Lwt_list.map_s
      (fun sql_data ->
        let version = Sql.get sql_data#version
        and author = Sql.get sql_data#author
        and date = Sql.get sql_data#datetime in
         User_sql.get_basicuser_data (User_sql.Types.userid_from_sql author)
         >|= fun { user_fullname = author; _ } ->
         [Html5.F.pcdata (Int32.to_string version);
          Html5.F.pcdata ". ";
          Html5.F.pcdata (CalendarLib.Printer.Calendar.to_string date);
          Html5.F.pcdata " ";
          Html5.F.em [Html5.F.pcdata "by "; Html5.F.pcdata author];
          Html5.D.a
            ~service:Wiki_services.action_old_wikiboxcss
            [Html5.F.pcdata "view"](wb, ((wikipage, wbcss), version));
          Html5.F.br ();
         ]
      )
      l
    >|= List.flatten
    >|= fun l -> (classes, l)

   method private display_edit_wikipage_properties
                   ~bi ~classes
                   ~(wb:wikibox) wp =

     let (wiki, page) = wp in
     Wiki_sql.get_wikipage_info ~wiki ~page >>= fun wp ->
     let draw_form (wbname,
                    ((wikiidname, pagename),
                     (titlename,
                      ((wbidname, pathname))))) =
       [Html5.F.p
         [Ocsimore_common.input_opaque_int32 ~value:wiki wikiidname;
          Ocsimore_common.input_opaque_int32 ~value:wb wbname;
          Html5.D.string_input
            ~name:pagename ~input_type:`Hidden ~value:page ();
          Html5.F.pcdata "Title of the wikipage: ";
          Html5.D.string_input ~name:titlename
            ~input_type:`Text
            ~value:(match wp.wikipage_title with None -> "" | Some s -> s)
            ();
          Html5.F.pcdata "(if blank, the title of the wiki will be used";
          Html5.F.br ();

          Html5.F.pcdata "Wikibox to which the wikipage points to. Leave blank \
                          to delete the wikipage. ";
          Ocsimore_common.input_opaque_int32_opt ~hidden:false
                 ~value:(Some wp.wikipage_wikibox) wbidname;
          Html5.F.br ();

          Html5.F.pcdata "Path of the wikipage inside the wiki";
          Html5.D.string_input ~name:pathname
                 ~input_type:`Text ~value:wp.wikipage_page ();
          Html5.F.pcdata "Notice that changing this path will ";
          Html5.F.em [Html5.F.pcdata "not"];
          Html5.F.pcdata " update links to this wikipage.";
          Html5.F.br ();

          Html5.D.button
            ~button_type:`Submit [Html5.F.pcdata "Save"];
         ]
       ]
     in
     Lwt.return
      (classes,
       [Html5.D.post_form
          ~a:[Html5.F.a_accept_charset ["utf-8"]]
          ~service:Wiki_services.action_send_wikipage_properties
          draw_form ()]
      )

  method private display_edit_css_list ~bi ~classes ~(wb:wikibox) wikipage =
    let wiki, page = wikipage in
    (match page with
      | None -> Wiki_sql.get_css_for_wiki ~wiki
      | Some page -> Wiki_sql.get_css_for_wikipage ~wiki ~page
    ) >>= fun l ->
    (match page with
       | None -> bi.bi_rights#can_create_wikicss wiki
       | Some page -> bi.bi_rights#can_create_wikipagecss (wiki, page)
    ) >>= fun can_create ->
    let select_media name media =
      let l = [ `Braille; `Embossed; `Handheld; `Print;
                `Projection; `Screen; `Speech; `TTY; `TV]
      in
      let in_media m = List.mem m media in
      let l =
        List.map
          (fun s -> Html5.D.Option ([], s, None, in_media s))
          l
      in
      Html5.D.user_type_multiple_select
        (fun x -> Wiki_types.string_of_media_type [x])
        ~name
        ~a:[Html5.F.a_style "vertical-align: top"; Html5.F.a_size 5]
        (Html5.D.Option ([],
                                       `All,
                                       Some (Html5.F.pcdata "all media"),
                                       in_media `All)
        )
        l
    in
    let aux (css_wb, (_, ver)) =
      bi.bi_rights#can_view_history css_wb.wikibox        >>= fun csshist ->
      bi.bi_rights#can_write_wikibox css_wb.wikibox         >>= fun csswr ->
      bi.bi_rights#can_set_wikibox_specific_permissions css_wb.wikibox
                                                              >>= fun cssperm ->
      let wbcss_ = ((wiki, page), css_wb.wikibox) in
      let v1 = if csshist then
        [Html5.D.a ~service:Wiki_services.action_css_history
           [Html5.F.pcdata "History"] (wb, wbcss_)
        ]
      else []
      and v2 = if csswr then
        [Html5.D.a ~service:Wiki_services.action_edit_css
           [Html5.F.pcdata "Edit"] (wb, (wbcss_, None))
        ]
      else []
      and v3 = if cssperm then
        [Html5.D.a
           ~service:Wiki_services.action_css_permissions
           [Html5.F.pcdata "Permissions"] (wb, wbcss_)
        ]
      else []
      and v4 =
        [Html5.D.a
           ~service:Wiki_services.action_old_wikiboxcss
           [Html5.F.pcdata "View"] (wb, (wbcss_, ver))
        ]
      in
      let fupdate (wbn, (((((wikin, wpn), wbcssn), newwbcssn), median), rankn))=
        [Html5.F.div
           ~a:[Html5.F.a_class ["eliom_inline"]]
           (List.flatten
              [[Ocsimore_common.input_opaque_int32 ~value:wb wbn;
                Ocsimore_common.input_opaque_int32 ~value:css_wb.wikibox wbcssn;
                Ocsimore_common.input_opaque_int32 ~value:wiki wikin;
                Html5.F.pcdata "CSS ";
                Html5.D.int32_input ~input_type:`Text
                   ~a:[Html5.F.a_size 2] ~value:css_wb.rank ~name:rankn ();
                Html5.F.pcdata "(Id ";
                Html5.F.pcdata (Opaque.int32_t_to_string css_wb.wikibox);
                Html5.F.pcdata ") ";
                Ocsimore_common.input_opaque_int32 ~value:css_wb.wikibox newwbcssn];
               (match page with
                  | None -> []
                  | Some page ->
                      [Html5.D.string_input ~name:wpn
                           ~input_type:`Hidden ~value:page ()
                      ]
               );
               [select_media median css_wb.media;
                Html5.F.pcdata " ";
                Html5.D.button ~button_type:`Submit
                   [Html5.F.pcdata " Update media and CSS order"];
                Html5.F.pcdata " "];
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
        [Html5.F.div
           ~a:[Html5.F.a_class ["eliom_inline"]]
           (List.flatten
              [[Ocsimore_common.input_opaque_int32 ~value:wb wbn;
                Ocsimore_common.input_opaque_int32 ~value:css_wb.wikibox wbcssn;
                Ocsimore_common.input_opaque_int32 ~value:wiki wikin;
                Html5.D.int32_input ~input_type:`Hidden
                   ~value:css_wb.rank ~name:rankn ()];
               (match page with
                  | None -> []
                  | Some page ->
                      [Html5.D.string_input ~name:wpn
                           ~input_type:`Hidden ~value:page ()
                      ]
               );
               [Html5.D.button ~button_type:`Submit
                   [Html5.F.pcdata " Remove CSS"];
               ];
              ]
           )
        ]
      in
      if can_create then Lwt.return
        [Html5.F.div
           (List.flatten
              [[Html5.D.post_form ~keep_get_na_params:true
                  ~a:[Html5.F.a_accept_charset ["utf-8"];
                      Html5.F.a_class ["eliom_inline"]]
                  ~service:Wiki_services.action_send_css_options fupdate ();
                Html5.D.post_form ~keep_get_na_params:true
                  ~a:[Html5.F.a_accept_charset ["utf-8"];
                      Html5.F.a_class ["eliom_inline"]]
                  ~service:Wiki_services.action_send_css_options fdelete ();
                Html5.F.pcdata " "];
               v4;
               [Html5.F.pcdata " / "];
               v1;
               [Html5.F.pcdata " / "];
               v2;
               [Html5.F.pcdata " / "];
               v3;
              ]
           )
        ]
      else Lwt.return
        [Html5.F.p
           (List.flatten
              [v4;
               [Html5.F.pcdata " / "];
               v1;
               [Html5.F.pcdata " / "];
               v2;
               [Html5.F.pcdata " / "];
               v3;
              ]
           )
        ]
    in
    (if l = [] then
      Lwt.return [Html5.F.pcdata "There are currently no CSS"]
     else
       Lwt_list.fold_left_s
         (fun (x : Html5_types.flow5 Html5.F.elt list) e ->
            aux e >|= fun e -> x @ (Html5.F.br () :: e)
         )
         []
         l
    ) >>= fun forms ->
    (if can_create then
       let mform (wbn, ((wikin, wpn), (median, wbcssn))) =
         [Html5.F.p
           (List.flatten
             [[Html5.F.pcdata "Add another CSS: ";
               Html5.F.br ();
               Ocsimore_common.input_opaque_int32 ~value:wb wbn;
               Ocsimore_common.input_opaque_int32 ~value:wiki wikin];
              (match page with
                 | None -> []
                 | Some page ->
                     [Html5.D.string_input ~name:wpn
                        ~input_type:`Hidden ~value:page ()
                     ]
              );
              [Html5.F.pcdata "Media: ";
               select_media median [`All];
               Html5.F.br ();
               Html5.F.pcdata "Id: ";
               Ocsimore_common.input_opaque_int32_opt ~hidden:false wbcssn;
               Html5.F.pcdata
                 "Type an existing CSS id, or leave blank to create a new CSS";
               Html5.F.br ();
               Html5.D.button ~button_type:`Submit
                 [Html5.F.pcdata "Add"]];
             ]
           )
         ]
       in Lwt.return
            (   forms
             @ [Html5.F.br ();
                Html5.D.post_form
                  ~a:[Html5.F.a_accept_charset ["utf-8"]]
                  ~service:Wiki_services.action_create_css mform ();
               ]
            )
     else
       Lwt.return []
    ) >|= fun r ->
    (classes, [Html5.F.div ~a:[Html5.F.a_class classes] r])


  method display_interactive_wikibox_aux : 'a.
    bi:_ -> ?classes:_ ->
      ?rows:_ -> ?cols:_ ->
         ?special_box:_ -> _ ->
           (([> Html5_types.div ] as 'a) Html5.F.elt list * bool) Lwt.t =

    fun ~bi ?(classes=[]) ?rows ?cols ?special_box wb ->
      lwt (r, code) =
        let classes = wikibox_class::classes in
        match_lwt Wiki_services.get_override_wikibox () with
          | Some (wb', override) when wb = wb' ->
              self#display_overriden_interactive_wikibox ~bi ~classes ?rows ?cols
                ?special_box ~wb_loc:wb ~override () >|= fun (b, c) ->
              ([Html5.F.div ~a:[Html5.F.a_class ["overridden"]] b], c)
          | _ ->
              lwt (c, code) =
                try_lwt
                  Wiki_data.wikibox_content ~rights:bi.bi_rights wb >|= fun c ->
                  (Lwt.return c, true)
                with e -> Lwt.return (Lwt.fail e, false)
              in
              let c =
                try_lwt
                  lwt (typ, _, version) = c in
                  match Eliom_reference.Volatile.get preview_wikibox_content with
                    | Some (wb', c') when wb = wb' ->
                        Lwt.return (typ, Some c', version)
                    | _ -> c
                with _ -> c
              in
              let classes =
                match Eliom_reference.Volatile.get preview_wikibox_content with
                  | Some (wb', _) when wb = wb' -> "preview" :: classes
                  | _ -> classes
              in
              error_box#bind_or_display_error c
                (self#display_wikiboxcontent ~classes
                   ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
              >>= (self#menu_view ~bi ?special_box wb) >|=
                fun r -> (r, code)
      in
      self#wrap_error ~wb r >|=
        fun e -> ((e : [ `Div ] Html5.F.elt list :> [> `Div ] Html5.F.elt list), code)

  method display_overriden_interactive_wikibox : 'a.
    bi:_ -> ?classes:_ ->
      ?rows:_ -> ?cols:_ ->
        ?special_box:_ -> wb_loc:_ ->
          override:_ -> unit ->
            (([> `Div | `P ] as 'a) Html5.F.elt list * bool) Lwt.t =

    fun ~bi ?(classes=[]) ?rows ?cols ?special_box ~wb_loc ~override () ->
    let display_error () =
      Lwt.return
        ([error_box#display_error_box
           ~classes:(frozen_wb_class::classes)
           ~message:"You are not allowed to do that."
           ()],
         false)
    and ok r = Lwt.return (r, true)
    in
    (* UNUSED
    let place_second_in_list (x,y) = Lwt.return (x,[y]) in
    *)
    (match override with
      | EditWikitext wb ->
          (bi.bi_rights#can_write_wikibox wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content' ~rights:bi.bi_rights wb)
                  (fun x ->
                     self#display_wikitext_edit_form_help ~bi ?cols ?rows
                       ~previewonly:true ~wb ~classes x)
                >>= (self#menu_edit_wikitext ~bi ?special_box wb_loc)
                >>= ok
            | false -> display_error ()
          )

      | EditCss ((wikipage, wbcss), css) ->
          (bi.bi_rights#can_write_wikibox wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (match css with
                     | None ->
                       Wiki_data.wikibox_content'
                         ~rights:bi.bi_rights
                         wbcss
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
          (bi.bi_rights#can_set_wikibox_specific_permissions wb >>= function
            | true ->
                self#display_edit_wikibox_perm_form ~bi ~classes wb
                >>= self#menu_edit_wikibox_perms ~bi ?special_box wb_loc
                >>= ok
          | false -> display_error ()
          )

      | EditWikiOptions wiki ->
          (bi.bi_rights#can_set_wiki_permissions wiki >>= fun b1 ->
           bi.bi_rights#can_edit_metadata wiki        >>= fun b2 ->
           match b1 || b2 with
            | true ->
                (self#display_edit_wiki_option_form ~classes
                   ~wb:wb_loc ~options:b2 ~perms:b1 wiki)
                >>= self#menu_edit_wiki_options ~bi ?special_box wb_loc wiki
                >>= ok
            | false -> display_error ()
          )

      | PreviewWikitext (wb, (content, version)) -> begin
          bi.bi_rights#can_write_wikibox wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~version~rights:bi.bi_rights wb
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
                                                            >>= fun (_, form) ->
                     Eliom_reference.get Wiki_services.desugar_messages >|= fun desugar_messages ->
                     let desugar_messages_list =
                       if desugar_messages <> [] then
                          let open Html5.F in
                          let li_for_msg (_, msg) = li [pcdata msg] in
                          [div ~a:[a_class ["desugar_warnings"]] [
                             p [pcdata "Warnings"];
                             ul (List.map li_for_msg desugar_messages)
                          ]]
                       else []
                     in
                     (classes,
                      Html5.F.(p ~a:[a_class [box_title_class]] [pcdata "Preview"]) ::
                        desugar_messages_list @
                        prev ::
                        form))
                >>= self#menu_edit_wikitext ~bi ?special_box wb_loc
                >>= ok
            | false -> display_error ()
        end

      | EditWikipageProperties wp ->
          (bi.bi_rights#can_admin_wikipage wp >>= function
             | true ->
                self#display_edit_wikipage_properties ~bi ~classes ~wb:wb_loc wp
                >>= (self#menu_edit_wikipage_properties ~bi ?special_box
                       wb_loc wp)
                >>= ok
             | false -> display_error ()
          )

      | History wb ->
          (bi.bi_rights#can_view_history wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_history ~rights:bi.bi_rights ~wb)
                  (self#display_wikitext_history ~bi ~classes ~wb)
                >>= (self#menu_wikitext_history ~bi ?special_box wb_loc)
                >>= ok
            | false -> display_error ()
          )

      | CssHistory ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_view_history wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_history ~rights:bi.bi_rights ~wb:wbcss)
                  (self#display_css_history ~bi ~classes ~wb:wb_loc ~wbcss
                     ~wikipage:(wiki,wikipage))
                >>= (self#menu_css_history ~bi ?special_box wb_loc
                        (wiki, wikipage))
                >>= ok
            | false -> display_error ()
          )

      | CssPermissions ((wiki, wikipage), wbcss) ->
          (bi.bi_rights#can_set_wikibox_specific_permissions wbcss
                                                                   >>= function
            | true ->
                (self#display_edit_wikibox_perm_form ~bi ~classes wbcss)
                >>= (self#menu_edit_css_perms ~bi ?special_box wb_loc
                       (wiki, wikipage))
                >>= ok
          | false -> display_error ()
          )

      | Oldversion (wb, version) ->
          (bi.bi_rights#can_view_oldversions wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~version ~rights:bi.bi_rights wb)
                  (self#display_wikiboxcontent ~classes
                     ~bi:(Wiki_widgets_interface.add_ancestor_bi wb bi))
                >>= (self#menu_old_wikitext ~bi ?special_box wb_loc version)
                >>= ok
            | false -> display_error ()
          )

      | CssOldversion (((wiki, page), wbcss), version) ->
          (bi.bi_rights#can_view_oldversions wbcss >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content ~version
                     ~rights:bi.bi_rights wbcss)
                  (self#display_raw_wikiboxcontent ~classes)
                >>= (self#menu_old_css ~bi ?special_box wb_loc (wiki, page))
                >>= ok
            | false -> display_error ()
          )

      | Src (wb, version)->
          (bi.bi_rights#can_view_oldversions_src wb >>= function
            | true ->
                error_box#bind_or_display_error
                  (Wiki_data.wikibox_content
                     ~version ~rights:bi.bi_rights wb)
                  (self#display_raw_wikiboxcontent ~classes)
                >>= (self#menu_src_wikitext ~bi ?special_box wb_loc version)
                >>= ok
            | false -> display_error ()
          )
        : (([`Div | `P ] Html5.F.elt list * bool) Lwt.t)
        :> (([> `Div | `P ] Html5.F.elt list * bool) Lwt.t))


  method display_interactive_wikibox : 'a.
    bi:_ -> ?classes:_ ->
      ?rows:_ -> ?cols:_ ->
        ?special_box:_ -> _ ->
          ([> `Div ] as 'a) Html5.F.elt list Lwt.t =

     fun ~bi ?(classes=[]) ?rows ?cols ?special_box wb ->
     lwt () = Wiki_services.add_wiki_css_header () in
     fst =|< self#display_interactive_wikibox_aux
               ~bi ?rows ?cols ~classes ?special_box wb


   method css_header : 'a.
     ?page:_ -> _ ->
       ([> Html5_types.link] as 'a) Html5.F.elt list Lwt.t =

     fun ?page wiki ->
     let css_url_service service arg media =
       Eliom_content.Html5.F.css_link
         ?a:(if media = []
             then None
             else Some [Html5.F.a_media media])
         ~uri:(Html5.D.make_uri ~service arg)
       ()
     in
     (* BB It is necessary to add the versions of the CSS wikiboxes to the
      * service to prevent caching when the content has changed (at least for
      * XHR request to the CSS). We are also sending links CSS wikiboxes with
      * current version = None because we want 404 error to happen in
      * [Wiki_self_services.pagecss_service]. *)
     let add_version wb =
       Wiki_sql.current_wikibox_version wb
         >|= fun version -> wb, version
     in
     lwt css =
       match Wiki_self_services.find_servwikicss wiki with
        | None -> Lwt.return []
        | Some wikicss_service ->
            lwt wb_list_with_media = Wiki_sql.get_css_wikibox_for_wiki ~wiki in
            if !Ocsimore_config.aggregate_css then
              Lwt_list.map_s
                (fun (media, wb_list) ->
                   Lwt_list.map_s add_version wb_list >|= fun wb_version_list ->
                     css_url_service wikicss_service wb_version_list media)
                (grouped_by_media wb_list_with_media)
            else
              Lwt_list.map_s
                (fun css_wb ->
                   add_version css_wb.wikibox >|= fun wikibox_version ->
                     css_url_service wikicss_service [wikibox_version] css_wb.media)
                wb_list_with_media
     in
     match page with
       | None -> Lwt.return css
       | Some page ->
           lwt wb_list_with_media = Wiki_sql.get_css_wikibox_for_wikipage ~wiki ~page in
           lwt ll =
             if !Ocsimore_config.aggregate_css then
               Lwt_list.map_s
                 (fun (media, wb_list) ->
                   Lwt_list.map_s add_version wb_list >|= fun wb_version_list ->
                    css_url_service
                      Wiki_services.pagecss_service
                      ((wiki, page), wb_version_list)
                      media)
                 (grouped_by_media wb_list_with_media)
             else
               Lwt_list.map_s
                 (fun css_wb ->
                    add_version css_wb.wikibox >|= fun wikibox_version ->
                      css_url_service
                        Wiki_services.pagecss_service
                        ((wiki, page), [wikibox_version])
                        css_wb.media)
                 wb_list_with_media
           in
           Lwt.return (css @ ll)

   method private display_container : 'a.
     wiki:_ -> sectioning:_ -> menu_style:_ ->
       page:_ ->
         gen_box:(sectioning:bool -> Wiki_widgets_interface.menu_style ->
                  (Wiki_types.wikibox option *
                     ([< Html5_types.flow5 ] as 'a)
                     Html5.F.elt list *
                     Wiki_widgets_interface.page_displayable * string option)
                    Lwt.t) ->
           (Html5_types.html Html5.F.elt * int) Lwt.t =

     fun ~wiki ~sectioning ~menu_style ~page:(page, page_list) ~gen_box ->
     Wiki_sql.get_wiki_info_by_id ~id:wiki >>= fun wiki_info ->
     lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
     let wb_container = wiki_info.wiki_container in
     gen_box ~sectioning menu_style >>= fun (_, subbox, err_code, title) ->
     lwt () = Eliom_reference.set Wiki_widgets_interface.page_displayable_eref err_code in
     (* We render the container, if it exists *)
     lwt page_content =
       match wb_container with
         | None -> Lwt.return [Html5.F.div subbox]
         | Some wb_container ->
             Wiki.default_bi ~rights ~wikibox:wb_container >>= fun bi ->
               let fsubbox ~sectioning:st ms =
               if ms = menu_style && st = sectioning then
                 Lwt.return (Some subbox)
               else
                 gen_box ~sectioning:st ms >>= fun (_, subbox, _, _) ->
                 Lwt.return (Some subbox)
             in
             let fsubbox =
               (fsubbox :> sectioning:bool -> Wiki_widgets_interface.menu_style ->
                (Html5_types.flow5 Html5.F.elt list) option Lwt.t) in
             let bi = { bi with  bi_subbox = fsubbox;
                          bi_page = wiki, Some page_list;
                          bi_menu_style = menu_style } in
             self#display_interactive_wikibox ~bi
               ~classes:[Wiki_syntax.class_wikibox wb_container]
               ~special_box:(WikiContainerBox wiki) wb_container
     in
     lwt css = self#css_header ~page wiki in
     let code = match err_code with
       | Wiki_widgets_interface.Page_displayable -> 200
       | Wiki_widgets_interface.Page_404 -> 404
       | Wiki_widgets_interface.Page_403 -> 403 in
     let title = match code with
       | 403 -> "Access forbidden"
       | _ -> match title with
           | Some title -> title
           | None -> wiki_info.wiki_descr in
     (match wiki_info.wiki_deleted with
       | true ->
         Page_site.html_page ~css ~title:"Wiki deleted"
           ([Html5.F.div [Html5.F.pcdata "Wiki deleted"]] :> Html5_types.body_content Html5.F.elt list)
       | false ->
         Page_site.html_page ~css ~title
           (page_content :> Html5_types.body_content Html5.F.elt list)
     ) >|= fun r -> (r, code)


   (* Displays the wikibox corresponding to a wikipage. This function, properly
      applied, is suitable for use with [display_container]. *)
   method private display_wikipage_wikibox ~wiki ~page:(page, page_list) ?subbox () =
     Wiki_sql.get_wiki_info_by_id ~id:wiki >>= fun wiki_info ->
     lwt rights = Wiki_models.get_rights wiki_info.wiki_model in
     let wb_container = wiki_info.wiki_container in
     Lwt.return
     (fun ~sectioning menu_style ->
       try_lwt
          (* We render the wikibox for the page *)
          lwt { wikipage_wikibox = box; wikipage_title = title; _ } =
            Wiki_sql.get_wikipage_info ~wiki ~page
          in
          lwt bi = Wiki.default_bi ~wikibox:box ~rights in
          let bi = { bi with bi_page = wiki, Some page_list;
                             bi_menu_style = menu_style;
                             bi_sectioning = sectioning; } in
          let bi = match subbox with
            | None -> bi
            | Some subbox ->
              let bi = Wiki_widgets_interface.add_ancestor_bi box bi in
              { bi with bi_subbox = subbox bi; } in
          lwt (subbox, allowed) =
            self#display_interactive_wikibox_aux ~bi
              ~special_box:(WikiPageBox (wiki, page)) box
          in
          Lwt.return (Some box,
                      subbox,
                      (if allowed
                       then Wiki_widgets_interface.Page_displayable
                       else Wiki_widgets_interface.Page_403),
                      title)
        with Not_found ->
            (* No wikibox. We create a default page, to insert into the
               container *)
            let draw_form (wbname, (wikiidname, pagename)) =
              [Html5.F.p
                (List.flatten
                   [[Ocsimore_common.input_opaque_int32 ~value:wiki wikiidname
                    ];
                     (* Used to know where to display errors (only possible
                        if there is a container, otherwise we don't know
                        what to override) *)
                    (match wb_container with
                       | None -> []
                       | Some container ->
                           [Ocsimore_common.input_opaque_int32
                                 ~value:container wbname]
                    );
                    [Html5.D.string_input ~name:pagename
                       ~input_type:`Hidden ~value:page ();
                     Html5.D.string_input
                       ~input_type:`Submit ~value:"Create it!" ();
                    ];
                   ]
                )
              ]
            in
            let draw_upload_form (widname, (wbname, (pathname, filename))) =
              let current_path =
                Eliom_request_info.get_original_full_path_string ()
              in
              [Html5.F.p
                  (List.flatten
                     [[Ocsimore_common.input_opaque_int32 ~value:wiki widname];
                       (* Used to know where to display errors (only possible
                          if there is a container, otherwise we don't know
                          what to override) *)
                      (match wb_container with
                        | None -> []
                        | Some container ->
                            [Ocsimore_common.input_opaque_int32
                                ~value:container wbname]
                      );
                      [Html5.D.string_input ~name:pathname
                          ~input_type:`Hidden ~value:current_path ();
                       Html5.D.file_input ~name:filename ();
                       Html5.D.string_input
                         ~input_type:`Submit ~value:"Or upload a file" ();
                      ];
                     ]
                  )
              ]
            in
            lwt c = rights#can_create_wikipages wiki in
            let form =
              if c then
                [Html5.D.post_form
                   ~service:Wiki_services.action_create_page draw_form ()
                ]
              else []
            and form_upload =
              if c then
                [Html5.D.post_form
                    ~service:Wiki_services.action_upload_file
                    draw_upload_form ()
                ]
              else []
            and err_msg = !Language.messages.Language.page_does_not_exist
            in
            Lwt.return
              (None,
               (Html5.F.p [Html5.F.pcdata err_msg] :: form @ form_upload),
               Wiki_widgets_interface.Page_404,
               None)
     )


   (* Displaying of an entire page. We just pass the proper rendering
   function to [display_container] *)
   method display_wikipage ~wiki ~sectioning ~menu_style ~page =
     lwt gen_box =
       (self#display_wikipage_wikibox ~wiki ~page ()
        :> (sectioning:bool -> Wiki_widgets_interface.menu_style ->
          (Wiki_types.wikibox option *
           Html5_types.flow5 Html5.F.elt list *
           Wiki_widgets_interface.page_displayable * string option)
          Lwt.t)
         Lwt.t)
     in
     self#display_container ~wiki ~sectioning ~menu_style ~page ~gen_box

   method display_wikifile ~wiki ~sectioning ~menu_style ~template ~file =
     let path = Eliom_lib.Url.remove_slash_at_beginning (Neturl.split_path template) in
     let page = Eliom_lib.Url.string_of_url_path ~encode:false path, path in
     let subbox bi ~sectioning _ =
       let bi = { bi  with bi_sectioning = sectioning } in
       match file with
       | Ocsigen_local_files.RFile file ->
           Lwt_io.with_file ~mode:Lwt_io.input file
             (fun ch ->
               lwt data = Lwt_io.read ch in
               lwt xml = Wiki_syntax.xml_of_wiki
                 (Wiki_syntax.cast_wp Wiki_syntax.wikicreole_parser) bi data in
               Lwt.return (Some xml))
       | Ocsigen_local_files.RDir _ -> Lwt.return None in
     lwt gen_box =
       (self#display_wikipage_wikibox ~wiki ~page ~subbox ()
        :> (sectioning:bool -> Wiki_widgets_interface.menu_style ->
          (Wiki_types.wikibox option *
           Html5_types.flow5
           Html5.F.elt list *
           Wiki_widgets_interface.page_displayable * string option)
          Lwt.t)
         Lwt.t)
     in
     self#display_container ~wiki ~sectioning ~menu_style ~page ~gen_box

   method display_wikibox ~wiki ~sectioning ~menu_style ~template ~wb =
     let path = Eliom_lib.Url.remove_slash_at_beginning (Neturl.split_path template) in
     let page = Eliom_lib.Url.string_of_url_path ~encode:false path, path in
     let subbox bi ~sectioning menu_style =
       let bi = { bi  with bi_sectioning = sectioning;
                               bi_menu_style = menu_style;
                } in
       lwt xml =
         (self#display_interactive_wikibox ~bi wb
          :> Html5_types.flow5 Html5.F.elt list Lwt.t) in
       Lwt.return (Some xml)
     in
     lwt gen_box =
       (self#display_wikipage_wikibox ~wiki ~page ~subbox ()
        :>  (sectioning:bool -> Wiki_widgets_interface.menu_style ->
         (Wiki_types.wikibox option *
          Html5_types.flow5 Html5.F.elt list *
          Wiki_widgets_interface.page_displayable * string option)
         Lwt.t) Lwt.t) in
     self#display_container ~wiki ~sectioning ~menu_style ~page ~gen_box

   method display_all_wikis =
     (* Lists of all wikis *)
     let l = ref [] in
     Wiki_sql.iter_wikis (fun w -> Lwt.return (l := w :: !l)) >>= fun () ->
     let l =
       List.sort (fun w1 w2 -> compare w1.wiki_title w2.wiki_title) !l
     in

     let wiki_line w =
       let servpage = Wiki_self_services.find_servpage w.wiki_id in
       let line =
         let img path text = [Page_site.icon ~path ~text] in
         let id = Opaque.int32_t_to_string w.wiki_id in
         let name = match servpage with
           | None -> Html5.F.strong [Html5.F.pcdata w.wiki_title]
           | Some service ->
               Html5.F.strong
                 [Html5.D.a ~service [Html5.F.pcdata w.wiki_title] []]
         in
         let edit =
           Html5.D.a ~service:Wiki_services.edit_wiki
             (img "imgedit.png" "Edit wiki options") w.wiki_id
         in
         let view_wikiboxes =
           Html5.D.a ~service:Wiki_services.view_boxes
             (img "viewboxes.png" "View all boxes") w.wiki_id
         in
         let edit_perm =
           Html5.D.a
             ~service:Wiki_services.edit_wiki_permissions_admin
             (img "imgeditperms.png" "View permissions") w.wiki_id
         in
         let delete_or_undelete ~msg ~delete str =
         (* Don't use opaque type because Eliom_parameter doesn't support
            user defined types already *)
           let param = sql_of_wiki w.wiki_id in
           Html5.D.Raw.a ~a:[
             Html5.D.a_onclick {{ fun _ ->
               let answer = Dom_html.window##confirm
                 (Js.string %msg)
               in
               if Js.to_bool answer then
                 Eliom_client.exit_to ~service:%Wiki_services.delete_wiki ()
                   (%param, %delete)
               else
                 ()
             }};
             Html5.F.a_class ["jslink"];
           ] [Html5.F.pcdata str]
         in
         let delete =
           delete_or_undelete
             ~msg:"Do you really want to delete this wiki ?"
             ~delete:true
             "Delete"
         and undelete =
           delete_or_undelete
             ~msg:"Do you really want to undelete this wiki ?"
             ~delete:false
             "Undelete"
         in
         (Html5.F.tr ~a:[Html5.F.a_class ["wikis"]]
            [Html5.F.td ~a:[Html5.F.a_class ["wikiid"]] [Html5.F.pcdata id];
             Html5.F.td ~a:[Html5.F.a_class ["wikiname"]] [name];
             Html5.F.td ~a:[Html5.F.a_class ["wikidescr"]]
               [Html5.F.pcdata w.wiki_descr];
             Html5.F.td [edit];
             Html5.F.td [view_wikiboxes];
             Html5.F.td [edit_perm];
             Html5.F.td [if w.wiki_deleted then undelete else delete];
            ]
         )
       in
       Wiki_sql.get_wikipages_of_a_wiki ~wiki:w.wiki_id ()
       >>= (fun wikipages ->
         Lwt_list.map_s
           (fun wikipage -> Lwt.return (
             let pagename = Sql.get wikipage#pagename in
             let name = match servpage with
               | None -> Html5.F.strong [Html5.F.pcdata pagename]
               | Some service ->
                   Html5.F.strong
                     [Html5.D.a ~service
                         [Html5.F.pcdata pagename] [pagename]
                     ]
             in
             Html5.F.tr
               [Html5.F.td [];
                Html5.F.td ~a:[Html5.F.a_class ["wikiname"]] [name];
                Html5.F.td [];
                Html5.F.td [];
                Html5.F.td [];
                Html5.F.td [];
                Html5.F.td [];
               ]
            )) wikipages
       ) >>= fun lines_wikipages ->
       Wiki_models.get_rights w.wiki_model
       >>= fun rights ->
       rights#can_create_wikipages w.wiki_id
       >>= fun can_create_wikipages ->
       let add_wikipage =
         if can_create_wikipages then [
           Html5.F.tr [
             Html5.F.td [];
             Html5.F.td [
               Html5.D.post_form
                 ~service:Wiki_services.action_create_page
                 (fun (_, (wikiidname, pagename)) -> [
                   Ocsimore_common.input_opaque_int32
                     ~value:w.wiki_id
                     wikiidname;
                   Html5.F.string_input
                     ~name:pagename
                     ~input_type:`Text
                     ();
                   Html5.F.string_input
                     ~input_type:`Submit
                     ~value:"Create"
                     ();
                 ]) ()
             ];
             Html5.F.td [Html5.F.pcdata "Create a new page for this wiki"];
           ]
         ]
         else []
       in
       Lwt.return (line :: lines_wikipages @ add_wikipage)
     in
     Lwt_list.fold_left_s (fun acc x ->
       wiki_line x
       >>= fun line ->
       Lwt.return (acc @ line)
     ) [] l
     >>= fun l ->
     Lwt.return
       [ Html5.F.table ~a:[Html5.F.a_class ["table_admin"]]
          (Html5.F.tr
             [Html5.F.th [Html5.F.pcdata "Id"];
              Html5.F.th [Html5.F.pcdata "Wiki"];
              Html5.F.th [Html5.F.pcdata "Description"];
              Html5.F.th [Html5.F.pcdata ""];
              Html5.F.th [Html5.F.pcdata ""];
              Html5.F.th [Html5.F.pcdata ""];
              Html5.F.th [Html5.F.pcdata ""];
             ]
          )
          l;
       ]

end

class phrasing_wikibox
  (error_box : Widget.widget_with_error_box)
  (user_widgets: User_widgets.user_widget_class)
    : Wiki_widgets_interface.interactive_wikibox =
object (self)

  inherit dynamic_wikibox error_box user_widgets

  method! draw_edit_form
         ~page ~rows:_ ~cols:_
         wb warning1 warning2 curversion content
         previewonly
         (actionname, (page_wiki_path_names, ((wbname, versionname), contentname))) =
    [ Html5.F.p
       (List.flatten
          [warning1;
           hidden_page_inputs page page_wiki_path_names;
           [Ocsimore_common.input_opaque_int32 ~value:wb wbname;
            Html5.D.int32_input ~input_type:`Hidden
              ~name:versionname ~value:curversion ();
            Html5.D.string_input
              ~a:[Html5.F.a_class ["wikitextarea"]]
              ~input_type:`Text ~name:contentname ~value:content ();
            Html5.F.br ()];
           warning2;
           [Html5.D.string_button
              ~name:actionname ~value:"preview" [Html5.F.pcdata "Preview"]];
           (if previewonly
            then []
            else [Html5.D.string_button ~name:actionname
                    ~value:"save" [Html5.F.pcdata "Save"] ]);
          ]
       )
    ]


  method! display_wikitext_edit_form_help
      ~bi
      ~classes ?rows ?cols
      ~previewonly ~wb
      data =
    self#display_wikitext_edit_form ~bi
      ~classes:[] ?rows ?cols ~previewonly ~wb data >|= fun (_, f) ->
    (classes, [f])


 end
