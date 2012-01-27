
let (>>=) = Lwt.(>>=)
open User_sql.Types
open Wiki_types

let ( ** ) = Eliom_parameters.prod

let is_wiki_syntax ct =
  let cvt = Wiki_types.string_of_content_type in
  let ct = cvt ct in
  (ct = cvt Wiki_syntax.wikicreole_content_type) ||
  (ct = cvt Wiki_syntax.reduced_wikicreole_content_type0) ||
  (ct = cvt Wiki_syntax.reduced_wikicreole_content_type1) ||
  (ct = cvt Wiki_syntax.reduced_wikicreole_content_type2) ||
  (ct = cvt Wiki_syntax.wikicreole_inline_content_type)





(** Code to migrate from old wikibox ids to uids. Although outdated, this
  code can be used as an example to update all the wikiboxes content *)
(*
let () =
  Wiki_syntax.add_preparser_extension
    ~wp:Wiki_syntax.wikicreole_parser ~name:"wikibox"
  (fun (_sp, wb) args c ->
     Ocsigen_messages.console2 "Wikibox found";
     (try
        try
          Ocsigen_messages.console2 "Changing";
          let box = Int32.of_string (List.assoc "box" args) in
          Wiki_sql.wikibox_wiki wb >>= fun wid ->
          let wid = Wiki_ext.extract_wiki_id args wid in
          Ocsigen_messages.console2
            (Printf.sprintf "Changing %ld %s" box (string_of_wiki wid));
          Wiki_sql.wikibox_new_id wid box >>= fun box' ->
          Ocsigen_messages.console2
            (Printf.sprintf "New wikibox %s" (string_of_wikibox box'));
          let s = (Wiki_syntax.string_of_extension "wikibox"
                     (("box", string_of_wikibox box') ::
                        (* We remove the wiki information *)
                        List.remove_assoc "wiki" (List.remove_assoc "box" args)) c) in
          Lwt.return (Some s)

        with Not_found ->
          Ocsigen_messages.console2 "Error?";
          (* No box, the preparser extension will take care of this
             case, we do nothing *)
          Lwt.return None
      with Failure _ -> Ocsigen_messages.console2 "Error"; Lwt.return None
        | Not_found -> Ocsigen_messages.console2 "Box not found";
            Lwt.return None)
  )


let service_update_wikiboxes_uid = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "update"]
  ~get_params:Eliom_parameters.unit ()

let () =
  Eliom_duce.Xhtml.register service_update_wikiboxes_uid
    (fun sp () () ->
       let r = ref 0 in
       let wp = Wiki_models.get_default_wiki_preparser Wiki_site.wikicreole_model in
       Wiki_sql.update_wikiboxes
         (fun ~wikibox:wb ~version ~content ~content_type ->
            if is_wiki_syntax content_type then (
              Ocsigen_messages.console2 (Printf.sprintf "%d: wb %s, ver %ld%!"
                                  (incr r; !r) (string_of_wikibox wb) version);
              match content with
                | None -> Lwt.return None
                | Some content ->
                    wp (sp, wb) content >>= fun r -> Lwt.return (Some r)
            )
            else
              Lwt.return None
         )
       >>= fun () ->
       Ocsimore_page.html_page sp {{ [<p>"Done"] }}
    )
*)


(** Update all the links in a the content of a wikibox. Previously used for
    the transition of the <<syntax>> to [[wiki(i):]]. Can be used to migrate
    an entire wiki somewhere else *)
(*
let action_update_links = Eliom_services.new_post_coservice'
  ~name:"update_links_aux"
  ~post_params:(Ocsimore_common.eliom_opaque_int32 "old" ** (Ocsimore_common.eliom_opaque_int32 "new" ** Eliom_parameters.string "path")) ()

let () =
  Eliom_duce.Xhtml.register action_update_links
    (fun sp () (oldwiki, (newwiki, newwikipath)) ->
       let wp = Wiki_syntax.copy_parser Wiki_syntax.wikicreole_parser in
       Wiki_syntax.set_link_extension wp
         (Wiki_syntax.translate_link ~oldwiki ~newwiki ~newwikipath);
       Sql.full_transaction_block
         (fun db ->
            Wiki_sql.update_wikiboxes ~db
              (fun ~wikibox:wb ~version:_ ~content ~content_type ->
                 if is_wiki_syntax content_type then
                   match content with
                     | None -> Lwt.return None
                     | Some content ->
                         Wiki_syntax.preparse_extension wp (sp, wb) content
                         >>= fun r -> Lwt.return (Some r)
                 else
                   Lwt.return None
              )
            >>= fun () ->
            Wiki_sql.rewrite_wikipages ~db ~oldwiki ~newwiki ~path:newwikipath)
       >>= fun () ->
       Ocsimore_page.html_page sp {{ [<p>"Done"] }}
    )

let service_update_links = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "update_links"]
  ~get_params:Eliom_parameters.unit ()

let () =
  Eliom_duce.Xhtml.register service_update_links
    (fun sp () () ->
       let draw_form (oldname, (newname, pathname)) =
         {{ [ <p>['Old wiki: '
                  {: Ocsimore_common.input_opaque_int32 ~hidden:false oldname :}
                  <br>[]
                  'New wiki: '
                  {: Ocsimore_common.input_opaque_int32 ~hidden:false newname :}
                  <br>[]
                  'Path :'
                  {: Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
                     ~name:pathname () :} <br>[]
                  {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                     {{ "Change" }} :}
                 ] ] }}
       in
       Ocsimore_page.html_page sp
         ({{ [ {: Eliom_duce.Xhtml.post_form ~a:{{ { accept-charset="utf-8" } }}
                  ~service:action_update_links ~sp draw_form () :} ] }}))
*)

