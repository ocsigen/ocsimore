open Lwt
open User_sql.Types
open Wiki_types


(* Code to migrate from old wikibox ids to uids *)
let service_update_wikiboxes_uid = Eliom_services.new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir; "update"]
  ~get_params:Eliom_parameters.unit ()

let () =
  Eliom_duce.Xhtml.register service_update_wikiboxes_uid
    (fun sp () () ->
       let r = ref 0 in
       let wp = Wiki_models.get_default_wiki_preparser Ocsisite.wikicreole_model in
       Wiki_sql.update
         (fun wb version content ->
            Ocsigen_messages.console2 (Printf.sprintf "%d: wb %s, ver %ld%!"
                                (incr r; !r) (string_of_wikibox wb) version);
            match content with
              | None -> Lwt.return None
              | Some content ->
                  wp (sp, wb) content >>= fun r -> Lwt.return (Some r)
         )
       >>= fun () ->
       Ocsimore_page.html_page sp {{ [<p>"Done"] }}
    )

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



(* Default permissions for the migration to the new permissions system *)
let _ = Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_title = name} ->
        User.add_to_group ~user:(basic_user User.anonymous)
          ~group:(Wiki.wiki_wikiboxes_grps.grp_reader $ wiki)
        >>= fun () ->
        User.add_to_group ~user:(basic_user User.anonymous)
          ~group:(Wiki.wiki_files_readers $ wiki)
        >>= fun () ->
        try Scanf.sscanf name "wikiperso for %s"
          (fun user ->
             User.get_user_by_name user
             >>= fun user ->
             User.add_to_group ~user ~group:(Wiki.wiki_admins $ wiki)
          )
        with Scanf.Scan_failure _ -> Lwt.return ()

     ))
