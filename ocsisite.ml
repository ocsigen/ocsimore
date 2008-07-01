open Lwt

(*       default_groups = [];
       registration_mail_from = ("Ocsimore", 
                                 "webmaster@example.jussieu.fr");
       registration_mail_subject = "Ocsimore"
*)

let default_data = (None, None)

let (pam, basicusercreation) =
  let rec find_wikidata ((pam, basicusercreation) as data) = function
    | [] -> Lwt.return data
    | (Simplexmlparser.Element ("pam", ["service", s], []))::l -> 
        if Session_manager.pam_loaded ()
        then find_wikidata (Some (Some s), basicusercreation) l
        else
          raise
            (Ocsigen_config.Config_file_error
               "Ocsimore compiled without PAM support");
    | (Simplexmlparser.Element ("pam", [], []))::l -> 
        if Session_manager.pam_loaded ()
        then find_wikidata (Some None, basicusercreation) l
        else
          raise
            (Ocsigen_config.Config_file_error
               "Ocsimore compiled without PAM support");
    | (Simplexmlparser.Element ("notsecure", [], []))::l -> 
        Session_manager.set_secure false;
        find_wikidata data l
    | (Simplexmlparser.Element ("basicusercreation", atts, []))::l -> 
        let registration_mail_from = 
          try
            List.assoc "registration_mail_from" atts
          with Not_found -> 
            raise
              (Ocsigen_config.Config_file_error
                 "Missing registration_mail_from attribute inside <basicusercreation>")
        in
        let registration_mail_addr = 
          try
            List.assoc "registration_mail_addr" atts
          with Not_found -> 
            raise
              (Ocsigen_config.Config_file_error
                 "Missing registration_mail_addr attribute inside <basicusercreation>")
        in
        let registration_mail_subject = 
          try
            List.assoc "registration_mail_subject" atts
          with Not_found -> "Ocsimore registration"
        in
        (try
          Users.group_list_of_string (List.assoc "groups" atts)
        with Not_found -> Lwt.return [Users.authenticated_users.Users.id])
        >>= fun default_groups ->
        find_wikidata 
          (pam, 
           Some ((registration_mail_from, registration_mail_addr),
                 registration_mail_subject, 
                 default_groups)) 
          l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsisite config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata default_data c)




let container ?css content =
  let css = match css with
    | None -> {{ [] }}
    | Some c -> c
  in
  {{
     <html>[
       <head>[
         <title>"Ocsimore"
           !css
       ]
       <body>content
     ]
   }}


class login_widget_basic_user_creation ~sessman data =
object (self)

  inherit User_widgets.login_widget_basic_user_creation ~sessman data


  method container ~sp ~sd ~contents = Lwt.return (container contents)


end;;


class creole_wikibox () adminwikiinfo = object
  inherit Wiki_widgets.editable_wikibox () adminwikiinfo

  method pretty_print_wikisyntax ?subbox ~ancestors ~sp ~sd w content =
    Wiki_syntax.xml_of_wiki ?subbox ~ancestors ~sp ~sd w content

  method container = container

end

let wiki_help_box = 3l

let get_admin_wiki_fun = ref (fun () -> failwith "Ocsisite.get_admin_wiki")

let get_admin_wiki () = (!get_admin_wiki_fun ()).Wiki.id


let wikibox =
  Lwt_unix.run
    (let sminfo = {
       Session_manager.url = ["users"];
       administrator = Users.admin;
       login_actions = (fun sp sess -> return ());
       logout_actions = (fun sp -> return ());
     }
     in
     let sm = 
       match pam with
         | Some pam_service -> 
             new Session_manager.sessionmanager_pam pam_service sminfo
         | None -> new Session_manager.sessionmanager sminfo 
     in

     (match basicusercreation with
       | Some data ->
           let _ = new User_widgets.login_widget_basic_user_creation sm data in
           ()
       | None -> 
           let _ = new User_widgets.login_widget sm in
           ());

     Lwt.return (new creole_wikibox () (get_admin_wiki, wiki_help_box))
    )


let _ =
  let a =
    Lwt_unix.run
      ((* creating a wiki for the administration boxes: *)
        Wiki.create_wiki 
          ~title:"Adminwiki"
          ~descr:"Administration boxes"
          ~wikibox:wikibox
          ?path:None
          (*       ~readers:[Users.admin]
                   ~writers:[Users.admin]
                   ~rights_adm:[Users.admin]
                   ~wikiboxes_creators:[Users.admin]
                   ~container_adm:[Users.admin]
                   ~page_creators:[Users.admin]
                   ~css_editors:[Users.admin]
                   ~admins:[Users.admin] *)
          ~boxrights:true
          ()
      )
  in
  get_admin_wiki_fun := (fun () -> a)


let _ =
    Lwt_unix.run
(* Now done in new_wikibox
      (Lwt.catch
         (fun () -> 
            Wiki_cache.get_wikibox_data ~wikibox:(get_admin_wiki (), wiki_help_box) ()
            >>= fun _ -> Lwt.return ())
         (function
            | Not_found ->
*)
(*VVV Warning!! Dangerous! How to do this in cleaner way? *)
      (Wiki.new_wikibox
         ~boxid:wiki_help_box
         ~wiki:(!get_admin_wiki_fun ())
         ~author:Users.admin.Users.id
         ~comment:"Wikisyntax help" 
         ~content:"===Wiki syntax===

This wiki is using [[http://www.wikicreole.org|Wikicreole]]'s syntax, with a few extensions.

{{../creole_cheat_sheet.png|Wikicreole's syntax}}"
         ()
       >>= fun _ -> Lwt.return ())

