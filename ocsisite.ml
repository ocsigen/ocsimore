
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
        find_wikidata (Some (Some s), basicusercreation) l
    | (Simplexmlparser.Element ("pam", [], []))::l -> 
        find_wikidata (Some None, basicusercreation) l
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



class creole_wikibox () = object
  inherit Wiki_widgets.editable_wikibox ()

  method pretty_print_wikisyntax ?subbox ~ancestors ~sp ~sd w content =
    Wiki_syntax.xml_of_wiki ?subbox ~ancestors ~sp ~sd w content

  method container = container

end

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

     (* widgets creation: *)
     (match basicusercreation with
       | Some data ->
           let _ = new User_widgets.login_widget_basic_user_creation sm data in
           ()
       | None -> 
           let _ = new User_widgets.login_widget sm in
           ());

     let mywikibox = new creole_wikibox () in
     (* all widgets created *)

     Lwt.return mywikibox
    )


