
open Lwt


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


class example_sessionmanager ~sessionmanagerinfo =
object (self)

  inherit Session_manager.sessionmanager ~sessionmanagerinfo
    
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
    (let example_sminfo = {
       Session_manager.url = ["users"];
       default_groups = [];
       administrator = Users.admin;
       login_actions = (fun sp sess -> return ());
       logout_actions = (fun sp -> return ());
       registration_mail_from = ("Ocsimore", 
                                 "webmaster@example.jussieu.fr");
       registration_mail_subject = "Ocsimore"
     }
     in
     let example_sm = new example_sessionmanager example_sminfo in

     (* widgets creation: *)
     let _ = new User_widgets.login_widget example_sm in
     let mywikibox = new creole_wikibox () in
     (* all widgets created *)

     Lwt.return mywikibox
    )


