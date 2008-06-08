
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

(*
class login_widget ~sessman =
object (self)

  inherit User_widgets.login_widget ~sessman
(*VVV or login_widget_basic_user_creation, etc. make this configurable *)
(*       default_groups = [];
       registration_mail_from = ("Ocsimore", 
                                 "webmaster@example.jussieu.fr");
       registration_mail_subject = "Ocsimore"
*)
(*
  method container ~sp ~sd ~contents = Lwt.return (container contents)
*)

end;;
*)
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
     let sm = new Session_manager.sessionmanager sminfo in

     (* widgets creation: *)
     let _ = new User_widgets.login_widget sm in
     let mywikibox = new creole_wikibox () in
     (* all widgets created *)

     Lwt.return mywikibox
    )


