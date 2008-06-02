(** Eliom module that registers a wiki *)

open Lwt
open Eliom_duce.Xhtml


let wiki_name =
  let rec find_wikidata = function
    | [Simplexmlparser.Element ("name", [], s)] -> 
        Ocsigen_parseconfig.parse_string s
    | _ -> raise (Ocsigen_extensions.Error_in_config_file
                    ("Unexpected content inside Ocsiwiki config"))
  in
  let c = Eliom_sessions.get_config () in
  find_wikidata c

let wiki_name_duce = Ocamlduce.Utf8.make wiki_name

let container ?css content =
  let css = match css with
    | None -> {{ [] }}
    | Some c -> c
  in
 {{
    <html>[
      <head>[
        <title>wiki_name_duce
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

let _ =
  Lwt_unix.run
    (let example_sminfo = {
       Session_manager.url = ["users"];
       default_groups = [];
       administrator = Users.admin;
       login_actions = (fun sp sess -> return ());
       logout_actions = (fun sp -> return ());
       registration_mail_from = (wiki_name, 
                                 "webmaster@example.jussieu.fr");
       registration_mail_subject = wiki_name
     }
     in
     let example_sm = new example_sessionmanager example_sminfo in

     (* widgets creation: *)
     let _ = new User_widgets.login_widget example_sm in
     let mywikibox = new creole_wikibox () in
     (* all widgets created *)

     (* creating a wiki: *)
     Wiki.create_wiki 
       ~title:wiki_name
       ~descr:""
       ~wikibox:mywikibox
       ~path:[]
       () >>= fun wiki ->

     Lwt.return ()
    )


