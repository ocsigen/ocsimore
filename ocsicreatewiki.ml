(* Eliom module that creates a new wiki. Will be removed when administration
   pages are written *)

open Lwt
open User_sql.Types
open Eliom_duce.Xhtml


type wiki_data =
    {
      name:string;
      path:string list;
      readers:user list option;
      admins:user list option;
      boxrights:bool
    }

let default_wiki_data =
  {
    name = "Wiki";
    path = [];
    readers = None;
    admins = None;
    boxrights = false
  }

let wiki_data =
  let rec find_wikidata data = function
    | [] -> Lwt.return data
    | (Simplexmlparser.Element ("name", [], s))::l -> 
        let name = Ocsigen_parseconfig.parse_string s in
        find_wikidata 
          {data with name = name}
          l
    | (Simplexmlparser.Element ("path", [], s))::l -> 
        let path = 
          Neturl.split_path (Ocsigen_parseconfig.parse_string s)
        in
        find_wikidata 
          {data with path = path}
          l
    | (Simplexmlparser.Element ("readers", [], s))::l -> 
        Users.user_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with readers = Some a}
          l
    | (Simplexmlparser.Element ("admins", [], s))::l -> 
        Users.user_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with admins = Some a}
          l
    | (Simplexmlparser.Element ("boxrights", [], []))::l -> 
        find_wikidata 
          {data with boxrights = true}
          l
    | (Simplexmlparser.Element (s, _, _))::_ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content <"^s^"> inside Ocsiwiki config"))
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsiwiki config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata default_wiki_data c)

let wiki_name_duce = Ocamlduce.Utf8.make wiki_data.name


let _ =
  Lwt_unix.run (
     Wiki_services.create_and_register_wiki
       ~model:Ocsisite.wikicreole_model
       ~author:Users.admin
       ~title:wiki_data.name
       ~descr:""
       ~path:wiki_data.path
       ?readers:wiki_data.readers
       ?admins:wiki_data.admins
       ~boxrights:wiki_data.boxrights
       ~container_text:Wiki_services.default_container_page
       ()
    )
