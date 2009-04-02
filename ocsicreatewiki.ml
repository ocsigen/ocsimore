(* Eliom module that creates a new wiki. Will be removed when administration
   pages are written *)

open Lwt
open Eliom_duce.Xhtml


type wiki_data =
    {
      name:string;
      path:string list;
      readers:User_sql.userid list option;
      writers:User_sql.userid list option;
      rights_adm:User_sql.userid list option;
      wikiboxes_creators:User_sql.userid list option;
      container_adm:User_sql.userid list option;
      page_creators:User_sql.userid list option;
      css_editors:User_sql.userid list option;
      admins:User_sql.userid list option;
      boxrights:bool
    }

let default_wiki_data =
  {
    name = "Wiki";
    path = [];
    readers = None;
    writers = None;
    rights_adm = None;
    wikiboxes_creators = None;
    container_adm = None;
    page_creators = None;
    css_editors = None;
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
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with readers = Some a}
          l
    | (Simplexmlparser.Element ("writers", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with writers = Some a}
          l
    | (Simplexmlparser.Element ("rightsadm", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with rights_adm = Some a}
          l
    | (Simplexmlparser.Element ("wikiboxescreators", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with wikiboxes_creators = Some a}
          l
    | (Simplexmlparser.Element ("containeradm", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with container_adm = Some a}
          l
    | (Simplexmlparser.Element ("pagecreators", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with page_creators = Some a}
          l
    | (Simplexmlparser.Element ("csseditors", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
        >>= fun a ->
        find_wikidata 
          {data with css_editors = Some a}
          l
    | (Simplexmlparser.Element ("admins", [], s))::l -> 
        Users.group_list_of_string (Ocsigen_parseconfig.parse_string s) 
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
       ~title:wiki_data.name
       ~descr:""
       ~path:wiki_data.path
       ?readers:wiki_data.readers
       ?writers:wiki_data.writers
       ?rights_adm:wiki_data.rights_adm
       ?wikiboxes_creators:wiki_data.wikiboxes_creators
       ?container_adm:wiki_data.container_adm
       ?page_creators:wiki_data.page_creators
       ?css_editors:wiki_data.css_editors
       ?admins:wiki_data.admins
       ~boxrights:wiki_data.boxrights
       ~container_page:Wiki_services.default_container_page
       ~wikibox_widget:Ocsisite.wikibox_widget
       ()
    )


