(* Ocsimore
 * Copyright (C) 2008 Vincent Balat
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
Parsing the global Ocsimore configuration

@author Vincent Balat
*)

let unexpected_pcdata ~in_tag pcdata =
  Ocsigen_extensions.Error_in_config_file
    ("Unexpected pcdata ..."^pcdata^"... inside "^in_tag)

let unexpected_tag ~in_tag tag =
  Ocsigen_extensions.Error_in_config_file
    ("Unexpected tag \""^tag^"\" in content of \""^in_tag^"\"")

let unexpected_content ~in_tag =
  Ocsigen_extensions.Error_in_config_file
    ("Unexpected content in tag \""^in_tag^"\"")

let unexpected_value ~in_tag ~in_attribute value =
  Ocsigen_extensions.Error_in_config_file
    ("Unexpected value \""^value^"\" for attribute \""^in_attribute^"\" in tag \""^in_tag^"\"")

let unexpected_attribute ~in_tag attr =
  Ocsigen_extensions.Error_in_config_file
    ("Unexpected attribute \""^attr^"\" inside tag \""^in_tag^"\"")

let parse_yesno ~in_attribute yesno =
  match yesno with
    | "yes" -> true
    | "no" -> false
    | _ -> raise (Ocsigen_extensions.Error_in_config_file
                    ("Incorrect value for attribute '"^in_attribute^"'"))

let rec parse_config = function
  | [] -> ()
  | (Simplexmlparser.Element ("internationalization" as in_tag, attrs, content)) :: l ->
       List.iter
         (function | "language", "francais" ->
                Language.messages := Language.messages_french
            | "language", "english" ->
                Language.messages := Language.messages_english
            | "language" as in_attribute, lang ->
                raise (unexpected_value ~in_tag ~in_attribute lang)
            | attr, _ ->
                raise (unexpected_attribute ~in_tag attr))
         attrs;
       if content <> [] then
         raise (unexpected_content ~in_tag);
       parse_config l
  | (Simplexmlparser.Element ("mailer" as in_tag, attrs, content)) :: l ->
      List.iter
        (function | "bin", mailer ->
               Ocsimore_config.mailer := mailer;
           | attr, _ ->
               raise (unexpected_attribute ~in_tag attr))
        attrs;
      if content <> [] then
        raise (unexpected_content ~in_tag:"mailer");
      parse_config l
  | (Simplexmlparser.Element ("administration" as in_tag, attrs, content))::l ->
       List.iter
         (function | "path", path ->
                Ocsimore_config.admin_dir := path;
            | attr, _ ->
                raise (unexpected_attribute ~in_tag attr))
         attrs;
       if content <> [] then
         raise (unexpected_content ~in_tag);
      parse_config l
  | (Simplexmlparser.Element ("internals" as in_tag, attrs, content))::l ->
       List.iter
         (function
            | "application-name", name ->
                Ocsimore_config.application_name := name
            | "aggregate-css" as in_attribute, yesno ->
                Ocsimore_config.aggregate_css := parse_yesno ~in_attribute yesno
            | attr, _ ->
                raise (unexpected_attribute ~in_tag attr))
         attrs;
       List.iter
         (function | Simplexmlparser.Element ("group", [("name", name)], [s]) ->
                Ocsimore_config.dyngroupstobecreated :=
                  (name, s)::!Ocsimore_config.dyngroupstobecreated;
            | Simplexmlparser.Element ("group", _, _) ->
                raise (Ocsigen_extensions.Error_in_config_file
                         ("Unexpected content for tag \"group\""))
            | Simplexmlparser.Element (name, _, _) ->
                raise (Ocsigen_extensions.Error_in_config_file
                         ("Unexpected content \""^name^"\" in Ocsimore's tag internals"))
            | Simplexmlparser.PCData pcdata ->
                raise (unexpected_pcdata ~in_tag pcdata))
         content;
       if content <> [] then
         raise (Ocsigen_extensions.Error_in_config_file "Unexpected content in tag \"internals\"");
       parse_config l
  | (Simplexmlparser.Element ("database" as in_tag, attribs, content))::l ->
    List.iter
      (function | "name", name -> Ocsimore_config.db_name := name;
         | "user", user -> Ocsimore_config.db_user := user
         | "host", host -> Ocsimore_config.db_host := Ocsimore_config.opt host
         | "port", port ->
           begin try
             Ocsimore_config.db_port :=
               Eliom_pervasives.map_option int_of_string (Ocsimore_config.opt port)
             with _ ->
               raise (Ocsigen_extensions.Error_in_config_file
                        ("Incorrect attribute inside ocsimore database config: port ("^port^") isn't an integer."))
           end
         | "socket-dir", dir ->
             Ocsimore_config.db_unix_domain_socket_dir := Ocsimore_config.opt dir
         | "password-file", name ->
             (let c = open_in name in
              try
                let s = input_line c in
                Ocsimore_config.db_password := Some s;
                close_in c
              with e -> close_in c; raise e)
         | attr, _ ->
             raise (unexpected_attribute ~in_tag attr))
      attribs;
    if content <> [] then
      raise (unexpected_content ~in_tag);
    parse_config l
  | (Simplexmlparser.Element ("wiki" as in_tag, attrs, content)) :: l ->
    List.iter
      (function | "headings-backref" as in_attribute, yesno ->
           Ocsimore_config.wiki_headings_backref := parse_yesno ~in_attribute yesno
         | attr, _ -> raise (unexpected_attribute ~in_tag attr))
      attribs;
  | Simplexmlparser.Element (name, _, _) :: _ ->
      raise (unexpected_tag ~in_tag:"ocsimore module" name)
  | Simplexmlparser.PCData pcdata :: _ ->
      raise (unexpected_pcdata ~in_tag:"ocsimore module" pcdata)


let () =
  Ocsigen_extensions.register_extension
    ~name:"ocsimore"
    ~init_fun:parse_config
    ()
