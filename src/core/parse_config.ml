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

let (>>=) = Lwt.bind

let rec parse_config = function
  | [] -> ()
  | (Simplexmlparser.Element ("passwordfile", [("name", name)], []))::l ->
      (let c = open_in name in
       try
         let s = input_line c in
         Ocsimore_config.db_password := Some s;
         close_in c
       with e -> close_in c; raise e);
      parse_config l
  | (Simplexmlparser.Element ("group", [("name", name)], [s]))::l ->
      Ocsimore_config.dyngroupstobecreated :=
        (name, s)::!Ocsimore_config.dyngroupstobecreated;
      parse_config l
  | (Simplexmlparser.Element ("language", [("lang", "francais")], []))::l ->
      Language.messages := Language.messages_french;
      parse_config l
  | (Simplexmlparser.Element ("language", [("lang", "english")], []))::l ->
      Language.messages := Language.messages_english;
      parse_config l
  | (Simplexmlparser.Element ("application_name", ["name", name], _)) :: l ->
      Ocsimore_config.application_name := name;
      parse_config l
  | (Simplexmlparser.Element ("database", attribs, []))::l ->
    List.iter (function
                  | "name", name -> Ocsimore_config.db_name := name;
                  | "user", user -> Ocsimore_config.db_user := user
                  | "host", host -> Ocsimore_config.db_host := Ocsimore_config.opt host
                  | "post", port ->
                    begin try
                      Ocsimore_config.db_port :=
                        Eliom_pervasives.map_option int_of_string (Ocsimore_config.opt port)
                      with _ ->
                        raise (Ocsigen_extensions.Error_in_config_file
                                 ("Incorrect attribute inside ocsimore database config: port ("^port^") isn't an integer."))
                    end
                  | "socket_dir", dir ->
                      Ocsimore_config.db_unix_domain_socket_dir := Ocsimore_config.opt dir
                  | attrib, _ ->
                    raise (Ocsigen_extensions.Error_in_config_file
                             ("Unexpected attribute inside ocsimore database config: "^attrib)))
      attribs;
    parse_config l
  | Simplexmlparser.Element (name, _, _) :: _ ->
      raise (Ocsigen_extensions.Error_in_config_file
               ("Unexpected content <"^name^"> inside ocsimore config"))
  | Simplexmlparser.PCData pcdata :: _ ->
      raise (Ocsigen_extensions.Error_in_config_file
               ("Unexpected pcdata ..."^pcdata^"... inside ocsimore config"))

let () = Ocsigen_extensions.register_extension
  ~name:"ocsimore" ~init_fun:parse_config ()
