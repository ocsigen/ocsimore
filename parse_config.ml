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

let _ =
  let rec parse_config = function
    | [] -> Lwt.return ()
    | (Simplexmlparser.Element ("passwordfile", [("name", name)], []))::l -> 
        let c = Lwt_chan.open_in name in
        Lwt.catch
          (fun () -> 
             Lwt_chan.input_line c >>= fun s ->
             Ocsimore_config.password := s;
             Lwt_chan.close_in c)
          (fun e -> Lwt_chan.close_in c >>= fun () ->
             Lwt.fail e) >>= fun () ->
        parse_config l
    | (Simplexmlparser.Element ("group", [("name", name)], [s]))::l -> 
        Ocsimore_config.dyngroupstobecreated := 
          (name, s)::!Ocsimore_config.dyngroupstobecreated;
        parse_config l
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                    "Unexpected content inside ocsimore config")
  in
  let c = Ocsigen_extensions.get_config () in
  Lwt_unix.run (parse_config c)

  
