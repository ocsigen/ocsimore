(* Ocsimore
 * Copyright (C) 2008
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

   @author Vincent Balat
*)

let (>>=) = Lwt.bind

let get_wikibox_data, update_wikibox =
  let module C = Cache.Make (struct 
                               type key = (int32 * int32)
                               type value = (string * 
                                               User_sql.userid * 
                                               string * 
                                               CalendarLib.Calendar.t) option
                             end) 
  in
  let cache = C.create 64 in
  ((fun ?version ~wikibox () ->
    match version with
      | None ->
          (try
            Lwt.return (C.find cache wikibox)
          with Not_found ->
            print_string (Int32.to_string (snd wikibox));
            print_endline " -> wikibox: db access";
            Wiki_sql.get_wikibox_data wikibox () >>= fun r ->
            C.add cache wikibox r;
            Lwt.return r)
      | Some v ->
          print_string (Int32.to_string (snd wikibox));
          print_endline " (with version) -> wikibox: db access";
          Wiki_sql.get_wikibox_data ?version ~wikibox ()
   ),
  (fun ~wiki ~wikibox ~author ~comment ~content 
    ?readers ?writers ?admins () ->
     C.remove cache (wiki, wikibox);
     Wiki_sql.update_wikibox ~wiki ~wikibox ~author ~comment ~content 
       ?readers ?writers ?admins ()))

let get_box_for_page, set_box_for_page =
  let module C = Cache.Make (struct 
                               type key = (int32 * string)
                               type value = int32
                             end) 
  in
  let cache = C.create 64 in
  ((fun ~wiki ~page ->
      try
        Lwt.return (C.find cache (wiki, page))
      with Not_found ->
        print_string page;
        print_endline " -> wikipage: db access";
        Wiki_sql.get_box_for_page ~wiki ~page >>= fun r ->
        C.add cache (wiki, page) r;
        Lwt.return r
   ),
   (fun ~wiki ~id ~page ->
      C.remove cache (wiki, page);
      Wiki_sql.set_box_for_page ~wiki ~id ~page
   ))


(***)
module H = Hashtbl.Make(struct
                          type t = int32
                          let equal = (=)
                          let hash = Hashtbl.hash 
                        end)

let wiki_info_table = H.create 8

let find_wiki id =
  try
    Lwt.return (H.find wiki_info_table id)
  with Not_found -> Wiki_sql.find_wiki ~id ()

