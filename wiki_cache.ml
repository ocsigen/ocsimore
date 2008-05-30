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

let get_wikibox_data, 
  get_readers_,
  get_writers_,
  get_rights_adm_,
  get_wikiboxes_creators_,
  update_wikibox =
  let module C = Cache.Make (struct 
                               type key = (int32 * int32)
                               type value = (string * 
                                               User_sql.userid * 
                                               string * 
                                               CalendarLib.Calendar.t) option
                             end) 
  in
  let module C2 = Cache.Make (struct 
                                type key = (int32 * int32)
                                type value = User_sql.userid list
                             end) 
  in
  let cache = C.create (fun a -> Wiki_sql.get_wikibox_data a ()) 64 in
  let cacher = C2.create Wiki_sql.get_readers 64 in
  let cachew = C2.create Wiki_sql.get_writers 64 in
  let cachera = C2.create Wiki_sql.get_rights_adm 64 in
  let cachewc = C2.create Wiki_sql.get_wikiboxes_creators 64 in
  ((fun ?version ~wikibox () ->
    match version with
      | None -> C.find cache wikibox
      | Some v ->
          print_string (Int32.to_string (snd wikibox));
          print_endline " (with version) -> wikibox: db access";
          Wiki_sql.get_wikibox_data ?version ~wikibox ()
   ),
   C2.find cacher,
   C2.find cachew,
   C2.find cachera,
   C2.find cachewc,
  (fun ~wiki ~wikibox ~author ~comment ~content 
    ?readers ?writers ?rights_adm ?wikiboxes_creators () ->
     C.remove cache (wiki, wikibox);
     C2.remove cacher (wiki, wikibox);
     C2.remove cachew (wiki, wikibox);
     C2.remove cachera (wiki, wikibox);
     C2.remove cachewc (wiki, wikibox);
     Wiki_sql.update_wikibox ~wiki ~wikibox ~author ~comment ~content 
       ?readers ?writers ?rights_adm ?wikiboxes_creators ()))

let get_box_for_page, set_box_for_page =
  let module C = Cache.Make (struct 
                               type key = (int32 * string)
                               type value = int32
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) -> Wiki_sql.get_box_for_page ~wiki ~page) 64 
  in
  ((fun ~wiki ~page -> C.find cache (wiki, page)),
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

