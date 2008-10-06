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

let debug_print_cache = ref false

let print_cache s =
  if !debug_print_cache then print_endline s


let get_wikibox_data, 
  get_readers_,
  get_writers_,
  get_rights_adm_,
  get_wikiboxes_creators_,
  populate_readers,
  populate_writers,
  populate_rights_adm,
  populate_wikiboxes_creators,
  remove_readers,
  remove_writers,
  remove_rights_adm,
  remove_wikiboxes_creators,
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
  let cache = C.create (fun a -> Wiki_sql.get_wikibox_data_ a ()) 64 in
  let cacher = C2.create Wiki_sql.get_readers_ 64 in
  let cachew = C2.create Wiki_sql.get_writers_ 64 in
  let cachera = C2.create Wiki_sql.get_rights_adm_ 64 in
  let cachewc = C2.create Wiki_sql.get_wikiboxes_creators_ 64 in
  ((fun ?version ~wikibox () ->
    match version with
      | None -> 
          print_cache "cache wikibox ";
          C.find cache wikibox
      | Some v ->
          print_cache (Int32.to_string (snd wikibox) ^ " (with version) -> wikibox: db access");
          Wiki_sql.get_wikibox_data_ ?version ~wikibox ()
   ),
   (fun a -> print_cache "cache readers "; C2.find cacher a),
   (fun a -> print_cache "cache writers "; C2.find cachew a),
   (fun a -> print_cache "cache ra "; C2.find cachera a),
   (fun a -> print_cache "cache wc "; C2.find cachewc a),
  (fun a b r -> C2.remove cacher (a, b);  Wiki_sql.populate_readers_ a b r),
  (fun a b r -> C2.remove cachew (a, b);  Wiki_sql.populate_writers_ a b r),
  (fun a b r -> C2.remove cachera (a, b);  Wiki_sql.populate_rights_adm_ a b r),
  (fun a b r -> C2.remove cachewc (a, b);  Wiki_sql.populate_wikiboxes_creators_ a b r),
  (fun a b r -> C2.remove cacher (a, b);  Wiki_sql.remove_readers_ a b r),
  (fun a b r -> C2.remove cachew (a, b);  Wiki_sql.remove_writers_ a b r),
  (fun a b r -> C2.remove cachera (a, b);  Wiki_sql.remove_rights_adm_ a b r),
  (fun a b r -> C2.remove cachewc (a, b);  Wiki_sql.remove_wikiboxes_creators_ a b r),
  (fun ~wiki ~wikibox ~author ~comment ~content ->
     C.remove cache (wiki, wikibox);
     C2.remove cacher (wiki, wikibox);
     C2.remove cachew (wiki, wikibox);
     C2.remove cachera (wiki, wikibox);
     C2.remove cachewc (wiki, wikibox);
     Wiki_sql.update_wikibox_ ~wiki ~wikibox ~author ~comment ~content))

let get_box_for_page, set_box_for_page =
  let module C = Cache.Make (struct 
                               type key = (int32 * string)
                               type value = int32
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) -> Wiki_sql.get_box_for_page_ ~wiki ~page) 64 
  in
  ((fun ~wiki ~page -> 
      let page = Ocsigen_lib.remove_end_slash page in
      print_cache "cache wikipage ";
      C.find cache (wiki, page)),
   (fun ~wiki ~id ~page ->
      let page = Ocsigen_lib.remove_end_slash page in
      C.remove cache (wiki, page);
      Wiki_sql.set_box_for_page_ ~wiki ~id ~page
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
    print_cache "cache wiki ";
    Lwt.return (H.find wiki_info_table id)
  with Not_found -> Wiki_sql.find_wiki_ ~id

let update_wiki ~wiki_id ~container_id () =
  H.remove wiki_info_table wiki_id;
  Wiki_sql.update_wiki_ ~wiki:wiki_id ~container_id ()

(***)
let get_css_for_page, set_css_for_page =
  let module C = Cache.Make (struct 
                               type key = (int32 * string)
                               type value = string option
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) -> Wiki_sql.get_css_for_page_ ~wiki ~page) 64 
  in
  ((fun ~wiki ~page -> 
      print_cache "cache css";
      C.find cache (wiki, page) >>= function
        | None -> Lwt.fail Not_found
        | Some p -> Lwt.return p),
   (fun ~wiki ~page content ->
      C.remove cache (wiki, page);
      Wiki_sql.set_css_for_page_ ~wiki ~page content
   ))

(***)
let get_css_for_wiki, set_css_for_wiki =
  let module C = Cache.Make (struct 
                               type key = int32
                               type value = string option
                             end) 
  in
  let cache = 
    C.create (fun wiki -> Wiki_sql.get_css_for_wiki_ ~wiki) 8
  in
  ((fun ~wiki -> 
      print_cache "cache wikicss";
      C.find cache wiki >>= function
        | None -> Lwt.fail Not_found
        | Some p -> Lwt.return p),
   (fun ~wiki content ->
      C.remove cache wiki;
      Wiki_sql.set_css_for_wiki_ ~wiki content
   ))
