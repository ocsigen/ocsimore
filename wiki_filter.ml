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
   Filtering wiki syntax to perform special actions
   (before saving it to the database)
   @author Vincent Balat
*)

let (>>=) = Lwt.bind

module W = Wikicreole

module H = Hashtbl.Make(struct
                          type t = string
                          let equal = (=)
                          let hash = Hashtbl.hash 
                        end)

let preparser_extension_table = H.create 8

let add_preparser_extension k f = H.add preparser_extension_table k f

type c = {
  sp : Eliom_sessions.server_params;
  sd : Ocsimore_common.session_data;
  buf : Buffer.t;
}

let nothing _ = ()

let make_plugin_action wiki_id =
  let subst = ref [] in
  ((fun name start end_ params args content ->
      subst := (start, 
                end_, 
                (try
                   H.find preparser_extension_table name
                     wiki_id params args content 
                 with Not_found -> Lwt.return None))::!subst)
   ,
   fun () -> !subst
  )

let builder plugin_action =
  { W.chars = nothing;
    W.strong_elem = nothing;
    W.em_elem = nothing;
    W.a_elem = (fun _ _ _ -> ());
    W.br_elem = nothing;
    W.img_elem = (fun _ _ -> ());
    W.tt_elem = nothing;
    W.nbsp = ();
    W.p_elem = nothing;
    W.pre_elem = nothing;
    W.h1_elem = nothing;
    W.h2_elem = nothing;
    W.h3_elem = nothing;
    W.h4_elem = nothing;
    W.h5_elem = nothing;
    W.h6_elem = nothing;
    W.ul_elem = nothing;
    W.ol_elem = nothing;
    W.hr_elem = nothing;
    W.table_elem = nothing;
    W.inline = nothing;
    W.block_plugin = (fun _ _ _ _ -> ());
    W.a_content_plugin = (fun _ _ _ _ -> ());
    W.link_plugin = (fun _ _ _ _ -> ("", ()));
    W.plugin_action = plugin_action;
    W.error = nothing;
  }

let preparse_extension ((sp, _, _) as param) wiki_id content =
  let (plugin_action, get_subst) = make_plugin_action wiki_id in
  let builder = builder plugin_action in
  ignore (Wikicreole.from_string sp param builder content);
  let buf = Buffer.create 1024 in
  Lwt_util.fold_left
    (fun pos (start, end_, replacement) -> 
       replacement >>= function
         | None -> Lwt.return pos;
         | Some replacement ->
             Buffer.add_substring buf content pos (start - pos);
             Buffer.add_string buf replacement;
             Lwt.return end_
    )
    0
    (List.rev (get_subst ()))
  >>= fun pos ->
  let l = String.length content in
  if pos < l 
  then Buffer.add_substring buf content pos (l - pos);
  Lwt.return (Buffer.contents buf)
