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

(* Wiki_syntax : here because of circular dependencies *)
let extension_table = Hashtbl.create 8

let find_extension ~name =
  Hashtbl.find extension_table name
(* end Wiki_syntax *)

module Wikifilter_table = Hashtbl.Make(struct
                                         type t = string
                                         let equal = (=)
                                         let hash = Hashtbl.hash 
                                       end)

let preparser_extension_table = Wikifilter_table.create 8

let add_preparser_extension ~name f = 
  Wikifilter_table.add preparser_extension_table name f

let wikifilter_find = Wikifilter_table.find preparser_extension_table

type c = {
  sp : Eliom_sessions.server_params;
  buf : Buffer.t;
}

let nothing _ _ = ()
let nothing1 _ = ()

let make_plugin_action wiki =
  let subst = ref [] in
  ((fun name start end_ params args content ->
      subst := (start, 
                end_, 
                (try
                   wikifilter_find name
                     wiki params args content 
                 with Not_found -> Lwt.return None))::!subst)
   ,
   fun () -> !subst
  )

let builder plugin_action =
  { W.chars = nothing1;
    W.strong_elem = nothing;
    W.em_elem = nothing;
    W.a_elem = (fun _ _ _ _ -> ());
    W.make_href = (fun _ a -> a);
    W.br_elem = nothing1;
    W.img_elem = (fun _ _ _ -> ());
    W.tt_elem = nothing;
    W.monospace_elem = nothing;
    W.underlined_elem = nothing;
    W.linethrough_elem = nothing;
    W.subscripted_elem = nothing;
    W.superscripted_elem = nothing;
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
    W.dl_elem = nothing;
    W.hr_elem = nothing1;
    W.table_elem = nothing;
    W.inline = nothing1;
    W.plugin =
      (fun name -> 
         let wiki_content =
           try fst (find_extension ~name)
           with Not_found -> false
         in (wiki_content, (fun _ _ _ -> Wikicreole.A_content ())));
    W.plugin_action = plugin_action;
    W.error = nothing1;
  }

let preparse_extension ((sp, _) as param) wiki content =
  let (plugin_action, get_subst) = make_plugin_action wiki in
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
