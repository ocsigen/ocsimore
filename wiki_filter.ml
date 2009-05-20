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

let preparser_extension_table :
    ((Eliom_sessions.server_params * Wiki_sql.Types.wikibox) ->
     (string * string) list ->
      string option ->
      string option Lwt.t) Wikifilter_table.t
= Wikifilter_table.create 8

let add_preparser_extension ~name f = 
  Wikifilter_table.add preparser_extension_table name f

let wikifilter_find = Wikifilter_table.find preparser_extension_table

type c = {
  sp : Eliom_sessions.server_params;
  buf : Buffer.t;
}

let nothing _ _ = ()
let nothing1 _ = ()

let make_plugin_action (* wiki *) =
  let subst = ref [] in
  ((fun name start end_ params args content ->
      subst := (start, 
                end_, 
                (try
                   wikifilter_find name
                     params args content 
                 with Not_found -> Lwt.return None))::!subst)
   ,
   fun () -> !subst
  )

let builder plugin_action =
  { Wikicreole.chars = nothing1;
    strong_elem = nothing;
    em_elem = nothing;
    a_elem = (fun _ _ _ _ -> ());
    make_href = (fun _ _ a -> a);
    br_elem = nothing1;
    img_elem = (fun _ _ _ -> ());
    tt_elem = nothing;
    monospace_elem = nothing;
    underlined_elem = nothing;
    linethrough_elem = nothing;
    subscripted_elem = nothing;
    superscripted_elem = nothing;
    nbsp = ();
    p_elem = nothing;
    pre_elem = nothing;
    h1_elem = nothing;
    h2_elem = nothing;
    h3_elem = nothing;
    h4_elem = nothing;
    h5_elem = nothing;
    h6_elem = nothing;
    ul_elem = nothing;
    ol_elem = nothing;
    dl_elem = nothing;
    hr_elem = nothing1;
    table_elem = nothing;
    inline = nothing1;
    plugin =
      (fun name -> 
         let wiki_content =
           try fst (find_extension ~name)
           with Not_found -> false
         in (wiki_content, (fun _ _ _ -> Wikicreole.A_content ())));
    plugin_action = plugin_action;
    error = nothing1;
  }

let preparse_extension (sp, wb : Eliom_sessions.server_params * Wiki_sql.Types.wikibox) content =
  let (plugin_action, get_subst) = make_plugin_action in
  let builder = builder plugin_action in
  ignore (Wikicreole.from_string sp (sp, wb) builder content);
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
