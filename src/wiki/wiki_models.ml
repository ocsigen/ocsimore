(* Ocsimore
 * Copyright (C) 2005
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
   @author Boris Yakobowski
*)

open Eliom_pervasives
open Opaque

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

(** Table of wiki models.
    Each wikis belongs to a "model" describing
    - the default wikisyntax name
    - the right model
    - the widgets

    Shall we put also error_box? yes I think.
*)

exception Wiki_model_does_not_exist of string

type wiki_model =
    {wm_syntax : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type;
     wm_rights : Wiki_types.wiki_rights;
     wm_widgets : Wiki_widgets_interface.interactive_wikibox;
    }

let register_wiki_model, get_rights, get_default_content_type, get_widgets =
  let module H = Hashtbl.Make(struct
                                type t = Wiki_types.wiki_model
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~content_type:wm_syntax ~rights:wm_rights ~widgets:wm_widgets ->
      let k = Wiki_types.wiki_model_of_string k in
      H.add t k {wm_syntax; wm_rights; wm_widgets;};
      k),
   (fun k ->
      try (H.find t k).wm_rights
      with Not_found ->
        raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k))),
   (fun k ->
      try (H.find t k).wm_syntax
      with Not_found ->
        raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k))),
   (fun k ->
      try (H.find t k).wm_widgets
      with Not_found ->
        raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k)))
  )

(* Opening types... *)
let register_wiki_model ~name ~content_type ~rights ~widgets =
  let content_type =
    (content_type
       : [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type
       :> HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type) in
  register_wiki_model ~name ~content_type ~rights ~widgets
let get_default_content_type k =
  (get_default_content_type k
     : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type
     :> [> HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type)

(** Table of wiki syntaxes. *)
exception Content_type_does_not_exist of string

type wiki_preprocessor = (module Wiki_syntax_types.Preprocessor)

let identity_preprocessor =
  let module Identity_preprocessor = struct
    let preparse_string ?href_action ?link_action _ s = Lwt.return s
    let desugar_string ?href_action ?link_action _ s = Lwt.return s
  end in
  (module Identity_preprocessor : Wiki_syntax_types.Preprocessor)

let preparse_string ?href_action ?link_action wpp p c =
  let module Preprocessor = (val wpp : Wiki_syntax_types.Preprocessor) in
  Preprocessor.preparse_string ?href_action ?link_action p c

let desugar_string ?href_action ?link_action wpp p c =
  let module Preprocessor = (val wpp : Wiki_syntax_types.Preprocessor) in
  Preprocessor.desugar_string ?href_action ?link_action p c

type +'res wiki_parser =
    Wiki_widgets_interface.box_info -> string -> 'res Lwt.t
      (* pretty printer *)

let register_flows_wiki_parser,
  get_flows_wiki_parser,
  get_flows_wiki_preprocessor =
  let module H =
    Hashtbl.Make(struct
                   type t = HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type
                   let equal = (=)
                   let hash = Hashtbl.hash
                 end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~preprocessor:(a:wiki_preprocessor) ~parser_:b ->
      let k = Wiki_types.content_type_of_string k in
      H.add t k (a, (b : [< HTML5_types.flow5] HTML5.M.elt list wiki_parser
                       :> HTML5_types.flow5 HTML5.M.elt list wiki_parser));
      k),
   (fun k ->
      try snd (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))),
   (fun k ->
      try fst (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))))

(* Opening types ... *)
let register_flows_wiki_parser ~name ~preprocessor ~parser_ =
  let parser_ =
    (parser_ : [< HTML5_types.flow5] HTML5.M.elt list wiki_parser
             :> HTML5_types.flow5 HTML5.M.elt list wiki_parser) in
  (register_flows_wiki_parser ~name ~preprocessor ~parser_
     : HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type
     :> [> HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type)
let get_flows_wiki_parser k =
  let k = (k : [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type) in
  (get_flows_wiki_parser k : HTML5_types.flow5 HTML5.M.elt list wiki_parser
     :> [> HTML5_types.flow5] HTML5.M.elt list wiki_parser)
let get_flows_wiki_preprocessor k =
  let k = (k : [< HTML5_types.flow5] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.flow5 HTML5.M.elt list Wiki_types.content_type) in
  get_flows_wiki_preprocessor k

let register_flows_wiki_parser',
  get_flows_wiki_parser',
  get_flows_wiki_preprocessor' =
  let module H =
    Hashtbl.Make(struct
                   type t = HTML5_types.flow5_without_header_footer HTML5.M.elt list Wiki_types.content_type
                   let equal = (=)
                   let hash = Hashtbl.hash
                 end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~preprocessor:a ~parser_:(b:HTML5_types.flow5_without_header_footer HTML5.M.elt list wiki_parser) ->
      let k' = Wiki_types.content_type_of_string k in
      H.add t k' (a, b);
      (* we also register a flows parser: *)
      ignore (register_flows_wiki_parser k a
                (fun bi s -> b bi s >|= fun r -> [HTML5.M.div (r:HTML5_types.flow5_without_header_footer HTML5.M.elt list :> HTML5_types.flow5 HTML5.M.elt list)]));
      k'),
   (fun k ->
      try snd (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))),
   (fun k ->
      try fst (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))))


(* Opening types ... *)
let register_flows_wiki_parser' ~name ~preprocessor ~parser_ =
  let parser_ =
    (parser_ : [< HTML5_types.flow5_without_header_footer] HTML5.M.elt list wiki_parser
             :> HTML5_types.flow5_without_header_footer HTML5.M.elt list wiki_parser) in
  (register_flows_wiki_parser' ~name ~preprocessor ~parser_
     : HTML5_types.flow5_without_header_footer HTML5.M.elt list Wiki_types.content_type
     :> [> HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type)
let get_flows_wiki_parser' k =
  let k = (k : [< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.flow5_without_header_footer HTML5.M.elt list Wiki_types.content_type) in
  (get_flows_wiki_parser' k : HTML5_types.flow5_without_header_footer HTML5.M.elt list wiki_parser
     :> [> HTML5_types.flow5_without_header_footer] HTML5.M.elt list wiki_parser)
let get_flows_wiki_preprocessor' k =
  let k = (k : [< HTML5_types.flow5_without_header_footer] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.flow5_without_header_footer HTML5.M.elt list Wiki_types.content_type) in
  get_flows_wiki_preprocessor' k


let register_phrasings_wiki_parser,
  get_phrasings_wiki_parser,
  get_phrasings_wiki_preprocessor =
  let module H =
    Hashtbl.Make(struct
                   type t = HTML5_types.phrasing HTML5.M.elt list Wiki_types.content_type
                   let equal = (=)
                   let hash = Hashtbl.hash
                 end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~preprocessor:a ~parser_:b ->
      let k' = Wiki_types.content_type_of_string k in
      H.add t k' (a, b);
      (* we also register a flows parser: *)
      ignore (register_flows_wiki_parser' k a
                (fun bi s -> b bi s >|= fun r -> [HTML5.M.div (r:HTML5_types.phrasing HTML5.M.elt list :> HTML5_types.div_content_fun HTML5.M.elt list)]));
      k'),
   (fun k ->
      try snd (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))),
   (fun k ->
      try fst (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))))

(* Opening types ... *)
let register_phrasings_wiki_parser ~name ~preprocessor ~parser_ =
  let parser_ =
    (parser_ : [< HTML5_types.phrasing] HTML5.M.elt list wiki_parser
             :> HTML5_types.phrasing HTML5.M.elt list wiki_parser) in
  (register_phrasings_wiki_parser ~name ~preprocessor ~parser_
     : HTML5_types.phrasing HTML5.M.elt list Wiki_types.content_type
     :> [> HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type)
let get_phrasings_wiki_parser k =
  let k = (k : [< HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.phrasing HTML5.M.elt list Wiki_types.content_type) in
  (get_phrasings_wiki_parser k : HTML5_types.phrasing HTML5.M.elt list wiki_parser
     :> [> HTML5_types.phrasing] HTML5.M.elt list wiki_parser)
let get_phrasings_wiki_preprocessor k =
  let k = (k : [< HTML5_types.phrasing] HTML5.M.elt list Wiki_types.content_type
             :> HTML5_types.phrasing HTML5.M.elt list Wiki_types.content_type) in
  get_phrasings_wiki_preprocessor k


let get_default_wiki_parser s =
  get_flows_wiki_parser (get_default_content_type s)

let get_default_wiki_preprocessor s =
  get_flows_wiki_preprocessor (get_default_content_type s)

let css_content_type =
  register_flows_wiki_parser
    ~name:"css"
    ~preprocessor:identity_preprocessor
    ~parser_:(fun _bi s -> Lwt.return [HTML5.M.pcdata s])

