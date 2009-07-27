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

open Opaque

let (>>=) = Lwt.bind

(** Table of wiki models.
    Each wikis belongs to a "model" describing 
    - the default wikisyntax name
    - the right model
    - the widgets

    Shall we put also error_box? yes I think.
*)

exception Wiki_model_does_not_exist of string

type wiki_model =
    {wm_syntax : Xhtmltypes_duce.flows Wiki_types.content_type;
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
  ((fun ~name:k ~content_type:a ~rights:b ~widgets:c -> 
      let k = Wiki_types.wiki_model_of_string k in
      H.add t k
      {wm_syntax=a;
       wm_rights=b;
       wm_widgets=c;};
      k),
   (fun k -> 
      try (H.find t k).wm_rights 
      with Not_found -> raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k))),
   (fun k -> 
      try (H.find t k).wm_syntax
      with Not_found -> raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k))),
   (fun k -> 
      try (H.find t k).wm_widgets
      with Not_found -> raise (Wiki_model_does_not_exist (Wiki_types.string_of_wiki_model k)))
  )

(** Table of wiki syntaxes. *)
exception Content_type_does_not_exist of string

type wiki_preparser = 
    Eliom_sessions.server_params * Wiki_types.wikibox -> string -> string Lwt.t

type 'res wiki_parser =
    Wiki_widgets_interface.box_info -> string -> 'res Lwt.t
      (* pretty printer *) 

let register_flows_wiki_parser, 
  get_flows_wiki_parser, 
  get_flows_wiki_preparser =
  let module H = Hashtbl.Make(struct
                                type t =
                                    Xhtmltypes_duce.flows
                                      Wiki_types.content_type
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~preparser:a ~parser:b -> 
      let k = Wiki_types.content_type_of_string k in
      H.add t k (a, b); 
      k),
   (fun k -> 
      try snd (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))),
   (fun k -> 
      try fst (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist 
                                 (Wiki_types.string_of_content_type k))))

let register_inlines_wiki_parser, 
  get_inlines_wiki_parser, 
  get_inlines_wiki_preparser =
  let module H = Hashtbl.Make(struct
                                type t =
                                    Xhtmltypes_duce.inlines
                                      Wiki_types.content_type
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let t = H.create 10 in
  ((fun ~name:k ~preparser:a ~parser:b -> 
      let k' = Wiki_types.content_type_of_string k in
      H.add t k' (a, b); 
      (* we also register a flows parser: *)
      ignore (register_flows_wiki_parser k a 
                (fun bi s -> b bi s >>= fun r -> Lwt.return {{ [ <div>r ] }}));
      k'),
   (fun k -> 
      try snd (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist
                                 (Wiki_types.string_of_content_type k))),
   (fun k -> 
      try fst (H.find t k)
      with Not_found -> raise (Content_type_does_not_exist 
                                 (Wiki_types.string_of_content_type k))))

let get_default_wiki_parser s = 
  get_flows_wiki_parser (get_default_content_type s)

let get_default_wiki_preparser s = 
  get_flows_wiki_preparser (get_default_content_type s)


let css_content_type = 
  register_flows_wiki_parser
    ~name:"css"
    ~preparser:(fun _ s -> Lwt.return s)
    ~parser:(fun _bi s -> Lwt.return (Ocamlduce.Utf8.make s))

