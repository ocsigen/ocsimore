(* Ocsimore
 * Copyright (C) 2009
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
   @author Boris Yakobowski
*)

let static_service = ref None

let set_service_for_static_files service =
  match !static_service with
    | None -> static_service := Some service
    | Some _ ->
        Ocsigen_messages.errlog "In Ocsimore_common: Static services already \
                    loaded. Ignoring call to set_service_for_static_files"

let static_service () =
  match !static_service with
    | None -> failwith "No service for static files"
    | Some s -> s


let static_file_uri ~sp ~path =
  Eliom_duce.Xhtml.make_uri ~service:(static_service ()) ~sp path



module Header = (
  struct

    type header = bool Polytables.key
    
    let header_table = ref []

    let create_header header_content =
      let k = Polytables.make_key () in
      header_table := (k, header_content)::!header_table;
      k

    let require_header header ~sp =
      Polytables.set
        ~table:(Eliom_sessions.get_request_cache sp)
        ~key:header
        ~value:true

    let generate_headers ~sp =
      let l =
        List.fold_left
          (fun beg (key, header_content) -> 
             let tobeincluded =
               try
                 Polytables.get
                   ~table:(Eliom_sessions.get_request_cache sp)
                   ~key
               with Not_found -> false
             in
             if tobeincluded
             then (header_content sp)::beg
             else beg)
          []
          !header_table
      in {{ (map {: l :} with i -> i) }}
        
  end : sig

    type header

    (** Define a new header *)
    val create_header : 
      (Eliom_sessions.server_params -> {{ [ Xhtmltypes_duce.head_misc* ] }})
      -> header
    
    (** Call this function every time you need a header to be included
        in the page. If this function is called several times for the same
        page with the same header, the header will be included only once.
    *)
    val require_header : header -> sp:Eliom_sessions.server_params -> unit

    (** This function is called to generate the headers for one page.
        Only required headers are included.
    *)
    val generate_headers : 
      sp:Eliom_sessions.server_params -> {{ [ Xhtmltypes_duce.head_misc* ] }}

  end)

let admin_pages_header = 
  Header.create_header
    (fun sp ->
       {{ [ {: Eliom_duce.Xhtml.css_link
               (static_file_uri sp ["ocsiadmin.css"]) () :}
          ] }})

(* special handling for onload functions (?) *)
let polytable_onload = Polytables.make_key ()

let onload_functions sp =
  try Polytables.get ~table:(Eliom_sessions.get_request_cache sp)
    ~key:polytable_onload
  with Not_found -> []

let add_onload_function ?(first = false) sp s =
  Polytables.set ~table:(Eliom_sessions.get_request_cache sp)
    ~key:polytable_onload
    ~value:(if first then onload_functions sp @ [s]
            else s :: onload_functions sp)

(* Obrowser *)
let obrowser_header =
  Header.create_header
    (fun sp ->
       let eliom_obrowser = Eliom_duce.Xhtml.js_script
         ~uri:(static_file_uri sp ["eliom_obrowser.js"]) ()
       and vm = Eliom_duce.Xhtml.js_script
         ~uri:(static_file_uri sp ["vm.js"]) ()
       in
       add_onload_function ~first:true sp
         (Printf.sprintf "main_vm = exec_caml ('%s/ocsimore_client.uue')"
            (static_file_uri sp ["."]));
       {{ [ vm eliom_obrowser  ] }}
    )

let add_onload_function = add_onload_function ~first:false

(* shortcuts: *)
let add_obrowser_header = Header.require_header obrowser_header
let add_admin_pages_header = Header.require_header admin_pages_header


let html_page
    ~sp ?(body_classes=[]) ?(css={{ [] }}) ?(title="Ocsimore") content =
  let title = Ocamlduce.Utf8.make title
  and headers = Header.generate_headers sp
  and classes = Ocsimore_lib.build_class_attr body_classes
  and onload_body, onload_script = match onload_functions sp with
    | [] -> {{ {} }}, {{ [] }}
    | l -> {{ { onload="bodyOnload()" } }},
        {{ [ <script type="text/javascript">
                         {: Printf.sprintf "function bodyOnload() { \n%s;\n}"
                            (String.concat ";\n" (List.rev l)) :}
           ] }}
  in
  Lwt.return {{
      <html xmlns="http://www.w3.org/1999/xhtml">[
        <head>[
          <title>title
          !headers
          !onload_script
          !css
        ]
        <body ({id="body" class={: classes :} } ++ onload_body)>content
      ]
    }}




(** Admin page *)


let root_service = ref None

let set_root_admin_service service =
  root_service := Some service


open Eliom_tools_common

type menu_link_service =
    (Eliom_services.get_service_kind, [ `Registrable ])
    Eliom_tools_common.one_page

let admin_menu = ref []

let menu_link (text, service) =
  (Ocamlduce.Utf8.make text, Site_tree (Main_page service, []))

let add_to_admin_menu ~name ~links =
  admin_menu :=
    ({{ [ <span class="admin-menu-root">{{ Ocamlduce.Utf8.make name }} ] }},
     Site_tree (Not_clickable, List.map menu_link links))
  :: !admin_menu

let admin_menu ?service sp =
  match !root_service with
    | None -> failwith "Ocsimore_admin_menu: no root service registered"
    | Some s ->
        let menu = Main_page s, List.rev !admin_menu in
        add_admin_pages_header sp;
        {{ [ !{{ Eliom_duce_tools.hierarchical_menu_depth_first ~id:"admin_menu"
                  ~whole_tree:true menu ?service ~sp }}
             <div id="after-admin-menu">[] ] }}
