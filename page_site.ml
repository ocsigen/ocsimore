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
open Lwt

let admin_staticdir =
  let rec find_wikidata (_staticadm  as data) = function
    | [] -> Lwt.return data

    | (Simplexmlparser.Element ("admin", ["staticdir", path], []))::l ->
        find_wikidata (Some path) l

    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Page_site config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_wikidata None c)

exception No_admin_staticdir

let admin_staticdir =
  match admin_staticdir with
    | Some s -> s
    | None -> Ocsigen_messages.errlog "Page_site: please supply the local path for Ocsimore's static files\n  Syntax: <admin staticdir=\"path\" />";
        raise No_admin_staticdir


let static_service = Eliom_predefmod.Any.register_new_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir ; "static"]
  ~get_params:(Eliom_parameters.suffix (Eliom_parameters.all_suffix "path"))
  (fun sp path () ->
     let path = admin_staticdir :: path in
     let file = Ocsigen_lib.string_of_url_path ~encode:false path in
     Eliom_predefmod.Files.send ~sp file
  )


let static_file_uri ~sp ~path =
  Eliom_duce.Xhtml.make_uri ~service:static_service ~sp path



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


let html_page ~sp ?(body_classes=[]) ?(css={{ [] }}) ?(title="Ocsimore") content =
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


let admin_root =
  Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;""]
    ~get_params:Eliom_parameters.unit ()


open Eliom_tools_common


let admin_menu = ref []

let menu_link (text, service) =
  (Ocamlduce.Utf8.make text, Site_tree (Main_page service, []))

let add_to_admin_menu ~name ~links =
  admin_menu :=
    ({{ [ <span class="admin-menu-root">{{ Ocamlduce.Utf8.make name }} ] }},
     Site_tree (Not_clickable, List.map menu_link links))
  :: !admin_menu

let admin_menu ?service sp =
  let menu = Main_page admin_root, List.rev !admin_menu in
  add_admin_pages_header sp;
  Eliom_duce_tools.hierarchical_menu_depth_first ~id:"admin_menu"
    ~whole_tree:true menu ?service ~sp


let add_status_function, status_text =
  let l = ref [] in
  (fun e -> l := e :: !l),
  (fun sp ->
     match !l with
       | [] -> Lwt.return {{ [] }}
       | hd :: tl ->
           hd ~sp >>= fun hd ->
           Lwt_util.fold_left (fun (r : Xhtmltypes_duce.flows) f ->
                                 f ~sp >>= fun (e : Xhtmltypes_duce.flows) ->
                                 Lwt.return {{ [ !e ' | ' !r ]}})
             hd tl
  )

let admin_page ~sp ~service ?(body_classes =[]) ?(css={{ [] }}) ?(title="Ocsimore") content =
  let menu = admin_menu ~service sp in
  status_text sp >>= fun status ->
  html_page ~sp ~title ~css ~body_classes:("admin" :: body_classes)
    {{ [!menu
        <div id="admin_body">content
        <div id="admin_status">status] }}


let icon ~sp ~path ~text =
  let uri = Ocamlduce.Utf8.make (static_file_uri sp [path])
  and alt = Ocamlduce.Utf8.make text
  in
  ({{ [ <img alt src=uri title=alt>[] ] }} :{{ [Xhtmltypes_duce.img*]}} )


let () = Eliom_duce.Xhtml.register admin_root
  (fun sp () () ->
     admin_page ~sp ~service:admin_root
       {{ [<p>"This is the Ocsimore main admin page. The links above will
                 help you configure your installation." ]}}
  )
