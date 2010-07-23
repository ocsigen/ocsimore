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




(** An alias for the services that are accepted in the admin menu. *)
type menu_link_service =
    (Eliom_services.get_service_kind,
     [ `Unregistrable | `Registrable ])
    Eliom_tools_common.one_page


let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

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
    | None ->
        Ocsigen_messages.errlog
         "Page_site: please supply the local path for Ocsimore's static files\n\
          Syntax: <admin staticdir=\"path\" />";
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
  Eliom_predefmod.Xhtml.make_uri ~service:static_service ~sp path



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
      List.flatten
        (List.fold_left
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
        )

  end : sig

    type header

    (** Define a new header *)
    val create_header :
         (   Eliom_sessions.server_params
          -> [`Link | `Meta | `Object | `Script | `Style ] XHTML.M.elt list)
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
         sp:Eliom_sessions.server_params
      -> [`Link | `Meta | `Object | `Script | `Style ] XHTML.M.elt list

  end)

let admin_pages_header =
  Header.create_header
    (fun sp ->
       [Eliom_predefmod.Xhtml.css_link
          (static_file_uri sp ["ocsiadmin.css"]) ()
       ]
    )

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

(*
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
 *)

let add_onload_function = add_onload_function ~first:false

(* shortcuts: *)
(*
let add_obrowser_header = Header.require_header obrowser_header
 *)
let add_admin_pages_header = Header.require_header admin_pages_header


let html_page ~sp ?body_classes ?(css=[]) ?(title="Ocsimore") content =
  let headers = Header.generate_headers sp in
  let body_attrs = match body_classes with
    | None -> [XHTML.M.a_id "body"]
    | Some l -> [XHTML.M.a_id "body"; XHTML.M.a_class l]
  in
(*  and onload_body, onload_script = match onload_functions sp with
    | [] -> {{ {} }}, {{ [] }}
    | l -> {{ { onload="bodyOnload()" } }},
        {{ [ <script type="text/javascript">
                         {: Printf.sprintf "function bodyOnload() { \n%s;\n}"
                            (String.concat ";\n" (List.rev l)) :}
           ] }}
  in
 *)
  Lwt.return
    (XHTML.M.html
       (XHTML.M.head
          (XHTML.M.title (XHTML.M.pcdata title))
          ((css
            :Xhtmltypes.link XHTML.M.elt list
            :> [ `Link | `Meta | `Object | `Script | `Style ] XHTML.M.elt list)
          @ headers))
       (XHTML.M.body ~a:body_attrs content)
    )



(** Admin page *)


let admin_root =
  Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;""]
    ~get_params:Eliom_parameters.unit ()


open Eliom_tools_common


let admin_menu = ref []

(*
let menu_link sp (text, service, f) =
  f sp >>= function
    | true -> Lwt.return
        (Some (Ocamlduce.Utf8.make text, Site_tree (Main_page service, [])))
    | false -> Lwt.return None
*)

let menu_link sp (text, service, f) =
  f sp >|= function
  | true -> Some ([XHTML.M.pcdata text], Site_tree (Main_page service, []))
  | false -> None

let add_to_admin_menu ~name ~links ~root =
  admin_menu := (name, links, root) :: !admin_menu

let admin_menu ?service sp =
  Lwt_list.map_s
    (fun (name, links, root) ->
       Lwt_list.fold_left_s
         (fun r me ->
            menu_link sp me >|= function | None -> r | Some me -> me::r
         )
         [] links >|= fun links ->
         ([XHTML.M.span
             ~a:[XHTML.M.a_class ["admin-menu-root"]]
             [XHTML.M.pcdata name]],
         Site_tree (Main_page root, List.rev links)
         )
    )
    !admin_menu >|= List.rev                               >>= fun admin_menu ->
  Lwt.return (Main_page admin_root, admin_menu)            >>= fun menu ->
  Lwt.return (add_admin_pages_header sp)                   >|= fun () ->
  Eliom_tools.Xhtml.hierarchical_menu_depth_first
     ~id:"admin_menu"
     ~whole_tree:false
     menu
     ?service
     ~sp


let add_status_function, status_text =
  let l = ref [] in
  (fun e -> l := e :: !l),
  (fun sp -> Lwt_list.map_s (fun x -> x ~sp) !l)

(*TODO: find a better place for [access_denied]'s definition and make customizable *)
let access_denied =
  [
    XHTML.M.h1 [XHTML.M.pcdata "Access denied"];
    XHTML.M.pcdata "You are not allowed to access this content. Please login.";
  ]

let admin_page ~sp
      ?service
      ?(body_classes =[]) ?(css=[]) ?(title="Ocsimore")
      ?(allow_unlogged=false)
      content =
  admin_menu ?service sp                  >>= fun menu ->
  status_text sp                          >>= fun status ->
  User.get_user_id ~sp                    >>= fun usr_id ->
  let content =
    if usr_id <> User.anonymous || allow_unlogged
    then content
    else access_denied
  in
  html_page ~sp ~title ~css ~body_classes:("admin" :: body_classes)
    (  menu
     @ [XHTML.M.div ~a:[XHTML.M.a_id "admin_body"] content;
        XHTML.M.div ~a:[XHTML.M.a_id "admin_status"] status;
       ]
    )


let icon ~sp ~path ~text =
  let src = static_file_uri sp [path] in
  XHTML.M.img ~src ~alt:text ~a:[XHTML.M.a_title text] ()


let ocsimore_admin_greetings =
  [
    XHTML.M.h1 [XHTML.M.pcdata "Ocsimore"];
    XHTML.M.p
      [XHTML.M.pcdata
         "This is the Ocsimore root admin page. The links on the left will \
          help you configure the different modules of your Ocsimore \
          installation."
      ];
  ]

let () = Eliom_predefmod.Xhtml.register admin_root
  (fun sp () () ->
     admin_page ~sp ~service:admin_root ~title:"Ocsimore"
       ocsimore_admin_greetings
  )
