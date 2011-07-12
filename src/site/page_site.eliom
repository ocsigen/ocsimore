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


open Eliom_pervasives

(** An alias for the services that are accepted in the admin menu. *)
type menu_link_service =
    (Eliom_services.get_service_kind,
     [ `Unregistrable | `Registrable ],
    Eliom_output.non_caml_service)
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
  let c = Eliom_config.get_config () in
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


let static_service = Eliom_output.Any.register_service
  ~path:[Ocsimore_lib.ocsimore_admin_dir ; "static"]
  ~get_params:(Eliom_parameters.suffix (Eliom_parameters.all_suffix "path"))
  (fun path () ->
     let path = admin_staticdir :: path in
     let file = Ocsigen_pervasives.Url.string_of_url_path ~encode:false path in
     Eliom_output.Files.send file
  )


let static_file_uri ~path = Eliom_output.Html5.make_uri ~service:static_service path



module Header = (
  struct

    type header = bool Polytables.key

    let header_table = ref []

    let create_header header_content =
      let k = Polytables.make_key () in
      header_table := (k, header_content)::!header_table;
      k

    let require_header header =
      Polytables.set
        ~table:(Eliom_request_info.get_request_cache ())
        ~key:header
        ~value:true

    let generate_headers () =
      List.flatten
        (List.fold_left
          (fun beg (key, header_content) ->
             let tobeincluded =
               try
                 Polytables.get
                   ~table:(Eliom_request_info.get_request_cache ())
                   ~key
               with Not_found -> false
             in
             if tobeincluded
             then (header_content ())::beg
             else beg)
          []
          !header_table
        )

  end : sig

    type header

    (** Define a new header *)
    val create_header :
         ( unit -> HTML5_types.head_content_fun HTML5.M.elt list )
      -> header

    (** Call this function every time you need a header to be included
        in the page. If this function is called several times for the same
        page with the same header, the header will be included only once.
    *)
    val require_header : header -> unit

    (** This function is called to generate the headers for one page.
        Only required headers are included.
    *)
    val generate_headers :
         unit
      -> HTML5_types.head_content_fun HTML5.M.elt list

  end)

let admin_pages_header =
  Header.create_header
    (fun () ->
       [Eliom_output.Html5.css_link
          (static_file_uri ["ocsiadmin.css"]) ()
       ]
    )

(* special handling for onload functions (?) *)
let polytable_onload = Polytables.make_key ()

let onload_functions () =
  try Polytables.get ~table:(Eliom_request_info.get_request_cache ())
    ~key:polytable_onload
  with Not_found -> []

let add_onload_function ?(first = false) s =
  Polytables.set ~table:(Eliom_request_info.get_request_cache ())
    ~key:polytable_onload
    ~value:(if first then onload_functions () @ [s]
            else s :: onload_functions ())

let add_onload_function = add_onload_function ~first:false

(* shortcuts: *)
let add_admin_pages_header () = Header.require_header admin_pages_header


let html_page ?body_classes ?(css=[]) ?(title="Ocsimore") content =
  let headers = Header.generate_headers () in
  let body_attrs = match body_classes with
    | None -> [HTML5.M.a_id "body"]
    | Some l -> [HTML5.M.a_id "body"; HTML5.M.a_class l]
  in
  Lwt.return
    (HTML5.M.html
       (HTML5.M.head
          (HTML5.M.title (HTML5.M.pcdata title))
          ((css
            :HTML5_types.link HTML5.M.elt list
            :> HTML5_types.head_content_fun HTML5.M.elt list)
          @ headers))
       (HTML5.M.body ~a:body_attrs content)
    )

(** Admin page *)


let admin_root =
  Eliom_services.service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;""]
    ~get_params:Eliom_parameters.unit ()


open Eliom_tools_common


let admin_menu = ref []

let menu_link (text, service, f) =
  f () >|= function
  | true -> Some ([HTML5.M.pcdata text], Site_tree (Main_page service, []))
  | false -> None

let add_to_admin_menu ~name ~links ~root =
  admin_menu := (name, links, root) :: !admin_menu

let admin_menu ?service () =
  Lwt_list.map_s
    (fun (name, links, root) ->
       Lwt_list.fold_left_s
         (fun r me ->
            menu_link me >|= function | None -> r | Some me -> me::r
         )
         [] links >|= fun links ->
         ([HTML5.M.span
             ~a:[HTML5.M.a_class ["admin-menu-root"]]
             [HTML5.M.pcdata name]],
         Site_tree (Main_page root, List.rev links)
         )
    )
    !admin_menu >|= List.rev                               >>= fun admin_menu ->
  Lwt.return (Main_page admin_root, admin_menu)            >>= fun menu ->
  Lwt.return (add_admin_pages_header ())                   >|= fun () ->
  Eliom_tools.Html5.hierarchical_menu_depth_first
     ~id:"admin_menu"
     ~whole_tree:false
     menu
     ?service


let add_status_function, status_text =
  let l = ref [] in
  (fun e -> l := e :: !l),
  (fun () -> Lwt_list.map_s (fun x -> x ()) !l)

(*TODO: find a better place for [access_denied]'s definition and make customizable *)
let access_denied =
  [
    HTML5.M.h1 [HTML5.M.pcdata "Access denied"];
    HTML5.M.pcdata "You are not allowed to access this content. Please login.";
  ]

let admin_page
      ?service
      ?(body_classes =[]) ?(css=[]) ?(title="Ocsimore")
      ?(allow_unlogged=false)
      content =
  admin_menu ?service ()                  >>= fun menu ->
  status_text ()                          >>= fun status ->
  User.get_user_id ()                     >>= fun usr_id ->
  let content =
    if usr_id <> User.anonymous || allow_unlogged
    then content
    else access_denied
  in
  html_page ~title ~css ~body_classes:("admin" :: body_classes)
    (  menu ()
     @ [HTML5.M.div ~a:[HTML5.M.a_id "admin_body"] content;
        HTML5.M.div ~a:[HTML5.M.a_id "admin_status"] status;
       ]
    )


let icon ~path ~text =
  let src = static_file_uri [path] in
  HTML5.M.img ~src ~alt:text ~a:[HTML5.M.a_title text] ()


let ocsimore_admin_greetings =
  [
    HTML5.M.h1 [HTML5.M.pcdata "Ocsimore"];
    HTML5.M.p
      [HTML5.M.pcdata
         "This is the Ocsimore root admin page. The links on the left will \
          help you configure the different modules of your Ocsimore \
          installation."
      ];
  ]

let () = Eliom_output.Html5.register admin_root
  (fun () () ->
     admin_page ~service:admin_root ~title:"Ocsimore"
       ocsimore_admin_greetings
  )
