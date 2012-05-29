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


open Eliom_content
open Ocsimore_lib
open Lwt_ops

(** An alias for the services that are accepted in the admin menu. *)
type menu_link_service =
    (Eliom_service.get_service_kind,
     [ `Unregistrable | `Registrable ],
    Eliom_registration.non_caml_service)
    Eliom_tools.one_page

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


let static_service =
  Eliom_registration.Any.register_service
    ~path:[!Ocsimore_config.admin_dir ; "static"]
    ~get_params:(Eliom_parameter.suffix (Eliom_parameter.all_suffix "path"))
    (fun path () ->
       let path = admin_staticdir :: path in
       let file = Eliom_lib.Url.string_of_url_path ~encode:false path in
       Eliom_registration.File.send ~options:(3600 * 24 * 7) file
       (* TODO: parametrize cache duration... *)
    )


let static_file_uri ~path =
  Eliom_content.Html5.F.make_uri ~service:static_service path

module Header : sig
  type header

  (** Define a new header *)
  val create_header : ( unit -> Html5_types.head_content_fun Html5.F.elt list ) -> header

  (** Call this function every time you need a header to be included
      in the page. If this function is called several times for the same
      page with the same header, the header will be included only once.
  *)
  val require_header : header -> unit Lwt.t

  (** This function is called to generate the headers for one page.
      Only required headers are included.
  *)
  val generate_headers : unit -> Html5_types.head_content_fun Html5.F.elt list Lwt.t

end = struct

  type header = bool Eliom_reference.eref

  let header_table = ref []

  let create_header header_content =
    let header_ref = Eliom_reference.eref ~scope:Eliom_common.request false in
    header_table := (header_ref, header_content) :: !header_table;
    header_ref

  let require_header header_ref =
    Eliom_reference.set header_ref true

  let generate_headers () =
    List.(fun xs -> flatten (map_filter (fun x -> x) xs)) =|< lwt_sequence
      (List.map
        (fun (header_ref, header_content) ->
           Eliom_reference.get header_ref >|= function
             | true -> Some (header_content ())
             | false -> None)
        !header_table)

end

(* special handling for onload functions (?) *)
let onload_functions_eref = Eliom_reference.eref ~scope:Eliom_common.request []

let add_onload_function ?(first = false) s =
  flip eref_modify onload_functions_eref
    (fun onload_functions ->
       if first then
         onload_functions @ [s]
       else
         s :: onload_functions)

let add_onload_function = add_onload_function ~first:false

let admin_pages_header =
  Header.create_header
    (fun () ->
       [Eliom_content.Html5.F.css_link
          (static_file_uri ["ocsiadmin.css"]) ()])

(* shortcuts: *)

let add_admin_pages_header () =
  Header.require_header admin_pages_header

let html_page ?body_classes ?(css=[]) ?title:(title'="Ocsimore") ?heading content =
  lwt headers = Header.generate_headers () in
  let body_attrs =
    match body_classes with
      | None -> [Html5.F.a_id "body"]
      | Some l -> [Html5.F.a_id "body"; Html5.F.a_class l]
  in
  Lwt.return Html5.F.(
    html
     (head
        (title (pcdata title'))
        ((css :> Html5_types.head_content_fun elt list)
         @ headers
         @ [Ocsimore_appl.application_script ~async:true ()]))
     (body ~a:body_attrs 
        (get_opt ~default:[]
           (Eliom_lib.Option.map (fun heading -> [h1 ~a:[a_class ["html_page_heading"]] [pcdata heading]]) heading)
         @ content))
  )

(** Admin page *)

let admin_root =
  Eliom_service.service
    ~path:[!Ocsimore_config.admin_dir;""]
    ~get_params:Eliom_parameter.unit ()

let admin_menu = ref []

let menu_link (text, service, f) =
  f () >|= function
    | true -> Some ([Html5.F.pcdata text], Eliom_tools.Site_tree (Eliom_tools.Main_page service, []))
    | false -> None

let add_to_admin_menu ~name ~links ~root =
  admin_menu := (name, links, root) :: !admin_menu

let admin_menu ?service () =
  lwt admin_menu =
    List.rev =|< Lwt_list.map_s
      (fun (name, links, root) ->
         Lwt_list.fold_left_s
           (fun r me ->
              menu_link me >|= function None -> r | Some me -> me :: r)
           [] links >|= fun links ->
           [Html5.F.span
              ~a:[Html5.F.a_class ["admin-menu-root"]]
              [Html5.F.pcdata name]],
           Eliom_tools.Site_tree (Eliom_tools.Main_page root, List.rev links))
      !admin_menu
  in
  let menu = Eliom_tools.Main_page admin_root, admin_menu in
  lwt () = add_admin_pages_header () in
  Lwt.return
    (Eliom_tools.Html5.hierarchical_menu_depth_first
       ~id:"admin_menu"
       ~whole_tree:false
       menu
       ?service)

let add_status_function, status_text =
  let l = ref [] in
  (fun e -> l := e :: !l),
  (fun () -> Lwt_list.map_s (fun x -> x ()) !l)

let admin_page
      ?service
      ?(body_classes =[])
      ?(css=[])
      ~title
      content =
  lwt menu = admin_menu ?service () in
  lwt status = status_text () in
  lwt usr_id = User.get_user_id () in
  html_page ~title:"Ocsimoreadmin" ~heading:title ~css ~body_classes:("admin" :: body_classes)
    (  menu ()
     @ Html5.F.div ~a:[Html5.F.a_id "admin_body"] content
     :: Html5.F.div ~a:[Html5.F.a_id "admin_status"] status
     :: []
    )

let body_to_div x =
  (x : Html5_types.body_content Html5.F.elt list
     :> Html5_types.flow5 Html5.F.elt list)

(** Dummy content to show when access is denied in [admin_body_content_with_permission_handler]. *)
let no_permission () =
  lwt userid = User.get_user_id () in
  Lwt.return Html5.F.(
    h1 [pcdata "No permission"] ::
    if userid = User.anonymous then
      [p [
        pcdata "You may want to login.";
(*         Html5.D.a ~service:User_services.service_login [pcdata "login"] () *)
      ]]
    else []
  )

let userid_permissions test = User.get_user_id () >>= test

let admin_body_content_with_permission_handler ~title ?service ~permissions ~display =
  fun get_args post_args ->
    lwt content =
      permissions get_args post_args >>= function
        | true ->
            begin try_lwt
              (display get_args post_args :> Html5_types.body_content Html5.F.elt list Lwt.t)
            with Failure msg ->
              Lwt.return Html5.F.([h2 [pcdata "Error"]; p [pcdata msg]])
            end
        | false ->
            no_permission ()
    in
    let content = body_to_div content in
    lwt service = match service with Some s -> (s get_args post_args : menu_link_service Lwt.t) >|= Ocsimore_lib.some | None -> Lwt.return None in
    lwt title = title get_args post_args in
    (admin_page ~title ?service content : Html5.F.html Lwt.t)

let icon ~path ~text =
  let src = static_file_uri [path] in
  Html5.F.img ~src ~alt:text ~a:[Html5.F.a_title text] ()


let ocsimore_admin_greetings =
  let open Html5.F in [
    h1 [pcdata "Ocsimore"];
    p [pcdata
       "This is the Ocsimore root admin page. The links on the left will \
        help you configure the different modules of your Ocsimore \
        installation."];
  ]

let () =
  Eliom_registration.Html5.register admin_root
    (fun () () ->
       admin_page ~service:admin_root ~title:"Ocsimore" ocsimore_admin_greetings);
  ()
