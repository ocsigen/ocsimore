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

let static_services :
    ((string list, unit, Eliom_services.get_service_kind, [ `WithSuffix ],
      [ `One of string list ] Eliom_parameters.param_name, unit,
      [ `Registrable ])
     Eliom_services.service * _) option ref = ref None

let set_service_for_static_files service =
  match !static_services with
    | None ->
        let service_uue =
          Eliom_predefmod.Text.register_new_service
            ~path:([Ocsimore_lib.ocsimore_admin_dir ; "ocsimore_client.js"])
            ~get_params:(Eliom_parameters.string "path")
            (fun _sp s () ->
               let vm = s ^ "/ocsimore_client.uue" in
               Lwt.return
                 (Printf.sprintf
                    "window.onload = function () { \
                       main_vm = exec_caml (\"%s\"); \
                     }" vm,
                  "text/javascript")
            )
        in
        static_services := Some (service, service_uue)
    | Some _ ->
        Ocsigen_messages.errlog "In Ocsimore_common: Static services already \
                    loaded. Ignoring call to set_service_for_static_files"

let static_file_uri ~sp ~path =
  let service = match !static_services with
    | None -> failwith "No service for static files"
    | Some (s, _) -> s
  in
  Eliom_duce.Xhtml.make_uri ~service ~sp path

let add_html_header_hook, headers =
  let l = ref [] in
  (fun f -> l := f :: !l),
  (fun sp ->
     List.fold_left (fun (head : {{ [ (Xhtmltypes_duce.link | Xhtmltypes_duce.script)* ] }}) f -> {{ head @ {: f sp :} }}) {{ [] }} (List.rev !l))



(* Functions to add a an header for ocsimore *)
let add_obrowser_header, must_add_obrowser_header =
  let key = Polytables.make_key () in
  (fun sp -> Polytables.set ~table:(Eliom_sessions.get_request_cache sp)
     ~key ~value:true),
  (fun sp ->
     try Polytables.get ~table:(Eliom_sessions.get_request_cache sp) ~key
     with Not_found -> false)

(* Function generating the Ocsimore header *)
let () =
  add_html_header_hook
    (fun sp ->
       if must_add_obrowser_header sp then
         let static_files_services, ocsimore_uue = match !static_services with
           | None -> failwith "No service for static files"
           | Some (s1, s2) -> s1, s2
         in
         let static_uri path = Eliom_duce.Xhtml.make_uri
           ~service:static_files_services ~sp [path] in
         let eliom_obrowser = Eliom_duce.Xhtml.js_script
           ~uri:(static_uri "eliom_obrowser.js") ()
         and vm = Eliom_duce.Xhtml.js_script ~uri:(static_uri "vm.js") ()
         and uue =
           let path = static_uri "."  in
           Eliom_duce.Xhtml.js_script
             ~uri:(Eliom_duce.Xhtml.make_uri ~service:ocsimore_uue ~sp path) ()
         in
         {{ [ vm eliom_obrowser uue ] }}
       else {{ [] }}
    )

let html_page ~sp ?(css={{ [] }}) ?(title="Ocsimore") content =
  let title = Ocamlduce.Utf8.make title in
  let links_hooks = headers sp
  in
  Lwt.return {{
      <html xmlns="http://www.w3.org/1999/xhtml">[
        <head>[
          <title>title
          !links_hooks
          !css
        ]
        <body>content
      ]
    }}
