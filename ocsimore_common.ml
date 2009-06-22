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
   @author Vincent Balat
*)

let (>>=) = Lwt.bind

exception Permission_denied

let action_failure_key = Polytables.make_key ()

let catch_action_failure ~sp ?(f_exn=fun exn -> exn) f =
  Lwt.catch
    f
    (fun exn ->
       Polytables.set (Eliom_sessions.get_request_cache sp)
         action_failure_key (f_exn exn);
       Lwt.return ())

let get_action_failure ~sp =
  try
    Some (Polytables.get ~table:(Eliom_sessions.get_request_cache sp)
            ~key:action_failure_key)
  with Not_found -> None

exception Incorrect_argument


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



let html_page ~sp ?(css={{ [] }}) ?(title="Ocsimore") content =
  let title = Ocamlduce.Utf8.make title in
  let static_files_services, ocsimore_uue = match !static_services with
    | None -> failwith "No service for static files"
    | Some (s1, s2) -> s1, s2
  in
  let static_uri path =
    Eliom_duce.Xhtml.make_uri ~service:static_files_services ~sp [path] in
  let eliom_obrowser = Eliom_duce.Xhtml.js_script
    ~uri:(static_uri "eliom_obrowser.js") ()
  and vm = Eliom_duce.Xhtml.js_script ~uri:(static_uri "vm.js") ()
  and uue =
    let path = static_uri "."  in
    Eliom_duce.Xhtml.js_script
      ~uri:(Eliom_duce.Xhtml.make_uri ~service:ocsimore_uue ~sp path) ()
  in
  Lwt.return {{
      <html xmlns="http://www.w3.org/1999/xhtml">[
        <head>[
          <title>title
          <script type="text/javascript">[]
          vm
          eliom_obrowser
          uue
          !css
        ]
        <body>content
      ]
    }}
(*  *)


type 'a eliom_usertype =
    ('a, [ `WithoutSuffix ], [ `One of 'a ] Eliom_parameters.param_name)
    Eliom_parameters.params_type

let eliom_opaque_int32 s =
  Eliom_parameters.user_type
    (fun s -> Opaque.int32_t (Int32.of_string s))
    (fun i -> Int32.to_string (Opaque.t_int32 i)) s


let input_opaque_int32 ?value ?(hidden=true) name =
  let f = Eliom_duce.Xhtml.user_type_input
    (fun v -> Int32.to_string (Opaque.t_int32 v)) ~name ?value
  in
  if hidden then
    f ~input_type:{: "hidden" :} ()
  else
    f ~input_type:{: "text" :} ()
