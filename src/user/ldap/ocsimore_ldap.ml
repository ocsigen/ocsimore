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
open Ocsimore_lib
module Ldap = Ldap_funclient

let (>>=) = Lwt.(>>=)

(*let mutex = Lwt_mutex.create () *)

let ldap_auth base uri ~name ~pwd =
(*  Lwt_mutex.lock mutex >>= fun () ->
  Lwt.finalize
    (fun () -> *)
       Lwt_preemptive.detach
         (fun () ->
           let conn = Ldap.init [uri] in
           try
             Ldap.bind_s ~who:(Printf.sprintf "uid=%s,%s" name base)
               ~cred:pwd ~auth_method:`Simple conn;
             Ldap.unbind conn
           with
             | Ldap_types.LDAP_Failure (`INVALID_CREDENTIALS, _, _) ->
               Ldap.unbind conn;
               raise User.BadPassword
             | exn ->
               Ldap.unbind conn;
               Lwt_log.ign_debug ~section ~exn "Ocsimore_ldap" ;
               raise exn
         )
         ()
(*    )
    (fun () -> Lwt_mutex.unlock mutex; Lwt.return ())
*)

(* Test code: *)
(*
let _ =
  let conn = Ldap.init ["ldaps://localhost:636/"] in
  match Ldap.search_s ~base:"dc=my-domain,dc=com" conn "uid=fff" with
    | [x] -> x
    | [] -> failwith "TEST"
    | _ -> failwith "Ldap error: Too much users match"
*)

let get_user (base, uri) user =
  Lwt_preemptive.detach
    (fun () ->
      let conn = Ldap.init [uri] in
      try
        match Ldap.search_s ~base conn ("uid=" ^ user) with
          | [] -> Ldap.unbind conn; None
          | [x] -> Ldap.unbind conn; Some x
          | _ -> raise (Failure "Ldap error: Too much users match")
      with e -> Ldap.unbind conn; raise e
    ) ()

let parse_config = function
  | [Simplexmlparser.Element ("ldap", [("base", base); ("uri", uri)], [])] ->
      User_external_auth.add_external_auth {
        User_external_auth.ext_auth_authenticate = ldap_auth base uri;
        ext_user_exists = (fun user ->
          get_user (base, uri) user >>= function
            | None -> Lwt.return false
            | Some _ -> Lwt.return true
        );
      }
  | _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             ("Unexpected content inside User_site config"))

let () =
  Ocsigen_extensions.register_extension
    ~name:"ocsimore-ldap"
    ~init_fun:parse_config
    ()
