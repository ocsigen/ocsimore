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
             | e ->
               Ldap.unbind conn;
               Ocsigen_messages.debug (fun () -> "Ocsimore_ldap: "^
                 Printexc.to_string e);
               raise e
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


let (base, uri) =
  let parse_config = function
    | [(Simplexmlparser.Element ("ldap", [("base", b); ("uri", u)], []))] ->
      Lwt.return (b, u)
    | _ ->
      Lwt.fail (Ocsigen_extensions.Error_in_config_file
                  ("Unexpected content inside User_site config"))
  in
  let c = Eliom_config.get_config () in
  Lwt_main.run (parse_config c)

let _ =
  User_external_auth.add_other_external_auth {
    User_external_auth.ext_auth_authenticate = ldap_auth base uri;
    ext_auth_fullname = (fun n -> Lwt.return n);
    get_and_create_user =  (fun user ->
    get_user (base, uri) user >>= function
      | None -> Lwt.return None
      | Some userdata ->
        let fullname =
          let pwd = "userPassword"
          and default = ""
          and concat_list = List.fold_left (fun acc elm -> acc ^ elm) "" in
          let rec search_in_search_result_entry = function
            | [] -> default
            | x::xs when x.Ldap_types.attr_type = pwd ->
              concat_list x.Ldap_types.attr_vals
            | x::xs -> search_in_search_result_entry xs in
          match userdata with
            | `Entry elm ->
              search_in_search_result_entry elm.Ldap_types.sr_attributes
            | `Referral _ -> default (* Don't know what is referral ? *)
        in
        User.create_user ~name:user
          ~pwd:User_sql.Types.External_Auth
          ~fullname
          ~email:(user ^ "@localhost")
          ()
        >>= fun userdata ->
        Lwt.return (Some userdata)
    );
  }
