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

let mutex = Lwt_mutex.create ()

let pam_auth ?(service = "") ~name ~pwd =
(*  Lwt_mutex.lock mutex >>= fun () ->
  Lwt.finalize
    (fun () ->
       Lwt_preemptive.detach
         (fun () -> *)
            try

              let pam = Pam.pam_start service ~user:name (fun _ _ -> pwd) in
              Pam.pam_set_item pam Pam.pam_item_fail_delay;
              Pam.pam_authenticate pam [] ~silent:true;
              ignore (Pam.pam_end pam);
              Lwt.return ()
            with (Pam.Pam_Error _) as e ->
              Ocsigen_messages.debug (fun () -> "Ocsimore_pam: "^
                                        Printexc.to_string e);
              Lwt.fail User.BadPassword
              | e ->
                  Ocsigen_messages.debug (fun () -> "Ocsimore_pam: "^
                                            Printexc.to_string e);
                  Lwt.fail e
(*
         )
         ()
    )
    (fun () -> Lwt_mutex.unlock mutex; Lwt.return ())
*)

let service =
  let parse_config = function
    | [(Simplexmlparser.Element ("pam", ["service", s], []))] ->
      Lwt.return (Some s)
    | [(Simplexmlparser.Element ("pam", [], []))] ->
      Lwt.return None
    | _ ->
      Lwt.fail (Ocsigen_extensions.Error_in_config_file
                  ("Unexpected content inside User_site config"))
  in
  let c = Eliom_config.get_config () in
  Lwt_main.run (parse_config c)


let () =
  User_external_auth.add_other_external_auth {
    User_external_auth.ext_auth_authenticate = pam_auth ?service;
    ext_auth_fullname = (fun n -> Lwt.return n);
    get_and_create_user = (fun _ ->
      Ocsigen_messages.warning
        "PAM authentification not supported by wikiperso";
      Lwt.return None
    );
  }
