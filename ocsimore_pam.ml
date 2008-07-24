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

let mutex = Lwt_mutex.create ()

let pam_auth ?(service = "") ~name ~pwd () =
  Lwt_mutex.lock mutex >>= fun () ->
  Lwt.finalize
    (fun () ->
       Lwt_preemptive.detach
         (fun () ->
            try
              let pam = Pam.pam_start service ~user:name (fun _ _ -> pwd) in
              Pam.pam_set_item pam (Pam.Pam_Fail_Delay (fun _ _ -> ()));
              Pam.pam_authenticate pam [] ~silent:true;
              ignore (Pam.pam_end pam)
            with Pam.Pam_Error _ -> raise Users.BadPassword
         )
         ()
    )
    (fun () -> Lwt_mutex.unlock mutex; Lwt.return ())

let _ = Session_manager.set_pam_auth pam_auth
