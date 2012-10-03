(* Ocsimore
 * Copyright (C) 2005
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

let (>>=) = Lwt.bind


type external_auth = {
  (** A function returning unit if the given user can be authentified by
      the given password, or failing with [BadUser] *)
  ext_auth_authenticate: name:string -> pwd:string -> unit Lwt.t;

  (** This function take the username and return if the user exists *)
  ext_user_exists : string -> bool Lwt.t;
}

(** Functions to add and get an external authentication method(s)
    Used by LDAP and PAM *)
let (get_external_auths, add_external_auth) =
  let r : external_auth list ref = ref [] in
  ((fun () -> !r), (fun x -> r := x :: !r))

(** Function which iter on external auths,
    call the function f for each method while there is a method
    and raises [BadUser] if no method matches *)
let iter_external_auths f =
  let rec iter = function
    | [] -> Lwt.fail User.BadUser
    | x::xs ->
        match_lwt f x with
          | true -> Lwt.return ()
          | false -> iter xs
  in
  iter (get_external_auths ())
