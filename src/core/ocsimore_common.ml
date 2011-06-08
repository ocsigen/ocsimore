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

open Eliom_pervasives

exception Ok

let (>>=) = Lwt.bind

exception Permission_denied

let action_failure_key = Polytables.make_key ()

let catch_action_failure ?(f_exc=fun exn -> exn) f =
  Lwt.catch
    f
    (fun exc ->
       Polytables.set (Eliom_request_info.get_request_cache ())
         action_failure_key (f_exc exc);
       Lwt.return ())

let get_action_failure () =
  try
    Some (Polytables.get ~table:(Eliom_request_info.get_request_cache ())
            ~key:action_failure_key)
  with Not_found -> None

exception Incorrect_argument




type 'a eliom_usertype =
    ('a, [ `WithoutSuffix ], [ `One of 'a ] Eliom_parameters.param_name)
    Eliom_parameters.params_type

let eliom_opaque_int32 s =
  Eliom_parameters.user_type
    (fun s -> Opaque.int32_t (Int32.of_string s))
    (fun i -> Int32.to_string (Opaque.t_int32 i)) s


let eliom_opaque_int32_opt s =
  Eliom_parameters.user_type
    (fun s ->
       if s = "" then None
       else Some (Opaque.int32_t (Int32.of_string s)))
    (fun i -> match i with
       | None -> ""
       | Some i -> Int32.to_string (Opaque.t_int32 i)) s


let input_opaque_int32 ?value ?(hidden=true) name =
  let f = Eliom_output.Html5.user_type_input
    (fun v -> Int32.to_string (Opaque.t_int32 v)) ~name ?value
  in
  if hidden then
    f ~input_type:`Hidden ()
  else
    f ~input_type:`Text ()

let input_opaque_int32_opt ?value ?(hidden=true) name =
  let f = Eliom_output.Html5.user_type_input
    (fun v -> match v with
       | None -> ""
       | Some v -> Int32.to_string (Opaque.t_int32 v)) ~name ?value
  in
  if hidden then
    f ~input_type:`Hidden ()
  else
    f ~input_type:`Text ()

