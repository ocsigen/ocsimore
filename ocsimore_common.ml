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

type session_data = Polytables.t

exception Session_data of session_data

let create_empty_sd = Polytables.create

let get_sd ~sp =
  let rec f = function
    | [] -> None
    | (Session_data sd)::_ -> Some sd
    | _::l -> f l
  in
  match f (Eliom_sessions.get_exn sp) with
    | None -> create_empty_sd ()
    | Some sd -> sd
