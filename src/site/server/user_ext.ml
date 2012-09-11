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
   Provides a function to register the wikicreole extensions for user management
   (i.e. login, logout, username).

   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)

open Eliom_content
open User_sql.Types

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

(** Registers the following wiki creole extensions
    - [<<username>>]
    - [<<loginbox>>]
    - [<<logoutbutton>>]
    - [<<logoutlink>>]
  *)
let register_user_extensions (user_widget : User_widgets.user_widget_class) =

  let f_loginbox _ args _c =
    `Flow5
      (let user_prompt = Ocsimore_lib.list_assoc_opt "user_prompt" args in
       let pwd_prompt = Ocsimore_lib.list_assoc_opt "pwd_prompt" args in
       let auth_error = Ocsimore_lib.list_assoc_opt "auth_error" args in
       let switchtohttps = Ocsimore_lib.list_assoc_opt "switch_to_https" args
       in
       lwt b =
         user_widget#display_login_widget
           ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps () in
       Lwt.return (b :> [>Html5_types.div] Html5.F.elt list)) in

  Wiki_syntax.register_interactive_simple_flow_extension
    ~name:"loginbox" ~reduced:false f_loginbox;

  let f_logoutbutton _ _args c =
    `Flow5
      (lwt content = match c with
        | Some c -> c
        | None -> Lwt.return [Html5.F.pcdata "logout"]
       in
       lwt f = user_widget#display_logout_button content in
       Lwt.return [f]) in

  let add_logoutbutton wp =
    Wiki_syntax.register_wiki_extension ~wp ~name:"logoutbutton"
      ~wp_rec:Wiki_syntax.reduced_wikicreole_parser_button_content f_logoutbutton in
  add_logoutbutton Wiki_syntax.wikicreole_parser;
  add_logoutbutton Wiki_syntax.wikicreole_parser_without_header_footer;

  let f_username _ _args _c =
    `Phrasing_without_interactive
      (lwt user_data = User.get_user_data () in
       Lwt.return [Html5.F.pcdata user_data.user_fullname]) in

  Wiki_syntax.register_simple_flow_extension ~name:"username" f_username;
  Wiki_syntax.register_simple_phrasing_extension ~name:"username" f_username;

  let f_logoutlink _ args c =
      (let content = match c with
         | Some c -> c
         | None -> Lwt.return [Html5.F.pcdata "logout"]
       in
       (user_widget#logout_uri,
        args,
        content)
      )
  in
  Wiki_syntax.register_link_phrasing_extension ~name:"logoutlink" ~reduced:false f_logoutlink
