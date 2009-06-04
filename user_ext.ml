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
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)

open Eliom_parameters
open User_sql.Types
open Wiki_widgets_interface

let (>>=) = Lwt.bind

let register_user_extensions wp (user_widget : User_widgets.user_widget) =
  let add_extension = Wiki_syntax.add_extension ~wp in

  add_extension ~name:"loginbox" ~wiki_content:true
    (fun bi args _c ->
       Wikicreole.Block
         (let user_prompt = Ocsimore_lib.list_assoc_opt "user_prompt" args in
          let pwd_prompt = Ocsimore_lib.list_assoc_opt "pwd_prompt" args in
          let auth_error = Ocsimore_lib.list_assoc_opt "auth_error" args in
          let switchtohttps = Ocsimore_lib.list_assoc_opt "switch_to_https" args
          in
          (user_widget#display_login_widget ~sp:bi.bi_sp
            ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ())
          >>= fun b ->
          Lwt.return {{ [ b ] }}));

  add_extension ~name:"username" ~wiki_content:true
    (fun bi _args _c ->
       Wikicreole.A_content
         (User.get_user_data ~sp:bi.bi_sp
          >>= fun ud ->
          Lwt.return (Ocamlduce.Utf8.make ud.user_fullname))
    );

  add_extension ~name:"logoutbutton" ~wiki_content:true
    (fun bi _args c ->
       Wikicreole.Block
         (let content = match c with
            | Some c -> c
            | None -> "logout"
          in
          Wiki_syntax.xml_of_wiki wp bi content
          >>= fun content ->
          user_widget#display_logout_button bi.bi_sp content >>= fun f ->
          Lwt.return {{ [ {: f :} ] }}
         )
    );

  add_extension ~name:"logoutlink" ~wiki_content:true
    (fun bi args c ->
       Wikicreole.Link_plugin
         (let content = match c with
            | Some c -> Wiki_syntax.a_content_of_wiki wp bi c
            | None -> Lwt.return (Ocamlduce.Utf8.make "logout")
          in
          (user_widget#logout_uri bi.bi_sp,
           args,
           content)
         )
    );
