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
let (>|=) = Lwt.(>|=)

let register_user_extensions (user_widget : User_widgets.user_widget_class) =
  let add_extension l ~name ~wiki_content f =
    List.iter (fun wp -> Wiki_syntax.add_extension ~wp ~name ~wiki_content f) l
  in
  let wikicreole_parser = Wiki_syntax.wikicreole_parser in
  let reduced_wikicreole_parser0 = Wiki_syntax.reduced_wikicreole_parser0 in
  let reduced_wikicreole_parser1 = Wiki_syntax.reduced_wikicreole_parser1 in
  let reduced_wikicreole_parser2 = Wiki_syntax.reduced_wikicreole_parser2 in
  let inline_wikicreole_parser = Wiki_syntax.inline_wikicreole_parser in

  add_extension
    [wikicreole_parser]
    ~name:"loginbox" ~wiki_content:true
    (fun bi args _c ->
       Wikicreole.Block
         (let user_prompt = Ocsimore_lib.list_assoc_opt "user_prompt" args in
          let pwd_prompt = Ocsimore_lib.list_assoc_opt "pwd_prompt" args in
          let auth_error = Ocsimore_lib.list_assoc_opt "auth_error" args in
          let switchtohttps = Ocsimore_lib.list_assoc_opt "switch_to_https" args
          in
          (user_widget#display_login_widget ~sp:bi.bi_sp
            ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ()) >|= fun b ->
          [(b: Xhtmltypes.form_content XHTML.M.elt :> Xhtmltypes.div_content XHTML.M.elt)]));

  add_extension
    [wikicreole_parser]
    ~name:"logoutbutton" ~wiki_content:true
    (fun bi _args c ->
       Wikicreole.Block
         (let content = match c with
            | Some c -> c
            | None -> "logout"
          in
          Wiki_syntax.xml_of_wiki
            Wiki_syntax.reduced_wikicreole_parser_button_content bi content
          >>= fun content ->
          user_widget#display_logout_button bi.bi_sp content >|= fun f ->
          [(f: Xhtmltypes.form XHTML.M.elt :> Xhtmltypes.div_content XHTML.M.elt)]
         )
    );

  let f = (fun bi _args _c ->
             Wikicreole.A_content
               (User.get_user_data ~sp:bi.bi_sp >|= fun ud ->
                [XHTML.M.pcdata ud.user_fullname])
          )
  in
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0;
     reduced_wikicreole_parser1; reduced_wikicreole_parser2]
    ~name:"username" ~wiki_content:true f;
  Wiki_syntax.add_extension
    ~wp:inline_wikicreole_parser ~name:"username" ~wiki_content:true f;

  add_extension
    [wikicreole_parser]
    ~name:"logoutlink" ~wiki_content:true
    (fun bi args c ->
       Wikicreole.Link_plugin
         (let content = match c with
            | Some c -> Wiki_syntax.a_content_of_wiki bi c
            | None -> Lwt.return [XHTML.M.pcdata "logout"]
          in
          (XHTML.M.string_of_uri (user_widget#logout_uri bi.bi_sp),
           args,
           content)
         )
    )


