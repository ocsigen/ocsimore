(* Ocsigen
 * http://www.ocsigen.org
 * Module extensiontemplate.ml
 * Copyright (C) 2007 Boris Yakobowski
 * CNRS - Université Paris Diderot Paris 7
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Wiki_sql.Types


(** Wikiperso is an eliom extension that can be used to provide
    personal wikis. That is, when the user accesses a page
    such as webserver.org/~bla/foo, a personal wiki is created
    for the user bla, and registered to the url ~bla *)

(** Wikiperso currently takes two config options (in this order)
    - username is a regexp with one parameter, which must
    extract from an url the name of the user for which the
    wiki must be created, or fail if no wiki must be created.

    - wikiroot is a regexp containing the string $USER. Replacing $USER
    by the user for which the wiki must be created must result
    in the root url for this wiki (of course relative to the current
    eliom root url). Correct values could be [~$USER] or [perso/$USER]

    An example configuration is given below

    <eliommodule module="/path/to/wikiperso.cmo">
      <options username="~([^/]*).*" wikiroot="~$USER" />
    </eliommodule>
*)


(* Extraction of the configuration options *)
let username, wikiroot = match Eliom_sessions.get_config () with
  | [Element("options", [("username", u);("wikiroot", w)],[])] ->
      u, w
  | _ -> raise (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside wikiperso config"))


(* [extract_user_name url] extracts from the url [url] the name of the
   user for which a wiki must be created, or fails if the url does not
   match *)
let extract_user_name pathstring =
  let regexp = Netstring_pcre.regexp username in
  match Netstring_pcre.full_split regexp pathstring with
    | [ Netstring_pcre.Delim _ ; Netstring_pcre.Group (_, user)] ->
        Some user
    | _l ->
        (* Debug code to see what has been matched *)
        (*
        let print_result = function
          | Netstring_pcre.Delim s -> Printf.printf "Delim%s," s
          | Netstring_pcre.Text s -> Printf.printf "Text%s," s
          | Netstring_pcre.NoGroup -> Printf.printf "NoGroup,"
          | Netstring_pcre.Group (i, s) -> Printf.printf "Group%d %s," i s
        in
        List.iter print_result _l; flush stdout;
        *)
        None

(* [wikipath user] finds the root url for the wiki of the user [user] *)
let wiki_path user =
  let path' = Netstring_pcre.global_replace (Netstring_pcre.regexp "\\$USER")
    user wikiroot in
  Netstring_pcre.split (Netstring_pcre.regexp "/") path'


(* [external user] returns an ocsimore user (creating it on the fly
   if needed) if [user] is a valid external user. The same authentification
   method as the one specified in the eliom module ocsisite is used.
   Currently, Pam authentification is not supported
*)
let external_user user =
  match Ocsisite.auth with
    | Ocsisite.NoExternalAuth -> return None
    | Ocsisite.Nis ->
        (Nis_chkpwd.userinfo user
         >>= function
           | None -> return None
           | Some userdata ->
               Users.create_user ~name:user
                 ~pwd:User_sql.External_Auth
                 ~fullname:userdata.Unix.pw_gecos
                 ~email:(user ^ "@localhost")
                 ~groups:[Users.authenticated_users.Users.id]
                 ()
               >>= fun userdata ->
                 return (Some userdata)
        )
    | Ocsisite.Pam _ ->
        Ocsigen_messages.warning
          "PAM authentification not supported by wikiperso";
        return None

(** Template pages, for the containers and the css of the new wikis. They are
    copied each time a new wiki is created *)

let default_welcome_page =
"=== <<wikiname>>\r\n\
\r\n\
<<content>>
\r\n
\r\n
<<cond notingroup='users'|
If you are an administator of this wiki, you can login to create this page:
<<loginbox user_prompt='User:' pwd_prompt='Password:' auth_error='Bad user or password'>>
>>
<<cond ingroup='users'|Your are connected as <<username>>.
<<logoutlink|Logout>>.>>
"
let default_wikicss = ""

let template_container_pagename = "wikiperso-template"
let template_css_pagename = "wikiperso_css-template"

let template_container = Ocsisite.register_named_wikibox
  ~page:template_container_pagename ~content:default_welcome_page
  ~content_type:Wiki_sql.Wiki ~comment:"Template for wikipersos container pages"

let template_wiki_css = Ocsisite.register_named_wikibox
  ~page:template_css_pagename ~content:default_wikicss
  ~content_type:Wiki_sql.Css ~comment:"Template for wikipersos css"


(** The function that creates the wikiperso when needed *)
let create_wikiperso ~wiki_title ~userdata =
  let gid = [userdata.Users.id] in
  template_container ()
  >>= fun container ->
  template_wiki_css ()
  >>= fun css ->
  (* We create the wiki, without supplying the [path] field. This will make
     the relocation of the wiki easier *)
  Wiki.really_create_wiki ~title:wiki_title
    ~descr:(Printf.sprintf !Language.messages.Language.wikiperso_wikidescr
              userdata.Users.fullname)
    (* ~boxrights:false *) ~writers:gid ~wikiboxes_creators:gid
    ~page_creators:gid ~css_editors:gid ~container_adm:gid
    ~wiki_css:css ~container_page:container
    ()


(** The function that answers for the extension. *)
let gen sp =
  let ri = Eliom_sessions.get_ri sp in
  (* We check that the url corresponds to a wiki *)
  match extract_user_name ri.ri_sub_path_string with
    | Some user ->
        (* If this is the case, we try to find the id of the corresponding
           ocsimore user. If it does not exists, we try external
           authentification *)
        Users.get_user_by_name user
        >>= fun userdata ->
        (if userdata <> Users.nobody then
           Lwt.return (Some userdata)
         else
           external_user user
        )
        >>= fun userinfo ->
        (match userinfo with
           | None -> Lwt.return ()
           | Some userdata ->
               (* If the user for which we must create a wiki
                  exists, we create this wiki if it does not already exists. *)
               let wiki_title = "wikiperso for " ^ user in
               Lwt.catch
                 (fun () -> Wiki_sql.get_wiki_info_by_name wiki_title
                    >>= fun { wiki_id = wiki } -> Lwt.return wiki)
                 (function
                    | Not_found -> create_wikiperso wiki_title userdata
                    | e -> Lwt.fail e)
               >>= fun wiki ->
               (* We then register the personal wiki at the correct url *)
               Wiki_services.register_wiki ~sp
                 ~path:(wiki_path userdata.Users.name)
                 ~wikibox_widget:Ocsisite.wikibox_widget ~wiki:wiki ()
        )
        >>= fun () ->
          (* In all cases, we just tell Eliom to continue. It will answer with
             the wiki if it has been successfully created *)
         return
           (Ext_next (Eliom_sessions.get_previous_extension_error_code sp))

    | None -> return
        (Ext_next (Eliom_sessions.get_previous_extension_error_code sp))



let _ =
  Eliom_extensions.register_eliom_extension gen
