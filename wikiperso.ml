(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2008-2009
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
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open User_sql.Types
open Wiki_types


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
let username, wikiroot, siteid = match Eliom_sessions.get_config () with
  | [Element("options", [("username", u);("wikiroot", w)],[])] ->
      u, w, None
  | [Element("options", [("username", u);("wikiroot", w);("siteid",h)],[])] ->
      u, w, Some h
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
   method as the one specified in the eliom module ocsi_wiki is used.
   Currently, Pam authentification is not supported
*)
let external_user user =
  match User_services.auth with
    | User_services.NoExternalAuth -> return None
    | User_services.Nis ->
        (Nis_chkpwd.userinfo user
         >>= function
           | None -> return None
           | Some userdata ->
               User.create_user ~name:user
                 ~pwd:User_sql.Types.External_Auth
                 ~fullname:userdata.Unix.pw_gecos
                 ~email:(user ^ "@localhost")
                 ()
               >>= fun userdata ->
                 return (Some userdata)
        )
    | User_services.Pam _ ->
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
<<cond ingroup='users'|You are connected as <<username>>.
<<logoutlink|Logout>>.>>
"
let default_wikicss = ""

let template_container_pagename = "wikiperso-template"
let template_css_pagename = "wikiperso_css-template"

let template_container = Wiki_site.register_named_wikibox
  ~page:template_container_pagename ~content:default_welcome_page
  ~content_type:Wiki_syntax.wikicreole_content_type
  ~comment:"Template for wikipersos container pages"

let template_wiki_css = Wiki_site.register_named_wikibox
  ~page:template_css_pagename ~content:default_wikicss
  ~content_type:Wiki_models.css_content_type
  ~comment:"Template for wikipersos css"


(** The function that creates the wikiperso when needed *)
let create_wikiperso ~model ~wiki_title ~userdata =
  let gid = [basic_user userdata.user_id]
  and author = userdata.user_id in
  template_container () >>= fun container ->
  template_wiki_css () >>= fun css ->
  (* We create the wiki, without supplying the [path] field. This will make
     the relocation of the wiki easier *)
  Wiki.create_wiki ~model
    ~title:wiki_title
    ~descr:(Printf.sprintf !Language.messages.Language.wikiperso_wikidescr
              userdata.user_fullname)
    ~admins:gid ~author ~container_text:container () >>= fun wiki ->
  Wiki_sql.add_css_aux ~wiki ~page:None ~author ~media:[`All] ()
  >>= fun wikibox ->
  Wiki_sql.update_wikibox ~author ~comment:"" ~content:(Some css)
    ~content_type:Wiki_models.css_content_type wikibox >>= fun _ ->
  Lwt.return wiki


(* Given a user name, we find the id of the corresponding ocsimore
   user. If it does not exists, we try external authentification *)
let find_user user =
  User.get_basicuser_by_login user
  >>= fun userdata ->
    if userdata <> User.nobody then
      Lwt.return (Some userdata)
    else
      external_user user


let can_have_wikiperso =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix:"wikiperso"
       ~name:"can_have_wikiperso"
       ~descr:"can have a wikiperso (unless they are in the group 'cannot_have_wikiperso')"
    )

let cannot_have_wikiperso =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix:"wikiperso"
       ~name:"cannot_have_wikiperso"
       ~descr:"are forbidden to have a wikiperso"
    )

let f_can_have_wikiperso sp user =
  User.in_group ~sp ~user ~group:can_have_wikiperso () >>= function
    | true ->
        User.in_group ~sp ~user ~group:cannot_have_wikiperso () >>= fun b ->
        Lwt.return (not b)
    | false -> Lwt.return false



(* We override all the standard wiki rights, so that anonymous
   has no write or admin rights at all *)
class wikiperso_rights =
  let is_not_anonymous sp =
    User.get_user_id sp >>= fun u -> Lwt.return (u <> User.anonymous) in

  let perm f ~sp arg =
    is_not_anonymous sp >>= function
      | false -> Lwt.return false
      | true -> f ~sp arg
  in
object
  inherit Wiki.wiki_rights as super

  method can_create_wiki = perm super#can_create_wiki
  method can_admin_wiki = perm super#can_admin_wiki
  method can_edit_metadata = perm super#can_edit_metadata

  method can_admin_wikibox = perm super#can_admin_wikibox
  method can_write_wikibox = perm super#can_write_wikibox

  method can_create_wikipages = perm super#can_create_wikipages
  method can_create_subwikiboxes = perm super#can_create_subwikiboxes
  method can_create_wikiboxes = perm super#can_create_wikiboxes
  method can_delete_wikiboxes = perm super#can_delete_wikiboxes

  method can_create_wikicss = perm super#can_create_wikicss
  method can_create_wikipagecss = perm super#can_create_wikipagecss

  method can_set_wikibox_specific_permissions =
    perm super#can_set_wikibox_specific_permissions

  method can_admin_wikipage = perm super#can_admin_wikipage
end

let wikiperso_rights = new wikiperso_rights


let wikiperso_model =
  Wiki_models.register_wiki_model
    ~name:"wikicreole-wikiperso"
    ~content_type:Wiki_syntax.wikicreole_content_type
    ~rights:wikiperso_rights
    ~widgets:Wiki_site.wikibox_widget


(** The function that answers for the extension. *)
let gen sp =
  let ri = Eliom_sessions.get_ri sp in
  (* We check that the url corresponds to a wiki *)
  match extract_user_name ri.ri_sub_path_string with
    | Some user ->
        (* If this is the case, we try to find the id of the corresponding
           ocsimore user. *)
        find_user user >>=
        (function
           | None -> Lwt.return ()
           | Some userid ->
               (* If the user for which we must create a wiki
                  exists, we create this wiki if it does not already exists,
                  and if the user has the right to have a wikiperso *)
               let wiki_title = "wikiperso for " ^ user in
               Lwt.catch
                 (fun () -> Wiki_sql.get_wiki_info_by_name wiki_title
                    >>= fun _ -> Lwt.return ())
                 (function
                    | Not_found ->
                        (f_can_have_wikiperso sp (basic_user userid) >>=function
                           | true ->
                               let model = wikiperso_model in
                               User_sql.get_basicuser_data userid >>= fun ud ->
                               create_wikiperso ~model ~wiki_title ~userdata:ud
                               >>= fun wiki ->
                               (* We then register the wiki at the correct url*)
                               Wiki_services.register_wiki ~sp ~wiki:wiki ()
                                 ~rights:Wiki_site.wiki_rights
                                 ~path:(wiki_path ud.user_login)
                                 ~siteids:(siteid, Wiki_site.siteid);
                               Lwt.return ()
                           | false -> Lwt.return ())
                    | e -> Lwt.fail e)
        )
        >>= fun () ->
          (* In all cases, we just tell Eliom to continue. It will answer with
             the wiki if it has been successfully created *)
         return
           (Ext_next (Eliom_sessions.get_previous_extension_error_code sp))

    | None -> return
        (Ext_next (Eliom_sessions.get_previous_extension_error_code sp))


(* We load the existing wikipersos at the correct path *)
let () =
  let regexp = Netstring_pcre.regexp "^wikiperso for (.*)$" in
  Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_title = title; wiki_siteid = siteid } ->
        (match Netstring_pcre.string_match regexp title 0 with
           | Some result ->
               let user = Netstring_pcre.matched_group result 1 title in
               Wiki_services.register_wiki ~rights:Wiki_site.wiki_rights
                 ~path:(wiki_path user) ~wiki:wiki
                 ~siteids:(siteid, Wiki_site.siteid) ()
           | None -> ()
        );
        Lwt.return ()
     )
  )

(*
(* Update to correct wikiperso model *)
let () =
  let regexp = Netstring_pcre.regexp "^wikiperso for (.*)$" in
  Lwt_unix.run
  (Wiki_sql.iter_wikis
     (fun { wiki_id = wiki; wiki_title = title } ->
        (match Netstring_pcre.string_match regexp title 0 with
           | Some _ ->
               Wiki_sql.update_wiki ~model:wikiperso_model wiki
           | None -> Lwt.return ()
        )
     )
  )
*)

let users_root =
  Eliom_services.new_service
    ~path:[Ocsimore_lib.ocsimore_admin_dir;"wikiperso"]
    ~get_params:Eliom_parameters.unit ()

let () = Eliom_predefmod.Xhtml.register users_root
  (fun sp () () ->
     User_sql.user_to_string can_have_wikiperso >>= fun s1 ->
     User_sql.user_to_string cannot_have_wikiperso >>= fun s2 ->
     Page_site.admin_page ~sp
       ~title:"Ocsimore - Wikiperso module"
       [XHTML.M.h1 [XHTML.M.pcdata "Wikiperso module"];
        XHTML.M.p [XHTML.M.pcdata "This is the Ocsimore admin page for the \
                                   wikiperso module. Wikipersos are wikis that \
                                   are automatically created for each Ocsimore \
                                   user, and on which the user has write \
                                   access."];
        XHTML.M.br (); XHTML.M.br ();
        XHTML.M.pcdata "Most of the configuration is done through the Ocsigen \
                        configuration file. You can however choose which users \
                        can have wikipersos by adding users or groups inside \
                        the following roles:";
        Eliom_predefmod.Xhtml.a ~service:User_services.service_view_group
          ~sp [XHTML.M.pcdata "users that can have a wikiperso"] s1;
        XHTML.M.pcdata " and ";
        Eliom_predefmod.Xhtml.a ~service:User_services.service_view_group
          ~sp [XHTML.M.pcdata "users that cannot have a wikiperso"] s2;
        XHTML.M.pcdata ".";
       ]
  )


let () = Page_site.add_to_admin_menu ~root:users_root ~name:"Wikiperso" ~links:[]



let _ = Eliom_extensions.register_eliom_extension gen
