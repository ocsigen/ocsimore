(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
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
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)


open Wiki_types
open User_sql.Types


let (>>=) = Lwt.bind


(* An exception raised when we register two wikis at the same path.
   The first two strings are the description of the conflicting wikis,
   the third string is the path *)
exception Wiki_already_registered_at_path of (string * string) * string


(* Create a wiki that is supposed not to exist already *)
let really_create_wiki ~title ~descr ?path ?staticdir ?(boxrights = true)
    ~author
    ?(admins=[basic_user author]) ?(readers = [basic_user Users.anonymous])
    ?wiki_css ~container_text
    ~model
    () =
  let path_string = Ocsimore_lib.bind_opt
    path (Ocsigen_lib.string_of_url_path ~encode:true)
  in
  (* We check that no wiki is already registered at the same path *)
  Ocsimore_lib.lwt_bind_opt path_string
    (fun path -> Wiki_sql.iter_wikis
       (fun { wiki_descr = wiki; wiki_pages = path' } ->
          if path' = Some path then
            Lwt.fail (Wiki_already_registered_at_path ((wiki, descr), path))
          else
            Lwt.return ())
    ) >>= fun _ ->
  (* Notice that there is a theoretical race condition in the code below,
     when the container wikibox receives its rights, in the case this
     container has changed between the creation of the wiki and the moments
     the rights are added *)
  Wiki_sql.new_wiki ~title ~descr ~pages:path_string
     ~boxrights ?staticdir ~container_text ~author ~model ()
   >>= fun (wiki_id, _wikibox_container) ->

   (* Putting users in groups *)
   (* Admins *)
   Users.add_list_to_group ~l:admins ~group:(apply_parameterized_group
                           Wiki_data.wiki_admins wiki_id) >>= fun () ->
   (* Readers *)
   Users.add_list_to_group ~l:readers
     ~group:(Wiki_data.wiki_wikiboxes_grps.grp_reader $ wiki_id) >>= fun () ->
   Users.add_list_to_group ~l:readers
     ~group:(Wiki_data.wiki_files_readers $ wiki_id) >>= fun () ->

   (match wiki_css with
      | None -> Lwt.return ()
      | Some css -> Wiki_sql.set_css_for_wiki
          ~wiki:wiki_id ~author (Some css)
   ) >>= fun () ->

   Lwt.return wiki_id


let new_wikitextbox ?db
    ~rights ~content_type ~sp ~wiki ~author ~comment ~content () =
  rights#can_create_genwikiboxes ~sp wiki
  >>= function
    | true -> Wiki_sql.new_wikibox ?db ~wiki ~author ~comment ~content
        ~content_type ()
    | false -> Lwt.fail Ocsimore_common.Permission_denied


(* Checks that [boxversion] is the current version of the wikibox *)
let modified_wikibox ~wikibox ~boxversion =
  Wiki_sql.current_wikibox_version wikibox
  >>= function
    | None -> Lwt.return None (* This case is not supposed to happen *)
    | Some curversion ->
        if curversion > boxversion then
          Lwt.return (Some curversion)
        else
          Lwt.return None


(** Exception raised when the content of a wikibox cannot be found *)
exception Unknown_box of wikibox * int32 option

let wikibox_content ~rights ~sp ?version wb =
  rights#can_read_wikibox ~sp ~wb >>= function
    | false -> Lwt.fail Ocsimore_common.Permission_denied
    | true ->
        Wiki_sql.get_wikibox_data ?version ~wikibox:wb () >>= function
          | None -> Lwt.fail (Unknown_box (wb, version))
          | Some (_com, _a, cont, _d, ct, ver) ->
              Lwt.return (ct, cont, ver)

let wikibox_content' ~rights ~sp ?version wikibox =
  wikibox_content ~rights ~sp ?version wikibox >>= fun (_, cont, ver) ->
  Lwt.return (cont, ver)


let save_wikibox_aux ~rights ~sp ~wb ~content ~content_type =
  (if content = None 
   then rights#can_delete_wikiboxes ~sp (fst wb)
   else rights#can_write_wikibox ~sp ~wb) >>= function
    | true ->
        Users.get_user_id sp >>= fun user ->
        Wiki_sql.update_wikibox ~wikibox:wb ~author:user ~comment:""
          ~content ~content_type

    | false -> Lwt.fail Ocsimore_common.Permission_denied


let save_wikitextbox ~rights ~content_type ~sp ~wb ~content =
  save_wikibox_aux ~rights ~sp ~wb ~content_type ~content

let save_wikicssbox ~rights ~sp ~wiki ~content =
  Wiki_sql.get_css_wikibox_for_wiki wiki >>= function
    | Some wb ->
        save_wikibox_aux ~rights ~sp ~wb
          ~content_type:Wiki_models.css_content_type ~content
    | None -> Lwt.fail Ocsimore_common.Incorrect_argument

let save_wikipagecssbox ~rights ~sp ~wiki ~page ~content =
  Wiki_sql.get_css_wikibox_for_wikipage wiki page >>= function
    | Some wb ->
        save_wikibox_aux ~rights ~sp ~wb
          ~content_type:Wiki_models.css_content_type ~content
    | None -> Lwt.fail Ocsimore_common.Incorrect_argument


(* XXX add rights *)
let wikibox_history = Wiki_sql.get_history


