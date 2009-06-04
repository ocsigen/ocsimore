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
Access to wiki data with permission checks

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)


open Wiki_types
open User_sql.Types


let (>>=) = Lwt.bind


let new_wikitextbox ?db
    ~rights ~content_type ~sp ~wiki ~author ~comment ~content () =
  rights#can_create_genwikiboxes ~sp wiki
  >>= function
    | true -> Wiki_sql.new_wikibox ?db ~wiki ~author ~comment ~content
        ~content_type ()
    | false -> Lwt.fail Ocsimore_common.Permission_denied



(** Exception raised when the content of a wikibox cannot be found *)
exception Unknown_box of wikibox * int32 option

let wikibox_content ~rights ~sp ?version wb =
  rights#can_read_wikibox ~sp wb >>= function
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
   else rights#can_write_wikibox ~sp wb) >>= function
    | true ->
        User.get_user_id sp >>= fun user ->
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


let wikibox_history ~rights ~sp ~wb =
  rights#can_view_history ~sp wb >>= function
    | true -> Wiki_sql.get_history wb
    | false -> Lwt.fail Ocsimore_common.Incorrect_argument




let create_wiki ~(rights : Wiki_types.wiki_rights) ~sp
    ~title ~descr ?path ?staticdir ?(boxrights = true)
    ~admin ?wiki_css ?container_text ~model () =
  rights#can_create_wiki sp () >>= function
    | true ->
        Wiki.create_wiki ~title ~descr ?path ?staticdir ~boxrights
          ~author:admin ~admins:[basic_user admin] ~readers:[]
          ?wiki_css ?container_text ~model ()
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied


let wiki_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki =
  Wiki_sql.get_css_wikibox_for_wiki wiki >>= function
    | None -> Lwt.fail Eliom_common.Eliom_404
    | Some wb ->
        wikibox_content rights sp wb >>= function
          | (_, Some cont, _) -> Lwt.return cont
          | (_, None, _) -> Lwt.fail Eliom_common.Eliom_404

let wikipage_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page =
  Wiki_sql.get_css_wikibox_for_wikipage wiki page >>= function
    | None -> Lwt.fail Eliom_common.Eliom_404
    | Some wb ->
        wikibox_content rights sp wb >>= function
          | (_, Some cont, _) -> Lwt.return cont
          | (_, None, _) -> Lwt.fail Eliom_common.Eliom_404
