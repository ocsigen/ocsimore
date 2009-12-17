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

type 'a rights_sp =
  rights:Wiki_types.wiki_rights ->
  sp: Eliom_sessions.server_params ->
  'a


let new_wikitextbox ?db ~rights ~sp ~content_type ~wiki ~author ~comment ~content () =
  rights#can_create_wikiboxes ~sp wiki
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
        Wiki_sql.get_wikibox_content ?version wb >>= function
          | None -> Lwt.fail (Unknown_box (wb, version))
          | Some (_com, _a, cont, _d, ct, ver) ->
              Lwt.return (ct, cont, ver)

let wikibox_content' ~rights ~sp ?version wikibox =
  wikibox_content ~rights ~sp ?version wikibox >>= fun (_, cont, ver) ->
  Lwt.return (cont, ver)


let save_wikibox_aux ~rights ~sp ~wb ~content ~content_type =
  (if content = None
   then
     Wiki_sql.wikibox_wiki wb >>= fun wiki ->
     rights#can_delete_wikiboxes ~sp wiki
   else rights#can_write_wikibox ~sp wb) >>= function
    | true ->
        User.get_user_id sp >>= fun user ->
        Wiki_sql.update_wikibox ~author:user ~comment:""
          ~content ~content_type ~ip:(Eliom_sessions.get_remote_ip sp) wb
    | false -> Lwt.fail Ocsimore_common.Permission_denied


let save_wikitextbox ~rights ~sp ~content_type ~wb ~content =
  save_wikibox_aux ~rights ~sp ~wb ~content_type ~content

exception BadCssWikibox

let save_wikicss_aux ~rights ~sp ~content ~wb l =
  try
    ignore (List.find (fun (wb', _, _) -> wb = wb') l);
    save_wikibox_aux ~rights ~sp ~wb
      ~content_type:Wiki_models.css_content_type ~content
  with Not_found -> Lwt.fail BadCssWikibox

let save_wikicssbox ~rights ~sp ~wiki ~content ~wb =
  Wiki_sql.get_css_wikibox_for_wiki ~wiki >>= fun l ->
  save_wikicss_aux ~rights ~sp ~content ~wb l


let save_wikipagecssbox ~rights ~sp ~wiki ~page ~content ~wb =
  Wiki_sql.get_css_wikibox_for_wikipage ~wiki ~page >>= fun l ->
  save_wikicss_aux ~rights ~sp ~content ~wb l



let wikibox_history ~rights ~sp ~wb =
  rights#can_view_history ~sp wb >>= function
    | true -> Wiki_sql.get_wikibox_history wb
    | false -> Lwt.fail Ocsimore_common.Permission_denied




let create_wiki ~(rights : Wiki_types.wiki_rights) ~sp
    ~title ~descr ?path ?staticdir ?(boxrights = true)
    ~admins ~readers ?container_text ~model () =
  User.get_user_id sp >>= fun u ->
  rights#can_create_wiki sp () >>= function
    | true ->
        Wiki.create_wiki ~title ~descr ?path ?staticdir ~boxrights
          ~author:u ~admins ~readers
          ?container_text ~model ()
    | false ->
        Lwt.fail Ocsimore_common.Permission_denied


let css_aux ~(rights : Wiki_types.wiki_rights) ~sp l =
  Lwt_util.fold_left
    (fun l (wb, media_type, rank) ->
       wikibox_content rights sp wb >>= function
         | (_, Some cont, _) -> Lwt.return ((wb, (cont, media_type, rank)) :: l)
         | (_, None, _) -> Lwt.return l
    ) [] l


let wiki_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki =
  Wiki_sql.get_css_wikibox_for_wiki wiki >>= fun l ->
  css_aux ~rights ~sp l

let wikipage_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page =
  Wiki_sql.get_css_wikibox_for_wikipage wiki page >>= fun l ->
  css_aux ~rights ~sp l



let set_wikibox_special_rights ~(rights : Wiki_types.wiki_rights) ~sp ~wb  ~special_rights =
  rights#can_set_wikibox_specific_permissions sp wb >>= function
    | true ->
        Sql.full_transaction_block (fun _db -> (*YYY should be made atomic... *)
          (if special_rights then
             (* When the wikibox starts using specific permissions, we
                automatically add the wiki defaults to the wikibox rights *)
             Wiki_sql.get_wikibox_info wb >>= fun { wikibox_wiki = wiki } ->
             User.GenericRights.iter_awr_lwt
               (fun it -> User_sql.add_to_group
                  ~user:(it.User.GenericRights.field Wiki.wiki_wikiboxes_grps $ wiki)
                  ~group:(it.User.GenericRights.field Wiki.wikibox_grps $ wb))
           else
             Lwt.return ()
          ) >>= fun () ->
          Wiki_sql.set_wikibox_special_rights wb special_rights
        )

    | false -> Lwt.fail Ocsimore_common.Permission_denied



exception Page_already_exists of wikibox

let create_wikipage ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page =
  rights#can_create_wikipages ~sp wiki >>= function
    | true ->
        Lwt.catch
          (fun () ->
             Wiki_sql.get_wikipage_info wiki page
             >>= fun { wikipage_wikibox = wb } ->
             Lwt.fail (Page_already_exists wb)
          )
          (function
             | Not_found ->
                 User.get_user_id ~sp >>= fun user ->
                 Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
                 let content_type = Wiki_models.get_default_content_type
                   wiki_info.wiki_model in
                 Sql.full_transaction_block (fun db ->
                   new_wikitextbox ~rights ~content_type ~sp ~wiki ~author:user
                     ~comment:(Printf.sprintf "wikipage %s in wiki %s"
                                 page (string_of_wiki wiki))
                     ~content:("== Page "^page^"==") ()
                   >>= fun wb ->
                   Wiki_sql.create_wikipage ~db ~wiki ~page ~wb
                 )

             | e -> Lwt.fail e)
    | false ->  Lwt.fail Ocsimore_common.Permission_denied


exception Css_already_exists


let add_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page ~media ?wbcss ()=
  (match page with
     | None -> rights#can_create_wikicss sp wiki
     | Some page -> rights#can_create_wikipagecss sp (wiki, page)
  ) >>= function
    | false -> Lwt.fail Ocsimore_common.Permission_denied
    | true ->
        User.get_user_id ~sp >>= fun user ->
        Wiki_sql.add_css_aux ~wiki ~page ~author:user ~media ?wbcss ()

let delete_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page ~wb =
  (match page with
     | None -> rights#can_create_wikicss sp wiki
     | Some page -> rights#can_create_wikipagecss sp (wiki, page)
  ) >>= function
    | false -> Lwt.fail Ocsimore_common.Permission_denied
    | true ->
        match page with
          | None ->
              Wiki_sql.remove_css_wiki ~wiki wb

          | Some page ->
              Wiki_sql.remove_css_wikipage ~wiki ~page wb


let update_css ~(rights : Wiki_types.wiki_rights) ~sp ~wiki ~page ~oldwb ~newwb ~media ~rank =
  (match page with
     | None -> rights#can_create_wikicss sp wiki
     | Some page -> rights#can_create_wikipagecss sp (wiki, page)
  ) >>= function
    | false -> Lwt.fail Ocsimore_common.Permission_denied
    | true ->
        Wiki_sql.update_css_wikibox_aux ~wiki ~page ~oldwb ~newwb ~media ~rank ()




let update_wiki ~(rights : Wiki_types.wiki_rights) ~sp ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki =
  if staticdir <> None || path <> None || boxrights <> None || model <> None || siteid <> None then
    rights#can_create_wiki sp () >>= function
      | true -> Wiki_sql.update_wiki
          ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki
      | false -> Lwt.fail Ocsimore_common.Permission_denied
  else
    rights#can_edit_metadata ~sp wiki >>= function
      | true ->
          Wiki_sql.update_wiki ?container ?descr wiki
      | false -> Lwt.fail Ocsimore_common.Permission_denied


let save_wikipage_properties ~(rights : Wiki_types.wiki_rights) ~sp ?title ?wb ?newpage (wiki, page as wp) =
  rights#can_admin_wikipage ~sp wp >>= function
    | true ->
        Wiki_sql.set_wikipage_properties ?wiki ~page ?title ?newpage ?wb ()
    | false -> Lwt.fail Ocsimore_common.Permission_denied
