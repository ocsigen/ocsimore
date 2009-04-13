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


open Wiki_sql.Types
let (>>=) = Lwt.bind


(** Role of user in the wiki (for one box) *)
type role = Admin | Author | Lurker | Nonauthorized;;


let get_sthg_ f ((w, _) as k) =
  Wiki_sql.get_wiki_info_by_id w
  >>= fun wiki_info ->
  if wiki_info.wiki_boxrights
  then
    f k >>= fun r -> Lwt.return (Some r)
  else Lwt.return None

let get_readers =
  get_sthg_ Wiki_sql.get_readers

let get_writers =
  get_sthg_ Wiki_sql.get_writers

let get_rights_adm =
  get_sthg_ Wiki_sql.get_rights_adm

let get_wikiboxes_creators =
  get_sthg_ Wiki_sql.get_wikiboxes_creators


let readers_group_name i = "wiki"^wiki_id_s i^"_readers"
let writers_group_name i = "wiki"^wiki_id_s i^"_writers"
let rights_adm_group_name i = "wiki"^wiki_id_s i^"_rights_givers"
let page_creators_group_name i = "wiki"^wiki_id_s i^"_page_creators"
let css_editors_group_name i = "wiki"^wiki_id_s i^"_css_editors"
let wikiboxes_creators_group_name i = "wiki"^wiki_id_s i^"_wikiboxes_creators"
let container_adm_group_name i = "wiki"^wiki_id_s i^"_container_adm"
let admin_group_name i = "wiki"^wiki_id_s i^"_admin"

let readers_group i = Users.get_user_id_by_name (readers_group_name i)
let writers_group i = Users.get_user_id_by_name (writers_group_name i)
let rights_adm_group  i = Users.get_user_id_by_name (rights_adm_group_name  i)
let page_creators_group i = 
                      Users.get_user_id_by_name (page_creators_group_name i)
let css_editors_group i = 
                      Users.get_user_id_by_name (css_editors_group_name i)
let wikiboxes_creators_group i = 
                      Users.get_user_id_by_name
                        (wikiboxes_creators_group_name i)
let container_adm_group i =
                      Users.get_user_id_by_name (container_adm_group_name i)
let admin_group i =   Users.get_user_id_by_name (admin_group_name i)


exception Found of int32

let new_wikibox ~wiki ~author ~comment ~content ~content_type ?readers ?writers ?rights_adm ?wikiboxes_creators () =
  Wiki_sql.get_wiki_info_by_id wiki
  >>= fun wiki_info ->
  (if wiki_info.wiki_boxrights
  then (
    (match readers with
       | Some r -> Lwt.return r
       | None ->
           readers_group wiki >>= fun r ->
           Lwt.return [r]) >>= fun readers ->
    (match writers with
       | Some r -> Lwt.return r
       | None ->
           writers_group wiki >>= fun r ->
           Lwt.return [r]) >>= fun writers ->
    (match rights_adm with
       | Some r -> Lwt.return r
       | None ->
           rights_adm_group wiki >>= fun r ->
           Lwt.return [r]) >>= fun rights_adm ->
    (match wikiboxes_creators with
       | Some r -> Lwt.return r
       | None ->
           wikiboxes_creators_group wiki >>= fun r ->
           Lwt.return [r]) >>= fun wikiboxes_creators ->
    Lwt.return
      (Some (readers, writers, rights_adm, wikiboxes_creators)))
  else Lwt.return None) >>= fun rights ->
  Lwt.catch
    (fun () ->
       Wiki_sql.new_wikibox ~wiki ~author ~comment
         ~content ~content_type ?rights ())
    (function Found b -> Lwt.return b | e -> Lwt.fail e)


let create_group_ name fullname =
  Users.create_user 
    ~name
    ~pwd:User_sql.Connect_forbidden
    ~fullname
    ~groups:[]
    ()


let add_to_group_ l g =
  List.fold_left
    (fun beg u -> 
       beg >>= fun () ->
       Users.add_to_group ~user:u ~group:g)
    (Lwt.return ())
    l

let can_sthg rights_box rights_wiki ~sp ~sd wiki id userid =
  if userid == Users.admin.Users.id
  then Lwt.return true
  else
    rights_box (wiki.wiki_id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left
            (fun b a ->
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group ~sp ~sd ~user:userid ~group:a ())
            (Lwt.return false)
            l
      | None ->
          rights_wiki wiki.wiki_id >>= fun g ->
          Users.in_group ~sp ~sd ~user:userid ~group:g ()


let can_change_rights = can_sthg get_rights_adm rights_adm_group
let can_read = can_sthg get_readers readers_group
let can_write = can_sthg get_writers writers_group
let can_create_wikibox = can_sthg get_wikiboxes_creators wikiboxes_creators_group

let get_role_ ~sp ~sd ((wiki : wiki), id) =
  Wiki_sql.get_wiki_info_by_id wiki >>= fun w ->
  Users.get_user_data sp sd >>= fun u ->
  let u = u.Users.id in
  can_change_rights ~sp ~sd w id u >>= fun cana ->
  if cana
  then Lwt.return Admin
  else
    can_write ~sp ~sd w id u >>= fun canw ->
    if canw
    then Lwt.return Author
    else 
      can_read ~sp ~sd w id u >>= fun canr ->
      if canr
      then Lwt.return Lurker
      else Lwt.return Nonauthorized


module Roles = Map.Make(struct
                          type t = wikibox
                          let compare = compare
                        end)

type wiki_sd = 
    {
      wikibox_role : wikibox -> role Lwt.t;
    }

let cache_find table f box =
  try 
    Lwt.return (Roles.find box !table)
  with Not_found -> 
    f box >>= fun v ->
    table := Roles.add box v !table;
    Lwt.return v

let default_wiki_sd ~sp ~sd =
  let cache = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  {wikibox_role = cache_find cache (get_role_ ~sp ~sd);
  }

(** The polytable key for retrieving wiki data inside session data *)
let wiki_key : wiki_sd Polytables.key = Polytables.make_key ()

let get_wiki_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:wiki_key
  with Not_found -> 
    let wsd = default_wiki_sd ~sp ~sd in
    Polytables.set sd wiki_key wsd;
    wsd



let get_role ~sp ~sd k =
  let wiki_sd = get_wiki_sd ~sp ~sd in
  wiki_sd.wikibox_role k


let user_can_save_wikibox ~sp ~sd wb =
  get_role sp sd wb >>= function
    | Admin | Author -> Lwt.return true
    | Lurker | Nonauthorized -> Lwt.return false


(* An exception raised when we register two wikis at the same path.
   The first two strings are the description of the conflicting wikis,
   the third string is the path *)
exception Wiki_already_registered_at_path of (string * string) * string


(* Create a wiki that is supposed not to exist already *)
let really_create_wiki ~title ~descr
    ?path
    ?(readers = [Users.anonymous.Users.id])
    ?(writers = [Users.authenticated_users.Users.id])
    ?(rights_adm = [])
    ?(wikiboxes_creators = [Users.authenticated_users.Users.id])
    ?(container_adm = [])
    ?(page_creators = [Users.authenticated_users.Users.id])
    ?(css_editors = [Users.authenticated_users.Users.id])
    ?(admins = [])
    ?(boxrights = true)
    ?staticdir
    ?wiki_css
    ~container_page
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
     ~boxrights ?staticdir ~container_page ()
   >>= fun (wiki_id, wikibox_container) ->

   (* Creating groups *)
   create_group_ (readers_group_name wiki_id)
     ("Users who can read wiki "^wiki_id_s wiki_id)
   >>= fun readers_data ->
     create_group_ (writers_group_name wiki_id)
       ("Users who can write in wiki "^wiki_id_s wiki_id)
   >>= fun writers_data ->
     create_group_ (rights_adm_group_name wiki_id)
       ("Users who can change rights in wiki "^wiki_id_s wiki_id)
   >>= fun rights_adm_data ->
     create_group_ (page_creators_group_name wiki_id)
       ("Users who can create pages in wiki "^wiki_id_s wiki_id)
   >>= fun page_creators_data ->
     create_group_ (css_editors_group_name wiki_id)
       ("Users who can edit css for wikipages of wiki "^wiki_id_s wiki_id)
   >>= fun css_editors_data ->
       create_group_ (wikiboxes_creators_group_name wiki_id)
         ("Users who can create wikiboxes in wiki "^wiki_id_s wiki_id)
   >>= fun wikiboxes_creators_data ->
     create_group_ (container_adm_group_name wiki_id)
       ("Users who can change the layout of pages "^wiki_id_s wiki_id)
   >>= fun container_adm_data ->
     create_group_ (admin_group_name wiki_id)
       ("Wiki administrator "^wiki_id_s wiki_id)
   >>= fun admin_data ->

   (* Putting users in groups *)
     add_to_group_ [admin_data.Users.id] wikiboxes_creators_data.Users.id
   >>= fun () ->
     add_to_group_ [admin_data.Users.id] page_creators_data.Users.id
   >>= fun () ->
     add_to_group_ [admin_data.Users.id] css_editors_data.Users.id
   >>= fun () ->
     add_to_group_ [admin_data.Users.id] rights_adm_data.Users.id
   >>= fun () ->
     add_to_group_ [admin_data.Users.id] container_adm_data.Users.id
   >>= fun () ->
     add_to_group_ [wikiboxes_creators_data.Users.id;
                    page_creators_data.Users.id;
                    css_editors_data.Users.id;
                    rights_adm_data.Users.id;
                    container_adm_data.Users.id]
       writers_data.Users.id
   >>= fun () ->
     add_to_group_ [writers_data.Users.id] readers_data.Users.id
   >>= fun () ->
     add_to_group_ readers readers_data.Users.id
   >>= fun () ->
       add_to_group_ writers writers_data.Users.id
   >>= fun () ->
     add_to_group_ rights_adm rights_adm_data.Users.id
   >>= fun () ->
     add_to_group_ page_creators page_creators_data.Users.id
   >>= fun () ->
     add_to_group_ css_editors css_editors_data.Users.id
   >>= fun () ->
     add_to_group_ wikiboxes_creators wikiboxes_creators_data.Users.id
   >>= fun () ->
     add_to_group_ admins admin_data.Users.id
   >>= fun () ->
     add_to_group_ container_adm container_adm_data.Users.id
   >>= fun () ->

   Wiki_sql.populate_readers (wiki_id, wikibox_container)
     [readers_data.Users.id] >>= fun () ->
   Wiki_sql.populate_writers (wiki_id, wikibox_container)
     [container_adm_data.Users.id] >>= fun () ->
   Wiki_sql.populate_rights_adm (wiki_id, wikibox_container)
     [rights_adm_data.Users.id] >>= fun () ->
   Wiki_sql.populate_wikiboxes_creators (wiki_id, wikibox_container)
     [wikiboxes_creators_data.Users.id] >>= fun () ->

   (match wiki_css with
      | None -> Lwt.return ()
      | Some css -> Wiki_sql.set_css_for_wiki
          ~wiki:wiki_id ~author:css_editors_data.Users.id css
   ) >>= fun () ->

   Lwt.return wiki_id






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




exception Unknown_box of wikibox


let retrieve_wikibox_aux ?version wikibox =
  Wiki_sql.get_wikibox_data ?version ~wikibox ()
  >>= fun result ->
  match result with
    | None -> Lwt.fail Not_found
    | Some (_com, _a, cont, _d, ct, ver) -> Lwt.return (ct, cont, ver)


let save_wikibox ~enough_rights ~sp ~sd ~wikibox ~content ~content_type =
  enough_rights ~sp ~sd wikibox >>= function
    | true ->
        Users.get_user_data sp sd
        >>= fun user ->
        Wiki_sql.update_wikibox ~wikibox
          ~author:user.Users.id ~comment:"" ~content ~content_type

    | false -> Lwt.fail Ocsimore_common.Permission_denied


let save_wikibox_permissions ~sp ~sd (wikibox, rights) =
  get_role sp sd wikibox >>= function
   | Admin ->
       let (addr, (addw, (adda, (addc, (delr, (delw, (dela, delc))))))) = rights
        in
        Users.group_list_of_string addr >>= fun readers ->
        Wiki_sql.populate_readers wikibox readers >>= fun () ->
        Users.group_list_of_string addw >>= fun w ->
        Wiki_sql.populate_writers wikibox w >>= fun () ->
        Users.group_list_of_string adda >>= fun a ->
        Wiki_sql.populate_rights_adm wikibox a >>= fun () ->
        Users.group_list_of_string addc >>= fun a ->
        Wiki_sql.populate_wikiboxes_creators wikibox a >>= fun () ->
        Users.group_list_of_string delr >>= fun readers ->
        Wiki_sql.remove_readers wikibox readers >>= fun () ->
        Users.group_list_of_string delw >>= fun w ->
        Wiki_sql.remove_writers wikibox w >>= fun () ->
        Users.group_list_of_string dela >>= fun a ->
        Wiki_sql.remove_rights_adm wikibox a >>= fun () ->
        Users.group_list_of_string delc >>= fun a ->
        Wiki_sql.remove_wikiboxes_creators wikibox a
    | _ -> Lwt.return () (* XXX Notify an error *)
