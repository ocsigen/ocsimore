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


let (>>=) = Lwt.bind

(** Role of user in the wiki (for one box) *)
type role = Admin | Author | Lurker | Nonauthorized;;
(* Admin can changes the permissions on boxes *)


type wiki_info = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  path : string list option;
  boxrights : bool
}

let get_wiki_by_id id =
  Wiki_cache.find_wiki id
  >>= fun (id, title, descr, path, br) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               path = path;
               boxrights = br
             }

let get_wiki_by_name title =
  Wiki_sql.find_wiki_ ~title () >>= fun (id, title, descr, path, br) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               path = path;
               boxrights = br;
             }

let get_sthg_ f ?wiki ((w, i) as k) =
  (match wiki with
    | Some w -> Lwt.return w
    | None -> get_wiki_by_id w)
  >>= fun wiki_info ->
  if wiki_info.boxrights
  then
    f k >>= fun r -> Lwt.return (Some r)
  else Lwt.return None

let get_readers =
  get_sthg_ Wiki_cache.get_readers_

let get_writers =
  get_sthg_ Wiki_cache.get_writers_

let get_rights_adm =
  get_sthg_ Wiki_cache.get_rights_adm_

let get_wikiboxes_creators =
  get_sthg_ Wiki_cache.get_wikiboxes_creators_


let readers_group_name i = "wiki"^Int32.to_string i^"_readers"
let writers_group_name i = "wiki"^Int32.to_string i^"_writers"
let rights_adm_group_name i = "wiki"^Int32.to_string i^"_rights_givers"
let page_creators_group_name i = "wiki"^Int32.to_string i^"_page_creators"
let wikiboxes_creators_group_name i = "wiki"^Int32.to_string i^"_wikiboxes_creators"
let container_adm_group_name i = "wiki"^Int32.to_string i^"_container_adm"
let admin_group_name i = "wiki"^Int32.to_string i^"_admin"

let readers_group i = Users.get_user_id_by_name (readers_group_name i)
let writers_group i = Users.get_user_id_by_name (writers_group_name i)
let rights_adm_group  i = Users.get_user_id_by_name (rights_adm_group_name  i)
let page_creators_group i = 
                      Users.get_user_id_by_name (page_creators_group_name i)
let wikiboxes_creators_group i = 
                      Users.get_user_id_by_name
                        (wikiboxes_creators_group_name i)
let container_adm_group i =
                      Users.get_user_id_by_name (container_adm_group_name i)
let admin_group i =   Users.get_user_id_by_name (admin_group_name i)



let new_wikibox ~wiki ~author ~comment ~content
    ?readers ?writers ?rights_adm ?wikiboxes_creators () =
  (if wiki.boxrights
  then (
    (match readers with 
       | Some r -> Lwt.return r
       | None -> 
           readers_group wiki.id >>= fun r -> 
           Lwt.return [r]) >>= fun readers ->
    (match writers with 
       | Some r -> Lwt.return r
       | None -> 
           writers_group wiki.id >>= fun r -> 
           Lwt.return [r]) >>= fun writers ->
    (match rights_adm with 
       | Some r -> Lwt.return r
       | None -> 
           rights_adm_group wiki.id >>= fun r -> 
           Lwt.return [r]) >>= fun rights_adm ->
    (match wikiboxes_creators with 
       | Some r -> Lwt.return r
       | None -> 
           wikiboxes_creators_group wiki.id >>= fun r -> 
           Lwt.return [r]) >>= fun wikiboxes_creators ->
    Lwt.return (Some (readers, writers, rights_adm, wikiboxes_creators)))
  else Lwt.return None) >>= fun rights ->
    Wiki_sql.new_wikibox
      ~wiki:wiki.id
      ~author
      ~comment
      ~content
      ?rights
      ()
      

let create_group_ name fullname =
  Lwt.catch
    (fun () -> 
       Users.create_user 
         ~name
         ~pwd:None
         ~fullname
         ~email:None
         ~groups:[])
    (function
       | Users.UserExists u -> Lwt.return u
       | e -> Lwt.fail e)

let add_to_group_ l g =
  List.fold_left
    (fun beg u -> 
       beg >>= fun () ->
       Users.add_to_group ~user:u ~group:g)
    (Lwt.return ())
    l

let create_wiki ~title ~descr
    ?sp
    ?path
    ?(readers = [Users.anonymous.Users.id])
    ?(writers = [Users.authenticated_users.Users.id])
    ?(rights_adm = [])
    ?(wikiboxes_creators = [Users.authenticated_users.Users.id])
    ?(container_adm = [])
    ?(page_creators = [Users.authenticated_users.Users.id])
    ?(admins = [])
    ?(boxrights = true)
    ~wikibox
    () =
  Lwt.catch 
    (fun () -> get_wiki_by_name title)
    (function
       | Not_found -> 
           (Wiki_sql.new_wiki ~title ~descr ~boxrights ()
           >>= fun id -> 
             let w =
               { id = id; 
                 title = title; 
                 descr = descr;
                 path = path;
                 boxrights = boxrights
               }
             in
           
           (* Creating groups *)
           create_group_
             (readers_group_name id) 
             ("Users who can read wiki "^Int32.to_string id)
           >>= fun readers_data ->
           create_group_
             (writers_group_name id) 
             ("Users who can write in wiki "^Int32.to_string id)
           >>= fun writers_data ->
           create_group_
             (rights_adm_group_name id) 
             ("Users who can change rights in wiki "^Int32.to_string id)
           >>= fun rights_adm_data ->
           create_group_
             (page_creators_group_name id) 
             ("Users who can create pages in wiki "^Int32.to_string id)
           >>= fun page_creators_data ->
           create_group_
             (wikiboxes_creators_group_name id) 
             ("Users who can create wikiboxes in wiki "^Int32.to_string id)
           >>= fun wikiboxes_creators_data ->
           create_group_
             (container_adm_group_name id)
             ("Users who can change the layout of pages "^Int32.to_string id)
           >>= fun container_adm_data ->
           create_group_
             (admin_group_name id) 
             ("Wiki administrator "^Int32.to_string id)
           >>= fun admin_data ->

           (* Putting users in groups *)
           add_to_group_ [admin_data.Users.id] 
             wikiboxes_creators_data.Users.id
             >>= fun () ->
           add_to_group_ [admin_data.Users.id] 
             page_creators_data.Users.id
             >>= fun () ->
           add_to_group_ [admin_data.Users.id] 
             rights_adm_data.Users.id
             >>= fun () ->
           add_to_group_ [admin_data.Users.id] 
             container_adm_data.Users.id
             >>= fun () ->
           add_to_group_ [wikiboxes_creators_data.Users.id;
                          page_creators_data.Users.id;
                          rights_adm_data.Users.id;
                          container_adm_data.Users.id] 
             writers_data.Users.id
             >>= fun () ->
           add_to_group_ [writers_data.Users.id] readers_data.Users.id
             >>= fun () ->
           add_to_group_ readers readers_data.Users.id >>= fun () ->
           add_to_group_ writers writers_data.Users.id >>= fun () ->
           add_to_group_ rights_adm rights_adm_data.Users.id >>= fun () ->
           add_to_group_ page_creators page_creators_data.Users.id >>= fun () ->
           add_to_group_ wikiboxes_creators wikiboxes_creators_data.Users.id
           >>= fun () ->

           (* Filling the first wikibox with admin container *)
           new_wikibox 
             ~wiki:w
             ~author:Users.admin.Users.id
             ~comment:"Admin container" 
             ~content:"= Ocsimore administration\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"
             ~writers:[container_adm_data.Users.id]
             ()
           >>= fun _ ->

           (* Filling the first wikibox with wikipage container *)
           new_wikibox 
             ~wiki:w
             ~author:Users.admin.Users.id
             ~comment:"Wikipage" 
             ~content:"= Ocsimore wikipage\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"
             ~writers:[container_adm_data.Users.id]
             ()
           >>= fun _ ->

           Lwt.return w)

       | e -> Lwt.fail e)
  >>= fun w ->

  (* Registering the service with suffix for wikipages *)
  (match path with
     | None -> ()
     | Some path ->

         let action_create_page =
           Eliom_predefmod.Actions.register_new_post_service' 
             ~name:("wiki_page_create"^Int32.to_string w.id)
             ~post_params:(Eliom_parameters.string "page")
             (fun sp () page ->
                let sd = Ocsimore_common.get_sd sp in
                Users.get_user_id ~sp ~sd >>= fun id ->
                new_wikibox 
                  w
                  id
                  "new wikipage" 
                  "**//new wikipage//**"
(*VVV readers, writers, rights_adm, wikiboxes_creators? *)
                  () >>= fun box ->
                Wiki_cache.set_box_for_page ~wiki:w.id ~id:box ~page >>= fun () ->
                Lwt.return [])
         in
         ignore
           (Eliom_duce.Xhtml.register_new_service
              ~path
              ?sp
              ~get_params:(Eliom_parameters.suffix 
                             (Eliom_parameters.all_suffix_string "page"))
              (fun sp path () -> 
                 let sd = Ocsimore_common.get_sd sp in
                 Lwt.catch
                   (fun () ->
                      Wiki_cache.get_box_for_page w.id path >>= fun box ->
                      wikibox#editable_wikibox ~sp ~sd ~data:(w.id, box)
(*VVV it does not work if I do not put optional parameters !!?? *)
                        ?rows:None ?cols:None ?classe:None ?subbox:None
                        ~ancestors:Wiki_syntax.no_ancestors
                        () >>= fun subbox -> 
                      Lwt.return {{ [ subbox ] }}
                   )
                   (function
                      | Not_found -> 
                          Users.get_user_id ~sp ~sd >>= fun userid ->
                          let draw_form name =
                            {{ [<p>[
                                   {: Eliom_duce.Xhtml.string_input
                                      ~input_type:{: "hidden" :} 
                                      ~name
                                      ~value:path () :}
                                     {: Eliom_duce.Xhtml.string_input
                                        ~input_type:{: "submit" :} 
                                        ~value:"Create it!" () :}
                                 ]] }}

                          in
                          page_creators_group w.id >>= fun creators ->
                          Users.in_group userid creators
                          >>= fun c ->
                          let form =
                            if c
                            then
                              {{ [ {: Eliom_duce.Xhtml.post_form
                                      ~service:action_create_page
                                      ~sp draw_form () :} ] }}
                            else {{ [] }}
                          in
                          Lwt.return
                            {{ [ <p>"That page does not exist." !form ] }}
                      | e -> Lwt.fail e
                   )
               >>= fun subbox ->
                 
               wikibox#editable_wikibox ~sp ~sd ~data:(w.id, 2l)
                 ?rows:None ?cols:None ?classe:None
                 ?subbox:(Some subbox) ~ancestors:Wiki_syntax.no_ancestors
                 ()
               >>= fun page ->

               Lwt.return
                 {{
                    <html>[
                      <head>[
                        <title>"Ocsimore administration"
                          {: Eliom_duce.Xhtml.css_link 
                             (Eliom_duce.Xhtml.make_uri
                                (Eliom_services.static_dir sp) 
                                sp ["example.css"]) () :}
(*VVV quel css ? quel layout de page ? *)
                      ]
                                <body>[ page ]
                    ]
                  }}
              )
           )
  );
  Lwt.return w



let can_change_rights wiki id userid =
  if userid == Users.admin.Users.id
  then Lwt.return true
  else
    get_rights_adm ~wiki (wiki.id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left 
            (fun b a -> 
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group userid a)
            (Lwt.return false) 
            l
      | None -> rights_adm_group wiki.id >>= fun g -> Users.in_group userid g

let can_read wiki id userid =
  if userid = Users.admin.Users.id
  then Lwt.return true
  else
    get_readers ~wiki (wiki.id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left 
            (fun b a -> 
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group userid a)
            (Lwt.return false) 
            l
      | None -> readers_group wiki.id >>= fun g -> Users.in_group userid g
    
let can_write wiki id userid =
  if userid = Users.admin.Users.id
  then Lwt.return true
  else
    get_writers (wiki.id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left 
            (fun b a -> 
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group userid a)
            (Lwt.return false) 
            l
      | None -> writers_group wiki.id >>= fun g -> Users.in_group userid g
    
let can_create_wikibox wiki id userid =
  if userid == Users.admin.Users.id
  then Lwt.return true
  else
    get_wikiboxes_creators (wiki.id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left
            (fun b a -> 
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group userid a)
            (Lwt.return false) 
            l
      | None -> wikiboxes_creators_group wiki.id >>= fun g -> 
                Users.in_group userid g


let get_role_ ~sp ~sd ((wiki : Wiki_sql.wiki), id) =
  get_wiki_by_id wiki >>= fun w ->
  Users.get_user_data sp sd >>= fun u ->
  let u = u.Users.id in
  can_change_rights w id u >>= fun cana ->
  if cana
  then Lwt.return Admin
  else
    can_write w id u >>= fun canw ->
    if canw
    then Lwt.return Author
    else 
      can_read w id u >>= fun canr ->
      if canr
      then Lwt.return Lurker
      else Lwt.return Nonauthorized




(** {2 Session data} *)

module Roles = Map.Make(struct
                          type t = int32 * int32
                          let compare = compare
                        end)

type wiki_sd = 
    {
      role : (int32 * int32) -> role Lwt.t;
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
  {role = cache_find cache (get_role_ ~sp ~sd);
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
  wiki_sd.role k




(** {2 } *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (int32 * int32)
  | Edit_perm of (int32 * int32)
  | Preview of ((int32 * int32) * string)
  | History of ((int32 * int32) * (int option * int option))
  | Oldversion of ((int32 * int32) * int32)
  | Src of ((int32 * int32) * int32)
  | Error of ((int32 * int32) * wiki_errors)

exception Wiki_action_info of wiki_action_info

let save_wikibox ~sp ~sd (((wiki_id, box_id) as d), content) =
  get_role sp sd d >>= fun role ->
  match role with
    | Admin
    | Author ->
        Lwt.catch
          (fun () ->
              Users.get_user_data sp sd >>= fun user ->
              Wiki_cache.update_wikibox
                wiki_id box_id
                user.Users.id
                "" content >>= fun _ ->
              Lwt.return [Ocsimore_common.Session_data sd])
          (fun e -> 
             Lwt.return 
               [Ocsimore_common.Session_data sd;
                Wiki_action_info (Error (d, Action_failed e))])
    | _ -> Lwt.return [Ocsimore_common.Session_data sd;
                       Wiki_action_info (Error (d, Operation_not_allowed))]


let save_wikibox_permissions ~sp ~sd (((wiki_id, box_id) as d), rights) =
  get_role sp sd d >>= fun role ->
  (match role with
    | Admin ->
        let (addr, (addw, (adda, (addc, 
                                  (delr, (delw, (dela, delc))))))) = rights in
        let f a =
          Lwt.catch
            (fun () -> 
               Users.get_user_id_by_name a >>= fun v -> 
               Lwt.return (Some v))
            (function 
               | Users.NoSuchUser _ -> Lwt.return None
               | e -> Lwt.fail e)
        in
        let r = Ocsigen_lib.split ' ' addr in
        Lwt_util.map f r 
        >>= fun readers ->
        Wiki_cache.populate_readers wiki_id box_id readers
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' addw in
        Lwt_util.map f r 
        >>= fun w ->
        Wiki_cache.populate_writers wiki_id box_id w
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' adda in
        Lwt_util.map f r 
        >>= fun a ->
        Wiki_cache.populate_rights_adm wiki_id box_id a
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' addc in
        Lwt_util.map f r 
        >>= fun a ->
        Wiki_cache.populate_wikiboxes_creators wiki_id box_id a
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' delr in
        Lwt_util.map f r 
        >>= fun readers ->
        Wiki_cache.remove_readers wiki_id box_id readers
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' delw in
        Lwt_util.map f r 
        >>= fun w ->
        Wiki_cache.remove_writers wiki_id box_id w
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' dela in
        Lwt_util.map f r 
        >>= fun a ->
        Wiki_cache.remove_rights_adm wiki_id box_id a
        >>= fun () ->
        let r = Ocsigen_lib.split ' ' delc in
        Lwt_util.map f r 
        >>= fun a ->
        Wiki_cache.remove_wikiboxes_creators wiki_id box_id a
    | _ -> Lwt.return ()) >>= fun () ->
(*  Lwt.return [Ocsimore_common.Session_data sd] NO! We want a new sd, or at least, remove role *)
  Lwt.return []
