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

open Wiki_sql


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


let get_sthg_ f ((w, _) as k) =
  get_wiki_by_id w
  >>= fun wiki_info ->
  if wiki_info.boxrights
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


let readers_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_readers"
let writers_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_writers"
let rights_adm_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_rights_givers"
let page_creators_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_page_creators"
let css_editors_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_css_editors"
let wikiboxes_creators_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_wikiboxes_creators"
let container_adm_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_container_adm"
let admin_group_name i = "wiki"^Wiki_sql.wiki_id_s i^"_admin"

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

let new_wikibox ?boxid ~wiki ~author ~comment ~content ~content_type ?readers ?writers ?rights_adm ?wikiboxes_creators () =
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
    Lwt.return
      (Some (readers, writers, rights_adm, wikiboxes_creators)))
  else Lwt.return None) >>= fun rights ->
  Lwt.catch
    (fun () ->
       (match boxid with
          | None -> Lwt.return ()
          | Some b -> 
             Wiki_sql.get_wikibox_data
               ~wikibox:(wiki.id, b) () >>= 
             function 
               | None -> Lwt.return ()
               | _ -> Lwt.fail (Found b))
(*VVV may create holes in the sequence of wikibox numbers *)
       >>= fun () ->
       Wiki_sql.new_wikibox
         ~wiki:wiki.id
         ?box:boxid
         ~author
         ~comment
         ~content
         ~content_type
         ?rights
         ())
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
    rights_box (wiki.id, id) >>= function
      | Some l -> (* acl are activated *)
          List.fold_left
            (fun b a ->
               b >>= fun b ->
               if b then Lwt.return true
               else Users.in_group ~sp ~sd ~user:userid ~group:a ())
            (Lwt.return false)
            l
      | None ->
          rights_wiki wiki.id >>= fun g ->
          Users.in_group ~sp ~sd ~user:userid ~group:g ()


let can_change_rights = can_sthg get_rights_adm rights_adm_group
let can_read = can_sthg get_readers readers_group
let can_write = can_sthg get_writers writers_group
let can_create_wikibox = can_sthg get_wikiboxes_creators wikiboxes_creators_group

let get_role_ ~sp ~sd ((wiki : Wiki_sql.wiki), id) =
  get_wiki_by_id wiki >>= fun w ->
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




(** {2 Session data} *)

module Roles = Map.Make(struct
                          type t = Wiki_sql.wiki * int32
                          let compare = compare
                        end)

type wiki_sd = 
    {
      role : (Wiki_sql.wiki * int32) -> role Lwt.t;
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
let send_static_file sp sd wiki dir page =
  readers_group wiki.id >>= fun g -> 
  Users.get_user_id ~sp ~sd >>= fun userid ->
  Users.in_group ~sp ~sd ~user:userid ~group:g () >>= fun b ->
  if b
  then Eliom_predefmod.Files.send ~sp (dir^"/"^page)
  else Lwt.fail Eliom_common.Eliom_404



(* Displaying of an entire page. We essentially render the page,
   and then include it inside its container *)
let display_page w (wikibox : Wiki_widgets.editable_wikibox) 
    action_create_page sp path page () =
  let sd = Ocsimore_common.get_sd sp in
  (* if there is a static page, we serve it: *)
  Lwt.catch
    (fun () ->
       match w.staticdir with
         | Some d -> send_static_file sp sd w d page
         | None -> Lwt.fail Eliom_common.Eliom_404)
    (function
       | Eliom_common.Eliom_404 ->
           (* otherwise, we serve the wiki page: *)
           Lwt.catch
             (fun () ->
                Wiki_sql.get_box_for_page w.id page
                >>= fun { Wiki_sql.wikipage_dest_wiki = wiki';
                          wikipage_wikibox = box; wikipage_title = title } ->
                  let bi =
                    { Wiki_syntax.bi_sp = sp;
                      Wiki_syntax.bi_sd = sd;
                      Wiki_syntax.bi_ancestors = Wiki_syntax.no_ancestors;
                      Wiki_syntax.bi_subbox = None;
                      Wiki_syntax.bi_page = Some path;
                    }
                  in
                wikibox#editable_wikibox_allowed ~bi ~data:(wiki', box)
                  ~cssmenu:(Some page) ()
                >>= fun (subbox, allowed) ->
                Lwt.return ({{ [ subbox ] }},
                            (if allowed then
                               Wiki_syntax.Page_displayable
                             else
                               Wiki_syntax.Page_403),
                            title)
             )
             (function
                | Not_found ->
                    Users.get_user_id ~sp ~sd
                    >>= fun userid ->
                    let draw_form name =
                      {{ [<p>[
                         {: Eliom_duce.Xhtml.string_input ~name
                            ~input_type:{: "hidden" :} ~value:page () :}
                         {: Eliom_duce.Xhtml.string_input
                            ~input_type:{: "submit" :} ~value:"Create it!" () :}
                           ]] }}
                    in
                    page_creators_group w.id
                    >>= fun creators ->
                    Users.in_group ~sp ~sd ~user:userid ~group:creators ()
                    >>= fun c ->
                      let form =
                        if c then
                          {{ [ {: Eliom_duce.Xhtml.post_form
                                  ~service:action_create_page
                                  ~sp draw_form () :} ] }}
                        else {{ [] }}
                      and err_msg =
                        !Language.messages.Language.page_does_not_exist
                      in
                      Lwt.return
                        ({{ [ <p>{:err_msg:} !form ] }},
                         Wiki_syntax.Page_404,
                         None)
                | e -> Lwt.fail e
             )
           >>= fun (subbox, err_code, title) ->
           Wiki_syntax.set_page_displayable sd err_code;
           let bi = { Wiki_syntax.bi_sp = sp;
                      Wiki_syntax.bi_sd = sd;
                      Wiki_syntax.bi_ancestors = Wiki_syntax.no_ancestors;
                      Wiki_syntax.bi_subbox = Some subbox;
                      Wiki_syntax.bi_page = Some path;
                    }
           in
           wikibox#editable_wikibox 
             ~bi
             ~data:(w.id, w.container_id)
             ~cssmenu:None
             ()

           >>= fun pagecontent ->
           wikibox#get_css_header ~bi ~wiki:w.id ~admin:false ~page ()

           >>= fun css ->
           let title = Ocamlduce.Utf8.make
             (match title with
                | Some title -> title
                | None -> w.descr)
           and code = match err_code with
                      | Wiki_syntax.Page_displayable -> 200
                      | Wiki_syntax.Page_404 -> 404
                      | Wiki_syntax.Page_403 -> 403
           in
           Eliom_duce.Xhtml.send ~sp ~code
             {{
                <html xmlns="http://www.w3.org/1999/xhtml">[
                  <head>[
                    <title>title
                      !css
    (*VVV quel titre ? quel layout de page ? *)
                   ]
                  <body>[ pagecontent ]
                ]
              }}
  | e -> Lwt.fail e)










let wikicss_service_handler wiki () =
  Lwt.catch
    (fun () -> Wiki_sql.get_css_for_wiki wiki)
    (function
       | Not_found -> Lwt.fail Eliom_common.Eliom_404
       | e -> Lwt.fail e
    )

(** Functions related to the creation of a wiki *)
(* Register the wiki [wiki] *)
let register_wiki ?sp ~path ~(wikibox : Wiki_widgets.editable_wikibox) ~wiki ?wiki_info () =
  (match wiki_info with
    | None -> get_wiki_by_id wiki
    | Some info -> Lwt.return info
  ) >>= fun wiki_info ->
  let action_create_page =
    Eliom_predefmod.Actions.register_new_post_coservice' ?sp
      ~name:("wiki_page_create"^Wiki_sql.wiki_id_s wiki)
      ~post_params:(Eliom_parameters.string "page")
      (fun sp () page ->
         let sd = Ocsimore_common.get_sd sp in
         Users.get_user_id ~sp ~sd
         >>= fun userid ->
           page_creators_group wiki
         >>= fun creators ->
           Users.in_group ~sp ~sd ~user:userid ~group:creators ()
         >>= fun c ->
           if c then
             Lwt.catch
               (fun () ->
                  Wiki_sql.get_box_for_page wiki page
                  >>= fun _ ->
                    (* The page already exists *)
                    Lwt.return [Ocsimore_common.Session_data sd]
                      (*VVV Put an error message *)
               )
               (function
                  | Not_found ->
                      new_wikibox ~wiki:wiki_info ~author:userid
                        ~comment:"new wikipage"
                        ~content:("=="^page^"==") ~content_type:Wiki_sql.Wiki ()
                        (*VVV readers, writers, rights_adm, wikiboxes_creators? *)
                      >>= fun box ->
                        Wiki_sql.set_box_for_page
                          ~sourcewiki:wiki ~wikibox:box ~page ()
                      >>= fun () ->
                        Lwt.return [Ocsimore_common.Session_data sd]
                  | e -> Lwt.fail e)
           else Lwt.return [Ocsimore_common.Session_data sd]
             (*VVV Put an error message *)
      )
  in

  (* Registering the service with suffix for wikipages *)
  (* Note that Eliom will look for the service corresponding to
     the longest prefix. Thus it is possible to register a wiki
     at URL / and another one at URL /wiki and it works,
     whatever be the order of registration *)
  let servpage =
    Eliom_predefmod.Any.register_new_service ~path ?sp
      ~get_params:(Eliom_parameters.suffix (Eliom_parameters.all_suffix "page"))
      (fun sp path () ->
         display_page wiki_info wikibox action_create_page sp
           path (Ocsigen_lib.string_of_url_path ~encode:true path) ())
  in
  Wiki_syntax.add_servpage wiki servpage;

  (* the same, but non attached: *)
  let naservpage =
    Eliom_predefmod.Any.register_new_coservice' ?sp
      ~name:("display"^Wiki_sql.wiki_id_s wiki)
      ~get_params:(Eliom_parameters.string "page")
      (fun sp page () ->
         let path =
           Ocsigen_lib.remove_slash_at_beginning
             (Ocsigen_lib.remove_dotdot (Neturl.split_path page))
         in
         let page =
           Ocsigen_lib.string_of_url_path
             ~encode:true
             path
         in
         display_page 
           wiki_info wikibox action_create_page sp path page ())
  in
  Wiki_syntax.add_naservpage wiki naservpage;

  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service ?sp
      ~path:(path@["__ocsiwikicss"])
      ~get_params:Eliom_parameters.unit
      (fun _sp () () -> wikicss_service_handler wiki ())
  in
  Wiki_syntax.add_servwikicss wiki wikicss_service;

  Lwt.return ()


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
    ~container_page
    () =
  let path_string = Ocsimore_lib.bind_opt
    path (Ocsigen_lib.string_of_url_path ~encode:true)
  in
  (* Notice that there is a theoretical race condition in the code below,
     when the container wikibox receives its rights, in the case this
     container has changed between the creation of the wiki and the moments
     the rights are added *)
  Wiki_sql.new_wiki ~title ~descr ~pages:path_string
     ~boxrights ?staticdir ~container_page ()
   >>= fun (wiki_id, wikibox_container) ->

   (* Creating groups *)
   create_group_ (readers_group_name wiki_id)
     ("Users who can read wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun readers_data ->
     create_group_ (writers_group_name wiki_id)
       ("Users who can write in wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun writers_data ->
     create_group_ (rights_adm_group_name wiki_id)
       ("Users who can change rights in wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun rights_adm_data ->
     create_group_ (page_creators_group_name wiki_id)
       ("Users who can create pages in wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun page_creators_data ->
     create_group_ (css_editors_group_name wiki_id)
       ("Users who can edit css for wikipages of wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun css_editors_data ->
       create_group_ (wikiboxes_creators_group_name wiki_id)
         ("Users who can create wikiboxes in wiki "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun wikiboxes_creators_data ->
     create_group_ (container_adm_group_name wiki_id)
       ("Users who can change the layout of pages "^Wiki_sql.wiki_id_s wiki_id)
   >>= fun container_adm_data ->
     create_group_ (admin_group_name wiki_id)
       ("Wiki administrator "^Wiki_sql.wiki_id_s wiki_id)
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

   Wiki_sql.populate_readers wiki_id wikibox_container
     [readers_data.Users.id] >>= fun () ->
   Wiki_sql.populate_writers wiki_id wikibox_container
     [container_adm_data.Users.id] >>= fun () ->
   Wiki_sql.populate_rights_adm wiki_id wikibox_container
     [rights_adm_data.Users.id] >>= fun () ->
   Wiki_sql.populate_wikiboxes_creators wiki_id wikibox_container
     [wikiboxes_creators_data.Users.id] >>= fun () ->

   Lwt.return wiki_id


let default_container_page =
  "= Ocsimore wikipage\r\n\r\n<<loginbox>>\r\n\r\n<<content>>"


(* Creates and registers a wiki if it does not already exists  *)
let create_wiki ~title ~descr
    ?sp
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
    ~wikibox
    ~container_page
    () =
  Lwt.catch
    (fun () -> get_wiki_by_name title)
    (function
       | Not_found ->
           begin
             really_create_wiki ~title ~descr ?path ~readers ~writers
               ~rights_adm ~wikiboxes_creators ~container_adm ~page_creators
               ~css_editors ~admins ~boxrights ?staticdir ~container_page ()
             >>= fun wiki_id ->
             get_wiki_by_id wiki_id
             >>= fun w ->
             match path with
               | None -> Lwt.return w
               | Some path ->
                   register_wiki ?sp ~path ~wikibox ~wiki:w.id ~wiki_info:w ()
             >>= fun () ->
             Lwt.return w
           end
       | e -> Lwt.fail e)



(** {2 } *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (Wiki_sql.wiki * int32)
  | Edit_perm of (Wiki_sql.wiki * int32)
  | Preview of ((Wiki_sql.wiki * int32) * (string * int32)) (* The second uple is the content of the wikibox, and the version number of the wikibox *when the edition started* *)
  | History of (Wiki_sql.wiki * int32)
  | Oldversion of ((Wiki_sql.wiki * int32) * int32)
  | Src of ((Wiki_sql.wiki * int32) * int32)
  | Error of ((Wiki_sql.wiki * int32) * wiki_errors)
  | Delete_Box of (Wiki_sql.wiki * int32)

exception Wiki_action_info of wiki_action_info

let save_wikibox ~sp ~sd ~wiki_id ~box_id ~content ~content_type =
  let d = (wiki_id, box_id) in
  get_role sp sd d >>= fun role ->
  match role with
    | Admin
    | Author ->
        Lwt.catch
          (fun () ->
              Users.get_user_data sp sd >>= fun user ->
              Wiki_sql.update_wikibox ~wiki:wiki_id ~wikibox:box_id
                ~author:user.Users.id ~comment:"" ~content ~content_type
              >>= fun _ ->
                (* I'm using a redirection to prevent repost *)
              Eliom_predefmod.Redirection.send ~sp
                Eliom_services.void_coservice'
              (*was: Lwt.return [Ocsimore_common.Session_data sd] *))
          (fun e -> 
             Eliom_predefmod.Action.send ~sp
               [Ocsimore_common.Session_data sd;
                Wiki_action_info (Error (d, Action_failed e))])
    | _ -> 
        Eliom_predefmod.Action.send ~sp
          [Ocsimore_common.Session_data sd;
           Wiki_action_info (Error (d, Operation_not_allowed))]


let save_wikibox_permissions ~sp ~sd (((wiki_id, box_id) as d), rights) =
  get_role sp sd d >>= fun role ->
  match role with
    | Admin ->
        let (addr, (addw, (adda, (addc, 
                                  (delr, (delw, (dela, delc))))))) = rights in
        Users.group_list_of_string addr >>= fun readers ->
        Wiki_sql.populate_readers wiki_id box_id readers >>= fun () ->
        Users.group_list_of_string addw >>= fun w ->
        Wiki_sql.populate_writers wiki_id box_id w >>= fun () ->
        Users.group_list_of_string adda >>= fun a ->
        Wiki_sql.populate_rights_adm wiki_id box_id a >>= fun () ->
        Users.group_list_of_string addc >>= fun a ->
        Wiki_sql.populate_wikiboxes_creators wiki_id box_id a >>= fun () ->
        Users.group_list_of_string delr >>= fun readers ->
        Wiki_sql.remove_readers wiki_id box_id readers >>= fun () ->
        Users.group_list_of_string delw >>= fun w ->
        Wiki_sql.remove_writers wiki_id box_id w >>= fun () ->
        Users.group_list_of_string dela >>= fun a ->
        Wiki_sql.remove_rights_adm wiki_id box_id a >>= fun () ->
        Users.group_list_of_string delc >>= fun a ->
        Wiki_sql.remove_wikiboxes_creators wiki_id box_id a
    | _ -> Lwt.return ()



(* Information related to the administration wiki; caution when
   changing this. See mli *)
let wiki_admin_name = "Adminwiki"
let get_admin_wiki () =
  get_wiki_by_name wiki_admin_name
  >>= fun wiki -> Lwt.return wiki.id
