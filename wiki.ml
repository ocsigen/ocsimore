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
  default_reader: Users.group;
  default_writer: Users.group;
  default_admin: Users.group option; (** the (default) group of users
                                         who can change rights for boxes
                                         if acl enabled. 
                                         Putting something here means that
                                         acl are enabled.
                                     *)
}

module H = Hashtbl.Make(struct
                          type t = int32
                          let equal = (=)
                          let hash = Hashtbl.hash 
                        end)

let wiki_info_table = H.create 8

let find_wiki id =
  try
    Lwt.return (H.find wiki_info_table id)
  with Not_found -> Wiki_sql.find_wiki ~id ()

let get_wiki_by_id id =
  find_wiki id
  >>= fun (id, title, descr,path, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               path = path;
               default_reader = r;
               default_writer = w; 
               default_admin = a;
             }

let get_wiki_by_name title =
  Wiki_sql.find_wiki ~title () >>= fun (id, title, descr, path, r, w, a) -> 
  Lwt.return { id = id; 
               title = title; 
               descr = descr;
               path = path;
               default_reader = r;
               default_writer = w; 
               default_admin = a
             }

let new_wikibox ~wiki ~author ~comment ~content =
  fun
    ?(readers = [wiki.default_reader]) 
    ?(writers = [wiki.default_writer]) 
    ?admins 
    () ->
  Wiki_sql.new_wikibox
    ~wiki:wiki.id
    ~author
    ~comment
    ~content
    ?rights:(match wiki.default_admin with
               | Some a -> Some (readers, 
                                 writers, 
                                 match admins with
                                   | None -> [a]
                                   | Some a -> a)
               | None -> None)
    ()


let create_wiki ~title ~descr
    ?sp
    ?path
    ?(reader = Users.anonymous_group)
    ?(writer = Users.anonymous_group)
    ?admin
    ~wikibox
    () =
  Lwt.catch 
    (fun () -> get_wiki_by_name title)
    (function
       | Not_found -> 
           (Wiki_sql.new_wiki ~title ~descr ~reader ~writer ?admin ()
           >>= fun id -> 
             let w =
               { id = id; 
                 title = title; 
                 descr = descr;
                 path = path;
                 default_reader = reader;
                 default_writer = writer; 
                 default_admin = admin;
               }
             in
           
           (* Filling the first wikibox with admin container *)
           new_wikibox 
             w
             "admin"
             "Admin container" 
             "= Ocsimore administration\r\n<<content>>"
             ()
           >>= fun _ ->

           (* Filling the first wikibox with wikipage container *)
           new_wikibox 
             w
             "admin"
             "Wikipage" 
             "= Ocsimore wikipage\r\n<<content>>"
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
                Users.get_user_name ~sp ~sd >>= fun name ->
                new_wikibox 
                  w
                  name
                  "new wikipage" 
                  "**//new wikipage//**"
(*VVV readers, writers, admins? *)
                  () >>= fun box ->
                Wiki_sql.set_box_for_page ~wiki:w.id ~id:box ~page >>= fun () ->
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
                      Wiki_sql.get_box_for_page w.id path >>= fun box ->
                      wikibox#editable_wikibox ~sp ~sd ~data:(w.id, box)
(*VVV it does not work if I do not put optional parameters !!?? *)
                        ?rows:None ?cols:None ?classe:None ?subbox:None
                        () >>= fun subbox -> 
                      Lwt.return {{ [ subbox ] }}
                   )
                   (function
                      | Not_found -> 
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
                          let form =
                            Eliom_duce.Xhtml.post_form
                              ~service:action_create_page
                              ~sp draw_form ()
                          in
                          Lwt.return
                            {{ [ <p>"That page does not exist." form ] }}
                      | e -> Lwt.fail e
                   )
               >>= fun subbox ->
                 
               wikibox#editable_wikibox ~sp ~sd ~data:(w.id, 2l)
                 ?rows:None ?cols:None ?classe:None
                 ?subbox:(Some subbox)
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



let can_admin get_admins wiki id user =
  if user == Users.anonymous
  then Lwt.return false
  else if user == Users.admin
  then Lwt.return true
  else
    match wiki.default_admin with
      | Some _ -> (* acl are activated *)
          (get_admins (wiki.id, id) >>= fun l ->
          Lwt.return (List.exists (fun a -> Users.in_group user a) l))
            (* was: Users.in_group user admin *)
      | None -> Lwt.return false

let can_read get_readers wiki id user =
  if user == Users.admin
  then Lwt.return true
  else
    match wiki.default_admin with
      | Some admin -> (* acl are activated *)
          get_readers (wiki.id, id) >>= fun l ->
          Lwt.return (List.exists (fun a -> Users.in_group user a) l)
      | None -> (* acl are not activated *)
          Lwt.return (Users.in_group user wiki.default_reader)
    
let can_write get_writers wiki id user =
  if user == Users.admin
  then Lwt.return true
  else
    match wiki.default_admin with
      | Some admin -> (* acl are activated *)
          get_writers (wiki.id, id) >>= fun l ->
          Lwt.return (List.exists (fun a -> Users.in_group user a) l)
      | None -> (* acl are not activated *)
          Lwt.return (Users.in_group user wiki.default_writer)
    

let get_role_ readers writers admins ~sp ~sd ((wiki : Wiki_sql.wiki), id) =
  get_wiki_by_id wiki >>= fun w ->
  Users.get_user_data sp sd >>= fun u ->
  can_admin admins w id u >>= fun cana ->
  if cana
  then Lwt.return Admin
  else
    can_write writers w id u >>= fun canw ->
    if canw
    then Lwt.return Author
    else 
      can_read readers w id u >>= fun canr ->
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
      readers : (int32 * int32) -> Users.group list Lwt.t;
      writers : (int32 * int32) -> Users.group list Lwt.t;
      admins : (int32 * int32) -> Users.group list Lwt.t;
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
  let readers = ref Roles.empty in
  let writers = ref Roles.empty in
  let admins = ref Roles.empty in
  (* We cache the values to retrieve them only once *)
  let readers = cache_find readers Wiki_sql.get_readers in
  let writers = cache_find writers Wiki_sql.get_writers in
  let admins = cache_find admins Wiki_sql.get_admins in
  {role = cache_find cache (get_role_ readers writers admins ~sp ~sd);
   readers = readers;
   writers = writers;
   admins = admins;
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

let get_readers ~sp ~sd k =
  let wiki_sd = get_wiki_sd ~sp ~sd in
  wiki_sd.readers k

let get_writers ~sp ~sd k =
  let wiki_sd = get_wiki_sd ~sp ~sd in
  wiki_sd.writers k

let get_admins ~sp ~sd k =
  let wiki_sd = get_wiki_sd ~sp ~sd in
  wiki_sd.admins k



(** {2 } *)
type wiki_errors =
  | Action_failed of exn
  | Operation_not_allowed

type wiki_action_info =
  | Edit_box of (int32 * int32)
  | Preview of (((int32 * int32) * string) * 
                  (string option * 
                     (string option * 
                        (string option * 
                           (string option * (string option * string option))))))
  | History of ((int32 * int32) * (int option * int option))
  | Oldversion of ((int32 * int32) * int32)
  | Src of ((int32 * int32) * int32)
  | Error of ((int32 * int32) * wiki_errors)

exception Wiki_action_info of wiki_action_info

let save_wikibox ~sp ~sd ((((wiki_id, box_id) as d), content), 
                          (addr, (addw, (adda, (delr, (delw, dela)))))) =
  get_role sp sd d >>= fun role ->
  (match role with
    | Admin
    | Author ->
        Lwt.catch
          (fun () ->
              Users.get_user_data sp sd >>= fun user ->
              Wiki_sql.update_wikibox
               wiki_id box_id
               user.Users.name
               "" content () >>= fun _ ->
                 Lwt.return [])
          (fun e -> 
             Lwt.return 
               [Ocsimore_common.Session_data sd;
                Wiki_action_info (Error (d, Action_failed e))])
    | _ -> Lwt.return [Ocsimore_common.Session_data sd;
                       Wiki_action_info (Error (d, Operation_not_allowed))])
    >>= fun r ->
  (match role with
    | Admin ->
        (match addr with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let readers = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_readers wiki_id box_id readers)
          >>= fun () ->
        (match addw with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let w = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_writers wiki_id box_id w)
          >>= fun () ->
        (match adda with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let a = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.populate_wbadmins wiki_id box_id a)
          >>= fun () ->
        (match delr with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let readers = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_readers wiki_id box_id readers)
          >>= fun () ->
        (match delw with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let w = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_writers wiki_id box_id w)
          >>= fun () ->
        (match dela with
          | None | Some "" -> Lwt.return ()
          | Some s -> 
              let r = Ocsigen_lib.split ' ' s in
              let a = List.map (fun x -> Users.get_group x) r in
              Wiki_sql.remove_wbadmins wiki_id box_id a)
    | _ -> Lwt.return ()) >>= fun () ->
  Lwt.return r
