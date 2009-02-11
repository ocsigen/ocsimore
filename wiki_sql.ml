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

open Opaque

type wiki = [`Wiki] int32_t
let wiki_from_sql (i : int32) = (int32_t i : wiki)
let sql_from_wiki (i : wiki) = t_int32 i
let wiki_id_s i = Int32.to_string (sql_from_wiki i)
let s_wiki_id s = (Opaque.int32_t (Int32.of_string s) : wiki)

let eliom_wiki = Eliom_parameters.user_type
  (fun s -> (int32_t (Int32.of_string s) : wiki))
  wiki_id_s

open Lwt
open Sql.PGOCaml
open Ocsimore_lib
open CalendarLib
open Sql


let new_wiki ~title ~descr ~pages ~boxrights ~staticdir ~container_page () =
  let container_wikibox = 0l in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "INSERT INTO wikis (title, descr, pages, boxrights, container_id, staticdir)
             VALUES ($title, $descr, $?pages, $boxrights, 0, $?staticdir);
                  "
     >>= fun () ->
     serial4 db "wikis_id_seq"
     >>= fun wiki_id ->
     let comment = Printf.sprintf "Container box for wiki %ld" wiki_id in
     PGSQL(db) "INSERT INTO wikiboxindex (wiki_id, id, comment)
                VALUES ($wiki_id, $container_wikibox, $comment)"
     >>= fun () ->
     let admin = Users.admin.Users.id in
     PGSQL(db) "INSERT INTO wikiboxes (id, wiki_id, author, content)
                VALUES (0, $wiki_id, $admin, $container_page)"
     >>= fun () ->
       return (wiki_from_sql wiki_id, container_wikibox)
    )


(** Update container_id (for now). *)
let update_wiki_ ~wiki ~container_id () =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "UPDATE wikis SET container_id = $container_id \
                  WHERE id = $wiki")



let populate_readers_ db wiki id readers =
  let wiki = t_int32 (wiki : wiki) in
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxreaders \
                             VALUES ($wiki, $id, $reader)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox readers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          readers

let populate_writers_ db wiki id writers =
  let wiki = t_int32 (wiki : wiki) in
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxwriters \
                             VALUES ($wiki, $id, $writer)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox writers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          writers

let populate_rights_adm_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxrightsgivers \
                             VALUES ($wiki, $id, $ra)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox rights givers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let populate_wikiboxes_creators_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "INSERT INTO wikiboxcreators \
                             VALUES ($wiki, $id, $ra)")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while setting wikibox creators: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let remove_readers_ db wiki id readers =
  let wiki = t_int32 (wiki : wiki) in
  match readers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun reader ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxreaders \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND reader = $reader")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox readers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          readers

let remove_writers_ db wiki id writers =
  let wiki = t_int32 (wiki : wiki) in
  match writers with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun writer ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxwriters \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND writer = $writer")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox writers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          writers

let remove_rights_adm_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxrightsgivers \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND wbadmin = $ra")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox rights givers: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

let remove_wikiboxes_creators_ db wiki id ra =
  let wiki = t_int32 (wiki : wiki) in
  match ra with
    | [] -> Lwt.return ()
    | _ ->
(*VVV Can we do this more efficiently? *)
        Lwt_util.iter_serial
          (fun ra ->
             Lwt.catch
               (fun () ->
                  PGSQL(db) "DELETE FROM wikiboxcreators \
                             WHERE wiki_id = $wiki \
                             AND id = $id \
                             AND creator = $ra")
               (function
                  | Sql.PGOCaml.PostgreSQL_Error (s, _) ->
                      Ocsigen_messages.warning 
                        ("Ocsimore: while removing wikibox creators: "^s);
                      Lwt.return ()
                  | e -> Lwt.fail e
               )
          )
          ra

type wikibox_content_type =
  | Css
  | Wiki
  | Deleted

exception IncorrectWikiboxContentType of string

let wikibox_content_type_of_string = function
  | "wiki" -> Wiki
  | "css" -> Css
  | "deleted" -> Deleted
  | s -> raise (IncorrectWikiboxContentType s)

let string_of_wikibox_content_type = function
  | Wiki -> "wiki"
  | Css -> "css"
  | Deleted -> "deleted"


(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox ~wiki ?box ~author ~comment ~content ~content_type ?rights () = 
  let wiki' = t_int32 (wiki : wiki)
  and content_type = string_of_wikibox_content_type content_type in
  Sql.full_transaction_block
    (fun db ->
       (match box with
          | None ->
              PGSQL(db)
                "SELECT max(id) FROM wikiboxes 
                 WHERE wiki_id = $wiki'" >>= fun last ->
              let boxid = match last with
                | [] | None::_ -> 1l
                | (Some last)::_ -> Int32.add last 1l
              in
              PGSQL(db) "INSERT INTO wikiboxindex (wiki_id, id)
                         VALUES ($wiki', $boxid)";
              >>= fun () ->
              PGSQL(db) "INSERT INTO wikiboxes \
                          (id, wiki_id, author, comment, content, content_type) \
                         VALUES \ 
                          ($boxid, $wiki', $author, $comment, $content, $content_type)"
              >>= fun () ->
              Lwt.return boxid
          | Some box ->
              PGSQL(db) "INSERT INTO wikiboxindex (wiki_id, id)
                         VALUES ($wiki', $box)"
              >>= fun () ->
              PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content, content_type) \
                  VALUES ($box, $wiki', $author, $comment, $content, $content_type)"
              >>= fun () ->
              Lwt.return box) >>= fun wbx_id ->
       (match rights with
         | None -> Lwt.return ()
         | Some (r, w, ra, wc) -> 
             populate_writers_ db wiki wbx_id w >>= fun () ->
             populate_readers_ db wiki wbx_id r >>= fun () ->
             populate_rights_adm_ db wiki wbx_id ra >>= fun () ->
             populate_wikiboxes_creators_ db wiki wbx_id wc
       ) >>= fun () ->
      Lwt.return wbx_id)

(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
let update_wikibox_ ~wiki ~wikibox ~author ~comment ~content ~content_type =
  let wiki = t_int32 (wiki : wiki)
  and content_type = string_of_wikibox_content_type content_type in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content, content_type) \
                  VALUES ($wikibox, $wiki, $author, \
                          $comment, $content, $content_type)" >>= fun () ->
       serial4 db "wikiboxes_version_seq")


(*
       (match readers with
         | None -> Lwt.return ()
         | Some r -> 
             PGSQL(db) "DELETE FROM wikiboxreaders \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_readers_ db wiki wikibox r) >>= fun () ->
       (match writers with
         | None -> Lwt.return ()
         | Some w -> 
             PGSQL(db) "DELETE FROM wikiboxwriters \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_writers_ db wiki wikibox w) >>= fun () ->
       (match rights_adm with
         | None -> Lwt.return ()
         | Some a -> 
             PGSQL(db) "DELETE FROM wikiboxrightsgivers \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_rights_adm_ db wiki wikibox a) >>= fun () ->
       (match wikiboxes_creators with
         | None -> Lwt.return ()
         | Some a -> 
             PGSQL(db) "DELETE FROM wikiboxcreators \
                        WHERE wiki_id = $wiki AND id = $wikibox" >>= fun () ->
             populate_wikiboxes_creators_ db wiki wikibox a) >>= fun () ->
*)


(** returns subject, text, author, datetime of a wikibox; 
    None if non-existant *)
let get_wikibox_data_ ?version ~wikibox:(wiki, id) () =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       (match version with
         | None ->
             PGSQL(db) "SELECT comment, author, content, datetime, content_type, version \
                        FROM wikiboxes \
                        WHERE wiki_id = $wiki \
                        AND id = $id \
                        AND version = \
                           (SELECT max(version) \
                            FROM wikiboxes \
                            WHERE wiki_id = $wiki \
                            AND id = $id)"
         | Some version ->
             PGSQL(db) "SELECT comment, author, content, datetime, content_type, version \
                        FROM wikiboxes \
                        WHERE wiki_id = $wiki \
                        AND id = $id \
                        AND version = $version")
       >>= function
         | [] -> Lwt.return None
         | [(c, a, v, d, t, ver)] ->
             Lwt.return (Some (c, a, v, d, wikibox_content_type_of_string t, ver))
         | _ ::_ -> assert false (* (wiki_id, wiki, version) is a primary key *)
    )

let current_wikibox_version_ ~wikibox:(wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT max(version) \
                   FROM wikiboxes \
                   WHERE wiki_id = $wiki \
                   AND id = $id"
    )
  >>= function
    | [] -> Lwt.return None
    | [v] -> Lwt.return v
    | _ -> assert false (* (wiki_id, wiki, version) is a primary key *)

(** returns subject, text, author, datetime of a wikibox; 
    None if non-existant *)
let get_history ~wiki ~id =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT version, comment, author, datetime \
                  FROM wikiboxes \
                  WHERE wiki_id = $wiki \
                  AND id = $id \
                  ORDER BY version DESC")

type wikipage = {
  wikipage_source_wiki: wiki;
  wikipage_page: string;
  wikipage_dest_wiki: wiki;
  wikipage_wikibox: int32;
  wikipage_title: string option;
}

(** return the box corresponding to a wikipage *)
let get_box_for_page_ ~wiki ~page =
  let wiki' = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT * \
                  FROM wikipages \
                  WHERE sourcewiki = $wiki' \
                  AND pagename = $page") >>= function
      | [] -> Lwt.fail Not_found
      | (_sourcewiki, wikibox, _page, destwiki, title) :: _ ->
          (* (sourcewiki, pagename) is a primary key *)
          Lwt.return ({
            wikipage_source_wiki = wiki;
            wikipage_page = page;
            wikipage_dest_wiki = (int32_t destwiki : wiki);
            wikipage_wikibox = wikibox;
            wikipage_title = title;
          })

(** Sets the box corresponding to a wikipage *)
let set_box_for_page_ ~sourcewiki ~page ?(destwiki=sourcewiki) ~wikibox ?title () =
  let sourcewiki = t_int32 (sourcewiki : wiki)
  and destwiki = t_int32 (destwiki: wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "DELETE FROM wikipages WHERE sourcewiki=$sourcewiki AND pagename = $page"
       >>= fun () ->
       PGSQL(db) "INSERT INTO wikipages VALUES ($sourcewiki, $wikibox, $page, $destwiki, $?title)"
    )


type wiki_info = {
  id : wiki;
  title : string;
  descr : string;
  boxrights : bool;
  pages : string option;
  container_id : int32;
  staticdir : string option;
}


let reencapsulate_wiki (w, t, d, p, br, ci, s) =
  { id = wiki_from_sql w;
    title = t;
    descr = d;
    boxrights = br;
    pages = p;
    container_id = ci;
    staticdir = s;
  }


let find_wiki_ ~id =
  let id = t_int32 (id : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT * \
                  FROM wikis \
                  WHERE id = $id"
       >>= fun r -> 
       (match r with
          | [c] -> Lwt.return (reencapsulate_wiki c)
          | [] -> Lwt.fail Not_found
          | _ -> assert false (* Impossible, as the 'id' field is a primary key *)
       )
    )


let find_wiki_by_name_ ~name =
  Lwt_pool.use Sql.pool 
    (fun db ->
       PGSQL(db) "SELECT * \
                  FROM wikis \
                  WHERE title = $name"
       >>= fun r -> 
       (match r with
          | [c] -> Lwt.return (reencapsulate_wiki c)
          | [] -> Lwt.fail Not_found
          | _ -> assert false (* Impossible, there is a UNIQUE constraint on the title field *)

       )
    )


let get_writers_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT writer FROM wikiboxwriters \
             WHERE id = $id AND wiki_id = $wiki")

let get_readers_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT reader FROM wikiboxreaders \
             WHERE id = $id AND wiki_id = $wiki")

let get_rights_adm_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT wbadmin FROM wikiboxrightsgivers \
             WHERE id = $id AND wiki_id = $wiki")

let get_wikiboxes_creators_ (wiki, id) =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT creator FROM wikiboxcreators \
             WHERE id = $id AND wiki_id = $wiki")

(****)
let populate_readers_ wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_readers_ db wiki_id id readers)

let populate_writers_ wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  populate_writers_ db wiki_id id writers)

let populate_rights_adm_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  populate_rights_adm_ db wiki_id id wbadmins)

let populate_wikiboxes_creators_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  populate_wikiboxes_creators_ db wiki_id id wbadmins)

let remove_readers_ wiki_id id readers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_readers_ db wiki_id id readers)

let remove_writers_ wiki_id id writers =
  Lwt_pool.use Sql.pool (fun db ->
  remove_writers_ db wiki_id id writers)

let remove_rights_adm_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  remove_rights_adm_ db wiki_id id wbadmins)

let remove_wikiboxes_creators_ wiki_id id wbadmins =
  Lwt_pool.use Sql.pool (fun db ->
  remove_wikiboxes_creators_ db wiki_id id wbadmins)

(** returns the css for a page or fails with [Not_found] if it does not exist *)
let get_css_for_page_ ~wiki ~page =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT css FROM css \
                  WHERE wiki = $wiki AND page = $page"
       >>= function
         | [] -> Lwt.return None
         | x::_ -> Lwt.return (Some x))

(** Sets the css for a wikipage *)
let set_css_for_page_ ~wiki ~page content =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db -> 
       PGSQL(db) "DELETE FROM css WHERE wiki = $wiki AND page = $page" 
       >>= fun () ->
       PGSQL(db) "INSERT INTO css VALUES ($wiki, $page, $content)"
    )

(** returns the global css for a wiki or fails with [Not_found] if it does not exist *)
let get_css_for_wiki_ ~wiki =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT css FROM wikicss \
                  WHERE wiki = $wiki"
       >>= function
         | [] -> Lwt.return None
         | x::_ -> Lwt.return (Some x))

(** Sets the global css for a wiki *)
let set_css_for_wiki_ ~wiki content =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use 
    Sql.pool
    (fun db -> 
       PGSQL(db) "DELETE FROM wikicss WHERE wiki = $wiki" 
       >>= fun () ->
       PGSQL(db) "INSERT INTO wikicss VALUES ($wiki, $content)"
    )


let wikis_path () =
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT id, pages FROM wikis")



(** Cached versions of the functions above *)

(* Print some debuggging informations if activated *)
let debug_print_cache = ref false
let print_cache s =
  if !debug_print_cache then print_endline s


let get_wikibox_data, 
  get_readers,
  get_writers,
  get_rights_adm,
  get_wikiboxes_creators,
  populate_readers,
  populate_writers,
  populate_rights_adm,
  populate_wikiboxes_creators,
  remove_readers,
  remove_writers,
  remove_rights_adm,
  remove_wikiboxes_creators,
  update_wikibox,
  current_wikibox_version
  =
  let module C = Cache.Make (struct 
                               type key = (wiki * int32)
                               type value = (string * 
                                               User_sql.userid * 
                                               string * 
                                               CalendarLib.Calendar.t *
                                               wikibox_content_type *
                                               int32
                                            ) option
                             end) 
  in
  let module C2 = Cache.Make (struct 
                                type key = (wiki * int32)
                                type value = User_sql.userid list
                             end) 
  in
  let module C3 = Cache.Make(struct
                               type key = (wiki * int32)
                               type value = int32 option (* currently wikibox version*)
                             end)
  in
  let cache = C.create (fun a -> get_wikibox_data_ a ()) 64 in
  let cacher = C2.create get_readers_ 64 in
  let cachew = C2.create get_writers_ 64 in
  let cachera = C2.create get_rights_adm_ 64 in
  let cachewc = C2.create get_wikiboxes_creators_ 64 in
  let cachewv = C3.create (fun b -> current_wikibox_version_ b) 64 in
  ((fun ?version ~wikibox () ->
    match version with
      | None -> 
          print_cache "cache wikibox ";
          C.find cache wikibox
      | Some _ ->
          print_cache (Int32.to_string (snd wikibox) ^ " (with version) -> wikibox: db access");
          get_wikibox_data_ ?version ~wikibox ()
   ),
   (fun a -> print_cache "cache readers "; C2.find cacher a),
   (fun a -> print_cache "cache writers "; C2.find cachew a),
   (fun a -> print_cache "cache ra "; C2.find cachera a),
   (fun a -> print_cache "cache wc "; C2.find cachewc a),
  (fun a b r -> C2.remove cacher (a, b);  populate_readers_ a b r),
  (fun a b r -> C2.remove cachew (a, b);  populate_writers_ a b r),
  (fun a b r -> C2.remove cachera (a, b);  populate_rights_adm_ a b r),
  (fun a b r -> C2.remove cachewc (a, b);  populate_wikiboxes_creators_ a b r),
  (fun a b r -> C2.remove cacher (a, b);  remove_readers_ a b r),
  (fun a b r -> C2.remove cachew (a, b);  remove_writers_ a b r),
  (fun a b r -> C2.remove cachera (a, b);  remove_rights_adm_ a b r),
  (fun a b r -> C2.remove cachewc (a, b);  remove_wikiboxes_creators_ a b r),
  (fun ~wiki ~wikibox ~author ~comment ~content ->
     C.remove cache (wiki, wikibox);
     C2.remove cacher (wiki, wikibox);
     C2.remove cachew (wiki, wikibox);
     C2.remove cachera (wiki, wikibox);
     C2.remove cachewc (wiki, wikibox);
     C3.remove cachewv (wiki, wikibox);
     update_wikibox_ ~wiki ~wikibox ~author ~comment ~content),
   (fun ~wikibox -> C3.find cachewv wikibox)
  )

let get_box_for_page, set_box_for_page =
  let module C = Cache.Make (struct 
                               type key = (wiki * string)
                               type value = wikipage
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) ->
                get_box_for_page_ ~wiki ~page) 64 
  in
  ((fun ~wiki ~page -> 
      let page = Ocsigen_lib.remove_end_slash page in
      print_cache "cache wikipage ";
      C.find cache (wiki, page)),
   (fun ~sourcewiki ~page ?(destwiki=sourcewiki) ~wikibox ?title () ->
      let page = Ocsigen_lib.remove_end_slash page in
      C.remove cache (sourcewiki, page);
      set_box_for_page_ ~sourcewiki ~page ~destwiki ~wikibox ?title ()
   ))



(***)

let get_wiki_by_id, get_wiki_by_name, update_wiki =
  let module HW = Hashtbl.Make(struct
                                type t = wiki
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let module HN = Hashtbl.Make(struct
                                type t = string
                                let equal = (=)
                                let hash = Hashtbl.hash
                              end)
  in
  let wiki_info_wiki_table = HW.create 8
  and wiki_info_name_table = HN.create 8
  in
  (* get_wiki_by_id *)
  (fun ~id ->
     try
       print_cache "cache wiki ";
       Lwt.return (HW.find wiki_info_wiki_table id)
     with Not_found -> find_wiki_ ~id),
  (* get_wiki_by_name *)
  (fun ~name ->
     try
       print_cache "cache wiki ";
       Lwt.return (HN.find wiki_info_name_table name)
     with Not_found -> find_wiki_by_name_ ~name),
  (* update_wiki *)
  (fun ~wiki_id ~container_id () ->
     HW.remove wiki_info_wiki_table  wiki_id;
     (* A bit drastic, but search by name (and update_wiki) are rarely
        used anyway. The alternative is to find the the id of the
        wiki, and to remove only this key *)
     HN.clear wiki_info_name_table;
     update_wiki_ ~wiki:wiki_id ~container_id ())


(***)
let get_css_for_page, set_css_for_page =
  let module C = Cache.Make (struct 
                               type key = (wiki * string)
                               type value = string option
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) -> get_css_for_page_ ~wiki ~page) 64 
  in
  ((fun ~wiki ~page -> 
      print_cache "cache css";
      C.find cache (wiki, page) >>= function
        | None -> Lwt.fail Not_found
        | Some p -> Lwt.return p),
   (fun ~wiki ~page content ->
      C.remove cache (wiki, page);
      set_css_for_page_ ~wiki ~page content
   ))

(***)
let get_css_for_wiki, set_css_for_wiki =
  let module C = Cache.Make (struct 
                               type key = wiki
                               type value = string option
                             end) 
  in
  let cache = 
    C.create (fun wiki -> get_css_for_wiki_ ~wiki) 8
  in
  ((fun ~wiki -> 
      print_cache "cache wikicss";
      C.find cache wiki >>= function
        | None -> Lwt.fail Not_found
        | Some p -> Lwt.return p),
   (fun ~wiki content ->
      C.remove cache wiki;
      set_css_for_wiki_ ~wiki content
   ))
