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
   @author Boris Yakobowski
*)

open Opaque
open Lwt
open Sql.PGOCaml
open Ocsimore_lib
open CalendarLib
open Sql


module Types = struct
type wiki_arg = [ `Wiki ]
type wikibox_arg = [ `Wikibox ]
type wiki = [`Wiki] int32_t
let wiki_of_sql (i : int32) = (int32_t i : wiki)
let sql_of_wiki (i : wiki) = t_int32 i
let string_of_wiki i = Int32.to_string (sql_of_wiki i)
let wiki_of_string s = (Opaque.int32_t (Int32.of_string s) : wiki)


(* For now. Someday the second int32 will be a properly opacified type *)
type wikibox_id = int32
type wikibox = wiki * wikibox_id
type wikibox_uid = wikibox_arg Opaque.int32_t

type wikipage = wiki * string

type wikipage_arg = [ `Wikipage ]
type wikipage_uid = wikipage_arg Opaque.int32_t

type wiki_info = {
  wiki_id : wiki;
  wiki_title : string;
  wiki_descr : string;
  wiki_pages : string option;
  wiki_boxrights : bool;
  wiki_container : wikibox_id;
  wiki_staticdir : string option;
}

type wikibox_info = {
  wikibox_id : wikibox;
  wikibox_uid: wikibox_uid;
  wikibox_comment: string option;
  wikibox_special_rights: bool;
}

type wikipage_info = {
  wikipage_source_wiki: wiki;
  wikipage_page: string;
  wikipage_dest_wiki: wiki;
  wikipage_wikibox: int32;
  wikipage_title: string option;
  wikipage_uid : wikipage_uid;
(*  wikipage_css_special_rights; *)
}

end
open Types

let sql_to_wikipage i : wikipage_uid = Opaque.int32_t i


let eliom_wiki = Eliom_parameters.user_type wiki_of_string string_of_wiki


let new_wiki_ ~title ~descr ~pages ~boxrights ~staticdir ~container_text ~author () =
  let container_wikibox = 0l
  and author = User_sql.Types.sql_from_user author in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
         "INSERT INTO wikis (title, descr, pages, boxrights, container_id, staticdir)
          VALUES ($title, $descr, $?pages, $boxrights, $container_wikibox, $?staticdir);
                  "
     >>= fun () ->
     serial4 db "wikis_id_seq"
     >>= fun wiki_id ->
     let comment = Printf.sprintf "Container box for wiki %ld" wiki_id in
     PGSQL(db) "INSERT INTO wikiboxindex (wiki_id, id, comment)
                VALUES ($wiki_id, $container_wikibox, $comment)"
     >>= fun () ->
     PGSQL(db) "INSERT INTO wikiboxes (id, wiki_id, author, content)
                VALUES ($container_wikibox, $wiki_id, $author, $container_text)"
     >>= fun () ->
       return (wiki_of_sql wiki_id, container_wikibox)
    )


let update_wiki_ ?container_id ?staticdir ?pages wiki =
  let wiki = t_int32 (wiki : wiki) in
  Sql.full_transaction_block
    (fun db ->
       (match container_id with
          | None -> Lwt.return ()
          | Some container_id ->
              PGSQL(db) "UPDATE wikis SET container_id = $container_id \
                         WHERE id = $wiki"
       ) >>= fun () ->
       (match staticdir with
          | None -> Lwt.return ()
          | Some staticdir ->
              PGSQL(db) "UPDATE wikis SET staticdir = $?staticdir \
                         WHERE id = $wiki"
       ) >>= fun () ->
       (match pages with
          | None -> Lwt.return ()
          | Some pages ->
              PGSQL(db) "UPDATE wikis SET pages = $?pages \
                         WHERE id = $wiki"
       )
    )


exception IncorrectWikiboxContentType of string

type wikibox_content_type =
  | Css
  | WikiCreole

let wikibox_content_type_of_string = function
  | "wiki" -> WikiCreole
  | "css" -> Css
  | s -> raise (IncorrectWikiboxContentType s)

let string_of_wikibox_content_type = function
  | WikiCreole -> "wiki"
  | Css -> "css"


type wikibox_content =
    wikibox_content_type * string option * int32


(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox_ ~wiki ~author ~comment ~content ~content_type () =
  let wiki' = t_int32 (wiki : wiki)
  and content_type = string_of_wikibox_content_type content_type
  and author = User_sql.Types.sql_from_user author
  in
  Sql.full_transaction_block
    (fun db ->
       (PGSQL(db) "SELECT max(id) FROM wikiboxes WHERE wiki_id = $wiki'"
        >>= fun last ->
        let boxid = match last with
          | [] | None::_ -> 1l
          | (Some last)::_ -> Int32.add last 1l
        in
        PGSQL(db) "INSERT INTO wikiboxindex (wiki_id, id, comment)
                   VALUES ($wiki', $boxid, $comment)"
        >>= fun () ->
        PGSQL(db) "INSERT INTO wikiboxes
                  (id, wiki_id, author, comment, content, content_type)
                  VALUES
                  ($boxid, $wiki', $author, '', $content, $content_type)"
        >>= fun () ->
        Lwt.return boxid)
    )


(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
let update_wikibox_ ~wikibox:(wiki, wbox) ~author ~comment ~content ~content_type =
  let wiki = t_int32 (wiki : wiki)
  and content_type = string_of_wikibox_content_type content_type
  and author = User_sql.Types.sql_from_user author
  in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "INSERT INTO wikiboxes \
                    (id, wiki_id, author, comment, content, content_type) \
                  VALUES ($wbox, $wiki, $author, \
                          $comment, $?content, $content_type)" >>= fun () ->
       serial4 db "wikiboxes_version_seq")



(** Returns the content of a wikibox, or [None] if the wikibox or version
    does not exists *)
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
         | (c, a, v, d, t, ver) :: _ ->
             Lwt.return (Some (c, User_sql.Types.user_from_sql a,
                               v, d, wikibox_content_type_of_string t, ver))
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


let get_history ~wikibox:(wiki, wbid) =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT version, comment, author, datetime \
                  FROM wikiboxes \
                  WHERE wiki_id = $wiki \
                  AND id = $wbid \
                  ORDER BY version DESC")


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
      | (_sourcewiki, wikibox, _page, destwiki, title, uid) :: _ ->
          (* (sourcewiki, pagename) is a primary key *)
          Lwt.return ({
            wikipage_source_wiki = wiki;
            wikipage_page = page;
            wikipage_dest_wiki = (int32_t destwiki : wiki);
            wikipage_wikibox = wikibox;
            wikipage_title = title;
            wikipage_uid = sql_to_wikipage uid;
          })

(** Sets the box corresponding to a wikipage *)
let set_box_for_page_ ~sourcewiki ~page ?(destwiki=sourcewiki) ~wbid ?title () =
  let sourcewiki = t_int32 (sourcewiki : wiki)
  and destwiki = t_int32 (destwiki: wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "DELETE FROM wikipages WHERE sourcewiki=$sourcewiki AND pagename = $page"
       >>= fun () ->
       PGSQL(db) "INSERT INTO wikipages VALUES ($sourcewiki, $wbid, $page, $destwiki, $?title)"
    )


let reencapsulate_wiki (w, t, d, p, br, ci, s) =
  { wiki_id = wiki_of_sql w;
    wiki_title = t;
    wiki_descr = d;
    wiki_pages = p;
    wiki_boxrights = br; 
    wiki_container = ci;
    wiki_staticdir = s;
  }


let find_wiki_ ~id =
  let id = t_int32 (id : wiki) in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "SELECT * FROM wikis
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
       PGSQL(db) "SELECT * FROM wikis
                  WHERE title = $name"
       >>= fun r -> 
       (match r with
          | [c] -> Lwt.return (reencapsulate_wiki c)
          | [] -> Lwt.fail Not_found
          | _ -> assert false (* Impossible, there is a UNIQUE constraint on the title field *)

       )
    )


let get_css_wikibox_aux_ ~wiki ~page =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       (match page with
         | None ->
             PGSQL(db) "SELECT wikibox FROM css \
                        WHERE wiki = $wiki AND page IS NULL"
         | Some page ->
             PGSQL(db) "SELECT wikibox FROM css \
                        WHERE wiki = $wiki AND page = $page"
       ) >>= function
         | [] -> Lwt.return None
         | x::_ -> Lwt.return (Some (wiki_of_sql wiki, x))
    )

let get_css_wikibox_for_wikipage_ ~wiki ~page =
  get_css_wikibox_aux_ ~wiki ~page:(Some page)

let get_css_wikibox_for_wiki_ ~wiki =
  get_css_wikibox_aux_ ~wiki ~page:None

let set_css_wikibox_aux_ ~wiki ~page ~wikibox =
  let wiki = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "INSERT INTO css (wiki, page, wikibox) \
                  VALUES ($wiki, $?page, $wikibox)"
    )


let iter_wikis f =
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT * FROM wikis")
  >>= fun l ->
  Lwt_util.iter (fun wiki_info -> f (reencapsulate_wiki wiki_info)) l


let get_wikibox_info (wid, wbid as wb) =
  let wiki = t_int32 (wid : wiki) in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT * FROM wikiboxindex
                          WHERE wiki_id = $wiki AND id = $wbid"
       >>= function
         | [] -> Lwt.fail Not_found
         | (_, _, comment, rights, uid) :: _ ->
             Lwt.return {
               wikibox_id = wb;
               wikibox_uid = Opaque.int32_t uid;
               wikibox_comment = comment;
               wikibox_special_rights = rights;
             }
    )

let set_wikibox_special_rights_ (wid, wbid) v =
  let wiki = t_int32 (wid : wiki) in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "UPDATE wikiboxindex SET specialrights = $v
                          WHERE wiki_id = $wiki AND id = $wbid"
    )

(* XXX : cache this *)
let wikibox_from_uid wbuid =
  let uid = t_int32 (wbuid: wikibox_uid) in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT wiki_id, id FROM wikiboxindex
                          WHERE uid = $uid"
       >>= function
         | (wiki, wbid) :: _ -> Lwt.return (wiki_of_sql wiki, wbid)
         | [] -> Lwt.fail Not_found
    )

(** Cached versions of the functions above *)

(* Print some debuggging informations if activated *)
let debug_print_cache = ref false
let print_cache s =
  if !debug_print_cache then print_endline s


let
  get_wikibox_data,
  new_wiki,
  new_wikibox,
  update_wikibox,
  current_wikibox_version,
  set_wikibox_special_rights
  =
  let module C = Cache.Make (struct 
                               type key = wikibox
                               type value = (string * 
                                               User_sql.Types.userid * 
                                               string option *
                                               CalendarLib.Calendar.t *
                                               wikibox_content_type *
                                               int32
                                            ) option
                             end) 
  in
  let module C3 = Cache.Make(struct
                               type key = wikibox
                               type value = int32 option (* currently wikibox version*)
                             end)
  in
  let cache = C.create (fun a -> get_wikibox_data_ a ()) 64 in
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
  (fun ~title ~descr ~pages ~boxrights ~staticdir ~container_text ~ author () ->
     new_wiki_ ~title ~descr ~pages ~boxrights ~staticdir ~container_text ~author ()
     >>= function (wiki, wikibox) ->
     C.remove cache (wiki, wikibox);
     C3.remove cachewv (wiki, wikibox);
     Lwt.return (wiki, wikibox)),
  (fun ~wiki ~author ~comment ~content ~content_type () ->
     new_wikibox_ ~wiki ~author ~comment ~content ~content_type ()
     >>= fun wikibox ->
     C.remove cache (wiki, wikibox);
     C3.remove cachewv (wiki, wikibox);
     Lwt.return wikibox),
  (fun ~wikibox ~author ~comment ~content ->
     C.remove cache wikibox;
     C3.remove cachewv wikibox;
     update_wikibox_ ~wikibox ~author ~comment ~content),
  (fun ~wikibox -> C3.find cachewv wikibox),
  (fun wb v ->
     C.remove cache wb;
     set_wikibox_special_rights_ wb v)
  )

let get_wikipage_info, set_box_for_page =
  let module C = Cache.Make (struct 
                               type key = wikipage
                               type value = wikipage_info
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
   (fun ~sourcewiki ~page ?(destwiki=sourcewiki) ~wbid ?title () ->
      let page = Ocsigen_lib.remove_end_slash page in
      C.remove cache (sourcewiki, page);
      set_box_for_page_ ~sourcewiki ~page ~destwiki ~wbid ?title ()
   ))



(***)

let get_wiki_info_by_id, get_wiki_info_by_name, update_wiki =
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
  (fun ?container_id ?staticdir ?pages wiki_id ->
     HW.remove wiki_info_wiki_table  wiki_id;
     (* A bit drastic, but search by name (and update_wiki) are rarely
        used anyway. The alternative is to find the the id of the
        wiki, and to remove only this key *)
     HN.clear wiki_info_name_table;
     update_wiki_ ?container_id ?staticdir ?pages wiki_id)

(***)
(* Functions related to css. Since css are stored in wikiboxes,
   we only cache the association (wiki, page) -> wikibox *)
let get_css_wikibox, set_css_wikibox_in_cache =
  let module C = Cache.Make (struct 
                               type key = (wiki * string option)
                               type value = wikibox option
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) -> get_css_wikibox_aux_ ~wiki ~page) 64
  in
  ((fun ~wiki ~page -> 
      print_cache "cache css";
      C.find cache (wiki, page)),
   (fun ~wiki ~page box ->
      C.add cache (wiki, page) (Some (wiki, box));
      set_css_wikibox_aux_ wiki page box
   )
  )


let get_css_wikibox_for_wiki ~wiki =
  get_css_wikibox ~wiki ~page:None

let get_css_wikibox_for_wikipage ~wiki ~page =
  get_css_wikibox ~wiki ~page:(Some page)

let get_css_aux ~wiki ~page =
  get_css_wikibox ~wiki ~page
  >>= function
    | None -> Lwt.return None
    | Some wikibox ->
        get_wikibox_data wikibox ()
        >>= function
          | Some (_, _, Some content, _, _, _) -> Lwt.return (Some content)
          | Some (_, _, None, _, _, _) | None -> Lwt.return None

let get_css_for_wikipage ~wiki ~page =
  get_css_aux ~wiki ~page:(Some page)

let get_css_for_wiki ~wiki =
  get_css_aux ~wiki ~page:None


let set_css_aux ~wiki ~page ~author content =
  get_css_wikibox ~wiki ~page
  >>= function
    | None ->
        (* If the CSS must be deleted ([content=None]) and no css currently
           exists, we do simply do nothing. (Alternatively, we could create
           a box and set its content to NULL, but this does not seem really
           useful.) *)
        (match content with
           | None -> Lwt.return ()
           | Some content ->
               new_wikibox ~wiki ~comment:"" ~author ~content ~content_type:Css ()
               >>= fun wikibox ->
               set_css_wikibox_in_cache ~wiki ~page wikibox
        )
    | Some wbid ->
        update_wikibox ~wikibox:wbid ~author ~comment:""
          ~content ~content_type:Css
        >>= fun _ -> Lwt.return ()

let set_css_for_wikipage ~wiki ~page ~author content =
  set_css_aux ~wiki ~page:(Some page) ~author content

let set_css_for_wiki ~wiki ~author content =
  set_css_aux ~wiki ~page:None ~author content
