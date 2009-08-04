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
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Opaque
open Lwt
open Sql.PGOCaml
open Ocsimore_lib
open CalendarLib
open Sql


open Wiki_types

let sql_to_wikipage i : wikipage_uid = Opaque.int32_t i

let wrap db f = match db with
  | None -> full_transaction_block f
  | Some db -> f db



let update_wiki_ ?db ?container ?staticdir ?path ?descr ?boxrights wiki =
  let f db =
  let wiki = t_int32 (wiki : wiki) in
    (match container with
       | None -> Lwt.return ()
       | Some container ->
           let container = match container with
             | None -> None
             | Some c -> Some (sql_of_wikibox c)
           in
           PGSQL(db) "UPDATE wikis SET container = $?container \
                      WHERE id = $wiki"
    ) >>= fun () ->
    (match staticdir with
       | None -> Lwt.return ()
       | Some staticdir ->
           PGSQL(db) "UPDATE wikis SET staticdir = $?staticdir \
                      WHERE id = $wiki"
    ) >>= fun () ->
    (match path with
       | None -> Lwt.return ()
       | Some pages ->
           PGSQL(db) "UPDATE wikis SET pages = $?pages \
                      WHERE id = $wiki"
    ) >>= fun () ->
    (match descr with
       | None -> Lwt.return ()
       | Some descr ->
           PGSQL(db) "UPDATE wikis SET descr = $descr \
                      WHERE id = $wiki"
    ) >>= fun () ->
    (match boxrights with
       | None -> Lwt.return ()
       | Some boxrights ->
           PGSQL(db) "UPDATE wikis SET boxrights = $boxrights \
                      WHERE id = $wiki"
    )
  in wrap db f



(** Inserts a new wikibox in an existing wiki and return its id. *)
let new_wikibox ?db ~wiki ~author ~comment ~content ~content_type () =
  let wiki' = t_int32 (wiki : wiki)
  and content_type = string_of_content_type content_type
  and author = User_sql.Types.sql_from_userid author
  in
  let f db =
    (PGSQL(db) "INSERT INTO wikiboxindex (wiki, comment)
                   VALUES ($wiki', $comment)"
     >>= fun () ->
     serial4 db "wikiboxindex_uid_seq" >>= fun boxid ->
     PGSQL(db) "INSERT INTO wikiboxescontent
                  (wikibox, author, comment, content, content_type)
                VALUES
                  ($boxid, $author, '', $content, $content_type)"
     >>= fun () ->
     Lwt.return (wikibox_of_sql boxid))
  in
  match db with
    | None -> Sql.full_transaction_block f
    | Some db -> f db


(** Inserts a new version of an existing wikibox in a wiki 
    and return its version number. *)
let update_wikibox_ ~wb ~author ~comment ~content ~content_type =
  let wikibox = sql_of_wikibox wb
  and author = User_sql.Types.sql_from_userid author
  in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db) "INSERT INTO wikiboxescontent \
                    (wikibox, author, comment, content, content_type) \
                 VALUES ($wikibox, $author, $comment, $?content, $content_type)"
       >>= fun () ->
       serial4 db "wikiboxes_version_seq")



(** Returns the content of a wikibox, or [None] if the wikibox or version
    does not exists *)
let get_wikibox_data_ ?version ~wb () =
  let wikibox = sql_of_wikibox wb in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       (match version with
         | None ->
             PGSQL(db) "SELECT comment, author, content, datetime,
                            content_type, version
                        FROM wikiboxescontent
                        WHERE wikibox=$wikibox
                        AND version =
                           (SELECT max(version)
                            FROM wikiboxescontent
                            WHERE wikibox=$wikibox)"
         | Some version ->
             PGSQL(db) "SELECT comment, author, content, datetime,
                            content_type, version
                        FROM wikiboxescontent
                        WHERE wikibox=$wikibox AND version=$version")
       >>= function
         | [] -> Lwt.return None
         | (c, a, v, d, t, ver) :: _ ->
             Lwt.return (Some (c, User_sql.Types.userid_from_sql a,
                               v, d, t, ver))
    )

let current_wikibox_version_ ~wb =
  let wikibox = sql_of_wikibox wb in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT max(version)
                   FROM wikiboxescontent
                   WHERE wikibox = $wikibox"
    )
  >>= function
    | [] -> Lwt.return None
    | [v] -> Lwt.return v
    | _ -> assert false (* (wikibox, version) is a primary key *)


let get_history ~wb =
  let wikibox = sql_of_wikibox wb in
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db) "SELECT version, comment, author, datetime
                  FROM wikiboxescontent
                  WHERE wikibox = $wikibox
                  ORDER BY version DESC")


(** return the box corresponding to a wikipage *)
let get_box_for_page_ ~wiki ~page =
  let wiki' = t_int32 (wiki : wiki) in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "SELECT * FROM wikipages
                  WHERE wiki = $wiki'
                  AND pagename = $page") >>= function
      | [] -> Lwt.fail Not_found
      | (_wiki, wikibox, _page, title, uid) :: _ ->
          (* (wiki, pagename) is a primary key *)
          Lwt.return ({
            wikipage_wiki = wiki;
            wikipage_page = page;
            wikipage_wikibox = (int32_t wikibox);
            wikipage_title = title;
            wikipage_uid = sql_to_wikipage uid;
          })


let create_wikipage_ ?db ~wiki ~page ~wb =
  let wiki = t_int32 (wiki : wiki)
  and wb = t_int32 (wb : wikibox) in
  wrap db
    (fun db ->
       PGSQL(db) "INSERT INTO wikipages (wiki, wikibox, pagename)
                  VALUES ($wiki, $wb, $page)"
    )

let set_wikipage_properties_ ?db ~wiki ~page ?title ?newpage ?wb () =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       (match title with
          | None -> Lwt.return ()
          | Some "" ->
              PGSQL(db) "UPDATE wikipages SET title = NULL
                         WHERE wiki = $wiki and pagename = $page"
          | Some s ->
              PGSQL(db) "UPDATE wikipages SET title = $s
                         WHERE wiki = $wiki and pagename = $page"
       ) >>= fun () ->
       (match newpage with
          | None -> Lwt.return ()
          | Some page ->
              PGSQL(db) "UPDATE wikipages SET pagename = $page
                         WHERE wiki = $wiki and pagename = $page"
       ) >>= fun () ->
       (match wb with
          | None -> Lwt.return ()
          | Some None ->
              PGSQL(db) "DELETE FROM wikipages
                         WHERE wiki = $wiki and pagename = $page"
          | Some (Some wb) ->
              Ocsigen_messages.console2 "Bla";
              let wb = sql_of_wikibox wb in
              PGSQL(db) "UPDATE wikipages SET wikibox = $wb
                         WHERE wiki = $wiki and pagename = $page"
       )
    )


let reencapsulate_wiki (w, t, d, p, br, ci, s, m) =
  { wiki_id = wiki_of_sql w;
    wiki_title = t;
    wiki_descr = d;
    wiki_pages = p;
    wiki_boxrights = br;
    wiki_container =
      (match ci with None -> None | Some ci -> Some (wikibox_of_sql ci));
    wiki_staticdir = s;
    wiki_model = Wiki_types.wiki_model_of_string m;
  }


let find_wiki_ id =
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


let find_wiki_by_name_ name =
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
         | x::_ -> Lwt.return (Some (wikibox_of_sql x))
    )

let get_css_wikibox_for_wikipage_ ~wiki ~page =
  get_css_wikibox_aux_ ~wiki ~page:(Some page)

let get_css_wikibox_for_wiki_ ~wiki =
  get_css_wikibox_aux_ ~wiki ~page:None

let set_css_wikibox_aux_ ~wiki ~page ~wb =
  let wiki = t_int32 (wiki : wiki)
  and wb = sql_of_wikibox wb in
  Lwt_pool.use
    Sql.pool
    (fun db ->
       PGSQL(db) "INSERT INTO css (wiki, page, wikibox) \
                  VALUES ($wiki, $?page, $wb)"
    )


let iter_wikis f =
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT * FROM wikis")
  >>= fun l ->
  Lwt_util.iter (fun wiki_info -> f (reencapsulate_wiki wiki_info)) l


let get_wikibox_info ?db wb =
  let wb' = sql_of_wikibox wb in
  let f db =
    PGSQL(db) "SELECT wiki, comment, specialrights, uid FROM wikiboxindex
                        WHERE uid = $wb'"
    >>= function
      | [] -> Lwt.fail Not_found
      | (wiki, comment, rights, _) :: _ ->
          Lwt.return {
            wikibox_wiki = wiki_of_sql wiki;
            wikibox_id = wb;
            wikibox_comment = comment;
            wikibox_special_rights = rights;
          }
  in
  match db with
    | None -> Sql.full_transaction_block f
    | Some db -> f db

let set_wikibox_special_rights_ ~wb v =
  let wb = sql_of_wikibox wb in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "UPDATE wikiboxindex SET specialrights = $v
                          WHERE uid = $wb"
    )

let wikibox_wiki_ wb =
  let wb = sql_of_wikibox wb in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT wiki FROM wikiboxindex
                          WHERE uid = $wb"
       >>= function
         | wiki :: _ -> Lwt.return (wiki_of_sql wiki)
         | [] -> Lwt.fail Not_found
    )

(** Cached versions of the functions above *)

(* Print some debuggging informations if activated *)
let debug_print_cache = ref false
let print_cache s =
  if !debug_print_cache then print_endline s


let
  get_wikibox_data,
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
                                               string (* content_type *) *
                                               int32
                                            ) option
                             end) 
  in
  let module C3 = Cache.Make(struct
                               type key = wikibox
                               type value = int32 option (* currently wikibox version*)
                             end)
  in
  let cache = 
    C.create 
      (fun a -> get_wikibox_data_ a ()) 64 
  in
  let cachewv = C3.create (fun b -> current_wikibox_version_ b) 64 in
  ((fun ?version ~wb () ->
      match version with
        | None -> C.find cache wb
        | Some _ -> get_wikibox_data_ ?version ~wb ()
   ),
  (fun ~wb ~author ~comment ~content ~content_type ->
     C.remove cache wb;
     C3.remove cachewv wb;
     update_wikibox_ ~wb ~author ~comment ~content ~content_type),
  (fun ~wb -> C3.find cachewv wb),
  (fun ~wb v ->
     C.remove cache wb;
     set_wikibox_special_rights_ wb v)
  )

let get_wikibox_data ?version ~wb () =
  wikibox_data_of_raw (get_wikibox_data ?version ~wb ())

let update_wikibox ~wb ~author ~comment ~content ~content_type =
  let content_type = string_of_content_type content_type in
  update_wikibox ~wb ~author ~comment ~content ~content_type

let get_wikipage_info, set_wikipage_properties, create_wikipage =
  let module C = Cache.Make (struct 
                               type key = wikipage
                               type value = wikipage_info
                             end) 
  in
  let cache = 
    C.create (fun (wiki, page) ->
                get_box_for_page_ ~wiki ~page) 64 
  in
  (fun ~wiki ~page -> 
      let page = Ocsigen_lib.remove_end_slash page in
      print_cache "cache wikipage ";
      C.find cache (wiki, page)),
   (fun ?db ~wiki ~page ?title ?newpage ?wb () ->
      let page = Ocsigen_lib.remove_end_slash page in
      C.remove cache (wiki, page);
      set_wikipage_properties_ ?db ~wiki ~page ?title ?newpage ?wb ()
   ),
   create_wikipage_ (* No problem with cache, as cache miss are not cached *)



(***)

let get_wiki_info_by_id, get_wiki_info_by_name, update_wiki =
  let module CW = Cache.Make(struct
                               type key = wiki
                               type value = wiki_info
                             end)
  in
  let module CN = Cache.Make(struct
                               type key = string
                               type value = wiki_info
                             end)
  in
  let wiki_info_wiki = CW.create find_wiki_ 8
  and wiki_info_name = CN.create find_wiki_by_name_ 8
  in
  (* get_wiki_by_id *)
  (fun ~id -> CW.find wiki_info_wiki id),
  (* get_wiki_by_name *)
  (fun ~name -> CN.find wiki_info_name name),
  (* update_wiki *)
  (fun ?db ?container ?staticdir ?path ?descr ?boxrights wiki ->
     CW.remove wiki_info_wiki  wiki;
     (* A bit drastic, but search by name (and update_wiki) are rarely
        used anyway. The alternative is to find the the id of the
        wiki, and to remove only this key *)
     CN.clear wiki_info_name;
     update_wiki_ ?db ?container ?staticdir ?path ?descr ?boxrights wiki)


(** Wiki of a wikibox *)
let wikibox_wiki =
  let module C = Cache.Make(struct type key = wikibox
                                   type value = wiki end) in
  let c = C.create wikibox_wiki_ 64 in
  (fun ~wb -> C.find c wb)


(** Functions related to css. Since css are stored in wikiboxes,
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
      C.add cache (wiki, page) (Some box);
      set_css_wikibox_aux_ wiki page box
   )
  )


let page_to_string wiki = function
  | None -> "wiki " ^ string_of_wiki wiki
  | Some page -> "page " ^ page ^ " of wiki " ^ string_of_wiki wiki

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
               new_wikibox ~wiki ~comment:("CSS for " ^page_to_string wiki page)
                 ~author ~content ~content_type:Wiki_models.css_content_type ()
               >>= fun wikibox ->
               set_css_wikibox_in_cache ~wiki ~page wikibox
        )
    | Some wb ->
        update_wikibox ~wb ~author ~comment:""
          ~content ~content_type:Wiki_models.css_content_type
        >>= fun _ -> Lwt.return ()

let set_css_for_wikipage ~wiki ~page ~author content =
  set_css_aux ~wiki ~page:(Some page) ~author content

let set_css_for_wiki ~wiki ~author content =
  set_css_aux ~wiki ~page:None ~author content


let new_wiki ~title ~descr ~pages ~boxrights ~staticdir ?container_text ~author ~model () =
  let model_sql = Wiki_types.string_of_wiki_model model in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
        "INSERT INTO wikis (title, descr, pages, boxrights, staticdir, model)
         VALUES ($title, $descr, $?pages, $boxrights, $?staticdir, $model_sql);"
     >>= fun () ->
     serial4 db "wikis_id_seq" >>= fun wiki_sql ->
     let wiki = wiki_of_sql wiki_sql in
     (match container_text with
        | None -> Lwt.return None
        | Some content ->
            let comment= Printf.sprintf "Container box for wiki %ld" wiki_sql in
            new_wikibox ~db ~wiki ~author ~comment ~content
              ~content_type:(Wiki_models.get_default_content_type model) ()
            >>= fun container ->
            update_wiki ~db ~container:(Some container) wiki >>= fun () ->
            Lwt.return (Some container)
     ) >>= fun container ->
     return (wiki, container)
    )





let update_wikiboxes ?db f =
  let f db =
    PGSQL(db) "SELECT version, wikibox, content, content_type
               FROM wikiboxescontent
               WHERE content_type='wikicreole'"
    >>= fun l ->
    Lwt_util.iter_serial (fun (version, wb, content, ct) ->
                          f ~wikibox:(wikibox_of_sql wb) ~version ~content
                            ~content_type:(Wiki_types.content_type_of_string ct)
                          >>= function
                            | None -> Lwt.return ()
                            | Some s ->
                                PGSQL (db)
                                  "UPDATE wikiboxescontent
                                   SET content = $s
                                   WHERE wikibox = $wb AND version = $version"
                         ) l
      >>= fun () -> Ocsigen_messages.console2 "Done updatinge wikiboxes content";
      Cache.clear_all_caches ();
      Lwt.return ()
  in match db with
    | None -> full_transaction_block f
    | Some db -> f db


let wikibox_new_id ~wiki ~wb_old_id =
  let wiki = sql_of_wiki wiki in
  Sql.full_transaction_block
    (fun db -> PGSQL(db) "SELECT uid FROM wikiboxindex
                          WHERE wiki=$wiki AND id=$wb_old_id"
     >>= function
       | [] -> Lwt.fail Not_found
       | uid :: _ -> Lwt.return (wikibox_of_sql uid)
    )

let rewrite_wikipages ?db ~oldwiki ~newwiki ~path =
  let oldwiki = sql_of_wiki oldwiki
  and newwiki = sql_of_wiki newwiki in
  let f db =
    PGSQL(db) "SELECT * FROM wikipages WHERE wiki=$oldwiki" >>= fun l ->
    Lwt_util.iter
      (fun (_, _wb, page, _, uid) ->
         match Ocsimore_lib.remove_prefix ~s:page ~prefix:path with
           | None -> Lwt.return ()
           | Some prefix ->
               PGSQL(db) "UPDATE wikipages
                          SET wiki = $newwiki, pagename= $prefix
                          WHERE uid=$uid"
      ) l >>= fun () ->
      Cache.clear_all_caches ();
      Ocsigen_messages.console2 "Done updatinge wikipages";
      Lwt.return ()
  in match db with
    | None -> full_transaction_block f
    | Some db -> f db
