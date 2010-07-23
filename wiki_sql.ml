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


(** Wikiboxes *)

(** Inserts a new wikibox in an existing wiki and return its id. *)
(* Not cached : by unicity constraints, we only insert data that
   did not exist previously, and which thus needs not be deleted
   from the cache *)
let new_wikibox ?db ~wiki ~author ~comment ~content ~content_type () =
  wrap db
    (fun db ->
       let wiki' = t_int32 (wiki : wiki)
       and content_type = string_of_content_type content_type
       and author = User_sql.Types.sql_from_userid author
       in
       PGSQL(db) "INSERT INTO wikiboxindex (wiki, comment)
                  VALUES ($wiki', $comment)"
       >>= fun () ->
       serial4 db "wikiboxindex_uid_seq" >>= fun boxid ->
       PGSQL(db) "INSERT INTO wikiboxescontent
                    (wikibox, author, comment, content, content_type)
                  VALUES
                    ($boxid, $author, '', $content, $content_type)"
       >>= fun () ->
       Lwt.return (wikibox_of_sql boxid)
    )


(** Inserts a new version of an existing wikibox in a wiki
    and return its version number. *)
let update_wikibox_ ?db ~author ~comment ~content ~content_type ?ip wb =
  let content_type = string_of_content_type content_type in
  wrap db
    (fun db ->
       let wikibox = sql_of_wikibox wb
       and author = User_sql.Types.sql_from_userid author
       in
       PGSQL(db) "INSERT INTO wikiboxescontent
                    (wikibox, author, comment, content, content_type, ip)
                 VALUES ($wikibox, $author, $comment, $?content, $content_type, $?ip)"
       >>= fun () ->
       serial4 db "wikiboxes_version_seq"
    )

(** Returns the content of a wikibox, or [None] if the wikibox or version
    does not exists *)
let get_wikibox_content_ ?version wb =
  Lwt_pool.use Sql.pool
    (fun db ->
       let wikibox = sql_of_wikibox wb in
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
             Lwt.return (Some (c,
                               User_sql.Types.userid_from_sql a, v, d, t, ver))
    )

let current_wikibox_version_  wb =
  Lwt_pool.use Sql.pool
    (fun db ->
       let wikibox = sql_of_wikibox wb in
       PGSQL(db) "SELECT max(version)
                  FROM wikiboxescontent
                  WHERE wikibox = $wikibox"
       >>= function
         | [] -> Lwt.return None
         | [v] -> Lwt.return v
         | _ -> assert false (* (wikibox, version) is a primary key *)
    )

(* Not cached : too rare, and too big *)
let get_wikibox_history ~wb =
  Lwt_pool.use Sql.pool
    (fun db ->
       let wikibox = sql_of_wikibox wb in
       PGSQL(db) "SELECT version, comment, author, datetime
                  FROM wikiboxescontent
                  WHERE wikibox = $wikibox
                  ORDER BY version DESC")


(** Wikiboxes metadata *)

let get_wikibox_info_ wb =
  Lwt_pool.use Sql.pool
    (fun db ->
       let wb' = sql_of_wikibox wb in
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
    )

let set_wikibox_special_rights_ ?db wb v =
  wrap db
    (fun db ->
       let wb = sql_of_wikibox wb in
       PGSQL(db) "UPDATE wikiboxindex
                  SET specialrights = $v
                  WHERE uid = $wb"
    )



(** Wikipages *)

(** return the box corresponding to a wikipage *)
let get_box_for_page_ ~wiki ~page =
  Lwt_pool.use Sql.pool
    (fun db ->
       let wiki' = t_int32 (wiki : wiki) in
       PGSQL(db) "SELECT * FROM wikipages
                  WHERE wiki = $wiki'
                  AND pagename = $page" >>= function
         | [] -> Lwt.fail Not_found
         | (_wiki, wikibox, _page, title, uid) :: _ ->
             (* (wiki, pagename) is a primary key *)
             Lwt.return {
               wikipage_wiki = wiki;
               wikipage_page = page;
               wikipage_wikibox = (int32_t wikibox);
               wikipage_title = title;
               wikipage_uid = sql_to_wikipage uid;
             }
    )

(* No need for cache, as the page does not exists yet *)
let create_wikipage ?db ~wiki ~page ~wb =
  let page = Ocsigen_lib.remove_end_slash page in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and wb = t_int32 (wb : wikibox) in
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
              let wb = sql_of_wikibox wb in
              PGSQL(db) "UPDATE wikipages SET wikibox = $wb
                         WHERE wiki = $wiki and pagename = $page"
       )
    )


(** Wikis *)

let reencapsulate_wiki (w, t, d, p, br, ci, s, m, h) =
  { wiki_id = wiki_of_sql w;
    wiki_title = t;
    wiki_descr = d;
    wiki_pages = p;
    wiki_boxrights = br;
    wiki_container =
      (match ci with None -> None | Some ci -> Some (wikibox_of_sql ci));
    wiki_staticdir = s;
    wiki_model = Wiki_types.wiki_model_of_string m;
    wiki_siteid = h;
  }

let update_wiki_ ?db ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki =
  wrap db
    (fun db ->
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
       ) >>= fun () ->
       (match model with
          | None -> Lwt.return ()
          | Some model ->
              let model = string_of_wiki_model model in
              PGSQL(db) "UPDATE wikis SET model = $model \
                         WHERE id = $wiki"
       ) >>= fun () ->
       (match siteid with
          | None -> Lwt.return ()
          | Some siteid ->
              PGSQL(db) "UPDATE wikis SET siteid = $?siteid \
                         WHERE id = $wiki"
       )
    )

let new_wiki ?db ~title ~descr ~pages ~boxrights ~staticdir ?container_text ~author ~model () =
  wrap db
    (fun db ->
       let model_sql = Wiki_types.string_of_wiki_model model in
       PGSQL(db)
       "INSERT INTO wikis (title, descr, pages, boxrights, staticdir, model)
       VALUES ($title, $descr, $?pages, $boxrights, $?staticdir, $model_sql);"
       >>= fun () ->
       serial4 db "wikis_id_seq" >>= fun wiki_sql ->
       let wiki = wiki_of_sql wiki_sql in
       (match container_text with
          | None -> Lwt.return None
          | Some content ->
              let comment = Printf.sprintf "Container box for wiki %ld"
                wiki_sql in
              new_wikibox ~db ~wiki ~author ~comment ~content
                ~content_type:(Wiki_models.get_default_content_type model) ()
              >>= fun container ->
              (* No problem wrt. wiki cache, as wiki is not yet cached *)
              update_wiki_ ~db ~container:(Some container) wiki >>= fun () ->
                Lwt.return (Some container)
       ) >>= fun container ->
       return (wiki, container)
    )


let find_wiki_ ?db id =
  wrap db
    (fun db ->
       let id = t_int32 (id : wiki) in
       PGSQL(db) "SELECT * FROM wikis
                  WHERE id = $id"
       >>= function
         | [c] -> Lwt.return (reencapsulate_wiki c)
         | [] -> Lwt.fail Not_found
         | _ -> assert false
             (* Impossible, as the 'id' field is a primary key *)
    )


let find_wiki_by_name_ ?db name =
  wrap db
    (fun db ->
       PGSQL(db) "SELECT * FROM wikis
                  WHERE title = $name"
       >>= function
         | [c] -> Lwt.return (reencapsulate_wiki c)
         | [] -> Lwt.fail Not_found
         | _ -> assert false (* Impossible, there is a UNIQUE constraint on the title field *)
    )

let iter_wikis ?db f =
  wrap db
    (fun db ->
       PGSQL(db) "SELECT * FROM wikis" >>= fun l ->
       Lwt_util.iter (fun wiki_info -> f (reencapsulate_wiki wiki_info)) l)




(** CSS *)

let page_opt_to_string wiki = function
  | None -> "wiki " ^ string_of_wiki wiki
  | Some page -> "page " ^ page ^ " of wiki " ^ string_of_wiki wiki

let get_css_wikibox_aux_ ?db ~wiki ~page =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       PGSQL(db) "SELECT wikibox, mediatype, rank FROM css \
                  WHERE wiki = $wiki AND page IS NOT DISTINCT FROM $?page
                  ORDER BY rank"
       >|=
       List.map
         (fun (wb, media, rank) ->
            let media = media_type_of_string media in
              wikibox_of_sql wb, media, rank)
    )


let get_css_wikibox_for_wikipage_ ?db ~wiki ~page =
  get_css_wikibox_aux_ ?db ~wiki ~page:(Some page)

let get_css_wikibox_for_wiki_ ?db ~wiki =
  get_css_wikibox_aux_ ?db ~wiki ~page:None

let add_css_wikibox_aux_ ?db ~wiki ~page ~media wb =
  let media = string_of_media_type media in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and wb = sql_of_wikibox wb in
       PGSQL(db) "INSERT INTO css (wiki, page, wikibox, mediatype, rank)
                  VALUES ($wiki, $?page, $wb, $media,
                   (SELECT COALESCE (max(rank) + 1, 1) from CSS
                    WHERE wiki=wiki AND page IS NOT DISTINCT FROM $?page)
                  )"
    )

let remove_css_wikibox_aux_ ?db ~wiki ~page wb =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       let wb = sql_of_wikibox wb in
       PGSQL(db) "DELETE FROM css
                  WHERE wiki = $wiki
                  AND page IS NOT DISTINCT FROM $?page
                  AND wikibox = $wb"
    )

let update_css_wikibox_aux_ ?db ~wiki ~page ~oldwb ~newwb ~media ~rank () =
  let media = string_of_media_type media in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and oldwb = sql_of_wikibox oldwb
       and newwb = sql_of_wikibox newwb in
       PGSQL(db) "UPDATE css
                  SET wikibox = $newwb, mediatype = $media, rank = $rank
                  WHERE wiki = $wiki AND page IS NOT DISTINCT FROM $?page AND
                        wikibox = $oldwb"
    )




(** Cached versions of the functions with underscores above *)


(** Wikiboxes, wikiboxescontent table *)

module CWbContent = Ocsigen_cache.Make (struct
                                  type key = wikibox
                                  type value = (string *
                                                User_sql.Types.userid *
                                                string option *
                                                CalendarLib.Calendar.t *
                                                string (* content_type *) *
                                                int32
                                               ) option
                                end)

module CWbVersion = Ocsigen_cache.Make(struct
                                 type key = wikibox
                                 type value = int32 option
                               end)

let cache_wb_content = new CWbContent.cache
  (fun wb -> get_wikibox_content_ wb) 64

let cache_wb_version = new CWbVersion.cache
  (fun wb -> current_wikibox_version_ wb) 64

let get_wikibox_content ?version wb =
  wikibox_data_of_raw
    (match version with
       | None -> cache_wb_content#find wb
       | Some _ -> get_wikibox_content_ ?version wb)

let current_wikibox_version = cache_wb_version#find

let update_wikibox ?db ~author ~comment ~content ~content_type ?ip wb =
  cache_wb_content#remove wb;
  cache_wb_version#remove wb;
  update_wikibox_ ?db ~author ~comment ~content ~content_type ?ip wb



(** Wikiboxes, wikiboxesindex table *)

module CWbInfo = Ocsigen_cache.Make(struct type key = wikibox
                                   type value = wikibox_info end)

let cache_wb_info = new CWbInfo.cache get_wikibox_info_ 64

let get_wikibox_info = cache_wb_info#find

let wikibox_wiki wb =
  get_wikibox_info wb >>= fun info -> Lwt.return info.wikibox_wiki

let set_wikibox_special_rights ?db ~wb v =
  cache_wb_info#remove wb;
  set_wikibox_special_rights_ ?db wb v



(** Wikipages *)

module CWp = Ocsigen_cache.Make (struct
                           type key = wikipage
                           type value = wikipage_info
                         end)

let cache_wp = new CWp.cache
  (fun (wiki, page) ->
     get_box_for_page_ wiki page) 64


let get_wikipage_info ~wiki ~page =
  let page = Ocsigen_lib.remove_end_slash page in
  cache_wp#find (wiki, page)

let set_wikipage_properties ?db ~wiki ~page ?title ?newpage ?wb () =
  let page = Ocsigen_lib.remove_end_slash page in
  cache_wp#remove (wiki, page);
  set_wikipage_properties_ ?db ~wiki ~page ?title ?newpage ?wb ()



(** Wikis *)

module CWId = Ocsigen_cache.Make(struct
                           type key = wiki
                           type value = wiki_info
                         end)



let cache_wiki_id = new CWId.cache find_wiki_ 8

let cache_wiki_name =
  let module CWN = Ocsigen_cache.Make(struct
                                type key = string
                                type value = wiki_info
                              end)
  in
  new CWN.cache find_wiki_by_name_ 8


let get_wiki_info_by_id ~id = cache_wiki_id#find id
let get_wiki_info_by_name ~name = cache_wiki_name#find name

let update_wiki ?db ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki =
  cache_wiki_id#remove wiki;
  (* A bit drastic, but search by name (and update_wiki) are rarely
     used anyway. The alternative is to find the the id of the
     wiki, and to remove only this key *)
  cache_wiki_name#clear ();
  update_wiki_ ?db ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki



(** Functions related to css. Since css are stored in wikiboxes,
   we only cache the association (wiki, page) -> wikibox *)

let cache_css =
  let module C = Ocsigen_cache.Make (struct
                               type key = (wiki * string option)
                               type value = (wikibox * media_type * int32) list
                             end)
  in
  new C.cache (fun (wiki, page) -> get_css_wikibox_aux_ wiki page) 64


let get_css_wikibox ~wiki ~page =
  cache_css#find (wiki, page)

let add_css_wikibox_aux ?db ~wiki ~page ~media wb =
  cache_css#remove (wiki, page);
  add_css_wikibox_aux_ ?db ~wiki ~page ~media wb

let remove_css_wikibox_aux ?db ~wiki ~page wb =
  cache_css#remove (wiki, page);
  remove_css_wikibox_aux_ ?db ~wiki ~page wb

let update_css_wikibox_aux ?db ~wiki ~page ~oldwb ~newwb ~media ~rank () =
  cache_css#remove (wiki, page);
  update_css_wikibox_aux_ ?db ~wiki ~page ~oldwb ~newwb ~media ~rank ()


(* Derived functions, again for css *)

let get_css_wikibox_for_wiki ~wiki =
  get_css_wikibox ~wiki ~page:None

let get_css_wikibox_for_wikipage ~wiki ~page =
  let page = Ocsigen_lib.remove_end_slash page in
  get_css_wikibox ~wiki ~page:(Some page)

let get_css_aux ~wiki ~page =
  get_css_wikibox ~wiki ~page >>= fun l ->
  Lwt_util.fold_left
    (fun l (wb, _, _ as wbcss) ->
       get_wikibox_content wb
       >>= function
         | Some (_, _, Some content, _, _, ver) ->
             Lwt.return ((wbcss, (content, ver)) :: l)
         | Some (_, _, None, _, _, _) | None -> Lwt.return l
    ) [] l

let get_css_for_wikipage ~wiki ~page =
  let page = Ocsigen_lib.remove_end_slash page in
  get_css_aux ~wiki ~page:(Some page)

let get_css_for_wiki ~wiki =
  get_css_aux ~wiki ~page:None


exception Unknown_Css of wikibox

let add_css_aux ?db ~wiki ~page ~author ~media ?wbcss () =
  wrap db
    (fun db ->
       (match wbcss with
          | None ->
              new_wikibox ~db ~wiki ~author ~content:""
                ~comment:("CSS for " ^page_opt_to_string wiki page)
                ~content_type:Wiki_models.css_content_type ()
          | Some wb ->
              Lwt.catch
                (fun () -> get_wikibox_info wb >|= fun _ -> wb)
                (function
                   | Not_found -> Lwt.fail (Unknown_Css wb)
                   | e -> Lwt.fail e
                )
       ) >>= fun wikibox ->
       add_css_wikibox_aux ~db ~wiki ~page ~media wikibox >|= fun () ->
       wikibox
    )



let remove_css_wiki ?db ~wiki =
  remove_css_wikibox_aux ?db ~wiki ~page:None

let remove_css_wikipage ?db ~wiki ~page =
  remove_css_wikibox_aux ?db ~wiki ~page:(Some page)




(*
let set_css_aux ?db ~wiki ~page ~author content =
  wrap db
    (fun db ->
       get_css_wikibox ~wiki ~page >>= function
         | None ->
             (* If the CSS must be deleted ([content=None]) and no css currently
                exists, we do simply do nothing. (Alternatively, we could create
                a box and set its content to NULL, but this does not seem really
                useful.) *)
             (match content with
                | None -> Lwt.return ()
                | Some content ->
                    new_wikibox ~db ~wiki ~author ~content
                      ~comment:("CSS for " ^page_opt_to_string wiki page)
                      ~content_type:Wiki_models.css_content_type ()
                    >>= fun wikibox ->
                    add_css_wikibox_aux ~db ~wiki ~page wikibox
             )
         | Some wb ->
             update_wikibox ~db ~author ~comment:""
               ~content ~content_type:Wiki_models.css_content_type wb
             >>= fun _ -> Lwt.return ()
    )

let set_css_for_wikipage ?db ~wiki ~page ~author content =
  let page = Ocsigen_lib.remove_end_slash page in
  set_css_aux ?db ~wiki ~page:(Some page) ~author content

let set_css_for_wiki ?db ~wiki ~author content =
  set_css_aux ?db ~wiki ~page:None ~author content
*)




(* Those two functions are used to make major changes to the database,
   and clear violently some of the caches *)

let update_wikiboxes ?db f =
  wrap db
    (fun db ->
       PGSQL(db) "SELECT version, wikibox, content, content_type
                  FROM wikiboxescontent
                  WHERE content_type='wikicreole'"
       >>= fun l ->
       Lwt_util.iter_serial
         (fun (version, wb, content, ct) ->
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
         >>= fun () -> Ocsigen_messages.console2 "Done updating wikiboxes";
         cache_wb_content#clear ();
         Lwt.return ()
    )


let rewrite_wikipages ?db ~oldwiki ~newwiki ~path =
  wrap db
    (fun db ->
       let oldwiki = sql_of_wiki oldwiki
       and newwiki = sql_of_wiki newwiki in
       PGSQL(db) "SELECT * FROM wikipages WHERE wiki=$oldwiki" >>= fun l ->
         Lwt_util.iter
           (fun (_, _wb, page, _, uid) ->
              match Ocsimore_lib.remove_prefix ~s:page ~prefix:path with
                | None -> Lwt.return ()
                | Some prefix ->
                    let prefix = Ocsimore_lib.remove_begin_slash prefix in
                    PGSQL(db) "UPDATE wikipages
                               SET wiki = $newwiki, pagename= $prefix
                               WHERE uid=$uid"
           ) l >>= fun () ->
           cache_wp#clear ();
           Ocsigen_messages.console2 "Done updating wikipages";
           Lwt.return ()
    )
