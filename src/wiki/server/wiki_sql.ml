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

open Eliom_lib
open Opaque
open Lwt
open Ocsi_sql


open Wiki_types

let sql_to_wikipage i : wikipage_uid = Opaque.int32_t i

let wrap db f = match db with
  | None -> full_transaction_block f
  | Some db -> f db


(** Wikiboxes *)

let wikiboxes_version_seq = <:sequence< serial "wikiboxes_version_seq" >>
let wikiboxindex_uid_seq = <:sequence< serial "wikiboxindex_uid_seq" >>
let css_uid_seq = (<:sequence< serial "css_uid_seq" >>)

let wikiboxindex = <:table< wikiboxindex (
  wiki integer NOT NULL,
  comment text,
  specialrights boolean NOT NULL DEFAULT(false),
  uid integer NOT NULL DEFAULT(nextval $wikiboxindex_uid_seq$)
) >>

let wikiboxescontent = <:table< wikiboxescontent (
  version integer NOT NULL DEFAULT(nextval $wikiboxes_version_seq$),
  comment text NOT NULL DEFAULT(""),
  author integer NOT NULL,
  content text,
  datetime timestamp NOT NULL DEFAULT(localtimestamp ()),
  content_type text NOT NULL DEFAULT("wikicreole"),
  wikibox integer NOT NULL,
  ip text
) >>

(** CSS *)
let css = (<:table< css (
  wiki integer NOT NULL,
  page text,
  wikibox integer NOT NULL,
  specialrights boolean NOT NULL DEFAULT(false),
  uid integer NOT NULL DEFAULT(nextval $css_uid_seq$),
  rank integer NOT NULL DEFAULT(1),
  mediatype text NOT NULL DEFAULT("all")
) >>)

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
       Lwt_Query.query db (<:insert< $wikiboxindex$ := {
         wiki = $int32:wiki'$;
         comment = $string:comment$;
         specialrights = wikiboxindex?specialrights;
         uid = wikiboxindex?uid
       } >>)
       >>= fun () ->
       Lwt_Query.value db (<:value< currval $wikiboxindex_uid_seq$ >>)
       >>= fun boxid ->
       Lwt_Query.query db (<:insert< $wikiboxescontent$ := {
         version = wikiboxescontent?version;
         comment = wikiboxescontent?comment;
         author = $int32:author$;
         content = $string:content$;
         datetime = wikiboxescontent?datetime;
         content_type = $string:content_type$;
         wikibox = $int32:boxid$;
         ip = ""
       } >>)
       >>= fun () ->
       Lwt.return (wikibox_of_sql boxid)
    )

let current_wikibox_version_  wb =
  let wikibox = sql_of_wikibox wb in
  Ocsi_sql.view_one (<:view< group {
    version = max[w.version]
  } | w in $wikiboxescontent$; w.wikibox = $int32:wikibox$ >>)
  >>= fun v ->
  Lwt.return v#?version

(** Inserts a new version of an existing wikibox in a wiki
    and return its version number. *)
let update_wikibox_ ?db ~old_version ~author ~comment ~content ~content_type ?ip wb =
  let content_type = string_of_content_type content_type in
  wrap db
    (fun db ->
       let wikibox = sql_of_wikibox wb
       and author = User_sql.Types.sql_from_userid author in
       current_wikibox_version_ wb >>= function
         | Some version when version = old_version ->
             Lwt_Query.query db (<:insert< $wikiboxescontent$ := {
               version = wikiboxescontent?version;
               comment = $string:comment$;
               author = $int32:author$;
               content = of_option $Option.map Sql.Value.string content$;
               datetime = wikiboxescontent?datetime;
               content_type = $string:content_type$;
               wikibox = $int32:wikibox$;
               ip = of_option $Option.map Sql.Value.string ip$
             } >>)
             >>= fun () ->
             Lwt_Query.value db (<:value< currval $wikiboxes_version_seq$ >>)
         | _ -> Lwt.return old_version
    )

(** Returns the content of a wikibox, or [None] if the wikibox or version
    does not exists *)
let get_wikibox_content_ ?version wb =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
       let wikibox = sql_of_wikibox wb in
       (match version with
         | None ->
           Lwt_Query.view_one db (<:view< group {
             version = max[w.version]
           } | w in $wikiboxescontent$;
               w.wikibox = $int32:wikibox$ >>)
           >>= fun version ->
           Lwt.return version#?version
         | Some version ->
           Lwt.return (Some version)
       ) >>= (function
         | Some version ->
           Lwt_Query.view_opt db (<:view< {
             w.comment;
             w.author;
             w.content;
             w.datetime;
             w.content_type;
             w.version
           } | w in $wikiboxescontent$; w.wikibox = $int32:wikibox$;
               w.version = $int32:version$ >>)
         | None -> Lwt.return None
        ) >>= function
         | None -> Lwt.return None
         | Some first ->
             Lwt.return (Some (first#!comment,
                               User_sql.Types.userid_from_sql first#!author,
                               first#?content, first#!datetime,
                               first#!content_type, first#!version))
    )


let get_wikiboxes_by_wiki wiki =
  let wiki = sql_of_wiki wiki in
  Ocsi_sql.view (<:view< {
    w.uid
  } | w in $wikiboxindex$; w.wiki = $int32:wiki$ >>)
  >>= Lwt_list.map_p (fun elm -> Lwt.return (Wiki_types.wikibox_of_sql elm#!uid))

(*
let get_wikiboxes_by_wiki' wiki =
  Lwt_pool.use Ocsi_sql.pool
    (fun db ->
      let wiki = sql_of_wiki wiki in
      PGSQL (db) "SELECT wikibox, version, author, datetime, content_type, content, comment
                  FROM (SELECT wci.wikibox, max(wci.version) AS version
                        FROM (SELECT c.* FROM wikiboxescontent c INNER JOIN wikiboxindex i ON c.wikibox = i.uid WHERE i.wiki = $wiki) AS wci
                        GROUP BY wci.wikibox) AS newest
                  INNER JOIN wikiboxescontent
                  USING (wikibox, version)
                  ORDER BY wikibox")
      >|= List.map (function
          (wikibox, Some version, author, datetime, typ, content, comment) ->
            (Wiki_types.wikibox_of_sql wikibox, version, User_sql.Types.userid_from_sql author,
             datetime, Wiki_types.content_type_of_string typ, content, comment)
         | _ -> assert false)  (* FIXME in the above SQL-query ... *)
*)

(* Not cached : too rare, and too big *)
let get_wikibox_history ~wb =
  let wikibox = sql_of_wikibox wb in
  Ocsi_sql.view (<:view< {
    w.version;
    w.comment;
    w.author;
    w.datetime
  } order by {
    w.version
  } desc | w in $wikiboxescontent$;
           w.wikibox = $int32:wikibox$ >>)

(** Wikiboxes metadata *)

let get_wikibox_info_ wb =
  let wb' = sql_of_wikibox wb in
  Ocsi_sql.view_opt (<:view< {
    w.wiki;
    w.comment;
    w.specialrights;
  } | w in $wikiboxindex$; w.uid = $int32:wb'$ >>)
  >>= function
    | None -> Lwt.fail Not_found
    | Some first ->
        Lwt.return {
          wikibox_wiki = wiki_of_sql (first#!wiki);
          wikibox_id = wb;
          wikibox_comment = first#?comment;
          wikibox_special_rights = first#!specialrights;
        }

let set_wikibox_special_rights_ ?db wb v =
  wrap db
    (fun db ->
       let wb = sql_of_wikibox wb in
       Lwt_Query.query db (<:update< w in $wikiboxindex$ := {
         specialrights = $bool:v$
       } | w.uid = $int32:wb$ >>)
    )

(** Wikipages *)

let wikipages_uid_seq = (<:sequence< serial "wikipages_uid_seq" >>)

let wikipages = (<:table< wikipages (
  wiki integer NOT NULL,
  wikibox integer NOT NULL,
  pagename text NOT NULL DEFAULT(""),
  title text,
  uid integer NOT NULL DEFAULT(nextval $wikipages_uid_seq$)
) >>)

(** return the box corresponding to a wikipage *)
let get_box_for_page_ ~wiki ~page =
  let wiki' = t_int32 (wiki : wiki) in
  Ocsi_sql.view_opt (<:view< w |
      w in $wikipages$;
      w.wiki = $int32:wiki'$; w.pagename = $string:page$ >>)
  >>= function
    | None -> Lwt.fail Not_found
    | Some first ->
        (* (wiki, pagename) is a primary key *)
        Lwt.return {
          wikipage_wiki = wiki;
          wikipage_page = page;
          wikipage_wikibox = (int32_t (first#!wikibox));
          wikipage_title = first#?title;
          wikipage_uid = sql_to_wikipage (first#!uid);
        }

let wikis_id_seq = (<:sequence< serial "wikis_id_seq" >>)

let wikis = (<:table< wikis (
  id integer NOT NULL DEFAULT(nextval $wikis_id_seq$),
  title text NOT NULL DEFAULT(""),
  descr text NOT NULL DEFAULT(""),
  pages text,
  boxrights boolean NOT NULL,
  container integer,
  staticdir text,
  model text NOT NULL DEFAULT("wikicreole"),
  siteid text,
  deleted boolean NOT NULL DEFAULT(false)
) >>)

let get_wikis () =
  Ocsi_sql.view (<:view< {
    w.id
  } | w in $wikis$ >>)
  >>= Lwt_list.map_p (fun elm ->
    Lwt.return (Wiki_types.wiki_of_sql elm#!id)
  )

(* No need for cache, as the page does not exists yet *)
let create_wikipage ?db ~wiki ~page ~wb =
  let page = Eliom_lib.Url.remove_end_slash page in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and wb = t_int32 (wb : wikibox) in
       Lwt_Query.query db (<:insert< $wikipages$ := {
         wiki = $int32:wiki$;
         wikibox = $int32:wb$;
         pagename = $string:page$;
         title = null;
         uid = wikipages?uid
       } >>)
    )

let set_wikipage_properties_ ?db ~wiki ~page ?title ?newpage ?wb () =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       (match title with
          | None -> Lwt.return ()
          | Some s ->
            Lwt_Query.query db (<:update< w in $wikipages$ := {
              title = if $bool:s = ""$ then null else $string:s$;
            } | w.wiki = $int32:wiki$; w.pagename = $string:page$ >>)
       ) >>= fun () ->
       (match newpage with
          | None -> Lwt.return ()
          | Some new_page ->
            Lwt_Query.query db (<:update< w in $wikipages$ := {
              pagename = $string:new_page$
            } | w.wiki = $int32:wiki$; w.pagename = $string:page$ >>)
       ) >>= fun () ->
       (match wb with
          | None -> Lwt.return ()
          | Some None ->
            Lwt_Query.query db (<:delete< w in $wikipages$ |
                w.wiki = $int32:wiki$; w.pagename = $string:page$ >>)
          | Some (Some wb) ->
              let wb = sql_of_wikibox wb in
            Lwt_Query.query db (<:update< w in $wikipages$ := {
              wikibox = $int32:wb$
            } | w.wiki = $int32:wiki$; w.pagename = $string:page$ >>)
       )
    )


(** Wikis *)

let reencapsulate_wiki data =
  { wiki_id = wiki_of_sql data#!id;
    wiki_title = data#!title;
    wiki_descr = data#!descr;
    wiki_pages = data#?pages;
    wiki_boxrights = data#!boxrights;
    wiki_container =
      (match data#?container with None -> None | Some ci -> Some (wikibox_of_sql ci));
    wiki_staticdir = data#?staticdir;
    wiki_model = Wiki_types.wiki_model_of_string (data#!model);
    wiki_siteid = data#?siteid;
    wiki_deleted = data#!deleted;
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
              Lwt_Query.query db (<:update< w in $wikis$ := {
                container = of_option $Option.map Sql.Value.int32 container$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match staticdir with
          | None -> Lwt.return ()
          | Some staticdir ->
              Lwt_Query.query db (<:update< w in $wikis$ := {
                staticdir = of_option $Option.map Sql.Value.string staticdir$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match path with
          | None -> Lwt.return ()
          | Some pages ->
              Lwt_Query.query db (<:update< w in $wikis$ := {
                pages = of_option $Option.map Sql.Value.string pages$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match descr with
          | None -> Lwt.return ()
          | Some descr ->
              Lwt_Query.query db (<:update< w in $wikis$ := {
                descr = $string:descr$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match boxrights with
          | None -> Lwt.return ()
          | Some boxrights ->
              Lwt_Query.query db (<:update< w in $wikis$ := {
                boxrights = $bool:boxrights$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match model with
          | None -> Lwt.return ()
          | Some model ->
              let model = string_of_wiki_model model in
              Lwt_Query.query db (<:update< w in $wikis$ := {
                model = $string:model$
              } | w.id = $int32:wiki$ >>)
       ) >>= fun () ->
       (match siteid with
          | None -> Lwt.return ()
          | Some siteid ->
              Lwt_Query.query db (<:update< w in $wikis$ := {
                siteid = of_option $Option.map Sql.Value.string siteid$
              } | w.id = $int32:wiki$ >>)
       )
    )

let delete_wiki_ ~delete id =
  wrap None
    (fun db ->
      Lwt_Query.query db (<:update< w in $wikis$ := {
        deleted = $bool:delete$;
      } | w.id = $int32:id$ >>)
    )

let find_wiki_ ?db id =
  wrap db
    (fun db ->
       let id = t_int32 (id : wiki) in
       Lwt_Query.view_opt db (<:view< w | w in $wikis$; w.id = $int32:id$ >>)
       >>= function
         | Some c -> Lwt.return (reencapsulate_wiki c)
         | None -> Lwt.fail Not_found
    )


let find_wiki_by_name_ ?db name =
  wrap db
    (fun db ->
       Lwt_Query.view_opt db (<:view< w | w in $wikis$; w.title = $string:name$ >>)
       >>= function
         | Some c -> Lwt.return (reencapsulate_wiki c)
         | None -> Lwt.fail Not_found
    )

let find_wiki_by_pages_ ?db page =
  wrap db
    (fun db ->
      Lwt_Query.view_opt db (<:view< w | w in $wikis$; w.pages = $string:page$ >>)
      >>= function
        | Some c -> Lwt.return (reencapsulate_wiki c)
        | None -> Lwt.fail Not_found
    )

let iter_wikis ?db ?(deleted=false) f =
  wrap db
    (fun db ->
       Lwt_Query.view db (<:view< w |
           w in $wikis$;
           w.deleted = $bool:deleted$ >>)
       >>= fun l ->
       Lwt_list.iter_p (fun wiki_info -> f (reencapsulate_wiki wiki_info)) l)




(** CSS *)

let page_opt_to_string wiki = function
  | None -> "wiki " ^ string_of_wiki wiki
  | Some page -> "page " ^ page ^ " of wiki " ^ string_of_wiki wiki

let get_css_wikibox_aux_ ?db ~wiki ~page () =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       Lwt_Query.view db (<:view< {
         c.wikibox;
         c.mediatype;
         c.rank
       } order by {
         c.rank
       } | c in $css$; c.wiki = $int32:wiki$;
           is_not_distinct_from c.page (of_option $Option.map Sql.Value.string page$)>>)
       >>= (Lwt_list.map_p (fun data ->
         let media = media_type_of_string (data#!mediatype) in
         Lwt.return {
           wikibox = wikibox_of_sql data#!wikibox;
           media = media;
           rank = data#!rank;
         }
       ))
    )


let add_css_wikibox_aux_ ?db ~wiki ~page ~media wb =
  let media = string_of_media_type media in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and wb = sql_of_wikibox wb in
       Lwt_Query.view_one db (<:view< group {
         rank = match max[c.rank] + 1 with null -> 1 | r -> r;
       } | c in $css$; c.wiki = $int32:wiki$;
           is_not_distinct_from c.page (of_option $Option.map Sql.Value.string page$) >>)
       >>= fun rank ->
       Lwt_Query.query db (<:insert< $css$ := {
         wiki = $int32:wiki$;
         page = of_option $Option.map Sql.Value.string page$;
         wikibox = $int32:wb$;
         specialrights = css?specialrights;
         uid = css?uid;
         mediatype = $string:media$;
         rank = $rank#rank$;
       } >>)
    )

let remove_css_wikibox_aux_ ?db ~wiki ~page wb =
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki) in
       let wb = sql_of_wikibox wb in
       Lwt_Query.query db (<:delete< c in $css$ |
           c.wiki = $int32:wiki$;
           is_not_distinct_from c.page (of_option $Option.map Sql.Value.string page$);
           c.wikibox = $int32:wb$ >>)
    )

let update_css_wikibox_aux_ ?db ~wiki ~page ~oldwb ~newwb ~media ~rank () =
  let media = string_of_media_type media in
  wrap db
    (fun db ->
       let wiki = t_int32 (wiki : wiki)
       and oldwb = sql_of_wikibox oldwb
       and newwb = sql_of_wikibox newwb in
       Lwt_Query.query db (<:update< c in $css$ := {
         wikibox = $int32:newwb$;
         mediatype = $string:media$;
         rank = $int32:rank$
       } | c.wiki = $int32:wiki$;
           is_not_distinct_from c.page (of_option $Option.map Sql.Value.string page$);
           c.wikibox = $int32:oldwb$ >>)
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

let update_wikibox ?db ~old_version ~author ~comment ~content ~content_type ?ip wb =
  cache_wb_content#remove wb;
  cache_wb_version#remove wb;
  update_wikibox_ ?db ~old_version ~author ~comment ~content ~content_type ?ip wb



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
     get_box_for_page_ ~wiki ~page) 64


let get_wikipage_info ~wiki ~page =
  let page = Eliom_lib.Url.remove_end_slash page in
  cache_wp#find (wiki, page)

let set_wikipage_properties ?db ~wiki ~page ?title ?newpage ?wb () =
  let page = Eliom_lib.Url.remove_end_slash page in
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

let cache_wiki_pages =
  let module CWN = Ocsigen_cache.Make(struct
                                type key = string
                                type value = wiki_info
                              end)
  in
  new CWN.cache find_wiki_by_pages_ 8


let get_wiki_info_by_id ~id = cache_wiki_id#find id
let get_wiki_info_by_name ~name = cache_wiki_name#find name
let get_wiki_info_by_pages ~pages = cache_wiki_pages#find pages

let update_wiki ?db ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki =
  cache_wiki_id#remove wiki;
  (* A bit drastic, but search by name (and update_wiki) are rarely
     used anyway. The alternative is to find the the id of the
     wiki, and to remove only this key *)
  cache_wiki_name#clear ();
  update_wiki_ ?db ?container ?staticdir ?path ?descr ?boxrights ?model ?siteid wiki

let delete_wiki ?(delete=true) wiki =
  let id = sql_of_wiki wiki in
  (** Clear the cache before deleting *)
  cache_wiki_id#remove wiki;
  cache_wiki_name#clear ();
  cache_wiki_pages#clear ();
  delete_wiki_ ~delete id



(** Functions related to css. Since css are stored in wikiboxes,
   we only cache the association (wiki, page) -> wikibox *)

let cache_css =
  let module C = Ocsigen_cache.Make (struct
                               type key = (wiki * string option)
                               type value = Wiki_types.css_wikibox list
                             end)
  in
  new C.cache (fun (wiki, page) -> get_css_wikibox_aux_ ~wiki ~page ()) 64


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
  let page = Eliom_lib.Url.remove_end_slash page in
  get_css_wikibox ~wiki ~page:(Some page)

let get_css_aux ~wiki ~page =
  get_css_wikibox ~wiki ~page >>= fun l ->
  Lwt_list.fold_left_s
    (fun l css_wb ->
       get_wikibox_content css_wb.wikibox
       >>= function
         | Some (_, _, Some content, _, _, ver) ->
             Lwt.return ((css_wb, (content, ver)) :: l)
         | Some (_, _, None, _, _, _) | None -> Lwt.return l
    ) [] l

let get_css_for_wikipage ~wiki ~page =
  let page = Eliom_lib.Url.remove_end_slash page in
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


let default_css_content =
  "body {\r\n\
       color: #333333;\r\n\
       font: 100% palatino,\"times new roman\",serif;\r\n\
       padding-left: 20%;\r\n\
       padding-right: 20%;\r\n\
   }"

let new_wiki ?db ~title ~descr ~pages ~boxrights ~staticdir ?container_text ~author ~model () =
  wrap db
    (fun db ->
       let model_sql = Wiki_types.string_of_wiki_model model in
       Lwt_Query.query db (<:insert< $wikis$ := {
         id = wikis?id;
         title = $string:title$;
         descr = $string:descr$;
         pages = of_option $Option.map Sql.Value.string pages$;
         boxrights = $bool:boxrights$;
         container = null;
         staticdir = of_option $Option.map Sql.Value.string staticdir$;
         model = $string:model_sql$;
         siteid = null;
         deleted = wikis?deleted;
       } >>)
       >>= fun () ->
       Lwt_Query.value db (<:value< currval $wikis_id_seq$ >>)
       >>= fun wiki_sql ->
       let wiki = wiki_of_sql wiki_sql in
       (* Create a default css *)
       add_css_aux ~db ~wiki ~author ~page:None ~media:[`All] ()
       >>= (fun wikibox ->
         Lwt_Query.query db (<:update< w in $wikiboxescontent$ := {
           content = $string:default_css_content$;
         } | w.wikibox = $int32:t_int32 wikibox$ >>)
       )
       >>= fun () ->
       (match container_text with
          | None -> Lwt.return None
          | Some content ->
              let comment = Printf.sprintf "Container box for wiki %ld"
                wiki_sql in
              lwt content_type = Wiki_models.get_default_content_type model in
              lwt container = new_wikibox ~db ~wiki ~author ~comment ~content ~content_type () in
              (* No problem wrt. wiki cache, as wiki is not yet cached *)
              update_wiki_ ~db ~container:(Some container) wiki >>= fun () ->
                Lwt.return (Some container)
       ) >>= fun container ->
       return (wiki, container)
    )




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
  let page = Eliom_lib.remove_end_slash page in
  set_css_aux ?db ~wiki ~page:(Some page) ~author content

let set_css_for_wiki ?db ~wiki ~author content =
  set_css_aux ?db ~wiki ~page:None ~author content
*)




(* Those two functions are used to make major changes to the database,
   and clear violently some of the caches *)

let update_wikiboxes ?db f =
  wrap db
    (fun db ->
      Lwt_Query.view db (<:view< {
        w.version;
        w.wikibox;
        w.content;
        w.content_type
      } | w in $wikiboxescontent$; w.content_type = "wikicreole" >>)
       >>= fun l ->
       Lwt_list.iter_s
         (fun data ->
            f ~wikibox:(wikibox_of_sql (data#!wikibox))
              ~version: (data#!version)
              ~content: (data#?content)
              ~content_type:(Wiki_types.content_type_of_string (data#!content_type))
            >>= function
              | None -> Lwt.return ()
              | Some s ->
                Lwt_Query.query db (<:update< w in $wikiboxescontent$ := {
                  content = $string:s$
                } | w.wikibox = $int32:data#!wikibox$;
                    w.version = $int32:data#!version$ >>)
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
       Lwt_Query.view db (<:view< w | w in $wikipages$;
                            w.wiki = $int32:oldwiki$ >>) >>= fun l ->
         Lwt_list.iter_p
           (fun data ->
              match Ocsimore_lib.remove_prefix ~s:(data#!pagename) ~prefix:path with
                | None -> Lwt.return ()
                | Some prefix ->
                    let prefix = Ocsimore_lib.remove_begin_slash prefix in
                    Lwt_Query.query db (<:update< w in $wikipages$ := {
                      wiki = $int32:newwiki$;
                      pagename = $string:prefix$
                    } | w.uid = $int32:data#!uid$ >>)
           ) l >>= fun () ->
           cache_wp#clear ();
           Ocsigen_messages.console2 "Done updating wikipages";
           Lwt.return ()
    )

let get_wikipages_of_a_wiki ~wiki () =
  let wiki = sql_of_wiki wiki in
  Lwt_pool.use Ocsi_sql.pool (fun db ->
    Lwt_Query.view db
      (<:view< w | w in $wikipages$; w.wiki = $int32:wiki$ >>)
  )

let get_wikis_id () =
  Ocsi_sql.view (<:view< {
    id = w.id;
    title = nullable w.title;
  } | w in $wikis$ >>)

let get_wikiboxes_id () =
  Ocsi_sql.view (<:view< {
    id = w.uid;
    title = null;
  } | w in $wikiboxindex$ >>)

let get_wikipages_id () =
  Ocsi_sql.view (<:view< {
    id = w.uid;
    title = w.title;
  } | w in $wikipages$ >>)
