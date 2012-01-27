(*-*-coding: utf-8;-*-*)

open CalendarLib

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

(***)

module Event = struct
  type status =
    Confirmed | Tentative | Cancelled | Hidden

  type t =
    { id : int32; minor_version : int32; major_version : int32;
      last_updated : Calendar.t; category : int32; status : status;
      start : PGOCaml.timestamptz; finish : PGOCaml.timestamptz;
      room : string; location : string; title : string;
      description : Wiki_types.wikibox }

  type cat =
    { cat_id : int32;
      cat_name : string;
      cat_path : string list;
      cat_desc : Wiki_types.wikibox;
      cat_editable : bool }

end

open Event

let max_status = 39l
let visible_status = 19l

let status_of_int i =
  match i with
    0l  -> Confirmed
  | 10l -> Tentative
  | 20l -> Cancelled
  | 30l -> Hidden
  | _   -> assert false

let int_of_status s =
  match s with
    Confirmed -> 0l
  | Tentative -> 10l
  | Cancelled -> 20l
  | Hidden   -> 30l

let string_of_status s =
  match s with
    Confirmed -> "confirmed"
  | Tentative -> "tentative"
  | Cancelled -> "cancelled"
  | Hidden    -> "hidden"

let status_of_string s =
  match s with
    "confirmed" -> Confirmed
  | "tentative" -> Tentative
  | "cancelled" -> Cancelled
  | "hidden"    -> Hidden
  | _           -> assert false

let make_event (i, v, v', u, c, st, s, f, r, l, t, d) =
  { id = i; minor_version = v; major_version = v';
    last_updated = u; category = c; status = status_of_int st;
    start = s; finish = f; room = r; location = l; title = t;
    description = Wiki_types.wikibox_of_sql d }

let make_events filter l =
  Lwt.return (List.filter filter (List.map make_event l))

let make_category (i, n, p, d, e) =
  Lwt.return
    {cat_id = i; cat_name = n; cat_desc = Wiki_types.wikibox_of_sql d;
     cat_editable = e; cat_path = Str.split (Str.regexp_string "/") p }

(****)

let find_event id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select event.id, minor_version, major_version,
             last_updated, category, status, start, finish,
             room, location, title, description
      from announcement.event where id = $id")) >>= fun e ->
  Lwt.return (make_event e)

(*
let find_description id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select description from announcement.event where id = $id"))
*)

let find_speakers id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL (dbh)
  "select name, affiliation from announcement.event_person, announcement.person
   where event = $id and person = id")

let find_category_by_path name =
  let name =
    if name = [] || name = [""] then "" else  String.concat "/" name ^ "/" in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
    "select id, name, path, description, editable
     from announcement.category where path = $name")) >>=
  make_category

let find_category_by_id id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select id, name, path, description, editable
      from announcement.category where id = $id")) >>=
  make_category

let last_update () =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select max(last_updated) from announcement.event"))

let transaction pool f =
  Lwt_pool.use pool (fun dbh ->
  PGOCaml.begin_work dbh >>= fun () ->
  Lwt.catch
    (fun () ->
       f dbh >>= fun r ->
       PGOCaml.commit dbh >>= fun () ->
       Lwt.return r)
    (fun e ->
       PGOCaml.rollback dbh >>= fun () ->
       Lwt.fail e))

let insert_person name affiliation =
  transaction Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select id from announcement.person
   where name = $name and affiliation = $affiliation" >>= fun l ->
  match l with
    [] ->
      PGSQL(dbh) "insert
                  into announcement.person (name, affiliation)
                  values ($name, $affiliation)" >>= fun () ->
      PGOCaml.serial4 dbh "announcement.person_id_seq"
  | [id] ->
      Lwt.return id
  | _ :: _ :: _ ->
      assert false)

let insert_persons l =
  Lwt_util.map_serial (fun (n, a) -> insert_person n a) l

let insert_desc wiki author comment content =
  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
  let content_type = 
    Wiki_models.get_default_content_type wiki_info.Wiki_types.wiki_model 
  in
  Wiki_sql.new_wikibox ~wiki ~author ~comment ~content ~content_type ()

let insert_event_person dbh event persons =
  Lwt_util.iter_serial
    (fun person ->
       PGSQL(dbh)
       "insert into announcement.event_person (event, person)
        values ($event, $person)")
    persons

let insert_event
      wiki author
      category start finish room location
      persons title desc comment status =
  let status = int_of_status status in
  insert_desc wiki author comment desc >>= fun desc ->
  let desc = Wiki_types.sql_of_wikibox desc in
  transaction Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "insert
   into announcement.event
        (minor_version, major_version, last_updated, start, finish,
         category, room, location, status, title, description)
   values (0, 0, 'now', $start :: timestamp, $finish :: timestamp,
           $category, $room, $location, $status,
           $title, $desc)" >>= fun () ->
  PGOCaml.serial4 dbh "announcement.event_id_seq" >>= fun event ->
  insert_event_person dbh event persons >>= fun () ->
  Lwt.return event)

let update_desc author wb content_type comment content =
  Wiki_sql.update_wikibox ~author ~comment ~content ~content_type wb

let check_no_concurrent_update dbh ev =
  let id = ev.id in
  PGSQL(dbh)
  "select minor_version from announcement.event where id = $id" >>= fun l ->
  (*XXX Should catch errors properly *)
  assert
    (match l with
       [vers] -> vers < ev.minor_version
     | _      -> false);
  Lwt.return ()

let update_event author ev desc content_type comment persons =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  check_no_concurrent_update dbh ev) >>= fun () ->
  (*XXX Not atomic! *)
  update_desc author ev.description content_type comment desc >>= fun _ ->
  transaction Common_sql.dbpool (fun dbh ->
  let id = ev.id in
  let minor_version = ev.minor_version in
  let major_version = ev.major_version in
  let start = fst ev.start in
  let finish = fst ev.finish in
  let room = ev.room in
  let location = ev.location in
  let status = int_of_status ev.status in
  let title = ev.title in
  check_no_concurrent_update dbh ev >>= fun () ->
  PGSQL(dbh)
  "delete from announcement.event_person where event = $id" >>= fun () ->
  insert_event_person dbh id persons >>= fun () ->
  PGSQL(dbh)
  "update announcement.event
   set minor_version = $minor_version,
       major_version = $major_version,
       last_updated  = 'now',
       start = ($start :: timestamp),
       finish = ($finish :: timestamp),
       room = $room,
       location = $location,
       status = $status,
       title = $title
   where id = $id")
