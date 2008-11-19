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
      description : int32 }

  type cat =
    { cat_id : int32;
      cat_name : string;
      cat_path : string list;
      cat_desc : int32;
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
    description = d }

let make_events filter l =
  Lwt.return (List.filter filter (List.map make_event l))

let make_category (i, n, p, d, e) =
  Lwt.return
    {cat_id = i; cat_name = n; cat_desc = d; cat_editable = e;
     cat_path = Str.split (Str.regexp_string "/") p }

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

let insert_text wiki author comment content =
  Wiki.new_wikibox ~content_type:Wiki_sql.Wiki
     ~wiki ~author ~comment ~content ()

let insert_event wiki author
                 category start finish room location
                 persons title desc comment status =
  let status = int_of_status status in
  insert_text wiki author comment desc >>= fun desc ->
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
  Lwt_util.iter_serial
    (fun person ->
       PGSQL(dbh)
       "insert into announcement.event_person (event, person)
        values ($event, $person)")
    persons >>= fun () ->
  Lwt.return event)

