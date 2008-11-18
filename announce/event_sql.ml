(*-*-coding: utf-8;-*-*)

open CalendarLib

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

(***)

module Event = struct
  type status =
    Confirmed | Tentative | Cancelled

  type t =
    { id : int32; minor_version : int32; major_version : int32;
      last_updated : Calendar.t; category : int32; status : status;
      start : PGOCaml.timestamptz; finish : PGOCaml.timestamptz;
      room : string; location : string; title : string;
      description : int32; comment : int32 }

  type cat =
    { cat_id : int32;
      cat_name : string;
      cat_path : string list;
      cat_desc : int32;
      cat_editable : bool }

end

open Event

let state_of_int i =
  match i with
    0l  -> Confirmed
  | 10l -> Tentative
  | 20l -> Cancelled
  | _   -> assert false

let int_of_state s =
  match s with
    Confirmed -> 0l
  | Tentative -> 10l
  | Cancelled -> 20l

let string_of_status s =
  match s with
    Confirmed -> "confirmed"
  | Tentative -> "tentative"
  | Cancelled -> "cancelled"

let status_to_string s =
  match s with
    "confirmed" -> Confirmed
  | "tentative" -> Tentative
  | "cancelled" -> Cancelled
  | _           -> assert false

let status_list =
  ["Confirmé", Confirmed;
   "À confirmer", Tentative;
   "Annulé", Cancelled]

let make_event (i, v, v', u, c, st, s, f, r, l, t, d, cm) =
  { id = i; minor_version = v; major_version = v';
    last_updated = u; category = c; status = state_of_int st;
    start = s; finish = f; room = r; location = l; title = t;
    description = d; comment = cm }

let make_events l = Lwt.return (List.map make_event l)

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
             room, location, title, description, comment
      from announcement.event where id = $id")) >>= fun e ->
  Lwt.return (make_event e)

let find_description id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select description from announcement.event where id = $id"))

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
