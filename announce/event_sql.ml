(*-*-coding: utf-8;-*-*)

open CalendarLib

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

(***)

module Event = struct
  type status =
    Confirmed | Tentative | Cancelled

  type t =
    { id : int32; version : int32; last_updated : Calendar.t;
      category : int32; status : status;
      start : PGOCaml.timestamptz; finish : PGOCaml.timestamptz;
      room : string; location : string; title : string; description : int32 }
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

let make_event (i, v, u, c, st, s, f, r, l, t, d) =
  { id = i; version = v; last_updated = u; category = c;
    status = state_of_int st;
    start = s; finish = f; room = r; location = l; title = t; description = d }

let make_events l = Lwt.return (List.map make_event l)

(****)

let find_event id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select event.id, version, last_updated, category, status, start, finish,
             room, location, title, description
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

let find_category_name name =
  let name =
    if name = [] || name = [""] then "" else  String.concat "/" name ^ "/" in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
    "select name from announcement.category where path = $name"))

let find_category id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select path, name from announcement.category where id = $id"))

let last_update () =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select max(last_updated) from announcement.event"))
