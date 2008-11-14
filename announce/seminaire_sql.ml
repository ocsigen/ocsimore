(*-*-coding: utf-8;-*-*)

open CalendarLib

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

open Event_sql.Event

(***)

(*XXX Quote the strings *)
let cat_pattern category =
  if category = [""] then "%" else String.concat "/" category ^ "/%"

let find_in_interval category start finish =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, version, last_updated, category, status, start, finish,
          room, location, title, description
   from announcement.event, announcement.category
   where start < $finish :: timestamp and finish > $start :: timestamp
     and event.category = category.id
     and path like $pat
   order by start") >>=
  Event_sql.make_events

let find_after category date =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, version, last_updated, category, status, start, finish,
          room, location, title, description
   from announcement.event, announcement.category
   where start >= $date :: timestamp
     and event.category = category.id
     and path like $pat
   order by start") >>=
  Event_sql.make_events

let find_before category date count =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, version, last_updated, category, status, start, finish,
          room, location, title, description
   from announcement.event, announcement.category
   where finish <= $date :: timestamp
     and event.category = category.id
     and path like $pat
   order by start desc limit $count") >>=
  Event_sql.make_events

let archive_start_date category =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select min(start :: timestamp)
      from announcement.event, announcement.category
      where event.category = category.id and path like $pat"))

(****)

let find_talk_categories () =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL (dbh)
  "select id, path, name from announcement.category order by path")

(*
let insert_talk category start finish room location
                speaker affiliation title abstract =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh) "insert
              into announcement.event (start, finish, category, room, location,
                                       title, description)
              values ($start,$finish,$category,$room,'',$title,$abstract)"
      >>= fun () ->
  PGOCaml.serial4 dbh "announcement.event_id_seq" >>= fun event ->
  PGSQL(dbh)
  "insert into announcement.talk (event, speaker, affiliation)
   values ($event, $speaker, $affiliation)"
      >>= fun () ->
  Lwt.return event)

let update_talk id start finish room location
                speaker affiliation title abstract =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "update announcement.event
   set start = $start, finish = $finish, room = $room, location = $location,
       title = $title, description = $abstract
   where id = $id" >>= fun () ->
  PGSQL(dbh)
  "update announcement.talk
   set speaker = $speaker, affiliation = $affiliation
   where event = $id")

let delete_talk id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "delete from announcement.talk where event = $id" >>= fun () ->
  PGSQL(dbh)
  "delete from announcement.event where id = $id")
*)
