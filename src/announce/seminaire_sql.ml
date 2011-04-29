(*-*-coding: utf-8;-*-*)

open CalendarLib

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

open Event_sql.Event

(***)

(*XXX Quote the strings *)
let cat_pattern category =
  if category = [""] then "%" else String.concat "/" category ^ "/%"

let find_in_interval filter category start finish =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, minor_version, major_version,
          last_updated, category, status, start, finish,
          event.room, event.location, title, event.description
   from announcement.event, announcement.category
   where start < $finish :: timestamp and finish > $start :: timestamp
     and event.category = category.id
     and path like $pat
   order by start") >>=
  Event_sql.make_events filter

let find_after filter category date =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, minor_version, major_version,
          last_updated, category, status, start, finish,
          event.room, event.location, title, event.description
   from announcement.event, announcement.category
   where start >= $date :: timestamp
     and event.category = category.id
     and path like $pat
   order by start") >>=
  Event_sql.make_events filter

let find_before category date count show_all =
  let pat = cat_pattern category in
  let max_status =
    if show_all then Event_sql.max_status else Event_sql.visible_status
  in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select event.id, minor_version, major_version,
          last_updated, category, status, start, finish,
          event.room, event.location, title, event.description
   from announcement.event, announcement.category
   where finish <= $date :: timestamp
     and event.category = category.id
     and path like $pat
     and status <= $max_status
   order by start desc limit $count") >>=
  Event_sql.make_events (fun _ -> true)

let find_category_defaults cat =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select time, duration, room, location
      from announcement.category where id = $cat"))

let archive_start_date category =
  let pat = cat_pattern category in
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select min(start :: timestamp)
      from announcement.event, announcement.category
      where event.category = category.id and path like $pat"))

(****)

let find_categories () =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL (dbh)
  "select id, path, name from announcement.category order by path")

(*
let insert_talk category start finish room location
                speaker affiliation title abstract =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh) "insert
              into announcement.event (start, finish, category, room, location,
                                       title, event.description)
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
       title = $title, event.description = $abstract
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
