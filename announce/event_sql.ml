(*-*-coding: utf-8;-*-*)

let (>>=) = Lwt.(>>=)

module PGOCaml = Common_sql.PGOCaml

(***)

let find_event id =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  Common_sql.unique_row
    (PGSQL(dbh)
     "select category, start, finish, room, title, description
      from announcement.event where id = $id")) >>= fun (c, s, f, r, t, d) ->
  Lwt.return (c, s, f, r, t, d)

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
