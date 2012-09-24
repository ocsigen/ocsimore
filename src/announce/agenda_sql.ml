(*-*-coding: utf-8;-*-*)

module PGOCaml = Common_sql.PGOCaml
let (>>=) = Lwt.bind

(****)

let find_events_in_interval start finish =
  Lwt_pool.use Common_sql.dbpool (fun dbh ->
  PGSQL(dbh)
  "select start, finish, event.id, name, event.room, event.location
   from announcement.event, announcement.category
   where start < $finish :: timestamp and finish > $start :: timestamp
     and event.category = category.id
   order by start") >>= fun l ->
   Lwt.return
     (List.map (fun (s, f, i, n, r, l) -> (s, f, i, n, r, l)) l)
