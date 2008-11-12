(*-*-coding: utf-8;-*-*)


let insert_person dbh name affiliation =
  PGSQL(dbh) "insert
              into announcement.person (name, affiliation)
              values ($name, $affiliation)";
  PGOCaml.serial4 dbh "announcement.person_id_seq"

let insert dbh category start finish speaker_id title abstract =
  let room = "" in
  PGSQL(dbh) "insert
              into announcement.event (version, last_updated, start, finish,
                                       category, room, location,
                                       status, title, description)
              values (0, 'now', $start :: timestamp, $finish :: timestamp,
                      $category, $room, '', 0, $title, $abstract)";
  PGOCaml.serial4 dbh "announcement.event_id_seq"

let insert_speaker dbh event person =
  PGSQL (dbh) "insert
               into announcement.event_person (event, person)
               values ($event, $person);"

let create_category dbh =
  PGSQL(dbh)
  "insert into announcement.category (name, path, kind)
   values ('Annonces', '', 0)";
  PGSQL(dbh)
  "insert into announcement.category (name, path, kind)
   values ('Exposés', 'exposes/', 0)";
  PGSQL(dbh)
  "insert into announcement.category (name, path, kind)
   values ('Séminaire PPS', 'exposes/seminaire/', 1)";
  PGOCaml.serial4 dbh "announcement.category_id_seq"
