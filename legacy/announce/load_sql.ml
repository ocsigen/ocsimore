(*-*-coding: utf-8;-*-*)


let insert_text wiki author content =
  Lwt_unix.run
    (Wiki_data.new_wikibox ~content_type:Wiki_sql.WikiCreole
       ~wiki:wiki.Wiki_sql.Types.wiki_id ~author ~comment:"" ~content ())

let insert_person dbh name affiliation =
  PGSQL(dbh) "insert
              into announcement.person (name, affiliation)
              values ($name, $affiliation)";
  PGOCaml.serial4 dbh "announcement.person_id_seq"

let insert dbh category start finish speaker_id title abstract =
  let room = "" in
  PGSQL(dbh) "insert
              into announcement.event
                   (minor_version, major_version, last_updated, start, finish,
                    category, room, location, status, title, description)
              values (0, 0, 'now', $start :: timestamp, $finish :: timestamp,
                      $category, $room, '', 0, $title, $abstract)";
  PGOCaml.serial4 dbh "announcement.event_id_seq"

let insert_speaker dbh event person =
  PGSQL (dbh) "insert
               into announcement.event_person (event, person)
               values ($event, $person);"

let add_category dbh wiki author
    (name, path, editable, time, duration, room, location, description) =
  let desc = insert_text wiki author description in
  match time with
    Some time ->
      PGSQL(dbh)
      "insert
       into announcement.category
            (name, path, editable, time, duration, room, location, description)
       values ($name, $path, $editable, $time, $duration, $room, $location, $desc)"
  | None ->
      PGSQL(dbh)
      "insert
       into announcement.category
            (name, path, editable, duration, room, location, description)
       values ($name, $path, $editable, $duration, $room, $location, $desc)"

let create_categories dbh wiki author =
  List.iter (add_category dbh wiki author)
    [("Annonces", "", false, None, 0l, "", "", "");
     ("Exposés", "exposes/", false, None, 0l, "", "", "");
     ("Séminaire PPS", "exposes/seminaire/", true,
      Some (CalendarLib.Time.make 11 0 0), 90l, "0C02", "Chevaleret",
"\
Jeudi à 11h,\\\\
175 rue du Chevaleret, Paris 13ème.

Responsables: Roberto Amadio et Claudia Faggian.\\\\
Contactez [[mailto:amadio@pps.jussieu.fr|Roberto Amadio]] pour être
inscrit à la liste de diffusion du séminaire.\
")];
  (* HACK: last is seminar *)
  PGOCaml.serial4 dbh "announcement.category_id_seq"
