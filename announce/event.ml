(*-*-coding: utf-8;-*-*)

let (>>=) = Lwt.bind
module P = Eliom_parameters
module M = Eliom_duce.Xhtml
open CalendarLib

let str = Ocamlduce.Utf8.make

(****)

(*XXX*)
let rec syntactic_conjunction l = String.concat " et " l

(****)

let format_date d =
  Common.set_french_date_names ();
  String.capitalize (Printer.Calendar.sprint "%A %d %B" (Common.local_time d))

let format_date_num d =
  Printer.Calendar.sprint "%d/%m/%y" (Common.local_time d)

let format_speaker speaker affiliation =
  speaker ^
  (if affiliation = "" || affiliation = "PPS" then "" else
   " (" ^ affiliation ^ ")")

let format_speakers speakers =
  syntactic_conjunction
    (List.map
       (fun (speaker, affiliation) -> format_speaker speaker affiliation)
       speakers)

let format_date_and_speakers d speakers =
  format_date d ^
  if speakers = [] then "" else (" — " ^ format_speakers speakers)

let format_description sp sd desc =
  Ocsisite.wikibox#noneditable_wikibox ~sp ~sd ~data:(Common.wiki_id, desc)
    ~ancestors:Wiki_syntax.no_ancestors ()

let format_location location room =
  match room, location with
    "", _ -> location
  | _, "" -> room
  | _     -> room ^ ", " ^ location

(****)

let events =
  let path = ["events"] in
  M.register_new_service
    ~path ~get_params:(P.suffix (P.string "id"))
    (fun sp id () ->
       (*XXX Validate *)
       let id = Int32.of_string id in
       Event_sql.find_event id
           >>= fun (cat_id, date, _, room, title, abstract) ->
       Event_sql.find_speakers id >>= fun speakers ->
       Event_sql.find_category cat_id >>= fun (category, talk_category) ->
(*XXX ???       feed_links sp category >>= fun l -> *)
       let speaker_frag =
         if speakers = [] then {{ [] }} else
         {{ [ !{:str (format_speakers speakers):} <br>[] ] }}
       in
       let speaker_title =
         if speakers = [] then "" else
         syntactic_conjunction (List.map fst speakers) ^ " - "
       in
       Common.wiki_page path sp {{ [] }}
         (fun sp sd ->
            format_description sp sd abstract
               >>= fun abstract ->
            Lwt.return
              (str (speaker_title ^ title),
               {{ [ <h1>[!(str talk_category)]
                    <h2>[!(str (format_date_num date))
                         <br>[]
                         !speaker_frag
                         !(str title)]
                    abstract ] }})))
