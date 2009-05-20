(*-*-coding: utf-8;-*-*)

let (>>=) = Lwt.bind
module P = Eliom_parameters
module M = Eliom_duce.Xhtml
open CalendarLib

let str = Ocamlduce.Utf8.make

open Event_sql.Event

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

let format_description sp desc =
  let wikibox = (Common.wiki_id, desc) in
  let bi = Wiki_widgets_interface.default_bi ~sp ~root_wiki:Common.wiki_id
    ~wikibox in
  Ocsisite.wikibox_widget#display_frozen_wikibox bi wikibox

let format_location location room =
  match room, location with
    "", _ -> location
  | _, "" -> room
  | _     -> room ^ ", " ^ location
