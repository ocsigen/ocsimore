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
  Wiki_sql.get_wiki_info_by_id Common.wiki_id >>= fun wiki_info ->
  let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
  Wiki.default_bi ~sp ~wikibox:desc ~rights >>= fun bi ->
  Wiki_site.wikibox_widget#display_frozen_wikibox bi desc

let format_location location room =
  match room, location with
    "", _ -> location
  | _, "" -> room
  | _     -> room ^ ", " ^ location
