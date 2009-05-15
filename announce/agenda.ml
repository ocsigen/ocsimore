(*-*-coding: utf-8;-*-*)

module P = Eliom_parameters
module M = Eliom_duce.Xhtml

open CalendarLib

let ( ** ) = P.( ** )
let (>>=) = Lwt.(>>=)

let str = Ocamlduce.Utf8.make

include Agenda_sql
open Event_sql.Event

(****)

let same_day c1 c2 =
  let day_start = Calendar.create (Calendar.to_date c1) Common.midnight in
  let day_end = Calendar.next day_start `Day in
  Date.compare (Calendar.to_date day_start) (Calendar.to_date c2) <= 0
    &&
  Date.compare (Calendar.to_date c2) (Calendar.to_date day_end) <= 0

let format_time_interval start finish =
  let start = Common.local_time start in
  let finish = Common.local_time finish in
  Common.set_french_date_names ();
  if same_day start finish then
    Printer.Calendar.sprint "%A %d %B" start ^ " de " ^
    Printer.Calendar.sprint "%Hh%M" start ^ " à " ^
(*XXX regarde si second minuit, tous les deux minuits *)
    Printer.Calendar.sprint "%Hh%M" finish
  else
(*XXX omettre l'heure lorsqu'elle est minuit *)
    "du " ^ Printer.Calendar.sprint "%A %d %B à %Hh%M" start ^
    " au " ^ Printer.Calendar.sprint "%A %d %B à %Hh%M" finish

let format_month d =
  Common.set_french_date_names ();
  String.capitalize (Printer.Date.sprint "%B %Y" d)

(****)

let format_entry_short abs sp id title speakers =
  let speakers =
    match speakers with
      [] -> ""
    | _  -> Event.format_speakers speakers ^ ", "
  in
  {{ [ !{:str speakers:}
       {:M.a abs sp {{[<em>(str title)]}} (Int32.to_string id):} ] }}

let filter_events start finish events =
  List.filter
    (fun (start', finish', _, _, _, _) ->
       Common.local_time start' < finish && Common.local_time finish' > start)
    events

let dl def l =
  Common.opt def
    (fun x r ->{{ [<dl>[!x !(map {:r:} with s -> s)]] }}) l

let format_events sp events =
  Common.lwt_map
    (fun (start, finish, id, name, room, location) ->
       let loc =
         if location = "" && room = "" then "" else
         if room = "" then
           Format.sprintf " (%s)" location
         else if location = "" then
           Format.sprintf " (%s)" room
         else
           Format.sprintf " (%s, %s)" room location
       in
       Event_sql.find_event id
           >>= fun {start = _date; room = _room; title = title; description = _abstract} ->
       Event_sql.find_speakers id >>= fun speakers ->
       let desc =
         format_entry_short Seminaire.events sp id title speakers in
       Lwt.return
         {{ [<dt>{:str
                     (String.capitalize (format_time_interval start finish) ^
                      loc ^ " — " ^ name):}
             <dd>(desc)] }})
    events >>= fun l ->
  Lwt.return (dl {{[]}} l)

let tr l =
  Common.opt []
    (fun x r ->[{{ <tr align="right">[x !{:r:}] }}]) l

let table l =
  Common.opt {{[]}}
    (fun x r ->{{ [<table>[x !{:r:}]] }}) l

let generate_calendar sp calendar (year, month) () =
  let start = Date.make year month 1 in
  let finish = Date.next start `Month in
  let days = Date.days_in_month start in
  let wd = Date.int_of_day (Date.day_of_week start) in
  let start = Calendar.create start Common.midnight in
  let finish = Calendar.create finish Common.midnight in
  find_events_in_interval start finish >>= fun events ->

  let offset = (wd + 5) mod 7 in
  let weeks = (days + offset + 7) / 7 in
  let format_day d =
    if d < 1 || d > days then
      {{<td>{:str "":}}}
    else begin
      let start = Date.make year month d in
      let finish = Date.next start `Day in
      let start = Calendar.create start Common.midnight in
      let finish = Calendar.create finish Common.midnight in
      let events = filter_events start finish events in
      if events = [] then
        {{<td>{:str (Format.sprintf "%d" d):}}}
      else
        {{<td>[<strong>{:str (Format.sprintf "%d" d):}]}}
    end
  in
  let lines =
    List.map
      (fun w ->
         tr
           (List.map
              (fun wd ->
                 let d = w * 7 + wd - offset in
                 format_day d)
              (Common.seq 0 6)))
      (Common.seq 0 (weeks - 1))
  in
  let days =
    tr (List.map (fun d -> {{<th>{:str d:}}})
           ["L";"M";"M";"J";"V";"S";"D"])
  in
  let lines = List.flatten (days :: lines) in
  format_events sp events >>= fun events ->
  Lwt.return
    (str "Agenda",
     {{ [<h2>[{:M.a calendar sp (str "<<") (year - 1, month):}
             !{:str " ":}
             {:M.a calendar sp (str "<")
                 (if month = 1 then (year - 1, 12) else
                  (year, month - 1)):}
             !{:str " ":}
             !{:str (format_month (Date.make year month 1)):}
             !{:str " ":}
             {:M.a calendar sp (str ">")
                 (if month = 12 then (year + 1, 1) else
                  (year, month + 1)):}
             !{:str " ":}
             {:M.a calendar sp (str ">>") (year + 1, month):}]
         !(table lines)
         <h2>{:str "Événements du mois":}
         !events] }})

let calendar_path = ["agenda"; ""]

let calendar =
  Eliom_services.new_service
    ~path:calendar_path ~get_params:(P.suffix (P.int "year" ** P.int "month"))
    ()

let _ =
  M.register calendar
    (fun sp (year, month) () ->
       Common.wiki_page calendar_path sp {{ [] }}
         (fun sp -> generate_calendar sp calendar (year, month) ()))

let main =
  M.register_new_service
    ~path:calendar_path ~get_params:P.unit
    (fun sp () () ->
       Common.wiki_page calendar_path sp {{ [] }}
         (fun sp ->
            let today = Date.today () in
            let year = Date.year today in
            let month = Date.int_of_month (Date.month today) in
            generate_calendar sp calendar (year, month) ()))
