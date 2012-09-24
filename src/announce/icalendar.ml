

(* MIME: text/calendar *)

let unsafe_chars = Str.regexp "[\\;,\n]"
let escape_text s =
  Str.global_substitute unsafe_chars
    (fun s ->
       let s' = Str.matched_string s in
       if s' = "\n" then "\\n" else "\\" ^ s')
    s

let is_char_start c = Char.code c land 192 <> 128

let rec split_line_rec line s i l =
  if i = l then begin
    [String.sub line s (i - s)]
  end else if i - s < 70 || not (is_char_start line.[i]) then
    split_line_rec line s (i + 1) l
  else
    String.sub line s (i - s) :: split_line_rec line i i l

let crlf = "\r\n"

let split_line line =
  String.concat (crlf ^ " ") (split_line_rec line 0 0 (String.length line))

(***)

(*
     eventc     = "BEGIN" ":" "VEVENT" CRLF
                  eventprop *alarmc
                  "END" ":" "VEVENT" CRLF

     eventprop  = *(

                ; the following are optional,
                ; but MUST NOT occur more than once

                class / created / description / dtstart / geo /
                last-mod / location / organizer / priority /
                dtstamp / seq / status / summary / transp /
                uid / url / recurid /

                ; either 'dtend' or 'duration' may appear in
                ; a 'eventprop', but 'dtend' and 'duration'
                ; MUST NOT occur in the same 'eventprop'

                dtend / duration /

                ; the following are optional,
                ; and MAY occur more than once

                attach / attendee / categories / comment /
                contact / exdate / exrule / rstatus / related /
                resources / rdate / rrule / x-prop

                )
*)

type status = Tentative | Confirmed | Cancelled
type transp = Opaque | Transparent
type event_end = Dtend of CalendarLib.Calendar.t | Duration of int

type event =
  { dtstart : CalendarLib.Calendar.t;
    event_end : event_end option;
    dtstamp : CalendarLib.Calendar.t;
    uid : string;  (* event_1234@hostname *)
    summary : string;
    description : string option;
    comment : string list;
    location : string option;
(* organizer ?*)
    sequence : int option;
    status : status option;
    transp : transp;
    created : CalendarLib.Calendar.t option;
    last_modified : CalendarLib.Calendar.t option;
    url : string option }

(*
     icalobject = 1*("BEGIN" ":" "VCALENDAR" CRLF
                  icalbody
                  "END" ":" "VCALENDAR" CRLF)
     calprops   = 2*(
                ; 'prodid' and 'version' are both REQUIRED,
                ; but MUST NOT occur more than once

                prodid /version /

                ; 'calscale' and 'method' are optional,
                ; but MUST NOT occur more than once

                calscale        /
                method          /

                x-prop
                )
     component  = 1*(eventc / todoc / journalc / freebusyc /
                / timezonec / iana-comp / x-comp)
*)
type t =
  { prodid : string; (*XXX see ISO 9070 *)
    events : event list;
    calname : string option;
    caldesc : string option }

let print_line ch name value =
  let s = Format.sprintf "%s:%s" name value in
  Format.fprintf ch "%s%s" (split_line s) crlf

let date d = CalendarLib.Printer.Calendar.sprint "%Y%m%dT%H%M%SZ" d

let duration d = Format.sprintf "PT%dS" d

let status s =
  match s with
    Tentative -> "TENTATIVE"
  | Confirmed -> "CONFIRMED"
  | Cancelled -> "CANCELLED"

let transp t =
  match t with
    Opaque      -> "OPAQUE"
  | Transparent -> "TRANSPARENT"

let print_txt ch name txt = print_line ch name (escape_text txt)
let print_int ch name i = print_line ch name (string_of_int i)
let print_date ch name d = print_line ch name (date d)

let opt_map f v = match v with None -> None | Some v -> Some (f v)
let opt print ch nm v = match v with None -> () | Some v -> print ch nm v
let list print ch nm v = List.iter (fun v -> print ch nm v) v

let print_event ch ev =
  print_line ch "BEGIN" "VEVENT";
  print_date ch "DTSTART" ev.dtstart;
  begin match ev.event_end with
    Some (Dtend d)    -> print_date ch "DTEND" d
  | Some (Duration d) -> print_line ch "DURATION" (duration d)
  | None              -> ()
  end;
  print_date ch "DTSTAMP" ev.dtstamp;
  print_txt ch "UID" ev.uid;
  print_txt ch "SUMMARY" ev.summary;
  opt print_txt ch "DESCRIPTION" ev.description;
  list print_txt ch "COMMENT" ev.comment;
  opt print_txt ch "LOCATION" ev.location;
  opt print_int ch "SEQUENCE" ev.sequence;
  opt print_line ch "STATUS" (opt_map status ev.status);
  print_line ch "TRANSP" (transp ev.transp);
  opt print_date ch "CREATED" ev.created;
  opt print_date ch "LAST-MODIFIED" ev.last_modified;
  opt print_txt ch "URL" ev.url;
  print_line ch "END" "VEVENT"

let print_calendar ch c =
  print_line ch "BEGIN" "VCALENDAR";
  print_txt ch "PRODID" c.prodid;
  print_line ch "VERSION" "2.0";
  print_line ch "CALSCALE" "GREGORIAN";
  print_line ch "METHOD" "PUBLISH";
  opt print_txt ch "X-WR-CALNAME" c.calname;
  opt print_txt ch "X-WR-CALDESC" c.caldesc;
  List.iter (fun ev -> print_event ch ev) c.events;
  print_line ch "END" "VCALENDAR"

let calendar c =
  let b = Buffer.create 1024 in
  Format.bprintf b "%a" print_calendar c;
  Buffer.contents b
