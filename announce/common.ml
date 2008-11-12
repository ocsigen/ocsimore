(*-*-coding: utf-8;-*-*)

(****)

open CalendarLib

let day_name d =
  match d with
    Date.Sun -> "dimanche"
  | Date.Mon -> "lundi"
  | Date.Tue -> "mardi"
  | Date.Wed -> "mercredi"
  | Date.Thu -> "jeudi"
  | Date.Fri -> "vendredi"
  | Date.Sat -> "samedi"

let month_name m =
  match m with
    Date.Jan -> "janvier"
  | Date.Feb -> "février"
  | Date.Mar -> "mars"
  | Date.Apr -> "avril"
  | Date.May -> "mai"
  | Date.Jun -> "juin"
  | Date.Jul -> "juillet"
  | Date.Aug -> "août"
  | Date.Sep -> "septembre"
  | Date.Oct -> "octobre"
  | Date.Nov -> "novembre"
  | Date.Dec -> "décembre"

let set_french_date_names () =
  Printer.day_name := day_name;
  Printer.month_name := month_name

let midnight = Time.make 00 00 00

(****)

(*
module H = XHTML.M
*)
module M = Eliom_duce.Xhtml

let head sp title =
 let title = Ocamlduce.Utf8.make title in
 {{ <head>[<title>(title)
           {: M.css_link (M.make_uri
                               ~service:(Eliom_services.static_dir sp)
                               ~sp ["style.css"]) ():} ] }}

let opt default c l = match l with [] -> default | x :: r -> c x r

(****)

let create_wiki () =
  let wikibox = Ocsisite.wikibox in
  Wiki.create_wiki
    ~title:"Announcements" ~descr:"Announcement manager" ~wikibox ()

let wiki_id = (Lwt_unix.run (create_wiki ())).Wiki.id
let page_id = 1l
let wiki_box = (wiki_id, page_id)

let (>>=) = Lwt.bind

let wiki_page path sp (headers : {{[Xhtmltypes_duce.head_misc*]}}) contents =
  let page = Ocsigen_lib.string_of_url_path path in
  let sd = Ocsimore_common.get_sd sp in
  contents sp sd
      >>= fun ((title, subbox) : ({{String}} * Xhtmltypes_duce.blocks)) ->
  Ocsisite.wikibox#editable_wikibox ~sp ~sd ~data:wiki_box
    ~subbox ~cssmenu:(Some page)
    ~ancestors:Wiki_syntax.no_ancestors
    () >>= fun box ->
  Ocsisite.wikibox#get_css_header ~sp ~wiki:wiki_id
    ?admin:(Some false) ~page ()
  >>= fun css ->
  Lwt.return
    ({{ <html>[ <head>[<title>title !css !headers] <body>[box] ] }} :
     {{ Xhtmltypes_duce.html }})

(****)

include Common_sql

(****)

let local_time (d, _) = d

let utc_time (d, z) = Calendar.convert d z Time_Zone.UTC