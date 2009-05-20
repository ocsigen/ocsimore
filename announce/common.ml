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

open Wiki_sql.Types

let wiki_info = Lwt_unix.run (Wiki_sql.get_wiki_info_by_name "Announcements")

let wiki_id = wiki_info.wiki_id
let page_id = wiki_info.wiki_container
let wiki_box = (wiki_id, page_id)

let (>>=) = Lwt.bind

let _ =
  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service
      ~path:["talks"; "__ocsiwikicss"]
      ~get_params:Eliom_parameters.unit
      (fun _sp () () -> Wiki_services.wikicss_service_handler wiki_id ())
  in
  Wiki_services.add_servwikicss wiki_id wikicss_service


let wiki_page path sp (headers : {{[Xhtmltypes_duce.head_misc*]}}) contents =
  let page = Ocsigen_lib.string_of_url_path ~encode:false path in
  contents sp
  >>= fun ((title, subbox) : ({{String}} * Xhtmltypes_duce.blocks)) ->
  let bi = { (Wiki_widgets_interface.default_bi ~sp ~root_wiki:wiki_id
             ~wikibox:wiki_box)
             with Wiki_widgets_interface.bi_subbox = Some subbox } in
  Ocsisite.wikibox_widget#display_interactive_wikibox ~bi ?cssmenu:None wiki_box
  >>= fun box ->
  Ocsisite.wikibox_widget#css_header ~bi ~admin:false ~page wiki_id
  >>= fun css ->
  Lwt.return
    ({{ <html xmlns="http://www.w3.org/1999/xhtml">[ <head>[<title>title !css !headers] <body>[box] ] }} :
     {{ Xhtmltypes_duce.html }})

(****)

include Common_sql

(****)

let local_time (d, _) = d

let utc_time (d, z) = Calendar.convert d z Time_Zone.UTC

