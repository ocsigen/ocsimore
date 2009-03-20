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

let wiki_info = Lwt_unix.run (Wiki_sql.get_wiki_by_name "Announcements")

let wiki_id = wiki_info.Wiki_sql.id
let page_id = wiki_info.Wiki_sql.container_id
let wiki_box = (wiki_id, page_id)

let (>>=) = Lwt.bind

let _ =
  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service
      ~path:["talks"; "__ocsiwikicss"]
      ~get_params:Eliom_parameters.unit
      (fun sp () () -> Wiki.wikicss_service_handler wiki_id ())
  in
  Wiki_syntax.add_servwikicss wiki_id wikicss_service


let wiki_page path sp (headers : {{[Xhtmltypes_duce.head_misc*]}}) contents =
  let page = Ocsigen_lib.string_of_url_path ~encode:false path in
  let sd = Ocsimore_common.get_sd sp in
  contents sp sd
  >>= fun ((title, subbox) : ({{String}} * Xhtmltypes_duce.blocks)) ->
  let bi =
    { Wiki_syntax.bi_sp = sp;
      Wiki_syntax.bi_sd = sd;
      Wiki_syntax.bi_ancestors = Wiki_syntax.no_ancestors;
      Wiki_syntax.bi_subbox = Some subbox;
      Wiki_syntax.bi_page = Some path;
    }
  in
  Ocsisite.wikibox#editable_wikibox
    ~bi
    ~data:wiki_box
    ~cssmenu:None
    () >>= fun box ->
  Ocsisite.wikibox#get_css_header ~bi ~wiki:wiki_id
    ?admin:(Some false) ~page ()
  >>= fun css ->
  Lwt.return
    ({{ <html xmlns="http://www.w3.org/1999/xhtml">[ <head>[<title>title !css !headers] <body>[box] ] }} :
     {{ Xhtmltypes_duce.html }})

(****)

include Common_sql

(****)

let local_time (d, _) = d

let utc_time (d, z) = Calendar.convert d z Time_Zone.UTC

