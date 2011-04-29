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

open Wiki_types

let wiki_info = Lwt_unix.run (Wiki_sql.get_wiki_info_by_name "Announcements")

let wiki_id = wiki_info.wiki_id
let page_id =
  match wiki_info.wiki_container with None -> assert false | Some wb -> wb
let wiki_box = page_id

let (>>=) = Lwt.bind

(* BY : disabled because of new scheme for css. Probably needs to be reenabled
   if announce is resurrected
let _ =
  let wikicss_service =
    Eliom_predefmod.CssText.register_new_service
      ~path:["talks"; "__ocsiwikicss"]
      ~get_params:Eliom_parameters.unit
      (fun _sp () () -> Wiki_sql.get_css_for_wiki wiki_id >>= function
         | None -> Lwt.return ""
         | Some css -> Lwt.return css)
  in
  Wiki_self_services.add_servwikicss wiki_id wikicss_service
*)

let wiki_page path sp (headers : {{[Xhtmltypes_duce.head_misc*]}}) contents =
  let page = Ocsigen_lib.string_of_url_path ~encode:false path in
  contents sp
  >>= fun ((title, subbox) : ({{String}} * Xhtmltypes_duce.blocks)) ->
  Wiki_sql.get_wiki_info_by_id wiki_id >>= fun wiki_info ->
  let rights = Wiki_models.get_rights wiki_info.wiki_model in
  Wiki.default_bi ~sp ~wikibox:wiki_box ~rights >>= fun bi ->
  let bi = { bi with Wiki_widgets_interface.bi_subbox =
      (fun _ -> Lwt.return (Some (None, subbox))) } in
  Wiki_site.wikibox_widget#display_interactive_wikibox ~bi wiki_box
  >>= fun box ->
  Wiki_site.wikibox_widget#css_header ~sp ~page wiki_id
  >>= fun css ->
  Lwt.return
    ({{ <html xmlns="http://www.w3.org/1999/xhtml">[ <head>[<title>title !css !headers] <body>[box] ] }} :
     {{ Xhtmltypes_duce.html }})

(****)

include Common_sql

(****)

let local_time (d, _) = d

let utc_time (d, z) = Calendar.convert d z Time_Zone.UTC

