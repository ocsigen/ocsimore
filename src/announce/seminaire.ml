(*-*-coding: utf-8;-*-*)

open User_sql.Types

(*

XXXX

XFORM
- cancel button
  (no need to check validity)

EDITEURS
** - Modifier la base de données...
- Etoile pour indiquer les champs obligatoires
- Commentaire
- Adapter au type d'événement ?

CATEGORIES
- active ou non (?)
** - Droit de lecture et d'écriture par catégorie

FLUX
** - Prendre en compte modification, création, destruction
     (tentative, confirmed, cancelled)
- Afficher les horaires et salles inhabituels

CALENDRIER
- Afficher les intervalles de date
   jeudi 5 avril de 16h00 à 18h00
   du jeudi 6 avril 16h00 au vendredi 15 avril 17h30
  Afficher le lieu, la salle
- Afficher différemment les événements longs et les événements courts
- Vacances et jours fériés

SEMINAIRE
- Afficher salles et horaires inhabituels
- Séances communes ???

EVENEMENT
- Evenement appartenant à plusieurs catégories
  (exposé commun, par exemple) ???

LOW-LEVEL
- Comment faire en cas d'erreur SQL ?
  (par exemple, redémarrage du serveur)
*)

module P = Eliom_parameters
module M = Eliom_duce.Xhtml
open CalendarLib

let (>>=) = Lwt.(>>=)
let ( ** ) = P.( ** )
let str = Ocamlduce.Utf8.make

open Event_sql.Event

(****)

let default_category = "Ensemble des exposés"

(****)

let talks_path p = "talks" :: p

(****)

let feed =
  Eliom_services.new_service
     ~path:(talks_path ["atom"])
     ~get_params:(P.suffix (P.all_suffix "category")) ()

let feed_link sp category =
  Event_sql.find_category_by_path category >>= fun cat ->
  let url = M.make_string_uri feed sp category in
  Lwt.return
    {{ <link rel="alternate" type="application/atom+xml"
        title={:str cat.cat_name:} href={:str url:}>
          [] }}

let rec feed_links_rec sp rem category : {{ [Xhtmltypes_duce.link *] }} Lwt.t =
  begin match rem with
    [] | [""] -> Lwt.return {{ [] }}
  | s :: r    -> feed_links_rec sp r (s :: category)
  end >>= fun l ->
  Lwt.try_bind
    (fun () -> feed_link sp (List.rev category))
    (fun l1 -> Lwt.return {{ [l1 !l] }})
    (fun e  -> match e with Not_found -> Lwt.return l | _ -> raise e)

let feed_links sp category = feed_links_rec sp category []

(****)

let ical =
  Eliom_services.new_service
     ~path:(talks_path ["icalendar"])
     ~get_params:(P.suffix (P.all_suffix "category")) ()

let calendar_link sp category =
  let img = M.make_uri ~service:(Eliom_services.static_dir sp)
                               ~sp ["stock_calendar.png"] in
  M.a ical sp
    {{[<img alt="Calendar" src={:str img:}>[] !" Calendrier"]}} category

(****)

let edit_link target sp txt arg = M.a target sp (str ("[" ^ txt ^ "]")) arg

let opt_enc f v = match v with None -> "" | Some v -> f v
let opt_dec f s = match s with "" -> None | _ -> Some (f s)

open Xform
open Xform.Ops

let talk_editor path service arg sp
      cat start finish room location title
      speakers desc comment status_list status
      cont =
  let duration =
    truncate
      (Time.Period.to_minutes
         (Calendar.Period.to_time (Calendar.sub finish start)) +. 0.5)
  in
  let page sp _arg error form =
   Common.wiki_page path sp {{ [] }} (fun _sp ->
    let txt =
      match error with
        | NoError -> (cat.cat_name ^ ": nouvel événement")
        | _ -> "Erreur"
    in
    Lwt.return
      (str txt,
       {{ [<h1>(str txt) form] }}))
  in
  let form =
    Xform.form service arg page sp
      (Xform.p
         (Xform.text "Date : " @+ Xform.date_input start @@
          Xform.text " — Durée : " @+
          Xform.bounded_int_input 0 1440 duration +@
          Xform.text "min")
             @@
       Xform.p
         (Xform.text "Salle : " @+ Xform.string_input room @@
          Xform.text " — Lieu : " @+ Xform.string_input location)
             @@
       Xform.extensible_list "Ajouter un orateur"
         ("", "") speakers
         (fun (speaker, aff) ->
           Xform.p
             (Xform.text "Orateur : " @+ Xform.string_input speaker @@
              Xform.text " — Affiliation : "@+ Xform.string_input aff))
            @@
       Xform.p
         (Xform.text "Titre : " @+
          Xform.check (Xform.string_input ~a:{{ {size = "60"} }} title)
            (fun s -> if s = "" then Some ("nécessaire") else None))
             @@
       Xform.p
         (Xform.text "Description :" @+ [{{<br>[]}}] @+
          Xform.text_area ~cols:80 ~rows:20 desc)
             @@
       Xform.p
         (Xform.text "Commentaire :" @+ [{{<br>[]}}] @+
          Xform.text_area ~cols:80 ~rows:3 comment)
             @@
       (let l =
          List.map (fun (nm, s) -> (nm, Event_sql.string_of_status s))
            status_list in
        Xform.p
          (Xform.text "État : " @+
           Xform.select_single l (Event_sql.string_of_status status)))
             @@
       Xform.p
          (Xform.submit_button "Valider" @@ Xform.submit_button "Annuler")
        |> (fun ((start, duration), ((room, location),
                 (persons, (title, (description, (comment, (status,
                 (_validate, _cancel)))))))) _sp ->
              let finish =
                Calendar.add start (Calendar.Period.minute duration)
              in
              let status = Event_sql.status_of_string status in
              cont start finish room location
                   (List.filter (fun (nm, _) -> nm <> "") persons)
                   title description comment status))
  in
  page sp arg NoError form

let create_event =
  let path = ["create"] in
  let create_event =
    Eliom_services.new_service ~path
      ~get_params:(P.suffix (P.all_suffix "category"))
      ()
  in
  M.register create_event
    (fun sp category () ->
        (*XXX Use default time, default duration and default room *)
        Event_sql.find_category_by_path category >>= fun cat ->
        assert cat.cat_editable; (*XXX Report an error?*)
        Seminaire_sql.find_category_defaults cat.cat_id
           >>= fun (time, duration, room, location) ->
        let time = match time with Some v -> v | None -> Time.now () in
        let start = Calendar.create (Date.today ()) time in
        let finish =
          Calendar.add start (Calendar.Period.minute (Int32.to_int duration))
        in
        let sl =
          ["Ne pas montrer pour l'instant", Hidden;
           "Annoncer plus tard", Tentative;
           "Annoncer tout de suite", Confirmed]
        in
        talk_editor path create_event category sp
          cat start finish room location
          "" [("", "")] "" "" sl Hidden
          (fun start finish room location persons
               title description comment status ->
             Event_sql.insert_persons persons >>= fun persons ->
             Event_sql.insert_event
                Common.wiki_info.Wiki_types.wiki_id User.admin
                cat.cat_id start finish room location
                persons title description comment status >>= fun id ->
             Lwt.return
               {{<html xmlns="http://www.w3.org/1999/xhtml">
                    [{:Common.head sp "":}
                         <body>[<p>{:(str (Format.sprintf "OK: %ld" id)):}]] }}));
  create_event

let edit_event =
  let path = ["edit"] in
  let edit_event =
    Eliom_services.new_service ~path
      ~get_params:(P.suffix (P.string "id"))
      ()
  in
  M.register edit_event
    (fun sp arg () ->
       (*XXX Validate *)
       let id = Int32.of_string arg in
       Event_sql.find_event id >>= fun ev ->
       Event_sql.find_speakers id >>= fun speakers ->
       Wiki_sql.get_wikibox_content ev.description
           >>= fun desc ->
       let (desc, comment) = match desc with
         | None | Some (_, _, None , _, _, _) -> ("", "")
         | Some (c, _, Some d, _, _, _) -> (d, c) in
       Event_sql.find_category_by_id ev.category >>= fun cat ->
       (*XXX BOGUS*)
       let sl =
         ["Ne pas montrer pour l'instant", Hidden;
          "Publier tout de suite", Confirmed;
          "Publier plus tard", Tentative]
       in
       talk_editor path edit_event arg sp
          cat (fst ev.start) (fst ev.finish) ev.room ev.location
          ev.title speakers desc comment sl ev.status
          (fun start finish room location persons
               title desc comment status ->
              let ev' =
                {ev with
                 minor_version = Int32.succ ev.minor_version;
                 major_version = Int32.succ ev.minor_version; (*XXX*)
                 (* Time zone are actually ignored here *)
                 start = (start, Time_Zone.Local);
                 finish = (finish, Time_Zone.Local);
                 room = room; location = location;
                 title = title; status = status}
              in
              Event_sql.insert_persons persons >>= fun persons ->
              Wiki_sql.get_wiki_info_by_id Common.wiki_id >>= fun wiki_info ->
              let rights = 
                Wiki_models.get_rights wiki_info.Wiki_types.wiki_model 
              in
              Wiki_data.wikibox_content ~sp ~rights ev'.description
              >>= fun (content_type, _, _) ->
              Event_sql.update_event User.admin
                ev' (Some desc) content_type comment persons >>= fun () ->
              Lwt.return
                {{<html xmlns="http://www.w3.org/1999/xhtml">
                     [{:Common.head sp "":}
                         <body>[<p>{:(str (Format.sprintf "OK")):}]] }}));
  edit_event

(****)

let events =
  let path = ["events"] in
  M.register_new_service
    ~path ~get_params:(P.suffix (P.string "id"))
    (fun sp id () ->
       (*XXX Validate *)
       let id = Int32.of_string id in
       Event_sql.find_event id
           >>= fun {category = cat_id; start = date; room = _room; title = title; description = abstract} ->
       Event_sql.find_speakers id >>= fun speakers ->
       Event_sql.find_category_by_id cat_id
           >>= fun cat ->
       feed_links sp cat.cat_path >>= fun l ->
       let speaker_frag =
         if speakers = [] then {{ [] }} else
         {{ [ !{:str (Event.format_speakers speakers):} <br>[] ] }}
       in
       let speaker_title =
         if speakers = [] then "" else
         Event.syntactic_conjunction (List.map fst speakers) ^ " - "
       in
       let edit =
         if cat.cat_editable then
          {{ [<p>[{:edit_link edit_event sp "Modifier"
                      (Int32.to_string id):}]] }}
         else
          {{ [] }}
       in
       Common.wiki_page path sp l
         (fun sp ->
            Event.format_description sp abstract
               >>= fun abstract ->
            Lwt.return
              (str (speaker_title ^ title),
               {{ [ <h1>[!{:str cat.cat_name:}]
                    !edit
                    <h2>[!(str (Event.format_date_num date))
                         <br>[]
                         !speaker_frag
                         !(str title)]
                    abstract ] }})))

(****)

let format_entry abs sp em ev =
  Event_sql.find_speakers ev.id >>= fun speakers ->
  let strong x = if em then {{[<strong>(x)]}}  else x in
  begin if em then
    Event.format_description sp ev.description >>= fun abstract ->
    Lwt.return {{ [ abstract ] }}
  else
    Lwt.return {{ [] }}
  end >>= fun abstract ->
  Event_sql.find_category_by_id ev.category >>= fun cat ->
  let edit =
    if cat.cat_editable then
      {{ [!{:str " — ":}
          {:edit_link edit_event sp "Modifier" (Int32.to_string ev.id):}] }}
    else
      {{ [] }}
  in
  Lwt.return
   {{[<dt>[!{:strong (str (Event.format_date_and_speakers ev.start speakers)):}
           !edit]
      <dd>[{:M.a abs sp (str ev.title) (Int32.to_string ev.id):}
           !abstract]]}}

(****)

let site_filter show_all ev =
  show_all || ev.status = Confirmed || ev.status = Tentative

let dl def l =
  Common.opt def (fun x r ->{{ [<dl>[!x !(map {:r:} with s -> s)]] }}) l

let archives =
  let path = talks_path ["archives"] in
  M.register_new_service
    ~path ~get_params:(P.suffix (P.int "year" ** P.all_suffix "category"))
    (fun sp (year, category) () ->
       feed_links sp category >>= fun l ->
       Common.wiki_page path sp l
         (fun sp ->
            let dates = Format.sprintf "%d-%d" year (year + 1) in
            let start = Date.lmake ~year ~month:8 () in
            let finish = Date.next start `Year in
            let start = Calendar.create start Common.midnight in
            let finish = Calendar.create finish Common.midnight in
            let show_all = true in (*XXXX*)
            Seminaire_sql.find_in_interval
              (site_filter show_all) category start finish
              >>= fun rows ->
            Common.lwt_map (format_entry events sp false) rows
                >>= fun l1 ->
            Event_sql.find_category_by_path category >>= fun cat ->
            Lwt.return
              (str (cat.cat_name ^ " - " ^ dates),
               {{ [ <h1>[!{:str cat.cat_name:}]
                    <h2>[!{:str ("Exposés " ^ dates):}]
                    !(dl {:{{[ ]}}:} l1) ] }})))

let year_of_date d =
  let month = Date.month d in
  Date.year d - if month < Date.Aug then 1 else 0

let ul_arch l =
  Common.opt {{[]}} (fun x r ->{{ [<ul class="archives">[x !{:r:}]] }}) l

let archive_list sp category =
  Seminaire_sql.archive_start_date category >>= fun start_date ->
  let finish_year = year_of_date (Date.today ()) in
  let start_year =
    match start_date with
      Some d -> year_of_date (Calendar.to_date d)
    | None   -> finish_year + 1
  in
  Lwt.return
    (ul_arch
       (List.map
          (fun y ->
             {{<li>[{:M.a archives sp
                       (str (Format.sprintf "%d-%d" y (y + 1)))
                       (y, category):}]}})
          (List.rev (Common_sql.seq start_year finish_year))))

(****)

let rec previous_day wd d =
  if Date.day_of_week d = wd then d else previous_day wd (Date.prev d `Day)

let summary_contents category sp =
  let today = Date.today () in
  let start = previous_day Date.Sat today in
  let finish = Date.next start `Week in
  let start = Calendar.create start Common.midnight in
  let finish = Calendar.create finish Common.midnight in
  Event_sql.find_category_by_path category >>= fun cat ->
  let wikibox = cat.cat_desc in
  Wiki_sql.get_wiki_info_by_id Common.wiki_id >>= fun wiki_info ->
  let rights = Wiki_models.get_rights wiki_info.Wiki_types.wiki_model in
  Wiki.default_bi ~sp ~wikibox ~rights >>= fun bi ->
  Wiki_site.wikibox_widget#display_interactive_wikibox wikibox ~bi
  >>= fun desc ->
  let show_all = true in (*XXXX*)
  Seminaire_sql.find_in_interval (site_filter show_all) category start finish
      >>= fun rows ->
  Common.lwt_map (format_entry events sp true) rows >>= fun l1 ->
  Seminaire_sql.find_after (site_filter show_all) category finish
      >>= fun rows ->
  Common.lwt_map (format_entry events sp false) rows >>= fun l2 ->
  Seminaire_sql.find_before category start 10L show_all >>= fun rows ->
  Common.lwt_map (format_entry events sp false) rows >>= fun l3 ->
  archive_list sp category >>= fun archives ->
  let edit =
    if cat.cat_editable then
     {{ [<p>[{:edit_link create_event sp "Ajouter un événement" category:}]] }}
    else
     {{ [] }}
  in
  Lwt.return
    (str cat.cat_name,
     {{ [ <h1>{:str cat.cat_name:}
          desc
          !edit
          <p>[{:calendar_link sp category:}]
          <h2>{:str "Cette semaine":}
          !(dl [<p>{:str "Rien cette semaine.":}] l1)
          <h2>{:str "À venir":}
          !(dl [<p>{:str "Rien de programmé pour l'instant.":}]
               l2)
          <h2>{:str "Passé":}
          !(dl {:{{[]}}:} l3)
          <h2>{:str "Archives":}
          !archives ] }})

let summary_page path sp category =
  feed_links sp category >>= fun l ->
  Common.wiki_page path sp l (summary_contents category)

let summary =
  let path = talks_path ["summary"] in
  M.register_new_service
    ~path
    ~get_params:(P.suffix (P.all_suffix "category"))
    (fun sp category () -> summary_page path sp category)

let ul l = Common.opt {{[]}} (fun x r ->{{ [<ul>[x !{:r:}]] }}) l

let groupes =
  let path = talks_path [""] in
  M.register_new_service
    ~path ~get_params:P.unit
    (fun sp () () ->
       Common.wiki_page path sp {{ [] }}
         (fun sp ->
            Seminaire_sql.find_categories () >>= fun cat ->
            let l =
              List.map
                (fun (_, uniq_name, name) ->
                   let uniq_name = Str.split (Str.regexp "/") uniq_name in
                   let item = M.a summary sp (str name) uniq_name in
                   {{ <li>[item] }})
                cat
            in
            Lwt.return
              (str "Séminaire et groupes de travail",
               {{ [ <h1>{:str "Séminaire et groupes de travail":}
                    !(ul l) ] }})))

(****)

let opt_map f v = match v with None -> None | Some v -> Some (f v)

let date_input date (hour_nm, (min_nm, (day_nm, (month_nm, year_nm)))) =
  let int a name value =
    M.int_input ~a ~input_type:{{"text"}} ~name ~value () in
  let min = Calendar.minute date in
  let hour = Calendar.hour date in
  let day = Calendar.day_of_month date in
  let month = Date.int_of_month (Calendar.month date) in
  let year = Calendar.year date in
  {{[(int {size = "2"} day_nm day)
     !{:str "/":}
     (int {size = "2"} month_nm month)
     !{:str "/":}
     (int {size = "4"} year_nm year)
     !{:str " à ":}
     (int {size = "2"} hour_nm hour)
     !{:str "h":}
     (int {size = "2"} min_nm min)]}}

let date_params s =
  let f s' = P.int (Format.sprintf "%s/%s" s s') in
  f "hour" ** f "min" ** f "day" ** f "month" ** f "year"

let date_rebuild (hour, (min, (day, (month, year)))) =
  Calendar.make year month day hour min 0

(*XXXXXXXXXXXXXXXXXXXXXX
let talk_editor =
  Eliom_services.new_service ~path:(talks_path ["edit"])
    ~get_params:(P.suffix (P.string "id"))
    ()

let talk_editor_action =
  M.register_new_post_coservice ~fallback:talk_editor
    ~post_params:(date_params "date" ** P.int "duration" **
                  P.string "location" ** P.string "room" **
                  P.string "speaker" ** P.string "aff" **
                  P.string "title" ** P.string "abstract" **
                  P.string "action")
    (fun sp id (date, (duration, (location, (room, (speaker,
                (affiliation, (title, (abstract, action)))))))) ->
       let id = opt_dec Int32.of_string id in
       let start = date_rebuild date in
       let finish = Calendar.add start (Calendar.Period.minute duration) in
       if action = "validate" then begin
         (match id with
              None ->
                (*XXX*)
                Event_sql.find_category_name "seminaire"
                    >>= fun (sem_category, _) ->
                insert_talk
                  sem_category start finish location room
                  speaker affiliation title abstract
            | Some id ->
                update_talk
                  id start finish location room
                  speaker affiliation title abstract >>= fun () ->
                Lwt.return id) >>= fun id ->
         Lwt.return
           {{<html>[
               {:Common.head sp "":}
               <body>[<p>[{:M.a events sp
                              {{[<em>{:str title:}]}}
                              (Int32.to_string id):}]]]}}
       end else if action = "delete" && id <> None then begin
         begin match id with
           None    -> assert false
         | Some id -> delete_talk id
         end >>= fun () ->
         Lwt.return
           {{<html>[
              {:Common.head sp "":}
              <body>[<p>{:str ("Exposé supprimé : " ^ title):}]] }}
       end else
         (*XXX ??? *)
         Lwt.return
           {{ <html>[{:Common.head sp "":} <body>[]] }})
(*
           (H.html (H.head (H.title (H.pcdata "")) [])
            (H.body [H.p [H.pcdata (opt_enc Int32.to_string id)];
                     H.p [H.pcdata (Printer.Calendar.to_string start)];
                     H.p [H.pcdata (Printer.Calendar.to_string finish)];
                     H.p [H.pcdata speaker];
                     H.p [H.pcdata affiliation];
                     H.p [H.pcdata title];
                     H.p [H.pcdata abstract];
                     H.p [H.pcdata action]])))
*)

let create_form
      id start finish location room speaker affiliation title abstract
      (date_nm, (dur_nm, (location_nm, (room_nm, (speaker_nm,
       (affiliation_nm, (title_nm, (abstract_nm, action_nm)))))))) =
    let int a name value =
      M.int_input ~a ~input_type:{{"text"}} ~name ~value () in
    let text a name value =
      M.string_input ~a ~input_type:{{"text"}} ~name ~value () in
    let button name value txt =
      M.string_button ~name ~value txt in
    let hidden name value =
      M.string_input ~input_type:{{"hidden"}} ~name ~value ()
    in
    let duration =
      truncate
        (Time.Period.to_minutes
           (Calendar.Period.to_time (Calendar.sub finish start)) +. 0.5)
    in
    {{[
      <p>[!{:str "Date : ":} !(date_input start date_nm)
          !{:str " — durée : ":} (int {size = "3"} dur_nm duration)
          !{:str "min":}]
      <p>[!{:str "Salle : ":}
          (text {} room_nm room)
          (hidden location_nm location)]
      <p>[!{:str "Orateur : ":}
          (text {} speaker_nm speaker)
          !{:str " — affiliation : ":}
          (text {} affiliation_nm affiliation)]
      <p>[!{:str "Titre : ":}
          (text {size = "60"} title_nm title)]
      <p>[!{:str "Résumé : ":} <br>[]
          {:M.textarea ~cols:80 ~rows:20 ~name:abstract_nm
              ~value:(str abstract) ():}]
      <p>[{:button action_nm "validate" (str "Valider"):}
          !{:match id with
               None    ->
                 {{ [] }}
             | Some id ->
                 {{ [{:button action_nm "delete" (str "Supprimer"):}] }}:}]]}}

let form =
  M.register talk_editor
(*
  M.register_new_service ~path:(talks_path ["edit"])
    ~get_params:(P.suffix (P.string "id"))
*)
    (fun sp id () ->
       let id = opt_dec Int32.of_string id in
         (*XXX Validate *)
       (match id with
          Some id ->
            Event_sql.find_event id
        | None ->
            (*XXX Use default time, default duration and default room *)
            let start = Calendar.create (Date.today ()) (Time.now ()) in
            let finish =
              Calendar.add start (Calendar.Period.minute 90) in
            Lwt.return (0l, start, finish, "", "", "", "", ""))
           >>= fun (_, start, finish, room, speaker,
                    affiliation, title, abstract) ->
       let f =
         M.post_form ~service:talk_editor_action ~sp
           (create_form
              id start finish "" room speaker affiliation title abstract)
           (opt_enc Int32.to_string id)
       in
       Lwt.return
         {{<html>[{:Common.head sp "":} <body>[f]]}})
*)

(****)

(**** Atom feed ****)

let feed_filter p = p.status <> Hidden && p.status <> Tentative

let _ =
  Eliom_atom.register feed
    (fun sp category () ->
       let today = Date.today () in
       let date = Date.prev today `Month in
       let date = Calendar.create date Common.midnight in
       Seminaire_sql.find_after feed_filter category date >>= fun rows ->
       Common.lwt_map
         (fun ev ->
            Event_sql.find_category_by_id ev.category >>= fun cat ->
            Event_sql.find_speakers ev.id >>= fun speakers ->
            let title =
              Event.format_date_and_speakers ev.start speakers ^ " — " ^
              ev.title
            in
            begin match ev.status with
            | Cancelled ->
                Lwt.return
                  ("ANNULÉ — " ^ title,
                   str "Cet événement est annulé")
            | Confirmed ->
                Event.format_description sp ev.description
                   >>= fun abstract ->
                Lwt.return (title, {{ [ abstract ] }})
            | Hidden | Tentative ->
                assert false
            end >>= fun (title, content) ->
            let p =
              M.make_string_uri ~absolute:true
                ~service:events ~sp (Int32.to_string ev.id) in
            let identity =
              { Atom_feed.id = p ^ "#" ^ Int32.to_string ev.major_version;
                Atom_feed.link = p;
                Atom_feed.updated = ev.last_updated;
                Atom_feed.title = title }
            in
            Lwt.return
              { Atom_feed.e_id = identity; Atom_feed.author = cat.cat_name;
                Atom_feed.content = content })
         rows
           >>= fun el ->
       let p = M.make_string_uri ~absolute:true ~service:summary ~sp category in
       Event_sql.last_update () >>= fun d ->
       let d =
         match d with
           Some d -> d
         | None   -> Time_Zone.on Calendar.from_unixfloat Time_Zone.UTC
                       (Unix.gettimeofday ())
       in
       Event_sql.find_category_by_path category >>= fun cat ->
       Lwt.return
         (M.make_string_uri ~absolute:true ~service:feed ~sp category,
          { Atom_feed.id = p; Atom_feed.link = p;
            Atom_feed.updated = d; Atom_feed.title = cat.cat_name }, el))

(**** iCalendar publishing ****)

let ical_filter p = p.status <> Hidden

let _ =
  Eliom_icalendar.register ical
    (fun sp category () ->
       let today = Date.today () in
       let date = Date.prev today `Month in
       let date = Calendar.create date Common.midnight in
       Seminaire_sql.find_after ical_filter category date >>= fun rows ->
       Event_sql.last_update () >>= fun stamp ->
       let stamp =
         match stamp with
           Some d -> d
         | None   -> Time_Zone.on Calendar.from_unixfloat Time_Zone.UTC
                       (Unix.gettimeofday ())
       in
       let hostname = Eliom_sessions.get_hostname ~sp in
       Common.lwt_map
         (fun ev ->
            Event_sql.find_category_by_id ev.category >>= fun cat ->
            Event_sql.find_speakers ev.id >>= fun speakers ->
(*
            Event.format_description sp ev.description >>= fun desc ->
            Event.format_description sp ev.comment >>= fun comment ->
*)
            let p =
              M.make_string_uri ~absolute:true ~service:events ~sp
                (Int32.to_string ev.id) in
            let loc = Event.format_location ev.room ev.location in
            Lwt.return
              { Icalendar.dtstart = Common.utc_time ev.start;
                Icalendar.event_end =
                  Some (Icalendar.Dtend (Common.utc_time ev.finish));
                Icalendar.dtstamp = stamp;
                Icalendar.uid =
                   Format.sprintf "event%ld@@%s" ev.id hostname;
                Icalendar.summary =
                   cat.cat_name ^ " — " ^
                   (if speakers = [] then "" else
                    (Event.format_speakers speakers ^ " — ")) ^
                   ev.title;
                Icalendar.description = None (*XXX Some desc*);
                Icalendar.comment = [] (*XXX Some comment*);
                Icalendar.location = if loc = "" then None else Some loc;
                Icalendar.sequence = Some (Int32.to_int ev.minor_version);
                Icalendar.status =
                  Some
                    (match ev.status with
                       Confirmed -> Icalendar.Confirmed
                     | Tentative -> Icalendar.Tentative
                     | Cancelled -> Icalendar.Cancelled
                     | Hidden    -> assert false);
                Icalendar.transp = Icalendar.Opaque;
                Icalendar.created = None;
                Icalendar.last_modified = Some ev.last_updated;
                Icalendar.url = Some p })
         rows
           >>= fun el ->
       Event_sql.find_category_by_path category >>= fun cat ->
       Lwt.return
         { Icalendar.prodid = "-//PPS//Events//EN"; (*???*)
           Icalendar.calname = Some cat.cat_name;
           Icalendar.caldesc = None;
           Icalendar.events = el })
