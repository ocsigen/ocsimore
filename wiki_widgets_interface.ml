open Wiki_sql.Types

(*********************************************************************)

(** Type used to avoid wikibox loops *)
type ancestors = wikibox list

type box_info =
  {bi_subbox: Xhtmltypes_duce.flows option;
   bi_ancestors: ancestors;
   bi_sp: Eliom_sessions.server_params;
   bi_sd: Ocsimore_common.session_data;
   bi_page: string list option}



let in_ancestors box (ancestors : ancestors) =
  List.mem box ancestors

let no_ancestors : ancestors = []

let add_ancestor x (a : ancestors) = x::a

let add_ancestor_bi x bi = { bi with
                               bi_ancestors = add_ancestor x bi.bi_ancestors }

(*
(** Type used to avoid wikibox loops *)
type ancestors

val no_ancestors : ancestors

val in_ancestors : (Wiki_sql.wiki * int32) -> ancestors -> bool

val add_ancestor : (Wiki_sql.wiki * int32) -> ancestors -> ancestors

(** Information available to display a box *)
type box_info =
  {bi_subbox: Xhtmltypes_duce.flows option;
   bi_ancestors: ancestors;
   bi_sp: Eliom_sessions.server_params;
   bi_sd: Ocsimore_common.session_data;
   bi_page: string list option}
*)


(*********************************************************************)

(* Used for conditions of the form <<cond http_code= >> *)
type page_displayable =
  | Page_displayable
  | Page_404
  | Page_403

let page_displayable_key : page_displayable Polytables.key =
  Polytables.make_key ()

let page_displayable sd =
  try Polytables.get ~table:sd ~key:page_displayable_key
  with Not_found -> Page_displayable

let set_page_displayable sd pd =
  Polytables.set ~table:sd ~key:page_displayable_key ~value:pd

(*
(** To be passed as information inside [sd] for evaluating conditions
<<cond http_code='404'| >> *)

type page_displayable =
  | Page_displayable
  | Page_404
  | Page_403

val page_displayable: Polytables.t -> page_displayable

val set_page_displayable: Polytables.t -> page_displayable -> unit
*)


(*********************************************************************)

(* Name of the wikipage containing the help for the syntax of the wiki *)
let wikisyntax_help_name = "wikisyntax-help"


(*********************************************************************)


class type virtual noneditable_wikibox =
  object

    inherit Widget.widget_with_error_box

     method container :
       ?css:{{ [ Xhtmltypes_duce.link* ] }} ->
       Xhtmltypes_duce.blocks ->
       Xhtmltypes_duce.html

     method display_basic_box :
      classe:string list ->
      Xhtmltypes_duce.flows ->
      Xhtmltypes_duce.block Lwt.t

    method display_noneditable_wikibox :
      bi:box_info ->
      ?classe:string list ->
      data:wikibox ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method virtual pretty_print_wikisyntax :
      wiki:wiki ->
      bi:box_info ->
      string -> Xhtmltypes_duce.flows Lwt.t

  end

type menu_item =
  | Edit
  | Edit_perm
  | Edit_css
  | History
  | View


class type virtual editable_wikibox =
  object

    inherit Widget.widget_with_error_box
    inherit noneditable_wikibox

    method display_edit_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      wikibox -> string * int32 -> Xhtmltypes_duce.form Lwt.t

    method display_full_edit_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      wikibox -> string * int32 -> Xhtmltypes_duce.flows Lwt.t

    method display_edit_perm_form :
      bi:box_info ->
      wikibox ->
      Xhtmltypes_duce.flows Lwt.t

    method display_history :
      bi:box_info ->
      wikibox ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list ->
      Xhtmltypes_duce.flows Lwt.t

    method display_menu_box :
      classe:string list ->
      ?service:menu_item ->
      ?cssmenu:string option ->
      ?title:string ->
      bi:box_info ->
      wikibox ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox :
      bi:box_info ->
      data:wikibox ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox_aux :
      bi:box_info ->
      data:wikibox ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> (Xhtmltypes_duce.block * bool) Lwt.t

    method display_edit_css_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      data:wikipage ->
      Ocamlduce.Utf8.repr ->
      Xhtmltypes_duce.flows Lwt.t

    method edit_css_box :
      bi:box_info ->
      data:wikipage ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method display_edit_wikicss_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      wiki:wiki ->
      Ocamlduce.Utf8.repr -> Xhtmltypes_duce.flows Lwt.t

    method edit_wikicss_box :
      bi:box_info ->
      wiki:wiki ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list -> unit -> Xhtmltypes_duce.block Lwt.t

    (** returns the css headers for one wiki and optionally one page.
        Set [?admin] to [true] for administration pages.
    *)
    method get_css_header :
      bi:box_info ->
      wiki:wiki ->
      ?admin:bool ->
      ?page:string ->
      unit ->
      {{ [ Xhtmltypes_duce.link* ] }} Lwt.t

    method display_page :
      bi:box_info ->
      wiki:wiki ->
      page:string ->
      path:string list ->
      Eliom_services.result_to_send Lwt.t

  end
