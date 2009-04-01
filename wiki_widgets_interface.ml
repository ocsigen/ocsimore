(** Type used to avoid wikibox loops *)
type ancestors = Wiki_sql.wikibox list

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
      data:Wiki_sql.wiki * int32 ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method virtual pretty_print_wikisyntax :
      wiki:Wiki_sql.wiki ->
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
      Wiki_sql.wiki * int32 -> string * int32 -> Xhtmltypes_duce.form Lwt.t

    method display_full_edit_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      Wiki_sql.wiki * int32 -> string * int32 -> Xhtmltypes_duce.flows Lwt.t

    method display_edit_perm_form :
      bi:box_info ->
      Wiki_sql.wiki * int32 ->
      Xhtmltypes_duce.flows Lwt.t

    method display_history :
      bi:box_info ->
      Wiki_sql.wiki * int32 ->
      (int32 * string * User_sql.userid * CalendarLib.Printer.Calendar.t) list ->
      Xhtmltypes_duce.flows Lwt.t

    method display_menu_box :
      classe:string list ->
      ?service:menu_item ->
      ?cssmenu:string option ->
      ?title:string ->
      bi:box_info ->
      Wiki_sql.wiki * int32 ->
      Xhtmltypes_duce.flows -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox :
      bi:box_info ->
      data:Wiki_sql.wiki * int32 ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method editable_wikibox_aux :
      bi:box_info ->
      data:Wiki_sql.wiki * int32 ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      ?cssmenu:string option ->
      unit -> (Xhtmltypes_duce.block * bool) Lwt.t

    method display_edit_css_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      data:Wiki_sql.wiki * string ->
      Ocamlduce.Utf8.repr ->
      Xhtmltypes_duce.flows Lwt.t

    method edit_css_box :
      bi:box_info ->
      data:Wiki_sql.wiki * string ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list ->
      unit -> Xhtmltypes_duce.block Lwt.t

    method display_edit_wikicss_form :
      bi:box_info ->
      ?rows:int ->
      ?cols:int ->
      wiki:Wiki_sql.wiki ->
      Ocamlduce.Utf8.repr -> Xhtmltypes_duce.flows Lwt.t

    method edit_wikicss_box :
      bi:box_info ->
      wiki:Wiki_sql.wiki ->
      ?rows:int ->
      ?cols:int ->
      ?classe:string list -> unit -> Xhtmltypes_duce.block Lwt.t

    (** returns the css headers for one wiki and optionally one page.
        Set [?admin] to [true] for administration pages.
    *)
    method get_css_header :
      bi:box_info ->
      wiki:Wiki_sql.wiki ->
      ?admin:bool ->
      ?page:string ->
      unit ->
      {{ [ Xhtmltypes_duce.link* ] }} Lwt.t

  end
