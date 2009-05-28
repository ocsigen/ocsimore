(* Ocsimore
 * http://www.ocsigen.org
 * Copyright (C) 2005-2009
 * Piero Furiesi - Jaap Boender - Vincent Balat - Boris Yakobowski -
 * CNRS - Université Paris Diderot Paris 7
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Wiki_types


(** This module declares the interface for the widget that is used
    to display wikiboxes, as well a few related types and functions.
    The widget itself is defined in wiki_widgets.ml
*)

(*********************************************************************)

(** Inductive type used by services to specify a special way to display a
   wikibox. See Wiki_widgets, method interactive_wikibox for details.

   For CSS related constructors, the wikibox is the wikibox holding
   the css. The string option contains the corresponding wikipage, or None
   if the css is for a wiki *)
type wikibox_override =
  (** Edition of a wikibox containing wikitext *)
  | EditWikitext of wikibox

  (** History of some wikitext *)
  | History of wikibox

  (** Old version of a wikitext. The second argument is the version number *)
  | Oldversion of (wikibox * int32)

  (** Src of a wikitext (same arguments as Oldversion) *)
  | Src of (wikibox * int32)

  (** Preview of a wikibox containing wikitext. The second uple is the
     wikitext, and the version number of the wikibox *when the
     edition started*.  This is used to display a warning in case
     of concurrent edits*)
  | PreviewWikitext of (wikibox * (string * int32))

  (** Edition of a CSS. The first arguments is the standard CSS arguments.
     The (string * int32) is the CSS and the time at which the edition
     started (as for PreviewWikitext), or None if we are about to
     start the edition *)
  | EditCss of ((wikibox  * string option) *(string * int32) option)

  (** History of a css *)
  | CssHistory of (wikibox * string option)

  (** Old version of a wikibox *)
  | CssOldversion of (wikibox * string option) * int32

  (** Edition of the permissions of a wikibox *)
  | EditWikiboxPerms of wikibox

  (** Edition of the permissions of a wiki *)
  | EditWikiPerms of wiki



(*VVV why in this module? *)
let override_wikibox_key : (wikibox * wikibox_override) Polytables.key = 
  Polytables.make_key ()

(** How to change the display of a wikibox: which wikibox is concerned,
   and what should be displayed instead *)
let get_override_wikibox ~sp =
  try
    Some (Polytables.get
            ~table:(Eliom_sessions.get_request_cache ~sp)
            ~key:override_wikibox_key)
  with Not_found -> None

let set_override_wikibox ~sp v =
  Polytables.set
    ~table:(Eliom_sessions.get_request_cache ~sp)
    ~key:override_wikibox_key
    ~value:v


let wikibox_error_key : (wikibox * exn) Polytables.key = 
  Polytables.make_key ()

(** The error to display in the wikibox *)
let get_wikibox_error ~sp =
  try
    Some (Polytables.get
            ~table:(Eliom_sessions.get_request_cache ~sp)
            ~key:wikibox_error_key)
  with Not_found -> None

let set_wikibox_error ~sp v =
  Polytables.set
    ~table:(Eliom_sessions.get_request_cache ~sp)
    ~key:wikibox_error_key
    ~value:v

(*********************************************************************)

module Ancestors : sig

(** Type used to avoid wikibox loops *)
type ancestors
val no_ancestors : ancestors
val in_ancestors : wikibox -> ancestors -> bool
val add_ancestor : wikibox -> ancestors -> ancestors

end = struct

type ancestors = wikibox list

let in_ancestors box (ancestors : ancestors) = List.mem box ancestors
let no_ancestors : ancestors = []
let add_ancestor x (a : ancestors) = x::a

end

(** Information available to display a box *)

type box_info =
  {bi_subbox: Xhtmltypes_duce.flows option;
   bi_ancestors: Ancestors.ancestors;
   bi_sp: Eliom_sessions.server_params;
   bi_box : wikibox (* Wikibox which is being displayed *);
   bi_rights: Wiki_types.wiki_rights;
}

let add_ancestor_bi x bi =
  { bi with bi_ancestors = Ancestors.add_ancestor x bi.bi_ancestors }

let default_bi ~sp ~wikibox ~rights =
  {
    bi_sp = sp;
    bi_ancestors = Ancestors.no_ancestors;
    bi_subbox = None;
    bi_box = wikibox;
    bi_rights = rights;
  }



(*********************************************************************)

(** Return code, used for conditions of the form <<cond http_code= >> *)
type page_displayable =
  | Page_displayable
  | Page_404
  | Page_403

let page_displayable_key : page_displayable Polytables.key =
  Polytables.make_key ()

let page_displayable sp =
  try
    Polytables.get
      ~table:(Eliom_sessions.get_request_cache sp)
      ~key:page_displayable_key
  with Not_found -> Page_displayable

let set_page_displayable sp pd =
  Polytables.set
    ~table:(Eliom_sessions.get_request_cache sp)
    ~key:page_displayable_key
    ~value:pd


(*********************************************************************)

(* Name of the wikipage containing the help for the syntax of the wiki *)
let wikisyntax_help_name = "wikisyntax-help"


(*********************************************************************)

type classes = string list


(** A class containing a few auxiliary methods related to wikis *)
class type wikibox_aux =
object

  (** Wraps some xhtml blocks inside an entire xhtml page *)
  method display_container :
    sp:Eliom_sessions.server_params ->
    ?css:{{ [ Xhtmltypes_duce.link* ] }} ->
    ?title:string ->
    Xhtmltypes_duce.blocks ->
    Xhtmltypes_duce.html

  (** Displays some xhtml elements inside a <div> *)
  method display_basic_box :
    classes:classes ->
    Xhtmltypes_duce.flows ->
    Xhtmltypes_duce.block Lwt.t

  (** Pretty-print the content of a wikibox *)
  method display_wikiboxcontent :
    bi:box_info ->
    classes:classes ->
    Wiki_types.wikibox_content ->
    (classes * Xhtmltypes_duce.flows) Lwt.t

  (** Display a wikibox without pretty-printing *)
  method display_raw_wikiboxcontent :
    classes:classes ->
    Wiki_types.wikibox_content ->
    (classes * Xhtmltypes_duce.flows) Lwt.t

end


(** A widget displaying wikiboxes with which the user cannot interact *)
class type frozen_wikibox =
object

  inherit wikibox_aux


  (** Css class for noneditable wikiboxes *)
  val frozen_wb_class: string

  method display_frozen_wikibox :
    bi:box_info ->
    ?classes:string list ->
    wikibox:wikibox ->
    Xhtmltypes_duce.block Lwt.t

end


(** The various tabs on top of an interactive wikibox *)
type menu_item =
  | Menu_Edit
  | Menu_EditWikiboxPerms
  | Menu_EditWikiPerms
  | Menu_History
  | Menu_View
  | Menu_HistoryCss
  | Menu_EditCss

(** Argument passed to the function building the interactive menu on top
    of a wikibox. Regular boxes receive no additional treatment. WikiContainer
    boxes are for boxes that are the container of a wiki, and receive buttons
    to edit the permissions or the css of the wiki. WikiPage boxes are
    for boxes that are wikipages main content, and receive button to edit
    the css or the permissions of the wikipages *)
type special_box =
  | RegularBox
  | WikiContainerBox of wiki
  | WikiPageBox of wikipage

(** A wikibox with which the user can interact *)
class type virtual interactive_wikibox =
  object

    inherit frozen_wikibox

    (** Displays the edition form for a wikibox [wb] containing wikitext.
       The flag [previewonly] governs the display of a button "save". If
       [previewonly] is true, [wb] is only previewed, and cannot saved
       immediately.

       The argument [content] is the text that is displayed in the edit form,
       or [None] if for example the box has been deleted and the user has not
       entered  new text yet.

       The argument [version] is the version of the wikibox when the user
       started edited the box, and is used to check for concurrent
       modifications.
    *)
    method display_wikitext_edit_form :
      bi:box_info ->
      classes:string list ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      wb:wikibox ->
      (** content *) string option * (** version *) int32 ->
      (classes * Xhtmltypes_duce.form) Lwt.t

    (** Same as [display_wikitext_edit_form], but with an help for the
       syntax of the wiki *)
    method display_wikitext_edit_form_help :
      bi:box_info ->
      classes:string list ->
      ?rows:int ->
      ?cols:int ->
      previewonly:bool ->
      wb:wikibox ->
      string option * int32 ->
      (classes * Xhtmltypes_duce.flows) Lwt.t

    (** Displays the edition form for the wikibox [wbcss], which is supposed
       to contain a CSS. The form is supposed to be displayed instead of the
       wikibox [wb]. (This information is used to create menu buttons.)
       The argument [wikipage] is supposed to be such that
       the css associated to the wikipage is in [wbcss]. The arguments
       [version] and [content] are as in [display_wikitext_edit_form]. *)
    method display_css_edit_form :
      bi:box_info ->
      classes:string list ->
      ?rows:int ->
      ?cols:int ->
      wb:wikibox ->
      wbcss:wikibox ->
      wikipage:string option ->
      (** content *) string option * (** version *) int32 ->
      (classes * Xhtmltypes_duce.flows) Lwt.t


    (** Display a form permitting to edit the permissions of the given wiki.
        The wikibox argument is the wikibox which is overridden if an error
        occurs *)
    method display_edit_wiki_perm_form :
      bi:box_info ->
      classes:string list ->
      wb:wikibox ->
      wiki ->
      (classes * Xhtmltypes_duce.flows) Lwt.t

    (** Display a form to edit the permissions of the given wikibox*)
    method display_edit_wikibox_perm_form :
      bi:box_info ->
      classes:string list ->
      wikibox ->
      (classes * Xhtmltypes_duce.flows) Lwt.t


    (** Display the history of the wikibox [wb], which is supposed to contain
       wikitext *)
    method display_wikitext_history :
      bi:box_info ->
      classes:string list ->
      wb:wikibox ->
      (int32 * string * int32 (* User_sql.Types.userid *) * CalendarLib.Printer.Calendar.t) list->
      (classes * Xhtmltypes_duce.flows) Lwt.t

    (** Display the history of the wikibox [wb], which is supposed to contain
       a CSS. See [display_css_edit_form] for the arguments [wbcss] and
       [wikipage]. *)
    method display_css_history :
      bi:box_info ->
      classes:string list ->
      wb:wikibox ->
      wbcss:wikibox ->
      wikipage:string option ->
      (int32 * string * int32 (* User_sql.Types.userid *) * CalendarLib.Printer.Calendar.t) list->
      (classes * Xhtmltypes_duce.flows) Lwt.t


    (** Adds an interactive menu and a title on top of [content]. The result
      of displaying [wb] is supposed to be the argument [content]. The argument
      [active_item] is used to know which tab to highlight in the menu.
      The argument [special_box] is used to specify whether we should add
      special button (see the documentation for [special_box].
      The [classes] argument must contain a list of css classes that will be
      added to the outermost xhtml element. *)
    method display_menu_box :
      bi:box_info ->
      classes:string list ->
      ?active_item:menu_item ->
      ?special_box:special_box ->
      ?title:string ->
      wb:wikibox ->
      (** content:*)Xhtmltypes_duce.flows ->
      Xhtmltypes_duce.block Lwt.t


    (** Display the wikibox [wb] as an interactive wikibox. We return the
       xhtml code, and the http return code. The options [classe] and
       [cssmenu] are the same as in [display_menu_box]. *)
    method display_interactive_wikibox_aux :
      bi:box_info ->
      ?classes:string list ->
      ?rows:int ->
      ?cols:int ->
      ?special_box:special_box ->
      (** wb:*)wikibox ->
      (Xhtmltypes_duce.block * bool) Lwt.t

    (** Same as [interactive_wikibox_aux], except that the http error
        code is not returned. *)
    method display_interactive_wikibox :
      bi:box_info ->
      ?classes:string list ->
      ?rows:int ->
      ?cols:int ->
      ?special_box:special_box ->
      (** wb:*)wikibox ->
      Xhtmltypes_duce.block Lwt.t

    (** Display the wikibox [wb_loc], but entirely overrides the content
        according to the argument [override]. The argument [wb_loc] is
        only used to build the interactive menu. *)
    method display_overriden_interactive_wikibox :
      bi:box_info ->
      ?classes:string list ->
      ?rows:int ->
      ?cols:int ->
      ?special_box:special_box ->
      wb_loc:wikibox ->
      override:wikibox_override ->
      ?exn:exn ->
      unit ->
      (Xhtmltypes_duce.block * bool) Lwt.t


    (** Returns the css headers for one wiki and optionally one page.
        Set [?admin] to [true] for administration pages. *)
    method css_header :
      bi:box_info ->
      ?admin:bool ->
      ?page:string ->
      wiki ->
      {{ [ Xhtmltypes_duce.link* ] }} Lwt.t


    (** Displaying of the content of an entire wikipage *)
    method display_wikipage :
      sp:Eliom_sessions.server_params ->
      wiki:wiki ->
      page:string ->
      (Xhtmltypes_duce.html * int) Lwt.t

  end


(** XXX This must be moved somewhere else. Unfortunately it cannot go into Wiki_services, as it is referenced from Wiki_syntax, itself reference from Wiki_services *)

(* a table containing the Eliom services generating pages
   for each wiki associated to an URL *)
module Servpages =
  Hashtbl.Make(struct
                 type t = wiki
                 let equal = (=)
                 let hash = Hashtbl.hash
               end)

let naservpages :
    (string,
     unit,
     [ `Nonattached of [ `Get ] Eliom_services.na_s ],
     [ `WithoutSuffix ],
     [ `One of string ] Eliom_parameters.param_name,
     unit,
     [`Registrable ]
    ) Eliom_services.service Servpages.t = Servpages.create 5
let servpages :
    (string list,
     unit,
     Eliom_services.get_service_kind,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameters.param_name,
     unit,
     [ `Registrable ]
    ) Eliom_services.service Servpages.t = Servpages.create 5
let servwikicss :
    (unit,
     unit,
     [ `Attached of
         [ `Internal of [ `Service | `Coservice ] * [ `Get ]
         | `External ] Eliom_services.a_s ],
     [ `WithoutSuffix ],
     unit,
     unit,
     [ `Registrable ]
    ) Eliom_services.service Servpages.t = Servpages.create 5

let add_naservpage = Servpages.add naservpages
let add_servpage = Servpages.add servpages
let add_servwikicss = Servpages.add servwikicss
let find_naservpage = Servpages.find naservpages
let find_servpage k =
  try Some (Servpages.find servpages k)
  with Not_found -> None
let find_servwikicss k =
  try Some (Servpages.find servwikicss k)
  with Not_found -> None
