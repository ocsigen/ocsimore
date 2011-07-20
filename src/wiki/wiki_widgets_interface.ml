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

open Eliom_pervasives
open Wiki_types

let (>>=) = Lwt.bind

(** This module declares the interface for the widget that is used
    to display wikiboxes, as well a few related types and functions.
    The widget itself is defined in wiki_widgets.ml
*)

(*********************************************************************)

(** Inductive type used by services to specify a special way to display a
   wikibox. See Wiki_widgets, method interactive_wikibox for details.

   For CSS related constructors, the first argument is of type
    [css_wikibox], described below. *)
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

  (** Edition of the properties of the given wikipage : title, deletion,
      moving,... *)
  | EditWikipageProperties of wikipage

  (** Edition of the CSSs for a page of a wiki. Presents the list
      of the CSS, with the possibilities to add or to remove some *)
  | EditCssList of (wiki * string option)

  (** Edition of a CSS. The first argument is the standard CSS arguments.
     The (string * int32) is the CSS and the time at which the edition
     started (as for PreviewWikitext), or None if we are about to
     start the edition *)
  | EditCss of (css_wikibox * (string * int32) option)

  (** History of a css *)
  | CssHistory of css_wikibox

  (** Edition of the permissions of a css *)
  | CssPermissions of css_wikibox

  (** Old version of a wikibox *)
  | CssOldversion of css_wikibox * int32

  (** Edition of the permissions of a wikibox *)
  | EditWikiboxPerms of wikibox

  (** Edition of the permissions or metadata of a wiki *)
  | EditWikiOptions of wiki

(** Arguments for the edition of a css. The [wiki] argument is the
wiki which is concerned. The [string option] argument is [None] if
the CSS is for the wiki, [Some page] if it is for the page [page]
of the wiki. The [wikibox] argument contains the wikibox which holds
the css *)
and css_wikibox = (wiki * string option) * wikibox


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


(*********************************************************************)

(** Information available to display a box *)

(* style of the menu for editable wikiboxes *)
type menu_style = [ `Linear | `Pencil | `None ]

type box_info = {
  bi_subbox: menu_style -> (wikibox option * HTML5_types.flow5 HTML5.M.elt list) option Lwt.t
    (* Function generating the text to paste inside an <<option>> extension.
       The wikibox option is (if available) the wikibox which gave rise to
       this text *);
  bi_ancestors: Ancestors.ancestors (* pages which are currently being
                                       displayed. Using to detect loops *);
  bi_box : wikibox (* Wikibox which is being displayed *);
  bi_wiki : wiki (* wiki of the box displayed *);
  bi_page : wiki * string list option (* page at the origin of the display request *);
  bi_rights: Wiki_types.wiki_rights;
  bi_menu_style : menu_style;
}

let add_ancestor_bi x bi =
  { bi with bi_ancestors = Ancestors.add_ancestor x bi.bi_ancestors }



(*********************************************************************)

(** Return code, used for conditions of the form <<cond http_code= >> *)
type page_displayable =
  | Page_displayable
  | Page_404
  | Page_403

let page_displayable_key : page_displayable Polytables.key =
  Polytables.make_key ()

let page_displayable () =
  try
    Polytables.get
      ~table:(Eliom_request_info.get_request_cache ())
      ~key:page_displayable_key
  with Not_found -> Page_displayable

let set_page_displayable pd =
  Polytables.set
    ~table:(Eliom_request_info.get_request_cache ())
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

  (** Displays some xhtml elements inside a <div> *)
  method display_basic_box :
    classes * HTML5_types.flow5 HTML5.M.elt list ->
    HTML5_types.flow5 HTML5.M.elt Lwt.t

  (** Pretty-print the content of a wikibox *)
  method display_wikiboxcontent :
    bi:box_info ->
    classes:classes ->
    HTML5_types.flow5 HTML5.M.elt list Wiki_types.wikibox_content ->
    (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t

  (** Display a wikibox without pretty-printing *)
  method display_raw_wikiboxcontent :
    classes:classes ->
    HTML5_types.flow5 HTML5.M.elt list Wiki_types.wikibox_content ->
    (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t

  (** If error has is supposed to be displayed for the wikibox [wb],
      displays this error and wraps it together with the xml argument in a
      div tag. Otherwise, displays only the xml argument *)
  method wrap_error :
    wb:wikibox ->
    HTML5_types.flow5 HTML5.M.elt list ->
    HTML5_types.flow5 HTML5.M.elt list

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
    HTML5_types.flow5 HTML5.M.elt list Lwt.t

end


(** The various tabs on top of an interactive wikibox *)
type menu_item =
  | Menu_Edit
  | Menu_EditWikiboxPerms
  | Menu_EditWikiOptions
  | Menu_History
  | Menu_View
  | Menu_Css
  | Menu_WikipageProperties

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
      (classes * HTML5_types.flow5 HTML5.M.elt) Lwt.t

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
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t

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
      wikipage:wiki * string option ->
      (** content *) string option * (** version *) int32 ->
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t


    (** Display a form permitting to edit the permissions of the given wiki.
        The wikibox argument is the wikibox which is overridden if an error
        occurs *)
    method display_edit_wiki_perm_form :
      classes:string list ->
      ?wb:wikibox ->
      wiki ->
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t

    (** Display a form to edit the permissions of the given wikibox*)
    method display_edit_wikibox_perm_form :
      bi:box_info ->
      classes:string list ->
      wikibox ->
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t


    (** Display the history of the wikibox [wb], which is supposed to contain
       wikitext *)
    method display_wikitext_history :
      bi:box_info ->
      classes:string list ->
      wb:wikibox ->
      (int32 * string * int32 (* User_sql.Types.userid *) * CalendarLib.Printer.Calendar.t) list->
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t

    (** Display the history of the wikibox [wb], which is supposed to contain
       a CSS. See [display_css_edit_form] for the arguments [wbcss] and
       [wikipage]. *)
    method display_css_history :
      bi:box_info ->
      classes:string list ->
      wb:wikibox ->
      wbcss:wikibox ->
      wikipage:wiki * string option ->
      (int32 * string * int32 (* User_sql.Types.userid *) * CalendarLib.Printer.Calendar.t) list->
      (classes * HTML5_types.flow5 HTML5.M.elt list) Lwt.t


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
      (** content:*)HTML5_types.flow5 HTML5.M.elt list ->
      HTML5_types.flow5 HTML5.M.elt list Lwt.t


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
      (HTML5_types.flow5 HTML5.M.elt list * bool) Lwt.t

    (** Same as [interactive_wikibox_aux], except that the http error
        code is not returned. *)
    method display_interactive_wikibox :
      bi:box_info ->
      ?classes:string list ->
      ?rows:int ->
      ?cols:int ->
      ?special_box:special_box ->
      (** wb:*)wikibox ->
      HTML5_types.flow5 HTML5.M.elt list Lwt.t

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
      unit ->
      (HTML5_types.flow5 HTML5.M.elt list * bool) Lwt.t


    (** Returns the css headers for one wiki and optionally one page. *)
    method css_header :
      ?page:string ->
      wiki ->
      HTML5_types.link HTML5.M.elt list Lwt.t


   (** Adds the container of the wiki around some content. The content
       is given by the function [gen_box], which takes as argument
       the default style of the menus for the wikiboxes, and returns
       the html code for the content, an option indicating whether
       the content originates from a wikibox (and which one), an html
       error code, and the title for the whole page. The page argument
       is the url of the page wrt. the root of the wiki. For technical
       reasons it must be specified as a string, and as the original
       path (ie. string list). The function returns the entire
       html page, and the http error code returned by [gen_box]. *)
    method display_container:
      wiki:wiki ->
      menu_style:menu_style ->
      page:(string * string list) ->
      gen_box:(menu_style ->
                 (wikibox option * HTML5_types.flow5 HTML5.M.elt list *
                  page_displayable * string option) Lwt.t) ->
      (HTML5_types.html HTML5.M.elt * int) Lwt.t

    (** Displaying of the content of an entire wikipage, ie. both
        the container (as per [display_container]) and the content
        of the wikibox that corresponds to the wikipage. *)
    method display_wikipage :
      wiki:wiki ->
      menu_style:menu_style ->
      page:(string * string list) ->
      (HTML5_types.html HTML5.M.elt * int) Lwt.t


    (** Displaying of the content of an template wikipage, with [file]
	as subbox.  *)
    method display_wikifile :
      wiki:wiki ->
	menu_style:menu_style ->
	  template:string ->
	    file: Ocsigen_local_files.resolved ->
	      (HTML5_types.html HTML5.M.elt * int) Lwt.t

    method display_wikibox :
      wiki:wiki ->
	menu_style:menu_style ->
	  template:string ->
	    wb: Wiki_types.wikibox ->
	      (HTML5_types.html HTML5.M.elt * int) Lwt.t

    (** Display of the list of all the wikis, as well as of some links to edit
        their properties *)
    method display_all_wikis :
      HTML5_types.flow5 HTML5.M.elt list Lwt.t

    (** Display edit form *)
    method draw_edit_form :
      rows:int ->
      cols:int ->
      Wiki_types.wikibox ->
      HTML5_types.phrasing HTML5.M.elt list ->
      HTML5_types.phrasing HTML5.M.elt list ->
      Int32.t ->
      string ->
      bool ->
      [ `One of string ] Eliom_parameters.param_name *
        (([ `One of Wiki_types.wikibox_arg Opaque.int32_t ]
            Eliom_parameters.param_name *
            [ `One of int32 ] Eliom_parameters.param_name) *
           [ `One of string ] Eliom_parameters.param_name) ->
      HTML5_types.form_content HTML5.M.elt list

  end

