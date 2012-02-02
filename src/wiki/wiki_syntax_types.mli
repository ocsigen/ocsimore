open Eliom_pervasives

(** *)

(* BB XXX why not type service_href = { a_link : ...; uri : HTML5.M.uri } *)

module type Service_href = sig

  val a_link :
    ?a:HTML5_types.a_attrib HTML5.M.attrib list ->
    'a HTML5.M.elt list ->
    [> 'a HTML5_types.a ] HTML5.M.elt

  val uri : HTML5.M.uri

end

type service_href = (module Service_href)

type href =
  | String_href of string
  | Service_href of service_href

type desugar_param = {
  dc_page_wiki : Wiki_types.wiki;
  dc_page_path : string list option;
  mutable dc_warnings : ((int * int) * string) list;
}

module type Preprocessor = sig

  (** [desugar_string dc content] does some possible syntactical desugaring in
      [content]. It should be safe to call this, i.e. there shall be no side
      effects in it. (The result must still be [Lwt.t] because it may access
      the DB read-only.  *)
  val desugar_string : desugar_param -> string -> string Lwt.t

  (** [preparse_string wb content] does possibly some replacements in [content]
      and may have arbitrary side effects in the process (e.g. creating
      wikiboxes etc.). *)
  val preparse_string : ?link_action:(string -> string option -> Wikicreole.attribs -> Wiki_types.wikibox -> string option Lwt.t)
    -> Wiki_types.wikibox -> string -> string Lwt.t

end

module type Parser = sig

  include Preprocessor

  type res

  val from_string:
    sectioning:bool ->
    Wiki_widgets_interface.box_info ->
    string ->
    res list

end

(** A wikicreole_parser is essentially a [Wikicreole.builder] object
    but easily extensible. That is, the fields [plugin] and
    [plugin_action] of [Wikicreole.builder], which are supposed to be
    functions, are here represented as association tables. Thus, it
    becomes easy (and, more importantly, not costly) to add
    extensions. *)
type 'a wikicreole_parser = (module Parser with type res = 'a)

type preparser =
    Wiki_types.wikibox ->
    Wikicreole.attribs ->
    string option ->
    string option Lwt.t

module rec ExtParser : sig

  module type ExtParser = sig

    include Parser

    type res_without_interactive
    type link_content

    type wikiparser = (res, res_without_interactive, link_content) ExtParser.ext_wikicreole_parser

    val from_string_without_interactive:
      sectioning:bool ->
      Wiki_widgets_interface.box_info ->
      string ->
      res_without_interactive list

    type non_interactive_syntax_extension =
      [ `Flow5 of res_without_interactive
      | `Phrasing_without_interactive of link_content ]

    type non_interactive_plugin =
        (Wiki_widgets_interface.box_info,
         non_interactive_syntax_extension) Wikicreole.plugin

    type interactive_syntax_extension =
        (res,
         (href * Wikicreole.attribs * res_without_interactive),
         link_content,
         (href * Wikicreole.attribs * link_content))
          Wikicreole.ext_kind

    type interactive_plugin =
        (Wiki_widgets_interface.box_info,
         interactive_syntax_extension) Wikicreole.plugin

    val register_interactive_extension:
      name:string -> wiki_content:bool -> interactive_plugin -> unit
    val register_non_interactive_extension:
      name:string -> wiki_content:bool -> non_interactive_plugin -> unit
    val register_subst: name:string -> preparser -> unit

    val set_link_subst:
      (string ->
       string option ->
       Wikicreole.attribs ->
       Wiki_types.wikibox ->
       string option Lwt.t) ->
      unit

  end

  type ('a, 'b, 'c) ext_wikicreole_parser =
      (module ExtParser
        with type res = 'a
        and type res_without_interactive = 'b
        and type link_content = 'c)

end

