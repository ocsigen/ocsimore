open Eliom_content
open CalendarLib


type error =
  | NoError
  | ErrorNoMsg
  | ErrorMsg of string

type 'a convert =
  | ConvError of string
  | Converted of 'a


module type Xform = sig

  type 'a monad

  type (+'html, +'o) t

  val id : (_, _) t -> string

  val string_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list
    -> string
    -> ([> Html5_types.input ] Html5.F.elt, string) t

  (** Maps the empty list to None, and all the others lists to Some *)
  val string_opt_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list
    -> string option
    -> ([> Html5_types.input] Html5.F.elt, string option) t

  val int_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list
    -> ?format:(int -> string)
    -> int
    -> ([> Html5_types.input | Html5_types.span ] Html5.F.elt, int) t
  val bounded_int_input :
    ?format:(int -> string)
    -> int -> int -> int
    -> ([> Html5_types.input | Html5_types.span] Html5.F.elt, int) t

  val bool_checkbox :
    ?a:Html5_types.input_attrib Html5.F.attrib list
    -> bool
    -> ([> Html5_types.input] Html5.F.elt, bool) t

  val text_area :
    ?a:Html5_types.textarea_attrib Html5.F.attrib list
    -> string
    -> ([> Html5_types.textarea] Html5.F.elt, string) t

  val submit_button : string -> ([> Html5_types.input ] Html5.F.elt, bool) t

  val select_single :
    (string * string) list -> string
    -> ([> Html5_types.select] Html5.F.elt, string) t

  val list :
    'i list
    -> ('i -> ([< Html5_types.form_content] Html5.F.elt, 'o) t)
    -> ([> Html5_types.form_content] Html5.F.elt, 'o list) t

  val list' :
    int
    -> ([< Html5_types.form_content] Html5.F.elt, 'o) t
    -> ([> Html5_types.form_content] Html5.F.elt, 'o list) t

  val extensible_list :
    string -> 'i -> 'i list ->
    ('i -> ([< Html5_types.form_content] Html5.F.elt, 'o) t) ->
    ([> Html5_types.form_content] Html5.F.elt, 'o list) t

(* Displays the input control for 'a, and a checkbox to encode Some/None *)
  val opt_input:
    input:('a -> (Html5_types.input Html5.F.elt, 'b) t) ->
    default:'a ->
    'a option ->
    ([> Html5_types.input] Html5.F.elt, 'b option) t


  module Ops : sig

    val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
    val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
    val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
    val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t
    val ( ||> ) : ('html, 'o1) t -> ('o1 -> 'o2 monad) -> ('html, 'o2) t

  end

  val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

  val check :
    (([> Html5_types.span] as 'b) Html5.F.elt, 'a) t ->
    ('a -> string option) ->
    ('b Html5.F.elt, 'a) t

  val convert :
   (([> Html5_types.span] as 'c) Html5.F.elt, 'a) t ->
    ('a -> 'b convert monad) ->
    ('c Html5.F.elt, 'b) t

  val hour_input : int -> int -> ([> Html5_types.input | Html5_types.pcdata | Html5_types.span ] Html5.F.elt, int * int) t
  val day_input :
    int -> int -> int -> ([> Html5_types.input | Html5_types.pcdata | Html5_types.span ] Html5.F.elt, int * int * int) t
  val date_input : Calendar.t -> ([> Html5_types.input | Html5_types.pcdata | Html5_types.span ] Html5.F.elt, Calendar.t) t

  val text : string -> [> Html5_types.pcdata ] Html5.F.elt list
  val strong : [< Html5_types.strong_content_fun ] Html5.F.elt list -> [> Html5_types.strong ] Html5.F.elt
  val p : ([< Html5_types.p_content_fun] Html5.F.elt, 'b) t -> ([> Html5_types.p] Html5.F.elt, 'b) t
  val table : ([< Html5_types.table_content_fun] Html5.F.elt, 'b) t -> ([> Html5_types.table] Html5.F.elt, 'b) t
  val tr : ([< Html5_types.tr_content_fun] Html5.F.elt, 'b) t -> ([> Html5_types.tr] Html5.F.elt, 'b) t
  val td : ([< Html5_types.td_content_fun] Html5.F.elt, 'b) t -> ([> Html5_types.td] Html5.F.elt, 'b) t
  val label_input_tr : label:string -> ?description:string -> ([<Html5_types.td_content_fun] Html5.F.elt, 'b) t -> (Html5_types.tr Html5.F.elt, 'b) t
  val fieldset :
    ?legend:[`Legend] Html5.F.elt ->
    ([<Html5_types.flow5] Html5.F.elt, 'b) t ->
    ([>Html5_types.fieldset] Html5.F.elt, 'b) t

  val form:
    fallback:(
      'a,
      unit,
      [ `Attached of
          ([`Internal of [< `Coservice | `Service ]], [ `Get ])
            Eliom_service.a_s ],
      [< Eliom_service.suff ],
      'b,
      unit,
      [< `Registrable ],
      Eliom_registration.http_service)
    Eliom_service.service
    -> get_args:'a
    -> page:( 'a
              -> error
              -> [>Html5_types.form] Html5.F.elt
              -> Html5.F.html Lwt.t)
    -> ?err_handler:(exn -> string option)
    -> (Html5_types.form_content Html5.F.elt,
        unit -> Eliom_registration.Html5.page Lwt.t) t
    -> [>Html5_types.form] Html5.F.elt monad

end

module Xform: Xform with type 'a monad = 'a
module XformLwt : Xform with type 'a monad = 'a Lwt.t

