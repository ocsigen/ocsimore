
open CalendarLib

type (+'html, +'o) t

type inline =
  {{ (Char | Xhtmltypes_duce.inline | Xhtmltypes_duce.misc_inline) }}

val string_input :
  ?a:Xhtmltypes_duce.input_attrs -> string -> (inline list, string) t
val int_input :
  ?a:Xhtmltypes_duce.input_attrs -> int -> (inline list, int) t
val bounded_int_input : int -> int -> int -> (inline list, int) t
val text_area :
  ?a:Eliom_duce.Xhtml.textarea_attrib_t ->
  rows:int -> cols:int -> string -> (inline list, string) t
val submit_button : string -> (inline list, bool) t
val select_single : (string * string) list -> string -> (inline list, string) t
(*val list : int -> ('a list, 'b) t -> ('a list, 'b list) t*)
val list : 'i list -> ('i -> (Xhtmltypes_duce.form_content list, 'o) t) -> (Xhtmltypes_duce.form_content list, 'o list) t

val list' : int -> (Xhtmltypes_duce.form_content list, 'o) t -> (Xhtmltypes_duce.form_content list, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (Xhtmltypes_duce.form_content list, 'o) t) ->
  (Xhtmltypes_duce.form_content list, 'o list) t

module Ops : sig

val (@@) : ('elt list, 'o1) t -> ('elt list, 'o2) t -> ('elt list, 'o1 * 'o2) t
val (+@) : ('a list, 'b) t -> 'a list -> ('a list, 'b) t
val (@+) : 'a list -> ('a list, 'b) t -> ('a list, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t

end

val wrap : ('html1 -> 'html2) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
  (inline list, 'a) t -> ('a -> string option) -> (inline list, 'a) t

val hour_input : int -> int -> (inline list, int * int) t
val day_input : int -> int -> int -> (inline list, int * int * int) t
val date_input : Calendar.t -> (inline list, Calendar.t) t

val text : string -> inline list
val p : (inline list, 'b) t -> (Xhtmltypes_duce.form_content list, 'b) t

val form :
  ('a, unit,
   [ `Attached of
       [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
       Eliom_services.a_s ],
   [< Eliom_services.suff ], 'b, unit, [< `Registrable ])
  Eliom_services.service ->
  'a ->
  (Eliom_sessions.server_params -> 'a -> bool ->
   Xhtmltypes_duce.form -> Xhtmltypes_duce.html Lwt.t) ->
  Eliom_sessions.server_params ->
  (Eliom_duce.Xhtml.form_content_elt list, Eliom_sessions.server_params -> Eliom_duce.Xhtml.page Lwt.t) t ->
  Xhtmltypes_duce.form
