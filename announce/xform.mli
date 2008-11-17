
open CalendarLib

type (+'html, +'o) t

type inline =
  {{ (Char | Xhtmltypes_duce.inline | Xhtmltypes_duce.misc_inline) }}

val string_input :
  ?a:Xhtmltypes_duce.input_attrs -> string -> (inline, string) t
val int_input :
  ?a:Xhtmltypes_duce.input_attrs -> ?format:(int -> string) ->
  int -> (inline, int) t
val bounded_int_input :
  ?format:(int -> string) -> int -> int -> int -> (inline, int) t
val text_area :
  ?a:Eliom_duce.Xhtml.textarea_attrib_t ->
  rows:int -> cols:int -> string -> (inline, string) t
val submit_button : string -> (inline, bool) t
val select_single : (string * string) list -> string -> (inline, string) t
(*val list : int -> ('a list, 'b) t -> ('a list, 'b list) t*)
val list : 'i list -> ('i -> (Xhtmltypes_duce.form_content, 'o) t) -> (Xhtmltypes_duce.form_content, 'o list) t

val list' : int -> (Xhtmltypes_duce.form_content, 'o) t -> (Xhtmltypes_duce.form_content, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (Xhtmltypes_duce.form_content, 'o) t) ->
  (Xhtmltypes_duce.form_content, 'o list) t

module Ops : sig

val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t

end

val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
  (inline, 'a) t -> ('a -> string option) -> (inline, 'a) t

val hour_input : int -> int -> (inline, int * int) t
val day_input : int -> int -> int -> (inline, int * int * int) t
val date_input : Calendar.t -> (inline, Calendar.t) t

val text : string -> inline list
val p : (inline, 'b) t -> (Xhtmltypes_duce.form_content, 'b) t

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
  (Eliom_duce.Xhtml.form_content_elt,
   Eliom_sessions.server_params -> Eliom_duce.Xhtml.page Lwt.t) t ->
  Xhtmltypes_duce.form
