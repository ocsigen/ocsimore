open CalendarLib

type inline =
  {{ (Char | Xhtmltypes_duce.inline | Xhtmltypes_duce.misc_inline) }}


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


val string_input :
  ?a:Xhtmltypes_duce.input_attrs -> string -> (inline, string) t

(** Maps the empty list to None, and all the others lists to Some *)
val string_opt_input :
  ?a:Xhtmltypes_duce.input_attrs ->
  string option -> (inline, string option) t

val int_input :
  ?a:Xhtmltypes_duce.input_attrs -> ?format:(int -> string) ->
  int -> (inline, int) t
val bounded_int_input :
  ?format:(int -> string) -> int -> int -> int -> (inline, int) t

val bool_checkbox :
  ?a:Xhtmltypes_duce.input_attrs -> bool -> (inline, bool) t

val text_area :
  ?a:Eliom_duce.Xhtml.textarea_attrib_t ->
  rows:int -> cols:int -> string -> (inline, string) t

val submit_button : string -> (inline, bool) t

val select_single : (string * string) list -> string -> (inline, string) t

val list : 'i list -> ('i -> (Xhtmltypes_duce.form_content, 'o) t) -> (Xhtmltypes_duce.form_content, 'o list) t

val list' : int -> (Xhtmltypes_duce.form_content, 'o) t -> (Xhtmltypes_duce.form_content, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (Xhtmltypes_duce.form_content, 'o) t) ->
  (Xhtmltypes_duce.form_content, 'o list) t

(* Displays the input control for 'a, and a checkbox to encode Some/None *)
val opt_input:
  input:('a -> (inline, 'b) t) ->
  default:'a ->
  'a option ->
  (inline, 'b option) t


module Ops : sig

val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t
val ( ||> ) : ('html, 'o1) t -> ('o1 -> 'o2 monad) -> ('html, 'o2) t

end

val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
  (inline, 'a) t -> ('a -> string option) -> (inline, 'a) t

val convert :
  (inline, 'a) t -> ('a -> 'b convert monad) -> (inline, 'b) t

val hour_input : int -> int -> (inline, int * int) t
val day_input : int -> int -> int -> (inline, int * int * int) t
val date_input : Calendar.t -> (inline, Calendar.t) t

val text : string -> inline list
val strong : inline list -> inline
val p : (inline, 'b) t -> (Xhtmltypes_duce.form_content, 'b) t

val form:
  fallback:('a, unit,
   [ `Attached of
       [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
       Eliom_services.a_s ],
   [< Eliom_services.suff ], 'b, unit, [< `Registrable ])
  Eliom_services.service ->
  get_args:'a ->
  page:(Eliom_sessions.server_params -> 'a -> error ->
        Xhtmltypes_duce.form -> Xhtmltypes_duce.html Lwt.t) ->
  sp:Eliom_sessions.server_params ->
  ?err_handler:(exn -> string option) ->
  (Eliom_duce.Xhtml.form_content_elt,
   Eliom_sessions.server_params -> Eliom_duce.Xhtml.page Lwt.t) t ->
  Xhtmltypes_duce.form monad

end

module Xform: Xform with type 'a monad = 'a
module XformLwt : Xform with type 'a monad = 'a Lwt.t
