open Eliom_pervasives
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


val string_input :
     ?a:HTML5_types.input_attrib HTML5.M.attrib list
  -> string
  -> (HTML5_types.phrasing HTML5.M.elt, string) t

(** Maps the empty list to None, and all the others lists to Some *)
val string_opt_input :
     ?a:HTML5_types.input_attrib HTML5.M.attrib list
  -> string option
  -> (HTML5_types.phrasing HTML5.M.elt, string option) t

val int_input :
     ?a:HTML5_types.input_attrib HTML5.M.attrib list
  -> ?format:(int -> string)
  -> int
  -> (HTML5_types.phrasing HTML5.M.elt, int) t
val bounded_int_input :
     ?format:(int -> string)
  -> int -> int -> int
  -> (HTML5_types.phrasing HTML5.M.elt, int) t

val bool_checkbox :
     ?a:HTML5_types.input_attrib HTML5.M.attrib list
  -> bool
  -> (HTML5_types.phrasing HTML5.M.elt, bool) t

val text_area :
     ?a:HTML5_types.textarea_attrib HTML5.M.attrib list
  -> rows:int -> cols:int -> string
  -> (HTML5_types.phrasing HTML5.M.elt, string) t

val submit_button : string -> (HTML5_types.phrasing HTML5.M.elt, bool) t

val select_single :
     (string * string) list -> string
  -> (HTML5_types.phrasing HTML5.M.elt, string) t

val list :
     'i list
  -> ('i -> (HTML5_types.form_content HTML5.M.elt, 'o) t)
  -> (HTML5_types.form_content HTML5.M.elt, 'o list) t

val list' :
     int
  -> (HTML5_types.form_content HTML5.M.elt, 'o) t
  -> (HTML5_types.form_content HTML5.M.elt, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (HTML5_types.form_content HTML5.M.elt, 'o) t) ->
  (HTML5_types.form_content HTML5.M.elt, 'o list) t

(* Displays the input control for 'a, and a checkbox to encode Some/None *)
val opt_input:
  input:('a -> (HTML5_types.phrasing HTML5.M.elt, 'b) t) ->
  default:'a ->
  'a option ->
  (HTML5_types.phrasing HTML5.M.elt, 'b option) t


module Ops : sig

val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t
val ( ||> ) : ('html, 'o1) t -> ('o1 -> 'o2 monad) -> ('html, 'o2) t

end

val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
     (HTML5_types.phrasing HTML5.M.elt, 'a) t
  -> ('a -> string option)
  -> (HTML5_types.phrasing HTML5.M.elt, 'a) t

val convert :
     (HTML5_types.phrasing HTML5.M.elt, 'a) t
  -> ('a -> 'b convert monad)
  -> (HTML5_types.phrasing HTML5.M.elt, 'b) t

val hour_input : int -> int -> (HTML5_types.phrasing HTML5.M.elt, int * int) t
val day_input :
  int -> int -> int -> (HTML5_types.phrasing HTML5.M.elt, int * int * int) t
val date_input : Calendar.t -> (HTML5_types.phrasing HTML5.M.elt, Calendar.t) t

val text : string -> HTML5_types.phrasing HTML5.M.elt list
val strong :
  HTML5_types.phrasing HTML5.M.elt list -> HTML5_types.phrasing HTML5.M.elt
val p :
     (HTML5_types.phrasing HTML5.M.elt, 'b) t
  -> (HTML5_types.form_content HTML5.M.elt, 'b) t

val form:
     fallback:(
       'a,
       unit,
       [ `Attached of
          ([`Internal of [< `Coservice | `Service ]], [ `Get ])
              Eliom_services.a_s ],
       [< Eliom_services.suff ],
       'b,
       unit,
       [< `Registrable ],
       Eliom_output.http_service)
          Eliom_services.service
  -> get_args:'a
  -> page:( 'a
           -> error
           -> [>HTML5_types.form] HTML5.M.elt
           -> HTML5.M.html Lwt.t)
  -> ?err_handler:(exn -> string option)
  -> (HTML5_types.form_content HTML5.M.elt,
      unit -> Eliom_output.Html5.page Lwt.t) t
  -> [>HTML5_types.form] HTML5.M.elt monad

end

module Xform: Xform with type 'a monad = 'a
module XformLwt : Xform with type 'a monad = 'a Lwt.t
