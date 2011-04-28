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
     ?a:XHTML_types.input_attrib XHTML.M.attrib list
  -> string
  -> (XHTML_types.inlinemix XHTML.M.elt, string) t

(** Maps the empty list to None, and all the others lists to Some *)
val string_opt_input :
     ?a:XHTML_types.input_attrib XHTML.M.attrib list
  -> string option
  -> (XHTML_types.inlinemix XHTML.M.elt, string option) t

val int_input :
     ?a:XHTML_types.input_attrib XHTML.M.attrib list
  -> ?format:(int -> string)
  -> int
  -> (XHTML_types.inlinemix XHTML.M.elt, int) t
val bounded_int_input :
     ?format:(int -> string)
  -> int -> int -> int
  -> (XHTML_types.inlinemix XHTML.M.elt, int) t

val bool_checkbox :
     ?a:XHTML_types.input_attrib XHTML.M.attrib list
  -> bool
  -> (XHTML_types.inlinemix XHTML.M.elt, bool) t

val text_area :
     ?a:XHTML_types.textarea_attrib XHTML.M.attrib list
  -> rows:int -> cols:int -> string
  -> (XHTML_types.inlinemix XHTML.M.elt, string) t

val submit_button : string -> (XHTML_types.inlinemix XHTML.M.elt, bool) t

val select_single :
     (string * string) list -> string
  -> (XHTML_types.inlinemix XHTML.M.elt, string) t

val list :
     'i list
  -> ('i -> (XHTML_types.form_content XHTML.M.elt, 'o) t)
  -> (XHTML_types.form_content XHTML.M.elt, 'o list) t

val list' :
     int
  -> (XHTML_types.form_content XHTML.M.elt, 'o) t
  -> (XHTML_types.form_content XHTML.M.elt, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (XHTML_types.form_content XHTML.M.elt, 'o) t) ->
  (XHTML_types.form_content XHTML.M.elt, 'o list) t

(* Displays the input control for 'a, and a checkbox to encode Some/None *)
val opt_input:
  input:('a -> (XHTML_types.inlinemix XHTML.M.elt, 'b) t) ->
  default:'a ->
  'a option ->
  (XHTML_types.inlinemix XHTML.M.elt, 'b option) t


module Ops : sig

val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t
val ( ||> ) : ('html, 'o1) t -> ('o1 -> 'o2 monad) -> ('html, 'o2) t

end

val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
     (XHTML_types.inlinemix XHTML.M.elt, 'a) t
  -> ('a -> string option)
  -> (XHTML_types.inlinemix XHTML.M.elt, 'a) t

val convert :
     (XHTML_types.inlinemix XHTML.M.elt, 'a) t
  -> ('a -> 'b convert monad)
  -> (XHTML_types.inlinemix XHTML.M.elt, 'b) t

val hour_input : int -> int -> (XHTML_types.inlinemix XHTML.M.elt, int * int) t
val day_input :
  int -> int -> int -> (XHTML_types.inlinemix XHTML.M.elt, int * int * int) t
val date_input : Calendar.t -> (XHTML_types.inlinemix XHTML.M.elt, Calendar.t) t

val text : string -> XHTML_types.inlinemix XHTML.M.elt list
val strong :
  XHTML_types.inlinemix XHTML.M.elt list -> XHTML_types.inlinemix XHTML.M.elt
val p :
     (XHTML_types.inlinemix XHTML.M.elt, 'b) t
  -> (XHTML_types.form_content XHTML.M.elt, 'b) t

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
       Eliom_services.http)
          Eliom_services.service
  -> get_args:'a
  -> page:( 'a
           -> error
           -> [>XHTML_types.form] XHTML.M.elt
           -> XHTML.M.html Lwt.t)
  -> ?err_handler:(exn -> string option)
  -> (XHTML_types.form_content XHTML.M.elt,
      unit -> Eliom_output.Xhtml.page Lwt.t) t
  -> [>XHTML_types.form] XHTML.M.elt monad

end

module Xform: Xform with type 'a monad = 'a
module XformLwt : Xform with type 'a monad = 'a Lwt.t
