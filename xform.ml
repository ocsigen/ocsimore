(*-*-coding: utf-8;-*-*)

open Lwt
open CalendarLib
module P = Eliom_parameters
module M = Eliom_duce.Xhtml
let ( ** ) = P.( ** )
let str = Ocamlduce.Utf8.make
type inline =
  {{ (Char | Xhtmltypes_duce.inline | Xhtmltypes_duce.misc_inline) }}

let def d v = match v with None -> d | Some v -> v
let opt_map f v = match v with None -> None | Some v -> Some (f v)
let opt_bind v f = match v with None -> None | Some v -> f v


(****)

module Name : sig
  type t
  val to_string : t -> string
  val first : t
  val next : t -> t
end = struct
  type t = int
  let to_string n = string_of_int n
  let first = 0
  let next n = n + 1
end

type 'a outcome = Success of 'a | Redisplay | Error


module type Xform = sig

type (+'html, +'o) t


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
(*val select_single : (string * 'a) list -> 'a -> (inline, 'a) t*)
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

end

module Make(Monad: sig
              type +'a t
              val return: 'a -> 'a t
              val (>>=): 'a t -> ('a -> 'b t) -> 'b t
end) = struct
open Monad

(****)

type ('content, 'spec, 'html, 'o) u =
  {form : 'content option -> 'spec -> ('html list * 'o outcome) Monad.t;
   params :
     Name.t -> ('content, [`WithoutSuffix], 'spec) P.params_type * Name.t}

type ('html, 'o, 'res) cont =
  {f : 'content 'spec . ('content, 'spec, 'html, 'o) u -> 'res}

type ('html, 'o) t = {unpack : 'res . ('html, 'o, 'res) cont -> 'res}

let pack f = {unpack = fun g -> g.f f}
let unpack f = f.unpack

let opt_outcome x = match x with None -> Redisplay | Some v -> Success v
let outcome_pair o1 o2 =
  match o1, o2 with
    Error, _ | _, Error    -> Error
  | Success v1, Success v2 -> Success (v1, v2)
  | _                      -> Redisplay
let outcome_map f o =
  match o with
    Error     -> Error
  | Redisplay -> Redisplay
  | Success v -> Success (f v)

(****)

let string_param name =
  (P.string (Name.to_string name), Name.next name)

let string_input ?a value =
  pack
   {form =
      (fun v' name ->
         return
         ([M.string_input ?a
              ~input_type:{{"text"}} ~name ~value:(def value v') ()],
         opt_outcome v'));
    params = string_param}

let text_area ?a ~rows ~cols value =
  pack
   {form =
      (fun v' name ->
         return
         ([M.textarea ?a
              ~rows ~cols ~name ~value:(str (def value v')) ()],
         opt_outcome v'));
    params = string_param}

let submit_button_int value =
  {form =
     (fun v' name ->
        return
        ([M.string_input ~input_type:{{"submit"}} ~name ~value ()],
         opt_outcome (opt_map (fun v' -> v' <> None) v')));
   params =
     (fun name -> (P.opt (P.string (Name.to_string name)), Name.next name))}

let submit_button value =
  pack (submit_button_int value)

(****)

(*XXXX Validate results*)
let select_single lst value =
  pack
   {form =
      (fun v' name ->
         let sel = def value v' in
         let lst =
           List.map
             (fun (l, v) -> M.Option ({{ {} }}, v, Some (str l), v = sel)) lst
         in
         return
         (begin match lst with
            []       -> []
          | hd :: tl -> [M.string_select ~name hd tl]
          end,
          opt_outcome v'));
    params = string_param}

let rec mapi_rec f n l =
  match l with
    []   -> []
  | a::l -> let r = f n a in r :: mapi_rec f (n + 1) l

let mapi f l = mapi_rec f 0 l

(*
let select_single lst n =
  (* XXX Validation: check integer in the right range *)
  pack
   {form =
      (fun v' name ->
         let sel = def (string_of_int n) v' in
         let l =
           mapi
             (fun i (l, _) ->
                let is = string_of_int i in
                M.Option ({{ {} }}, is, Some (str l), is = sel))
             lst
         in
         (begin match l with
            []       -> []
          | hd :: tl -> [M.string_select ~name hd tl]
          end,
          opt_outcome
            (opt_map (fun v' -> snd (List.nth lst (int_of_string v'))) v')));
    params = string_param}
*)

(****)

let wrap_int g f =
  {f with form = fun v name ->
     f.form v name >>= fun (x, r) -> return (g x, r)}

let wrap g f = unpack f {f = fun f -> pack (wrap_int g f)}

let concat_params params1 params2 name =
  let (p1, name) = params1 name in
  let (p2, name) = params2 name in
  (p1 ** p2, name)

let concat f1 f2 =
  {form =
     (fun v (name1, name2) ->
        f1.form (opt_map fst v) name1 >>= fun (l1, r1) ->
        f2.form (opt_map snd v) name2 >>= fun (l2, r2) ->
        return (l1 @ l2, outcome_pair r1 r2));
   params =
     (fun name ->
        let (p1, name) = f1.params name in
        let (p2, name) = f2.params name in
        (p1 ** p2, name))}

module Ops = struct

let (@@) f1 f2 =
  unpack f1 {f = fun f1 -> unpack f2 {f = fun f2 -> pack (concat f1 f2)}}

let (+@) f h =
  unpack f {f = fun f ->
  pack {f with form = fun v nm ->
          f.form v nm >>= fun (l, r) -> return (l @ h, r)}}

let (@+) h f =
  unpack f {f = fun f ->
  pack {f with form = fun v nm ->
          f.form v nm >>= fun (l, r) -> return (h @ l, r)}}

let (|>) f g =
  unpack f {f = fun f ->
  pack {f with
        form = fun v name ->
          f.form v name >>= fun (x, r) -> return (x, outcome_map g r)}}
end

open Ops

(****)

let empty_list =
  pack {form = (fun v _name -> return ([], opt_outcome v));
        params = (fun name -> (P.unit, name))}
  |> (fun () -> [])

let list l f =
  List.fold_right (fun v r -> f v @@ r |> (fun (x, l) -> x :: l)) l empty_list

let rec repeat n v = if n = 0 then [] else v :: repeat (n - 1) v

let oc_list l =
  List.fold_right
    (fun x r -> outcome_map (fun (x, r) -> x :: r) (outcome_pair x r))
    l (Success [])

let list' n f =
  unpack f {f = fun f ->
  pack
    {form =
       (fun v name ->
          let l =
            match v with
              None -> repeat n None
            | Some l -> List.map (fun x -> Some x) l
          in
          let l =
            name.P.it (fun name v' -> [f.form v' name]) l [] in
          (* XXX can we optimize this fold and the P.it above ? *)
          List.fold_left (fun r e -> e >>= fun e -> r >>= fun r ->
                            return (e :: r)) (return []) l
          >>= fun l ->
          return
        (List.flatten (List.map fst l),
         oc_list (List.map snd l)));
     params =
       (fun name -> (P.list (Name.to_string name) (fst (f.params Name.first)),
                     Name.next name))}
}

(****)

let error s = [{{<span class="errmsg">{:str s:} }} ]

let check f tst =
  unpack f {f = fun f ->
  pack {f with
        form = fun v name ->
                 f.form v name >>= fun (x, r) ->
                 match r with
                   Error | Redisplay ->
                     return (x, r)
                 | Success r ->
                     match tst r with
                       None    -> return (x, Success r)
                     | Some x' -> return (x @ error x', Error)}}


(****)

let text : string -> inline list = fun s -> {:{{str s}}:}
let p (x : (inline, 'b) t) = wrap (fun x -> [{{<p>{:x:}}}]) x
let hidden (x : (inline, 'b) t) =
  wrap (fun x -> [{{<div style="display:none">{:x:}}}]) x

(****)


(*XXXX Validate result *)
let int_input ?a ?(format = string_of_int) i =
  check (string_input ?a (format i))
        (fun _s -> None (*XXX Some (error s)*))
  |> (fun s -> int_of_string s)

let bounded_int_input ?format a b i =
  let l =
    string_of_int
      (max (String.length (string_of_int a)) (String.length (string_of_int b)))
  in
  check (int_input ?format ~a:{{ {maxlength = {:l:}; size = {:l:}} }} i)
  (fun i ->
   if i < a || i > b then
     Some (Format.sprintf "doit être entre %i et %i" a b)
   else
     None)

(****)

let extensible_list txt default l f =
  (list l f @@
   let button =
     wrap_int (fun x -> [{{<p>{:x:}}}]) (submit_button_int txt) in
   unpack (f default) {f = fun f ->
   pack
     {form =
        (fun v (name1, name2) ->
           let (l, b) =
             match v with
               None -> ([], None)
             | Some (l, b) ->
                 let l = List.map (fun x -> Some x) l in
                 ((if b <> None then l @ [None] else l), Some b)
           in
           let l = name1.P.it (fun name v' -> [f.form v' name]) l [] in
           List.fold_left (fun r e -> e >>= fun e -> r >>= fun r ->
                            return (e :: r)) (return []) l >>= fun l ->
           button.form b name2 >>= fun (b, _r) ->
           return
           (List.flatten (List.map fst l) @ b,
            oc_list (List.map snd l)));
      params =
        concat_params
          (fun name ->
             (P.list (Name.to_string name) (fst (f.params Name.first)),
              Name.next name))
          button.params}})
  |> (fun (l1, l2) -> l1 @ l2)

(****)

let hour_input hour min =
  bounded_int_input 0 23 hour +@ text "h" @@
  bounded_int_input ~format:(Format.sprintf "%02d") 0 59 min

let day_input day month year =
  bounded_int_input 1 31 day +@ text "/" @@
  bounded_int_input ~format:(Format.sprintf "%02d") 1 12 month +@ text "/" @@
  bounded_int_input 0 9999 year
  |> (fun (day, (month, year)) -> (day, month, year))

let date_input date =
  let min = Calendar.minute date in
  let hour = Calendar.hour date in
  let day = Calendar.day_of_month date in
  let month = Date.int_of_month (Calendar.month date) in
  let year = Calendar.year date in
  day_input day month year +@ text " à " @@ hour_input hour min
  |> (fun ((day, month, year), (hour, min)) ->
        Calendar.make year month day hour min 0)

end


type error =
  | NoError
  | ErrorNoMsg
  | ErrorMsg of string

module Xform = Make(struct
                       type +'a t = 'a
                       let return x = x
                       let (>>=) x f = f x
                     end)

open Xform
open Xform.Ops



let form ~fallback ~get_args ~page ~sp ?(err_handler = fun _ -> None) f =
  let f = hidden (submit_button "Submit") @@ f |> snd in
  unpack f {f = fun f ->
  let (params, _) = f.params Name.first in
  let service =
    Eliom_services.new_post_coservice ~fallback ~post_params:params ()
  in
  M.post_form ~service ~sp (fun names ->
    M.register_for_session ~sp ~service
          (fun sp get_args v ->
             match f.form (Some v) names with
             | (x, Success act) ->
                 Lwt.catch
                   (fun () -> act sp)
                   (fun e ->
                      match err_handler e with
                        | None -> Lwt.fail e
                        | Some err ->
                            let form = M.post_form ~service ~sp
                              (fun _ -> {{ {:x:} }}) get_args in
                            page sp get_args (ErrorMsg err) form
                   )
             | ((x : Eliom_duce.Xhtml.form_content_elt list),
                (Error | Redisplay as err))     ->
                  let form =
                    M.post_form ~service ~sp (fun _ -> {{ {:x:} }}) get_args in
                  let error = if err = Error then ErrorNoMsg else NoError in
                  page sp get_args error form
          );
    let r, _ = f.form None names in {{ {:r:}}})
    get_args}

(*
module XformLwt = Make(struct
                       type +'a t = 'a Lwt.t
                       let return = Lwt.return
                       let (>>=) = Lwt.bind
                     end)
*)
