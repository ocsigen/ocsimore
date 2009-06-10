(*-*-coding: utf-8;-*-*)

open CalendarLib
module P = Eliom_parameters
module M = Eliom_duce.Xhtml
let ( ** ) = P.( ** )
let str = Ocamlduce.Utf8.make

(****)

let def d v = match v with None -> d | Some v -> v
let opt_map f v = match v with None -> None | Some v -> Some (f v)
let opt_bind v f = match v with None -> None | Some v -> f v

(****)

type inline =
  {{ (Char | Xhtmltypes_duce.inline | Xhtmltypes_duce.misc_inline) }}

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

type ('content, 'spec, 'html, 'o) u =
  {form : 'content option -> 'spec -> 'html list * 'o outcome;
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
         ([M.string_input ?a
              ~input_type:{{"text"}} ~name ~value:(def value v') ()],
         opt_outcome v'));
    params = string_param}

let text_area ?a ~rows ~cols value =
  pack
   {form =
      (fun v' name ->
         ([M.textarea ?a
              ~rows ~cols ~name ~value:(str (def value v')) ()],
         opt_outcome v'));
    params = string_param}

let submit_button_int value =
  {form =
     (fun v' name ->
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
  {f with form = fun v name -> let (x, r) = f.form v name in (g x, r)}

let wrap g f = unpack f {f = fun f -> pack (wrap_int g f)}

let concat_params params1 params2 name =
  let (p1, name) = params1 name in
  let (p2, name) = params2 name in
  (p1 ** p2, name)

let concat f1 f2 =
  {form =
     (fun v (name1, name2) ->
        let (l1, r1) = f1.form (opt_map fst v) name1 in
        let (l2, r2) = f2.form (opt_map snd v) name2 in
        (l1 @ l2, outcome_pair r1 r2));
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
  pack {f with form = fun v nm -> let (l, r) = f.form v nm in (l @ h, r)}}

let (@+) h f =
  unpack f {f = fun f ->
  pack {f with form = fun v nm -> let (l, r) = f.form v nm in (h @ l, r)}}

let (|>) f g =
  unpack f {f = fun f ->
  pack {f with
        form = fun v name ->
                 let (x, r) = f.form v name in (x, outcome_map g r)}}
end

open Ops

(****)

let empty_list =
  pack {form = (fun v _name -> ([], opt_outcome v));
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
                 let (x, r) = f.form v name in
                 match r with
                   Error | Redisplay ->
                     (x, r)
                 | Success r ->
                     match tst r with
                       None    -> (x, Success r)
                     | Some x' -> (x @ error x', Error)}}


(****)

let text s = {:{{str s}}:}
let p (x : (inline, 'b) t) = wrap (fun x -> [{{<p>{:x:}}}]) x
let hidden (x : (inline, 'b) t) =
  wrap (fun x -> [{{<div style="display:none">{:x:}}}]) x

(****)

type error =
  | NoError
  | ErrorNoMsg
  | ErrorMsg of string

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
               (x, Success act) ->
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
    {{ {:fst (f.form None names):} }}) get_args}

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
           let (b, _r) = button.form b name2 in
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
