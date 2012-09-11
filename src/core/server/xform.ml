
open Eliom_lib
open Lwt_ops
open Eliom_content
open CalendarLib
open Ocsimore_lib
module P = Eliom_parameter
let ( ** ) = P.( ** )

let def d v = match v with None -> d | Some v -> v
let opt_map f v = match v with None -> None | Some v -> Some (f v)

type error =
  | NoError
  | ErrorNoMsg
  | ErrorMsg of string

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

type 'a convert =
  | ConvError of string
  | Converted of 'a

module type Xform = sig

  type 'a monad

  type (+'html, +'o) t

  val id : (_, _) t -> string

  val string_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list -> string -> ([> Html5_types.input] Html5.F.elt, string) t
  val string_opt_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list ->
    string option -> ([> Html5_types.input] Html5.F.elt, string option) t
  val int_input :
    ?a:Html5_types.input_attrib Html5.F.attrib list -> ?format:(int -> string) ->
    int -> ([> Html5_types.input | Html5_types.span ] Html5.F.elt, int) t
  val bounded_int_input :
    ?format:(int -> string) -> int -> int -> int -> ([> Html5_types.input | Html5_types.span ] Html5.F.elt, int) t
  val bool_checkbox :
    ?a:Html5_types.input_attrib Html5.F.attrib list -> bool -> ([> Html5_types.input] Html5.F.elt, bool) t
  val text_area :
    ?a:Html5_types.textarea_attrib Html5.F.attrib list ->
    string -> ([> Html5_types.textarea] Html5.F.elt, string) t
  val submit_button : string -> ([> Html5_types.input ] Html5.F.elt, bool) t
  val select_single : (string * string) list -> string -> ([> Html5_types.select] Html5.F.elt, string) t
(*val select_single : (string * 'a) list -> 'a -> (Html5_types.phrasing Html5.F.elt, 'a) t*)
(*val list : int -> ('a list, 'b) t -> ('a list, 'b list) t*)
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
    (([> Html5_types.span] as 'c) Html5.F.elt, 'a) t -> ('a -> 'b convert monad) -> ('c Html5.F.elt, 'b) t

  val hour_input : int -> int -> ([> Html5_types.input | Html5_types.pcdata | Html5_types.span ] Html5.F.elt, int * int) t
  val day_input : int -> int -> int -> ([> Html5_types.input | Html5_types.pcdata | Html5_types.span ] Html5.F.elt, int * int * int) t
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
    fallback:('a, unit,
              [ `Attached of
                  ([ `Internal of [< `Coservice | `Service ]], [ `Get ])
                    Eliom_service.a_s ],
              [< Eliom_service.suff ], 'b, unit, [< `Registrable ], Eliom_registration.Html5.return)
    Eliom_service.service ->
    get_args:'a ->
    page:('a -> error ->
          [>Html5_types.form] Html5.F.elt -> Html5.F.html Lwt.t) ->
    ?err_handler:(exn -> string option) ->
    (Html5_types.form_content Html5.F.elt,
     unit -> Eliom_registration.Html5.page Lwt.t) t ->
    [> Html5_types.form] Html5.F.elt monad


end

module Make(Monad: sig
  type +'a t
  val return: 'a -> 'a t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
end) = struct
  open Monad

(****)

  type 'a monad = 'a Monad.t

  type ('content, 'spec, 'html, 'o) u =
      {form : 'content option -> 'spec -> ('html list * 'o outcome) Monad.t;
       params :
         Name.t -> ('content, [`WithoutSuffix], 'spec) P.params_type * Name.t;
       id : string}

  type ('html, 'o, 'res) cont =
      {f : 'content 'spec . ('content, 'spec, 'html, 'o) u -> 'res}

  type (+'html, 'o) t = {unpack : 'res . ('html, 'o, 'res) cont -> 'res}

  let pack : (_, _, 'html, 'o) u -> ('html, 'o) t =
    fun f -> {unpack = fun g -> g.f f}

  let unpack : ('html, 'o) t -> ('html, 'o, 'res) cont -> 'res =
    fun { unpack } -> unpack

  let id x =
    unpack x {f = fun { id; _ } -> id }

  let opt_outcome x = match x with None -> Redisplay | Some v -> Success v
  let outcome_pair o1 o2 =
    match o1, o2 with
      | Error, _
      | _, Error               -> Error
      | Success v1, Success v2 -> Success (v1, v2)
      | Redisplay, Success _
      | Success _, Redisplay
      | Redisplay, Redisplay   -> Redisplay
  let outcome_map f o =
    match o with
        Error     -> Error
      | Redisplay -> Redisplay
      | Success v -> Success (f v)

  let outcome_map_monad f o =
    match o with
      | Error -> return Error
      | Redisplay -> return Redisplay
      | Success v -> f v >>= fun r -> return (Success r)

(****)

  let string_param name =
    (P.string (Name.to_string name), Name.next name)

  let string_input ?a value =
    let id = fresh_id () in
    pack
      {form =
          (fun v' name ->
            return
              ([Html5.F.string_input ~a:(Html5.F.a_id id :: def [] a)
                   ~input_type:`Text
                   ~name ~value:(def value v') ()],
               opt_outcome v'));
       params = string_param;
       id}


  let text_area ?a value =
    let id = fresh_id () in
    pack
      {form =
          (fun v' name ->
            return
              ([Html5.F.textarea ~a:(Html5.F.a_id id :: def [] a)
                   ~name ~value:(def value v') ()],
               opt_outcome v'));
       params = string_param;
       id}

(*   let v = M.bool_checkbox *)

  let bool_checkbox ?a checked =
    let id = fresh_id () in
    pack
      {form =
          (fun v' name ->
            return
              ([Html5.F.bool_checkbox ~a:(Html5.F.a_id id :: def [] a) ~name ~checked ()],
               opt_outcome v'));
       params = (fun name -> (P.bool (Name.to_string name), Name.next name));
       id}

  let submit_button_int value =
    let id = fresh_id () in
    {form =
        (fun v' name ->
          return
            ([Html5.F.string_input ~a:[Html5.F.a_id id] ~input_type:`Submit ~name ~value ()],
             opt_outcome (opt_map (fun v' -> v' <> None) v')));
     params =
        (fun name -> (P.opt (P.string (Name.to_string name)), Name.next name));
     id}

  let submit_button value =
    pack (submit_button_int value)

(****)

(* JJJ Validate results*)
  let select_single lst value =
    let id = fresh_id () in
    pack
      {form =
          (fun v' name ->
            let sel = def value v' in
            let lst =
              List.map
                (fun (l, v) ->
                  Html5.F.Option ([], v, Some (Html5.F.pcdata l), v = sel)
                )
                lst
            in
            return
              (begin match lst with
                  []       -> []
                | hd :: tl -> [Html5.F.string_select ~a:[Html5.F.a_id id] ~name hd tl]
               end,
                opt_outcome v'));
       params = string_param;
       id}

(*
  let select_single lst n =
(* JJJ Validation: check integer in the right range *)
  pack
  {form =
  (fun v' name ->
  let sel = def (string_of_int n) v' in
  let l =
  mapi
  (fun i (l, _) ->
  let is = string_of_int i in
  Html5.F.Option ({{ {} }}, is, Some (str l), is = sel))
  lst
  in
  (begin match l with
  []       -> []
  | hd :: tl -> [Html5.F.string_select ~name hd tl]
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
          (p1 ** p2, name));
     id = fresh_id ()}

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

    let (||>) f g =
      unpack f {f = fun f ->
        pack {f with
          form = fun v name ->
            f.form v name >>= fun (x, r) ->
            outcome_map_monad g r >>= fun r ->
            return (x, r)}}
  end

  open Ops

(****)

  let string_opt_input ?a value =
    (match value with
      | None -> string_input ?a ""
      | Some s -> string_input ?a s
    ) |> function
      | "" -> None
      | s -> Some s


  let empty_list =
    pack {form = (fun v _name -> return ([], opt_outcome v));
          params = (fun name -> (P.unit, name));
          id = fresh_id ()}
      |> (fun () -> [])

  let list l f :
    (Html5_types.form_content Html5.F.elt, 'a list) t =
    List.fold_right (fun v r -> f v @@ r |> (fun (x, l) -> x :: l)) l empty_list

  let list =
    (list :
    'a list ->
         ('a -> (Html5_types.form_content Html5.F.elt, 'b) t) ->
         (Html5_types.form_content Html5.F.elt, 'b list) t :>
    'a list ->
         ('a -> ([< Html5_types.form_content] Html5.F.elt, 'b) t) ->
         ([> Html5_types.form_content] Html5.F.elt, 'b list) t)

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
              name.P.it (fun name v' l ->
                l >>= fun l ->
                f.form v' name >>= fun r ->
                return (r :: l)) l (return [])
              >>= fun l ->
              return
                (List.flatten (List.map fst l),
                 oc_list (List.map snd l)));
         params =
            (fun name -> (P.list (Name.to_string name) (fst (f.params Name.first)),
                          Name.next name));
         id = fresh_id ()}
    }

  let list' =
    (list' :
    int
    -> (Html5_types.form_content Html5.F.elt, 'o) t
    -> (Html5_types.form_content Html5.F.elt, 'o list) t :>
    int
    -> ([< Html5_types.form_content] Html5.F.elt, 'o) t
    -> ([> Html5_types.form_content] Html5.F.elt, 'o list) t)

(****)

  let error s = [Html5.F.span ~a:[Html5.F.a_class ["errmsg"]] [Html5.F.pcdata s]]

  let check f tst =
    unpack f {f = fun f ->
      pack {f with
        form = fun v name ->
          f.form v name >>= fun (x, r) ->
          match r with
            | Error | Redisplay ->
              return (x, r)
            | Success r ->
              match tst r with
                  None    -> return (x, Success r)
                | Some x' -> return (x @ error x', Error)}}


  let convert f conv =
    unpack f {f = fun f ->
      pack {f with
        form = fun v name ->
          f.form v name >>= fun (x, r) ->
          match r with
            | Error -> return (x, Error)
            | Redisplay -> return (x, Redisplay)
            | Success r ->
              conv r >>= function
                | Converted v -> return (x, Success v)
                | ConvError x' -> return (x @ error x', Error)}}



(****)

  let text s = [Html5.F.pcdata s]
  let strong l = Html5.F.strong l
  let p (x : ([< Html5_types.p_content_fun] Html5.F.elt, 'b) t) =
    wrap (fun x -> [Html5.F.p x]) x

  let hidden x =
    wrap
      (fun x -> [Html5.F.div ~a:[Html5.F.a_style "display:none"] x])
      (x: (Html5_types.phrasing Html5.F.elt, 'a) t :> (Html5_types.flow5 Html5.F.elt, 'a) t)


  let table rows =
    wrap (function row :: rows -> [Html5.F.table row rows] | [] -> []) rows

  let tr cells =
    wrap (fun cells -> [Html5.F.tr cells]) cells

  let td xs =
    wrap (fun xs -> [Html5.F.td xs]) xs

  let fieldset ?legend x =
    wrap (fun x -> [Html5.F.fieldset ?legend x]) x

  let label_input_tr ~label ?description input =
    let lbl = text label in
    let a = [Html5.F.Raw.a_for (id input)] in
    let row = [Html5.F.td [Html5.F.label ~a lbl]] @+ td input in
    let row =
      match description with
        | None -> row
        | Some description ->
            row +@ [Html5.F.(td ~a:[a_class ["description"]] (text description))]
    in
    tr row

(****)


(* JJJ Validate result *)
  let int_input ?a ?(format = string_of_int) i =
    check (string_input ?a (format i))
      (fun _s -> None (*JJJ Some (error s)*))
      |> (fun s -> int_of_string s)

  let bounded_int_input ?format a b i =
    let l =
      max (String.length (string_of_int a)) (String.length (string_of_int b))
    in
    check (int_input ?format ~a:[Html5.F.a_maxlength l; Html5.F.a_size l] i)
      (fun i ->
        if i < a || i > b then
          Some (Format.sprintf "doit être entre %i et %i" a b)
        else
          None)

(****)

  let extensible_list txt default l f =
    (list l f @@
       let button =
         wrap_int (fun x -> [Html5.F.p x]) (submit_button_int txt) in
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
                 name1.P.it (fun name v' l ->
                   l >>= fun l ->
                   f.form v' name >>= fun r ->
                   return (r :: l)) l (return [])
                 >>= fun l ->
                 button.form b name2 >>= fun (b, _r) ->
                 return
                   (List.flatten (List.map fst l) @ b,
                    oc_list (List.map snd l)));
            params =
               concat_params
                 (fun name ->
                   (P.list (Name.to_string name) (fst (f.params Name.first)),
                    Name.next name))
                 button.params;
            id = fresh_id ()}})
      |> (fun (l1, l2) -> l1 @ l2)

  let extensible_list =
    (extensible_list
    : string -> 'i -> 'i list ->
     ('i -> (Html5_types.form_content Html5.F.elt, 'o) t) ->
     (Html5_types.form_content Html5.F.elt, 'o list) t
    :> string -> 'i -> 'i list ->
     ('i -> ([< Html5_types.form_content] Html5.F.elt, 'o) t) ->
     ([> Html5_types.form_content] Html5.F.elt, 'o list) t)

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

  let opt_input ~input ~default v =
    (bool_checkbox (v <> None) @@
      input (match v with None -> default | Some s -> s)
      |> (fun (checked, s) -> if checked then Some s else None))

end


module Xform = struct

  include Make(struct
    type +'a t = 'a
    let return x = x
    let (>>=) x f = f x
  end)

  open Ops

(* We must duplicate the function [form] for the Lwt and non-lwt versions,
   as the code is typable only for (>>=)  = Lwt.bind or (>>=) = id,
   and this cannot be expressed in the signature of the functor [Make] *)

  let form ~fallback ~get_args ~page ?(err_handler = fun _ -> None) f =
  (* Hidden button, present to catch a press on the enter key *)
    let f = hidden (submit_button "Submit") @@ f |> snd in
    unpack f {f = fun f ->
      let (params, _) = f.params Name.first in
      let service = Eliom_service.post_coservice ~timeout:600. ~fallback
        ~post_params:params ()
      in
      Html5.F.post_form ~service (fun names ->
        Eliom_registration.Html5.register ~scope:Eliom_common.session ~service
          (fun get_args v ->
            match f.form (Some v) names with
              | (x, Success act) ->
                Lwt.catch
                  act
                  (fun e ->
                    match err_handler e with
                      | None -> Lwt.fail e
                      | Some err ->
                        let form = Html5.F.post_form ~service
                          (fun _ -> x) get_args in
                        page get_args (ErrorMsg err) form
                  )
              | ((x : Html5_types.form_content Html5.F.elt list),
                 (Error | Redisplay as err))     ->
                let form =
                  Html5.F.post_form ~service (fun _ -> x) get_args in
                let error = if err = Error then ErrorNoMsg else NoError in
                page get_args error form
          );
        let r, _ = f.form None names in r)
        get_args}

end

module XformLwt = struct

  include Make(struct
    type +'a t = 'a Lwt.t
    let return = Lwt.return
    let (>>=) = Lwt.bind
  end)
  open Ops

  let form ~fallback ~get_args ~page ?(err_handler = fun _ -> None) f =
    let f = hidden (submit_button "Submit") @@ f |> snd in
    unpack f {f = fun f ->
      let (params, _) = f.params Name.first in
      let service = Eliom_service.post_coservice ~timeout:600. ~fallback
        ~post_params:params ()
      in
      Html5.F.lwt_post_form ~service (fun names ->
        Eliom_registration.Html5.register ~scope:Eliom_common.session ~service
          (fun get_args v ->
            f.form (Some v) names >>= function
              | (x, Success act) ->
                Lwt.catch
                  (fun () -> act ())
                  (fun e ->
                    match err_handler e with
                      | None -> Lwt.fail e
                      | Some err ->
                        let form = Html5.F.post_form ~service
                          (fun _ -> x) get_args in
                        page get_args (ErrorMsg err) form
                  )
              | ((x : Html5_types.form_content Html5.F.elt list),
                 (Error | Redisplay as err))     ->
                let form =
                  Html5.F.post_form ~service (fun _ -> x) get_args in
                let error = if err = Error then ErrorNoMsg else NoError in
                page get_args error form
          );
        f.form None names >|= fst)
        get_args}

end

