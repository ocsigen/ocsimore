(*-*-coding: utf-8;-*-*)

let (>>=) = Lwt.(>>=)

let lwt_map f l =
  List.fold_right
    (fun a l ->
       f a >>= fun b ->
       l >>= fun l ->
       Lwt.return (b :: l))
    l (Lwt.return [])

module PGOCaml = PGOCaml_generic.Make (struct include Lwt include Lwt_chan end)

let dbpool : (string, bool) Hashtbl.t PGOCaml.t Lwt_pool.t =
  Lwt_pool.create 5 (fun () -> PGOCaml.connect ())

(****)

let rec seq i j = if i > j then [] else i :: seq (i + 1) j

let unique_row e =
  e >>= fun l -> match l with [r] -> Lwt.return r | _ -> Lwt.fail Not_found

let not_null v =
  v >>= (fun v -> match v with None -> assert false | Some v -> Lwt.return v)
