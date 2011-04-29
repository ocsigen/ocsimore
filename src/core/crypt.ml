open Lwt

external crypt : string -> string -> string = "crypt_stub"

(* crypt(3) uses a global state *)
let check_mutex = Lwt_mutex.create ()

let () = Random.self_init ()

let crypt_passwd pwd =
  let salt () =
    let s = Random.int 64 in
    if s < 26 then
      Char.chr (s + 97 (* 'a' *))
    else if s < 52 then
      Char.chr (s - 26 + 65 (* 'A' *))
    else if s < 62 then
      Char.chr (s - 52 + 48 (* '0' *))
    else if s = 52 then
      '.'
    else '/'
  in
  let salt = Printf.sprintf "%c%c" (salt ()) (salt ()) in
  lwt () = Lwt_mutex.lock check_mutex in
  let r = crypt pwd salt in
  Lwt_mutex.unlock check_mutex;
  Lwt.return r

let check_passwd ~passwd ~hash =
  lwt () = Lwt_mutex.lock check_mutex in
    let computed = crypt passwd hash in
    Lwt_mutex.unlock check_mutex;
    return (computed = hash)
