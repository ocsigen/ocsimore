(* NIS password checker using ypmatch
 *
 * Copyright (C) 2008 Stéphane Glondu
 *   (Laboratoire PPS - CNRS - Université Paris Diderot)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  *)

open Lwt


(************************************************************************)
(* Unix wrapper *)

let run_process prog argv =
  let (in_read, in_write) = Unix.pipe () in
  let (out_read, out_write) = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
        Unix.dup2 out_read Unix.stdin;
        Unix.dup2 in_write Unix.stdout;
        Unix.dup2 in_write Unix.stderr;
        List.iter Unix.close [in_read; in_write; out_read; out_write];
        begin
          try Unix.execvp prog argv
          with _ -> return (Unix.WEXITED 127, "")
        end
    | pid ->
        List.iter Unix.close [in_write; out_read];
        Unix.close out_write; (* We have nothing to say *)
        let buffer = Buffer.create 1024 in
        let inchan = Lwt_chan.in_channel_of_descr (Lwt_unix.of_unix_file_descr in_read) in
        let rec loop () =
          Lwt_chan.input_char inchan >>= fun c ->
          Buffer.add_char buffer c;
          loop ()
        in
        finalize
          (fun () -> catch loop
             (function
                | End_of_file -> return (Buffer.contents buffer)
                | e -> fail e))
          (fun () -> Lwt_chan.close_in inchan) >>= fun s ->
        Lwt_unix.waitpid [] pid >>= function (_, status) ->
        return (status, s)


(************************************************************************)

external crypt : string -> string -> string = "crypt_stub"

(* crypt(3) uses a global state *)
let check_mutex = Lwt_mutex.create ()

let check login passwd =
  run_process "/usr/bin/ypmatch" [| "ypmatch"; login; "passwd" |] >>= function
    | (Unix.WEXITED 0, output) ->
        begin try
          Lwt_mutex.lock check_mutex >>= fun () ->
          let start_salt = String.index output ':' in
          let end_hash = String.index_from output (start_salt+1) ':' in
          let end_salt = String.rindex_from output end_hash '$' in
          let salt = String.sub output (start_salt+1) (end_salt-start_salt-1) in
          let hash = String.sub output (start_salt+1) (end_hash-start_salt-1) in
          let computed = crypt passwd salt in
          Lwt_mutex.unlock check_mutex;
          return (computed = hash)
        with Not_found ->
          Lwt_mutex.unlock check_mutex;
          return false
        end
    | _ -> return false
