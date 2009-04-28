(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**
User management

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

open User_sql.Types

let (>>=) = Lwt.bind


type 'a parametrized_user = userid

type users =
  | Ground of userid
  | Applied of userid * int32

let apply_parametrized_user g v = Applied (g, Opaque.t_int32 v)


exception NotAllowed
exception BadPassword
exception BadUser
exception UseAuth of userdata
exception Users_error of string



let create_anonymous () =
  Lwt.catch
    (fun () -> User_cache.get_user_by_name "anonymous")
    (function
       | Not_found ->
           (User_sql.new_user
              ~name:"anonymous"
              ~password:User_sql.Types.Connect_forbidden
              ~fullname:"Anonymous"
              ~email:None
              ~groups:[]
              ~dyn:false
            >>= fun (i, _) ->
           Lwt.return { user_id = i;
                        user_login = "anonymous";
                        user_pwd = User_sql.Types.Connect_forbidden;
                        user_fullname = "Anonymous";
                        user_email = None;
                        user_dyn = false;
                      })
       | e -> Lwt.fail e)

let anonymous = Lwt_unix.run (create_anonymous ())

let create_nobody () =
  Lwt.catch
    (fun () -> User_cache.get_user_by_name "nobody")
    (function
       | Not_found ->
           (User_sql.new_user
              ~name:"nobody"
              ~password:User_sql.Types.Connect_forbidden
              ~fullname:"Nobody"
              ~email:None
              ~groups:[]
              ~dyn:false
            >>= fun (i, _) ->
           Lwt.return { user_id = i;
                        user_login = "nobody";
                        user_pwd = User_sql.Types.Connect_forbidden;
                        user_fullname = "Nobody";
                        user_email = None;
                        user_dyn = false;
                      })
       | e -> Lwt.fail e)

let nobody = Lwt_unix.run (create_nobody ())




let create_admin () =
  let rec get_pwd message =
    print_string message;
    flush Pervasives.stdout;
    match (try
             let default = Unix.tcgetattr Unix.stdin in
             let silent = {default with
                             Unix.c_echo = false;
                             Unix.c_echoe = false;
                             Unix.c_echok = false;
                             Unix.c_echonl = false}
             in Some (default, silent)
           with _ -> None)
    with
      | Some (default, silent) ->
          Unix.tcsetattr Unix.stdin Unix.TCSANOW silent;
          (try
             let s = input_line Pervasives.stdin
             in Unix.tcsetattr Unix.stdin Unix.TCSANOW default; s
           with x ->
             Unix.tcsetattr Unix.stdin Unix.TCSANOW default; raise x)
      | None ->  input_line Pervasives.stdin

  and ask_pwd () =
    let pwd1 = get_pwd "Please enter a password for admin: "
    and pwd2 = get_pwd "\nPlease enter the same password again: " in
      if pwd1 = pwd2
      then (print_endline "\nNew password registered."; pwd1)
      else (print_endline "\nPasswords do not match, please try again."; ask_pwd ())

  and ask_email () =
    print_endline "\nEnter a valid e-mail address for admin: ";
    let email = input_line Pervasives.stdin in
      print_endline ("\n'" ^ email ^ "': Confirm this address? (Y/N)");
      match input_line Pervasives.stdin with
        | "Y"|"y" -> print_endline "\n Thank you."; email
        | _ -> print_endline "\n"; ask_email()
  in

  Lwt.catch
    (fun () -> User_cache.get_user_by_name "admin")
    (function
       | Not_found ->
           let pwd = ask_pwd () in
           let email = ask_email () in
           (User_sql.new_user
              ~name:"admin"
              ~password:(User_sql.Types.Ocsimore_user_crypt pwd)
              ~fullname:"Admin"
              ~email:(Some email)
              ~groups:[]
              ~dyn:false
            >>= fun (i, pwd) ->
           Lwt.return { user_id = i;
                        user_login = "admin";
                        user_pwd = pwd;
                        user_fullname = "Admin";
                        user_email = Some email;
                        user_dyn = false;
                      })
       | e -> Lwt.fail e)

let admin = Lwt_unix.run (create_admin ())

let get_user_by_name_from_db ~name =
  Lwt.catch
  (fun () -> User_cache.get_user_by_name name)
  (function
     | Not_found -> Lwt.return nobody
     | e -> Lwt.fail e)

let get_user_by_id_from_db ~id =
  Lwt.catch
  (fun () -> User_cache.get_user_by_id id)
  (function
     | Not_found -> Lwt.return nobody
     | e -> Lwt.fail e)


let get_user_id_by_name name =
  get_user_by_name_from_db name >>= fun u -> Lwt.return u.user_id

let get_user_name_by_id id =
  get_user_by_id_from_db id >>= fun u -> Lwt.return u.user_login


let get_user_fullname_by_id id =
  get_user_by_id_from_db id >>= fun u -> Lwt.return u.user_fullname


let get_user_by_name ~name =
  if name = anonymous.user_login
  then Lwt.return anonymous
  else
  if name = admin.user_login
  then Lwt.return admin
  else
  if name = nobody.user_login
  then Lwt.return nobody
  else get_user_by_name_from_db ~name

let get_user_by_id ~id =
  if id = anonymous.user_id
  then Lwt.return anonymous
  else
  if id = admin.user_id
  then Lwt.return admin
  else
  if id = nobody.user_id
  then Lwt.return nobody
  else get_user_by_id_from_db ~id

(* dynamic groups: *)
module DynGroups = Hashtbl.Make(
  struct
    type t = userid
    let equal a a' = a = a'
    let hash = Hashtbl.hash
  end)

let add_dyn_group, in_dyn_group, fold_dyn_groups =
  let table = DynGroups.create 5 in
  (DynGroups.add table,
   (fun k ~sp ~sd ->
      try
        DynGroups.find table k ~sp ~sd
      with Not_found -> Lwt.return false),
   fun f -> DynGroups.fold f table
  )



let create_user ~name ~pwd ~fullname ?email ~groups ?test () =
  get_user_by_name ~name >>= fun u ->
  if (u = nobody) && (name != nobody.user_login)
  then begin (* the user does not exist *)
    let dyn = not (test = None) in
    User_sql.new_user ~name ~password:pwd ~fullname ~email ~groups ~dyn
    >>= fun (i, pwd) ->
    (match test with
       | None -> ()
       | Some f -> add_dyn_group i f);
    Lwt.return
      { user_id = i;
        user_login = name;
        user_pwd = pwd;
        user_fullname = fullname;
        user_email = email;
        user_dyn = dyn;
      }
  end
  else begin
    (match test with
       | None -> ()
       | Some f -> add_dyn_group u.user_id f);
    Lwt.return u
  end


let create_unique_user =
  let digit s = s.[0] <- String.get "0123456789" (Random.int 10); s in
  let lock = Lwt_mutex.create () in
  fun ~name ~pwd ~fullname ?email ~groups ->
    let rec suffix name =
      get_user_by_name ~name >>= fun u ->
      if (u = nobody) && (name != nobody.user_login)
      then begin (* the user does not exist *)
        create_user
          ~name ~pwd ~fullname ?email ~groups () >>= fun x ->
        Lwt.return (x, name)
      end
      else suffix (name ^ (digit "X"))
    in
    Lwt_mutex.lock lock >>= fun () ->
    suffix name >>= fun r ->
    Lwt_mutex.unlock lock;
    Lwt.return r

(* BY 2009-03-13: deactivated because User_sql.update_data is deactivated. See this file *)
(*
let update_user_data ~user
    ?pwd
    ?(fullname = user.fullname)
    ?(email = user.email)
    ?groups () =
  user.fullname <- fullname;
  user.email <- email;
  (match pwd with None -> () | Some pwd -> user.pwd <- pwd);
  User_cache.update_data
    ~userid:user.user_id
    ~password:pwd
    ~fullname
    ~email
    ?groups
    ()
*)


let authenticate ~name ~pwd =
  get_user_by_name name >>= fun u ->
  if (u = nobody)
  then Lwt.fail BadUser
  else
    match u.user_pwd with
      | User_sql.Types.External_Auth -> Lwt.fail (UseAuth u)
      | Ocsimore_user_plain p ->
          if p = pwd then Lwt.return u else Lwt.fail BadPassword
      | Ocsimore_user_crypt h ->
          Nis_chkpwd.check_passwd ~passwd:pwd ~hash:h
          >>= fun ok ->
          if ok then Lwt.return u else Lwt.fail BadPassword
      | Connect_forbidden ->
          Lwt.fail BadPassword




let in_group_ ?sp ?sd ~user ~group () =
  let rec aux2 g = function
    | [] -> Lwt.return false
    | g2::l ->
        aux g2 g >>= function
          | true -> Lwt.return true
          | false -> aux2 g l
  and aux u g =
    User_cache.get_groups u >>= fun gl ->
    if List.mem g gl
    then Lwt.return true
    else aux2 g gl
  in
  if (user = nobody.user_id) || (group = nobody.user_id)
  then Lwt.return false
  else
    if (user = group) || (user = admin.user_id)
    then Lwt.return true
    else aux user group >>= function
      | true -> Lwt.return true
      | false ->
          match sp, sd with
            | Some sp, Some sd ->
                (in_dyn_group group ~sp ~sd >>= function
                  | true -> Lwt.return true
                  | false ->
                      fold_dyn_groups
                        (fun k f b ->
                           b >>= fun b ->
                           if b
                           then Lwt.return true
                           else if k <> group
                           then (f ~sp ~sd >>= function
                                   | false -> Lwt.return false
(*                                 | true when k = group -> Lwt.return true *)
                                   | true -> aux k group
                                )
                           else Lwt.return false
                        )
                        (Lwt.return false)
                )
            | _ -> Lwt.return false




let add_to_group ~user ~group =
  get_user_by_id group >>= fun { user_dyn = dy } ->
  if dy
  then begin
    Ocsigen_messages.warning
      ("Not possible to insert user "^userid_s user^
         " in group "^userid_s group^
         ". This group is dynamic (risk of loops). (ignoring)");
    Lwt.return ()
  end
  else
    if (user = nobody.user_id) || (group = nobody.user_id)
    then begin
      Ocsigen_messages.warning
        ("Not possible to insert user nobody into a group, or insert someone in group nobody. (ignoring)");
      Lwt.return ()
    end
    else
      in_group_ user group () >>= fun b ->
      if not b
      then
        in_group_ group user () >>= fun b ->
        if b
        then begin
          Ocsigen_messages.warning
            ("Circular group when inserting user "^userid_s user^
               " in group "^userid_s group^
               ". (ignoring)");
          Lwt.return ()
        end
        else User_cache.add_to_group user group
      else Lwt.return ()

let delete_user ~userid =
  User_cache.delete_user userid



(** {2 Session data} *)

let user_table: userid option Eliom_sessions.persistent_table =
  Eliom_sessions.create_persistent_table "ocsimore_user_table_v1"

type user_sd =
    userdata Lwt.t (* Lazy.t not really useful to make it lazy here *)

(** The polytable key for retrieving user data inside session data *)
let user_key : user_sd Polytables.key = Polytables.make_key ()

let get_user_data_ ~sp ~sd =
  Eliom_sessions.get_persistent_session_data ~table:user_table ~sp ()
  >>= function
    | Eliom_sessions.Data (Some u) ->
        Lwt.catch
          (fun () -> get_user_by_id_from_db u)
          (function
             | Not_found ->
                 Eliom_sessions.close_session ~sp () >>= fun () ->
                 Ocsimore_common.clear_sd ~sd;
                 Lwt.return anonymous
             | e -> Lwt.fail e)
    | Eliom_sessions.Data None -> Lwt.return admin
    | _ -> Lwt.return anonymous

let get_user_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:user_key
  with Not_found ->
    let ud = get_user_data_ ~sp ~sd in
    Polytables.set sd user_key ud;
    ud

let get_user_data ~sp ~sd = get_user_sd sp sd

let get_user_id ~sp ~sd = get_user_sd sp sd >>= fun u -> Lwt.return u.user_id

let get_user_name ~sp ~sd = get_user_sd sp sd >>= fun u -> Lwt.return u.user_login

let is_logged_on ~sp ~sd =
  get_user_sd sp sd >>= fun u ->
  Lwt.return (not ((u == anonymous) || (u == nobody)))

let authenticated_users =
  Lwt_unix.run
    (create_user ~name:"users" ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"Authenticated users"
       ~groups:[anonymous.user_id]
       ~test:is_logged_on
       ())

let anonymous_sd =
  let sd = Polytables.create () in
  Polytables.set sd user_key (Lwt.return anonymous);
  sd

let set_session_data ~sp ~sd user =
  Polytables.set sd user_key (Lwt.return user);
  if user == admin
  then
    Eliom_sessions.set_persistent_session_data ~table:user_table ~sp None
  else
    Eliom_sessions.set_persistent_session_data ~table:user_table ~sp (Some user.user_id)


let in_group ~sp ~sd ?user ~group () =
  (match user with
    | None -> get_user_id ~sp ~sd
    | Some user -> Lwt.return user)
  >>= fun user ->
  in_group_ ~sp ~sd ?user ~group ()


let group_list_of_string s =
  let f beg a =
    beg >>= fun beg ->
    get_user_id_by_name a >>= fun v ->
    if v = nobody.user_id
    then Lwt.return beg
    else Lwt.return (v::beg)
  in
  let r = Ocsigen_lib.split ' ' s in
  List.fold_left f (Lwt.return []) r

