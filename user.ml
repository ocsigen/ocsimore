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

exception ConnectionRefused
exception BadPassword
exception BadUser
exception UnknownUser of string
exception UseAuth of userid


(* YYY not really sure that all User_sql functions that transforms a
   string/id into a user properly return NotAnUser when they fail.
   Thus we still catch Not_found, just in case... *)


(* We might want to simply overwrite incorrect values by the correct ones *)
let possibly_create ~login ~fullname ?email ?pwd () =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> User_sql.get_basicuser_by_login login)
      (function
         | User_sql.NotAnUser | Not_found ->
             let email = match email with
               | None -> None
               | Some f -> f ()
             and password = match pwd with
               | None -> Connect_forbidden
               | Some f -> f ()
             in
             (User_sql.new_user
                ~name:login
                ~password
                ~fullname
                ?email
                ~dyn:false
              >>= fun (i, _) -> Lwt.return i)
         | e -> Lwt.fail e)
  )


let anonymous_login="anonymous"
let anonymous = possibly_create ~login:anonymous_login ~fullname:"Anonymous" ()
let anonymous' = basic_user anonymous
let nobody_login = "nobody"
let nobody = possibly_create ~login:nobody_login ~fullname:"Nobody" ()
let nobody' = basic_user nobody

let admin_login="admin"
let admin =
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
      then (print_endline "\nNew password registered.";
            User_sql.Types.Ocsimore_user_crypt pwd1)
      else (print_endline "\nPasswords do not match, please try again."; ask_pwd ())

  and ask_email () =
    print_endline "\nEnter a valid e-mail address for admin: ";
    let email = input_line Pervasives.stdin in
      print_endline ("\n'" ^ email ^ "': Confirm this address? (Y/N)");
      match input_line Pervasives.stdin with
        | "Y"|"y" -> print_endline "\n Thank you."; Some email
        | _ -> print_endline "\n"; ask_email()
  in
  possibly_create ~login:"admin" ~fullname:"Admin"
    ~pwd:ask_pwd ~email:ask_email ()

let admin' = basic_user admin


let param_user = {
  param_description = "login of the user";
  param_display = Some
    (fun uid ->
       User_sql.get_basicuser_data (userid_from_sql uid) >>= fun r ->
       Lwt.return (Printf.sprintf "'%s' (%s)" r.user_login r.user_fullname));
  find_param_functions =
    Some ((fun uname ->
             User_sql.get_basicuser_by_login uname >>= fun u ->
             Lwt.return (sql_from_userid u)),
          (fun uid ->
             User_sql.userid_to_string (userid_from_sql uid))
         );
}

let group_can_create_groups =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix:"users"
       ~name:"can_create_groups"
       ~descr:"can create new groups"
    )

let group_can_admin_group : [`User] parameterized_group =
  Lwt_unix.run
    (User_sql.new_parameterized_group ~prefix:"users"
       ~name:"can_admin_group"
       ~descr:"can add or remove people in the group"
       ~find_param:param_user
    )

let group_can_create_users =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix:"users" ~name:"GroupsCreators"
       ~descr:"can create new Ocsimore users")




let get_basicuser_by_login login =
  Lwt.catch
  (fun () -> User_sql.get_basicuser_by_login login)
  (function
     | Not_found | User_sql.NotAnUser -> Lwt.return nobody
     | e -> Lwt.fail e)

let get_user_by_name name =
  Lwt.catch
    (fun () -> User_sql.get_user_by_name name)
    (function
       | Not_found | User_sql.NotAnUser -> Lwt.return nobody'
       | e -> Lwt.fail e)


let user_list_of_string s =
  let f beg a =
    beg >>= fun beg ->
    Lwt.catch
      (fun () ->
         User_sql.get_user_by_name a >>= fun v ->
         if v = nobody'
         then Lwt.return beg
         else Lwt.return (v::beg)
      )
      (function
         | User_sql.NotAnUser -> Lwt.fail (UnknownUser a)
         | e -> Lwt.fail e)
  in
  let r = Ocsigen_lib.split '\n' s in
  List.fold_left f (Lwt.return []) r


(* dynamic groups: *)
module DynGroups = Hashtbl.Make(
  struct
    type t = user
    let equal a a' = a = a'
    let hash = Hashtbl.hash
  end)

let add_dyn_group, in_dyn_group, fold_dyn_groups =
  let table = DynGroups.create 5 in
  (DynGroups.add table,
   (fun k ~sp ->
      try
        DynGroups.find table k ~sp
      with Not_found -> Lwt.return false),
   fun f -> DynGroups.fold f table
  )


let ok_name name =
  try ignore (String.index name '#'); false
  with Not_found -> true

let create_user, create_fresh_user =
  let mutex_user = Lwt_mutex.create () in
  let aux already_existing ~name ~pwd ~fullname ?email ?test () =
    if ok_name name = false then
      Lwt.fail BadUser
    else
      Lwt_mutex.lock mutex_user >>= fun () ->
      get_basicuser_by_login name >>= fun u ->
      (if (u = nobody) && (name != nobody_login)
       then (* the user does not exist *)
         let dyn = not (test = None) in
         User_sql.new_user ~name ~password:pwd ~fullname ~email ~dyn
         >>= fun (u, _) -> Lwt.return u
       else
         already_existing u
      ) >>= fun u ->
      (match test with
         | None -> ()
         | Some f -> add_dyn_group (basic_user u) f
      );
      Lwt_mutex.unlock mutex_user;
      Lwt.return u
  in
  aux (fun u -> Lwt.return u),
  aux (fun _ -> raise BadUser) ?test:None



let authenticate ~name ~pwd =
  get_basicuser_by_login name >>= fun u ->
  if (u = nobody)
  then Lwt.fail BadUser
  else
    User_sql.get_basicuser_data u >>= fun u ->
    match u.user_pwd with
      | User_sql.Types.External_Auth -> Lwt.fail (UseAuth u.user_id)
      | Ocsimore_user_plain p ->
          if p = pwd then Lwt.return u else Lwt.fail BadPassword
      | Ocsimore_user_crypt h ->
          Nis_chkpwd.check_passwd ~passwd:pwd ~hash:h >>= fun ok ->
          if ok then Lwt.return u else Lwt.fail BadPassword
      | Connect_forbidden ->
          Lwt.fail BadPassword


(** {2 Session data} *)

let user_table: userid Eliom_sessions.persistent_table =
  Eliom_sessions.create_persistent_table "ocsimore_user_table_v1"

type user_sd = userid Lwt.t

(** The polytable key for retrieving user data inside session data *)
let user_key : user_sd Polytables.key = Polytables.make_key ()

let get_user_ ~sp =
  Eliom_sessions.get_persistent_session_data ~table:user_table ~sp ()
  >>= function
    | Eliom_sessions.Data u ->
        Lwt.catch
          (fun () -> User_sql.get_basicuser_data u >>= fun _ud -> Lwt.return u)
          (function
             | User_sql.NotAnUser | Not_found ->
                 Eliom_sessions.close_session ~sp () >>= fun () ->
                 Polytables.clear (Eliom_sessions.get_request_cache sp);
                 Lwt.return anonymous
             | e -> Lwt.fail e)
    | Eliom_sessions.Data_session_expired | Eliom_sessions.No_data ->
        Lwt.return anonymous

let get_user_sd ~sp =
  let rc = Eliom_sessions.get_request_cache sp in
  try
    Polytables.get ~table:rc ~key:user_key
  with Not_found ->
    let ud = get_user_ ~sp in
    Polytables.set rc user_key ud;
    ud

let get_user_id ~sp = get_user_sd sp

let get_user_data ~sp =
  get_user_sd sp >>= fun u ->
  User_sql.get_basicuser_data u

let get_user_name ~sp =
  get_user_data sp >>= fun u -> 
  Lwt.return u.user_login


type groups_sd = (user * user, bool) Hashtbl.t

let groups_key : groups_sd Polytables.key = Polytables.make_key ()


let in_group_ ?sp ~user ~group () =
  let get_in_cache, update_cache =
    match sp with
      | None ->
          (fun _ug -> raise Not_found),
          (fun _ug _v -> ())
      | Some sp ->
          let rc = Eliom_sessions.get_request_cache sp in
          let table =
            try Polytables.get ~table:rc ~key:groups_key
            with Not_found ->
              let table = Hashtbl.create 37 in
              Polytables.set rc groups_key table;
              table
          in
          ((Hashtbl.find table), Hashtbl.add table)
  in
  let return u g v =
    update_cache (u, g) v; Lwt.return v
  in
  let rec aux2 g = function
    | [] -> Lwt.return false
    | g2::l ->
        aux g2 g >>= function
          | true -> return g2 g true
          | false -> aux2 g l
  and aux u g =
(*    User_sql.user_to_string u >>= fun su ->
    User_sql.user_to_string g >>= fun sg ->
    Ocsigen_messages.errlog (Printf.sprintf "Is %s in %s?" su sg); *)
    try Lwt.return (get_in_cache (u, g))
    with Not_found ->
      User_sql.groups_of_user u >>= fun gl ->
      if List.mem g gl
      then return u g true
      else aux2 g gl
  in
  if (user = nobody') || (group = nobody')
  then Lwt.return false
  else
    if (user = group) || (user = admin')
    then Lwt.return true
    else aux user group >>= function
      | true -> return user group true
      | false ->
          match sp with
            | Some sp ->
                get_user_id sp >>= fun user' ->
                  if user = basic_user user' then
                    fold_dyn_groups
                      (fun k f b ->
                         b >>= function
                           | true -> Lwt.return true
                           | false ->
                               f ~sp >>= function
                                 | false -> Lwt.return false
                                 | true ->
                                     if k = group then
                                       Lwt.return true
                                     else
                                       aux k group
                      ) (Lwt.return false) >>= function r ->
                      return user group r
                  else
                    return user group false
            | _ -> return user group false


let add_to_group ~(user:user) ~(group:user) =
  User_sql.get_user_data group >>= fun { user_dyn = dy } ->
  if dy
  then
    User_sql.user_to_string user >>= fun us ->
    User_sql.user_to_string group >>= fun gs ->
    Ocsigen_messages.warning
      ("Not possible to insert user "^ us ^
         " in group "^ gs ^
         ". This group is dynamic (risk of loops). (ignoring)");
    Lwt.return ()
  else
    if (user = nobody') || (group = nobody')
    then begin
      Ocsigen_messages.warning
        ("Not possible to insert user nobody into a group, or insert someone in group nobody. (ignoring)");
      Lwt.return ()
    end
    else
      in_group_ group user () >>= function
        | true ->
            User_sql.user_to_string user >>= fun us ->
            User_sql.user_to_string group >>= fun gs ->
            Ocsigen_messages.warning
              ("Circular group when inserting user "^ us ^ " in group "^ gs ^
                 ". (ignoring)");
            Lwt.return ()
        | false -> User_sql.add_to_group user group

(* XXX Should remove check that we do not remove from a dyn group *)
let remove_from_group = User_sql.remove_from_group


let add_to_groups ~user ~groups =
  Lwt_util.iter_serial (fun group -> add_to_group ~user ~group) groups




let iter_list_group f ~l ~group =
  List.fold_left
    (fun beg u ->
       beg >>= fun () ->
       f ~user:u ~group)
    (Lwt.return ())
    l

let add_list_to_group = iter_list_group add_to_group
let remove_list_from_group = iter_list_group User_sql.remove_from_group

let iter_user_list f ~user ~l =
  List.fold_left
    (fun beg g ->
       beg >>= fun () ->
       f ~user ~group:g)
    (Lwt.return ())
    l

let add_user_to_list = iter_user_list add_to_group
let remove_user_from_list = iter_user_list User_sql.remove_from_group



let is_logged_on ~sp =
  get_user_sd sp >>= fun u ->
  Lwt.return (not ((u = anonymous) || (u = nobody)))


(* This is a dynamic group that contains the currently logged user.
   It is almost entirely equivalent to a group that contains all the users,
   as only the users that are effectively able to logging can be inside.
*)
let authenticated_users =
  Lwt_unix.run
    (create_user ~name:"users" ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"Authenticated users" ~test:is_logged_on ()
     >>= fun users ->
     add_to_group ~user:(basic_user users) ~group:anonymous' >>= fun () ->
     Lwt.return users
)


let is_external_user ~sp =
  get_user_data sp >>= fun u ->
  Lwt.return (u.user_pwd = External_Auth)


let external_users =
  Lwt_unix.run
    (create_user ~name:"external_users" ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"Users using external authentification"
       ~test:is_external_user ()
)


let set_session_data ~sp (user_id, username) =
  Polytables.set
    (Eliom_sessions.get_request_cache sp) user_key (Lwt.return user_id);
  Eliom_sessions.set_persistent_data_session_group
    ~set_max:(Some 2) ~sp username >>= fun () ->
  (* We store the user_id inside Eliom. Alternatively, we could
     just use the session group (and not create a table inside Eliom
     at all), but we would just obtain a string, not an userid *)
  Eliom_sessions.set_persistent_session_data ~table:user_table ~sp user_id


let in_group ~sp ?user ~group () =
  (match user with
    | None -> get_user_id ~sp >>= fun u -> Lwt.return (basic_user u)
    | Some user -> Lwt.return user)
  >>= fun user ->
  in_group_ ~sp ?user ~group ()


let user_from_userlogin_xform user =
  get_user_by_name user >>= fun u ->
  if u = basic_user nobody && user <> nobody_login then
    Lwt.return (Xform.ConvError ("This user does not exists: " ^ user))
  else
    Lwt.return (Xform.Converted u)


module GenericRights = struct

  (* We need second-order polymorphism for the accessors on
     admin_writer_reader fields *)
  type admin_writer_reader_access =
      { field : 'a. 'a admin_writer_reader -> 'a parameterized_group }


  let grp_admin = { field = fun grp -> grp.grp_admin }
  let grp_write = { field = fun grp -> grp.grp_writer }
  let grp_read  = { field = fun grp -> grp.grp_reader }

  let map_awr f =
    f grp_admin,
    f grp_write,
    f grp_read


  let map_awr_lwt f =
    f grp_admin >>= fun a ->
    f grp_write >>= fun w ->
    f grp_read  >>= fun r ->
    Lwt.return (a, w, r)

  let iter_awr_lwt f =
    f grp_admin >>= fun () ->
    f grp_write >>= fun () ->
    f grp_read

  let admin_writer_reader_groups grps =
    (fun i -> apply_parameterized_group grps.grp_reader i),
    (fun i -> apply_parameterized_group grps.grp_writer i),
    (fun i -> apply_parameterized_group grps.grp_admin i)





  let create_admin_writer_reader ~prefix ~name ~descr ~find_param =
    let namea, namew, namer =
      (name ^ "Admin",
       name ^ "Writer",
       name ^ "Reader")
    and descra, descrw, descrr =
      ("can admin " ^ descr,
       "can write in " ^ descr,
       "can read " ^ descr)
    in
    let f = User_sql.new_parameterized_group ~prefix ~find_param in
    Lwt_unix.run (
      f namea descra >>= fun ga ->
      f namew descrw >>= fun gw ->
      f namer descrr >>= fun gr ->
      User_sql.add_generic_inclusion ~subset:ga ~superset:gw >>= fun() ->
      User_sql.add_generic_inclusion ~subset:gw ~superset:gr >>= fun () ->

      Lwt.return { grp_admin = ga; grp_writer = gw; grp_reader = gr }
    )

end

