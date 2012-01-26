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

open Eliom_pervasives
open User_sql.Types

let (>>=) = Lwt.bind
let (=|<) = Lwt.(=|<)
let (>|=) = Lwt.(>|=)
let (=<<) = Lwt.(=<<)

exception ConnectionRefused
exception BadPassword
exception BadUser
exception UnknownUser of string
exception UseAuth of userid

let const x _ = x
let iter_option f = function Some x -> f x | None -> ()

module Request_cache = struct

  type 'a t = 'a lazy_t Eliom_references.eref

  let from_fun : (unit -> 'a) -> 'a t =
    fun f ->
      Eliom_references.eref
        ~scope:Eliom_common.request
        (Lazy.lazy_from_fun f)

  let get : 'a t -> 'a Lwt.t =
    fun eref -> 
      Lazy.force =|< Eliom_references.get eref

  let get_lwt : 'a Lwt.t lazy_t Eliom_references.eref -> 'a Lwt.t =
    fun eref ->
      get eref >>= fun x -> x

end


(* YYY not really sure that all User_sql functions that transforms a
   string/id into a user properly return NotAnUser when they fail.
   Thus we still catch Not_found, just in case... *)


(* We might want to simply overwrite incorrect values by the correct ones *)
let possibly_create ~login ~fullname ?email ?pwd () =
  Lwt_unix.run
    (try_lwt
       User_sql.get_basicuser_by_login login
     with User_sql.NotAnUser | Not_found ->
       let email = match email with
         | None -> None
         | Some f -> f ()
       and password = match pwd with
         | None -> Connect_forbidden
         | Some f -> f ()
       in
       fst =|< User_sql.new_user
         ~name:login
         ~password
         ~fullname
         ?email
         ~dyn:false)


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
       ~descr:"can create new groups")

let group_can_admin_group : [`User] parameterized_group =
  Lwt_unix.run
    (User_sql.new_parameterized_group ~prefix:"users"
       ~name:"can_admin_group"
       ~descr:"can add or remove people in the group"
       ~find_param:param_user)

let group_can_create_users =
  Lwt_unix.run
    (User_sql.new_nonparameterized_group ~prefix:"users" ~name:"GroupsCreators"
       ~descr:"can create new Ocsimore users")




let get_basicuser_by_login login =
  try_lwt
    User_sql.get_basicuser_by_login login
  with Not_found | User_sql.NotAnUser ->
    Lwt.return nobody

let get_user_by_name name =
  try_lwt
    User_sql.get_user_by_name name
  with Not_found | User_sql.NotAnUser ->
      Lwt.return nobody'


let user_list_of_string s =
  let f beg a =
    lwt beg = beg in
    try_lwt
      User_sql.get_user_by_name a >>= fun v ->
      if v = nobody'
      then Lwt.return beg
      else Lwt.return (v::beg)
    with User_sql.NotAnUser ->
      Lwt.fail (UnknownUser a)
  in
  let r = String.split '\n' s in
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
  DynGroups.add table,
  (fun k () ->
     try
       DynGroups.find table k ()
     with Not_found -> Lwt.return false),
  (fun f -> DynGroups.fold f table)


let ok_name name =
  try
    ignore (String.index name '#'); false
  with Not_found -> true

let create_user, create_fresh_user =
  let mutex_user = Lwt_mutex.create () in
  let aux already_existing ~name ~pwd ~fullname ?email ?test () =
    if ok_name name = false then
      Lwt.fail BadUser
    else
      lwt () = Lwt_mutex.lock mutex_user in
      lwt u = get_basicuser_by_login name in
      lwt u =
        if (u = nobody) && (name != nobody_login)
        then (* the user does not exist *)
          let dyn = not (test = None) in
          fst =|< User_sql.new_user ~name ~password:pwd ~fullname ~email ~dyn
        else
          already_existing u
      in
      iter_option (add_dyn_group (basic_user u)) test;
      Lwt_mutex.unlock mutex_user;
      Lwt.return u
  in
  aux (fun u -> Lwt.return u),
  aux (fun _ -> raise BadUser) ?test:None



let authenticate ~name ~pwd =
  lwt u = get_basicuser_by_login name in
  if (u = nobody) then
    Lwt.fail BadUser
  else
    lwt u = User_sql.get_basicuser_data u in
    match u.user_pwd with
      | User_sql.Types.External_Auth ->
          Lwt.fail (UseAuth u.user_id)
      | Ocsimore_user_plain p ->
          if p = pwd then
            Lwt.return u
          else
            Lwt.fail BadPassword
      | Ocsimore_user_crypt h ->
          lwt ok = Crypt.check_passwd ~passwd:pwd ~hash:h in
          if ok then
            Lwt.return u
          else
            Lwt.fail BadPassword
      | Connect_forbidden ->
          Lwt.fail BadPassword


(** {2 Session data} *)

let user_table: userid Eliom_state.persistent_table =
  Eliom_state.create_persistent_table
    ~scope:(Eliom_common.session:>Eliom_common.user_scope)
    "ocsimore_user_table_v1"

let user_ref =
  Eliom_references.eref
    ~scope:Eliom_common.session
    ~persistent:"ocsimore_user_table_v1"
    None

let get_user_ () =
  Eliom_references.get user_ref >>= function
    | Some u ->
        begin try_lwt
          const u =|< User_sql.get_basicuser_data u
        with User_sql.NotAnUser | Not_found ->
          lwt () = Eliom_state.discard ~scope:Eliom_common.session () in
          Polytables.clear (Eliom_request_info.get_request_cache ());
          Lwt.return anonymous
        end
    | None -> Lwt.return anonymous

let user_request_ref =
  Request_cache.from_fun get_user_

let get_user_sd () =
  Request_cache.get_lwt user_request_ref

let get_user_id () =
  get_user_sd ()

let get_user_data () =
  User_sql.get_basicuser_data =<< get_user_sd ()

let get_user_name () =
  let user_login { user_login } = user_login in
  user_login =|< get_user_data ()

let groups_cache_table_ref =
  Request_cache.from_fun (fun () -> Hashtbl.create 37)

let in_group_ ~user ~group () =
  lwt get_in_cache, update_cache =
    lwt table = Request_cache.get groups_cache_table_ref in
    Lwt.return (Hashtbl.find table, Hashtbl.add table)
  in
  let return u g v =
    update_cache (u, g) v;
    Lwt.return v
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
    try
      Lwt.return (get_in_cache (u, g))
    with Not_found ->
      lwt gl = User_sql.groups_of_user u in
      if List.mem g gl then
        return u g true
      else aux2 g gl
  in
  if (user = nobody') || (group = nobody') then
    Lwt.return false
  else
    if (user = group) || (user = admin') then
      Lwt.return true
    else
      aux user group >>= function
        | true ->
            return user group true
        | false ->
            lwt user' = get_user_id () in
            if user = basic_user user' then
              return user group =<< fold_dyn_groups
                (fun k f b ->
                   b >>= function
                     | true -> Lwt.return true
                     | false ->
                         f () >>= function
                           | false -> Lwt.return false
                           | true ->
                               if k = group then
                                 Lwt.return true
                               else
                                 aux k group)
                (Lwt.return false)
            else
              return user group false

let add_to_group ~(user:user) ~(group:user) =
  lwt { user_dyn = dy } = User_sql.get_user_data group in
  if dy
  then
    lwt us = User_sql.user_to_string user in
    lwt gs = User_sql.user_to_string group in
    Ocsigen_messages.warning
      ("Not possible to insert user "^ us ^
       " in group "^ gs ^
       ". This group is dynamic (risk of loops). (ignoring)");
    Lwt.return ()
  else
    if (user = nobody') || (group = nobody') then begin
      Ocsigen_messages.warning
        ("Not possible to insert user nobody into a group, or insert someone in group nobody. (ignoring)");
      Lwt.return ()
    end else
      in_group_ group user () >>= function
        | true ->
            lwt us = User_sql.user_to_string user in
            lwt gs = User_sql.user_to_string group in
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
       lwt () = beg in
       f ~user:u ~group)
    (Lwt.return ())
    l

let add_list_to_group = iter_list_group add_to_group
let remove_list_from_group = iter_list_group User_sql.remove_from_group

let iter_user_list f ~user ~l =
  List.fold_left
    (fun beg g ->
       lwt () = beg in
       f ~user ~group:g)
    (Lwt.return ())
    l

let add_user_to_list = iter_user_list add_to_group
let remove_user_from_list = iter_user_list User_sql.remove_from_group

let is_logged_on () =
  get_user_sd ()
    >|= fun u -> not ((u = anonymous) || (u = nobody))

(* This is a dynamic group that contains the currently logged user.
   It is almost entirely equivalent to a group that contains all the users,
   as only the users that are effectively able to logging can be inside.
*)
let authenticated_users =
  Lwt_unix.run
    (lwt users =
       create_user
         ~name:"users"
         ~pwd:User_sql.Types.Connect_forbidden
         ~fullname:"Authenticated users"
         ~test:is_logged_on ()
     in
     lwt () = add_to_group ~user:(basic_user users) ~group:anonymous' in
     Lwt.return users)


let is_external_user () =
  get_user_data ()
    >|= fun { user_pwd } -> user_pwd = External_Auth

let external_users =
  Lwt_unix.run
    (create_user
       ~name:"external_users"
       ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"Users using external authentification"
       ~test:is_external_user ())

let set_session_data (user_id, username) =
  lwt () = Eliom_references.set user_ref (Some user_id) in
  lwt () =
    Eliom_state.set_persistent_data_session_group
      ~scope:Eliom_common.session
      ~set_max:(Some 2)
      username
  in
  (* We store the user_id inside Eliom. Alternatively, we could
     just use the session group (and not create a table inside Eliom
     at all), but we would just obtain a string, not an userid *)
  Eliom_state.set_persistent_data ~table:user_table user_id


let in_group ?user ~group () =
  lwt user =
    match user with
      | None -> basic_user =|< get_user_id ()
      | Some user -> Lwt.return user
  in
  in_group_ ?user ~group ()


let user_from_userlogin_xform user =
  lwt u = get_user_by_name user in
  Lwt.return 
    (if u = basic_user nobody && user <> nobody_login then
       Xform.ConvError ("This user does not exists: " ^ user)
     else
       Xform.Converted u)

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
    lwt a = f grp_admin in
    lwt w = f grp_write in
    lwt r = f grp_read in
    Lwt.return (a, w, r)

  let iter_awr_lwt f =
    lwt () = f grp_admin in
    lwt () = f grp_write in
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
    Lwt_unix.run
      (lwt ga = f namea descra in
       lwt gw = f namew descrw in
       lwt gr = f namer descrr in
       lwt () = User_sql.add_generic_inclusion ~subset:ga ~superset:gw in
       lwt () = User_sql.add_generic_inclusion ~subset:gw ~superset:gr in
       Lwt.return { grp_admin = ga; grp_writer = gw; grp_reader = gr })

end

