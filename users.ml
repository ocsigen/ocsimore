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

exception NotAllowed
exception BadPassword
exception BadUser
exception UnknownUser of string
exception UseAuth of userid
exception Users_error of string


(* We might want to simply overwrite incorrect values by the correct ones *)
let possibly_create ~login ~fullname ?email ?pwd () =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> User_sql.get_basicuser_by_login login)
      (function
         | Not_found ->
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
                ~groups:[]
                ~dyn:false
              >>= fun (i, _) -> Lwt.return i)
         | e -> Lwt.fail e)
  )


let anonymous = possibly_create ~login:"anonymous" ~fullname:"Anonymous" ()
let anonymous' = basic_user anonymous
let nobody_login = "nobody"
let nobody = possibly_create ~login:nobody_login ~fullname:"Nobody" ()
let nobody' = basic_user nobody

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

let get_basicuser_by_login login =
  Lwt.catch
  (fun () -> User_sql.get_basicuser_by_login login)
  (function
     | Not_found -> Lwt.return nobody
     | e -> Lwt.fail e)

let get_user_by_name name =
  Lwt.catch
    (fun () -> User_sql.get_user_by_name name)
    (function
       | Not_found -> Lwt.return nobody'
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
         | Not_found -> Lwt.fail (UnknownUser a)
         | e -> Lwt.fail e)
  in
  let r = Ocsigen_lib.split ' ' s in
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
   (fun k ~sp ~sd ->
      try
        DynGroups.find table k ~sp ~sd
      with Not_found -> Lwt.return false),
   fun f -> DynGroups.fold f table
  )



let create_user ~name ~pwd ~fullname ?email ~groups ?test () =
  get_basicuser_by_login name
  >>= fun u ->
  (if (u = nobody) && (name != nobody_login)
   then (* the user does not exist *)
     let dyn = not (test = None) in
     User_sql.new_user ~name ~password:pwd ~fullname ~email ~groups ~dyn
     >>= fun (u, _) -> Lwt.return u
   else
     Lwt.return u)
   >>= fun u ->
   (match test with
     | None -> ()
     | Some f -> add_dyn_group (basic_user u) f
   );
   Lwt.return u


let create_unique_user =
  (* Buggy if all users with 0-9 exist *)
  let digit s = s.[0] <- String.get "0123456789" (Random.int 10); s in
  let lock = Lwt_mutex.create () in
  fun ~name ~pwd ~fullname ?email ~groups ->
    let rec suffix name =
      get_basicuser_by_login name
      >>= fun u ->
      if (u = nobody) && (name != nobody_login)
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
  User_sql.update_data
    ~userid:user.user_id
    ~password:pwd
    ~fullname
    ~email
    ?groups
    ()
*)


let authenticate ~name ~pwd =
  get_basicuser_by_login name >>= fun u ->
  if (u = nobody)
  then Lwt.fail BadUser
  else
    User_sql.get_basicuser_data u
    >>= fun u ->
      match u.user_pwd with
        | User_sql.Types.External_Auth -> Lwt.fail (UseAuth u.user_id)
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
    User_sql.groups_of_user u >>= fun gl ->
    if List.mem g gl
    then Lwt.return true
    else aux2 g gl
  in
  if (user = nobody') || (group = nobody')
  then Lwt.return false
  else
    if (user = group) || (user = admin')
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


let iter_list_group f ~l ~group =
  List.fold_left
    (fun beg u ->
       beg >>= fun () ->
       f ~user:u ~group)
    (Lwt.return ())
    l

let add_list_to_group = iter_list_group add_to_group
let remove_list_from_group = iter_list_group User_sql.remove_from_group


(** {2 Session data} *)

let user_table: userid Eliom_sessions.persistent_table =
  Eliom_sessions.create_persistent_table "ocsimore_user_table_v1"

type user_sd = userid Lwt.t

(** The polytable key for retrieving user data inside session data *)
let user_key : user_sd Polytables.key = Polytables.make_key ()

let get_user_ ~sp ~sd =
  Eliom_sessions.get_persistent_session_data ~table:user_table ~sp ()
  >>= function
    | Eliom_sessions.Data u ->
        Lwt.catch
          (fun () -> User_sql.get_basicuser_data u >>= fun _ud -> Lwt.return u)
          (function
             | Not_found ->
                 Eliom_sessions.close_session ~sp () >>= fun () ->
                 Ocsimore_common.clear_sd ~sd;
                 Lwt.return anonymous
             | e -> Lwt.fail e)
    | Eliom_sessions.Data_session_expired | Eliom_sessions.No_data ->
        Lwt.return anonymous

let get_user_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:user_key
  with Not_found ->
    let ud = get_user_ ~sp ~sd in
    Polytables.set sd user_key ud;
    ud

let get_user_id ~sp ~sd = get_user_sd sp sd

let get_user_data ~sp ~sd =
  get_user_sd sp sd >>= fun u ->
  User_sql.get_basicuser_data u

let get_user_name ~sp ~sd =
  get_user_data sp sd >>= fun u -> 
  Lwt.return u.user_login

let is_logged_on ~sp ~sd =
  get_user_sd sp sd >>= fun u ->
  Lwt.return (not ((u = anonymous) || (u = nobody)))

let authenticated_users =
  Lwt_unix.run
    (create_user ~name:"users" ~pwd:User_sql.Types.Connect_forbidden
       ~fullname:"Authenticated users"
       ~groups:[anonymous']
       ~test:is_logged_on
       ())

let anonymous_sd =
  let sd = Polytables.create () in
  Polytables.set sd user_key (Lwt.return anonymous);
  sd

let set_session_data ~sp ~sd user =
  Polytables.set sd user_key (Lwt.return user);
  Eliom_sessions.set_persistent_session_data ~table:user_table ~sp user


let in_group ~sp ~sd ?user ~group () =
  (match user with
    | None -> get_user_id ~sp ~sd >>= fun u -> Lwt.return (basic_user u)
    | Some user -> Lwt.return user)
  >>= fun user ->
  in_group_ ~sp ~sd ?user ~group ()


module GenericRights = struct

  (* We need second-order polymorphism for the accessors on
     admin_writer_reader fields *)
  type admin_writer_reader_access =
      { field : 'a. 'a admin_writer_reader -> 'a parameterized_group }


  let grp_admin = { field = fun grp -> grp.grp_admin }
  let grp_write = { field = fun grp -> grp.grp_writer }
  let grp_read  = { field = fun grp -> grp.grp_reader }

  let can_sthg f =
    f grp_admin,
    f grp_write,
    f grp_read

  let admin_writer_reader_groups grps =
    (fun i -> apply_parameterized_group grps.grp_reader i),
    (fun i -> apply_parameterized_group grps.grp_writer i),
    (fun i -> apply_parameterized_group grps.grp_admin i)


  let opaque_int32_to_string v = Int32.to_string (Opaque.t_int32 v)
  let string_to_opaque_int32 s = Opaque.int32_t (Int32.of_string s)


  let update_perms (add : string) (rem : string) grp arg =
    let group = grp $ arg in
    user_list_of_string add >>= fun addl ->
    user_list_of_string rem >>= fun reml ->
    add_list_to_group ~l:addl ~group >>= fun () ->
    remove_list_from_group ~l:reml ~group


  let ( ** ) = Eliom_parameters.prod

  type 'a grp_helper = {
    grp_eliom_params : (string * string, [ `WithoutSuffix ],
                        [ `One of string ] Eliom_parameters.param_name *
                        [ `One of string ] Eliom_parameters.param_name)
      Eliom_parameters.params_type;

    grp_eliom_arg_param:
      ('a Opaque.int32_t, [ `WithoutSuffix ],
       [ `One of 'a Opaque.int32_t ] Eliom_parameters.param_name)
      Eliom_parameters.params_type;

    grp_form:
      'a Opaque.int32_t -> text:string ->
        ([ `One of string ] Eliom_parameters.param_name *
         [ `One of string ] Eliom_parameters.param_name ->
          {{ Xhtmltypes_duce.inlines }})
          Lwt.t;

    grp_form_arg:
      'a Opaque.int32_t ->
      [`One of 'a Opaque.int32_t ] Eliom_parameters.param_name ->
      Xhtmltypes_duce.inline_forms;

    grp_save:
      'a Opaque.int32_t * (string * string) -> unit Lwt.t
  }


  let helpers_group ~prefix ~name ~grp =
    let name = prefix ^ "#" ^ name in
    let add = "add#" ^ name and rem = "rem#" ^ name in
    let params = (Eliom_parameters.string add) ** (Eliom_parameters.string rem)

    and param_arg = Eliom_parameters.user_type
      string_to_opaque_int32 opaque_int32_to_string "arg"

    and form_arg value arg_name =
      Eliom_duce.Xhtml.user_type_input opaque_int32_to_string
               ~input_type:{: "hidden" :} ~name:arg_name ~value ()

    and form arg ~(text : string) =
      User_sql.users_in_group ~group:(grp $ arg) >>= fun users ->
      (List.fold_left
         (fun s r ->
            s >>= fun s ->
            User_sql.user_to_string r
            >>= fun s2 -> Lwt.return (s^" "^s2))
         (Lwt.return "")
         users
      )
      >>= fun members ->
      let string_input arg =
        Eliom_duce.Xhtml.string_input ~input_type:{: "text" :} ~name:arg () in
      Lwt.return (fun (iadd, irem) ->
                    let textadd = "Add users: "
                    and textrem = "Remove users: " in
                    {{ [
                         !{: text :}    !{: members :}          <br>[]
                         !{: textadd :} {: string_input iadd :} <br>[]
                         !{: textrem :} {: string_input irem :} <br>[]
                       ] }}
                 )

    and update_perms (arg, (add, rem)) = update_perms add rem grp arg
    in
    {grp_eliom_params = params;
     grp_eliom_arg_param = param_arg;
     grp_form = form;
     grp_form_arg = form_arg;
     grp_save = update_perms}



  let create_admin_writer_reader ~prefix ~name ~descr =
    let namea, namew, namer =
      (name ^ "Admin",
       name ^ "Writer",
       name ^ "Reader")
    and descra, descrw, descrr =
      ("All rights on " ^ descr,
       "Can write in " ^ descr,
       "Can read the " ^ descr)
    in
    Lwt_unix.run (
      User_sql.new_parametrized_group prefix namea descra >>= fun ga ->
      User_sql.new_parametrized_group prefix namew descrw >>= fun gw ->
      User_sql.new_parametrized_group prefix namer descrr >>= fun gr ->
      User_sql.add_generic_inclusion ~subset:ga ~superset:gw >>= fun() ->
      User_sql.add_generic_inclusion ~subset:gw ~superset:gr >>= fun () ->

      Lwt.return { grp_admin = ga; grp_writer = gw; grp_reader = gr }
    )

  let helpers_admin_writer_reader ~prefix ~name groups =
    let h = helpers_group ~name in
    let helpa = h ~prefix:("adm#" ^ prefix)  ~grp:groups.grp_admin
    and helpw = h ~prefix:("wri#" ^ prefix)  ~grp:groups.grp_writer
    and helpr = h ~prefix:("read#" ^ prefix) ~grp:groups.grp_reader in

    let params = helpa.grp_eliom_params **
      (helpw.grp_eliom_params ** helpr.grp_eliom_params)

    and save ~sp ~sd (arg, ((adda, rema), ((addw, remw), (addr, remr)))) =
      in_group ~sp ~sd ~group:(groups.grp_admin $ arg) () >>= function
        | true ->
            helpa.grp_save (arg, (adda, rema)) >>= fun () ->
            helpw.grp_save (arg, (addw, remw)) >>= fun () ->
            helpr.grp_save (arg, (addr, remr))
        | false -> Lwt.fail Ocsimore_common.Permission_denied
    in
    let service () = Eliom_predefmod.Any.register_new_post_coservice'
      ~name:(prefix ^ "." ^ name ^ ".permissions")
      ~post_params:(helpa.grp_eliom_arg_param ** params)
      (fun sp () args ->
         let sd = Ocsimore_common.get_sd sp in
         save ~sp ~sd args >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp Eliom_services.void_coservice')
    in
    (service,

     (fun v ->
     helpa.grp_form v "Current administrators: " >>= fun forma ->
     helpw.grp_form v "Current writers: "        >>= fun formw ->
     helpr.grp_form v "Current readers: "        >>= fun formr ->
     let form (arg, (arga, (argw, argr))) =
       {{ [ <p>[ {: helpa.grp_form_arg v arg :}
                 !{: formr argr :}
                 !{: formw argw :}
                 !{: forma arga :}
                 {: Eliom_duce.Xhtml.button ~button_type:{: "submit" :}
                    {{ "Save" }} :}
              ] ] }}
     in
     Lwt.return form))


  type 'a params_save_permissions =
      [ `One of 'a Opaque.int32_t ] Eliom_parameters.param_name *
        ((input_string * input_string) *
           ((input_string * input_string) * (input_string * input_string)))
  and input_string = [ `One of string ] Eliom_parameters.param_name


end
