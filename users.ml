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

let (>>=) = Lwt.bind

type userdata = 
    { id: User_sql.userid;
      name: string;
      mutable pwd: string option;
      mutable fullname: string;
      mutable email: string;
    }
      
exception UserExists
exception NotAllowed
exception BadPassword
exception NoSuchUser
exception Users_error of string

type user = userdata

let get_user_by_name_from_db ~name =
  User_sql.find_user ~name () >>= fun ((i, n, p, d, e), pm) -> 
  Lwt.return { id = i; 
               name = n; 
               pwd = p; 
               fullname = d; 
               email = e;
             }

let get_user_id_by_name name =
  User_sql.find_user ~name () >>= fun ((i, n, p, d, e), pm) -> 
  Lwt.return i

let get_user_name_by_id id =
  User_sql.find_user ~id () >>= fun ((i, n, p, d, e), pm) -> 
  Lwt.return n

let get_user_by_id_from_db ~id =
  User_sql.find_user ~id () >>= fun ((i, n, p, d, e), pm) -> 
  Lwt.return { id = i; 
               name = n; 
               pwd = p; 
               fullname = d; 
               email = e;
             }


let create_anonymous () =
  Lwt.catch
    (fun () -> get_user_by_name_from_db ~name:"anonymous")
    (function
       | Not_found ->
           (User_sql.new_user 
              ~name:"anonymous" 
              ~password:None
              ~fullname:"Anonymous"
              ~email:""
              ~groups:[]
            >>= fun i ->
           Lwt.return { id = i;
                        name = "anonymous"; 
                        pwd = None; 
                        fullname = "Anonymous"; 
                        email = "";
                      })
       | e -> Lwt.fail e)

let anonymous = Lwt_unix.run (create_anonymous ())


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
    (fun () -> get_user_by_name_from_db ~name:"admin")
    (function
       | Not_found ->
           let pwd = ask_pwd () in
           let email = ask_email () in
           (User_sql.new_user 
              ~name:"admin" 
              ~password:(Some pwd)
              ~fullname:"Admin"
              ~email:email
              ~groups:[]
            >>= fun i ->
           Lwt.return { id = i;
                        name = "admin"; 
                        pwd = Some pwd; 
                        fullname = "Admin"; 
                        email = email;
                      })
       | e -> Lwt.fail e)

let admin = Lwt_unix.run (create_admin ())


let get_user_by_name ~name =
  if name = anonymous.name
  then Lwt.return anonymous
  else
  if name = admin.name
  then Lwt.return admin
  else get_user_by_name_from_db ~name

let get_user_by_id ~id =
  if id = anonymous.id
  then Lwt.return anonymous
  else
  if id = admin.id
  then Lwt.return admin
  else get_user_by_id_from_db ~id

let add_to_group ~user ~group =
  User_sql.add_to_group user.id group

let create_user ~name ~pwd ~fullname ~email ~groups =
  Lwt.catch 
    (fun () -> get_user_by_name ~name >>= fun _ -> Lwt.fail UserExists)
    (function 
       | Not_found ->
           (User_sql.new_user ~name ~password:pwd ~fullname ~email ~groups
           >>= fun i ->
           Lwt.return 
             { id = i;
               name = name; 
               pwd = pwd; 
               fullname = fullname; 
               email = email;
             }
           )
       | e -> Lwt.fail e)


let generate_password () = 
  let chars = "0123456789"^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"^
    "abcdefghijklmnopqrstuvwxyz" in
  let pwd = String.create 8 in
    for i = 0 to 7 do
      pwd.[i] <- String.get chars (Random.int (10+2*26))
    done;
    Some pwd


let mail_password ~name ~from_addr ~subject =
  Lwt.catch
    (fun () -> 
       get_user_by_name ~name >>= fun user -> 
       Lwt_preemptive.detach
         (fun () -> 
            ignore(Netaddress.parse user.email);
            Netsendmail.sendmail
              ~mailer:"/usr/sbin/sendmail"
              (Netsendmail.compose
                 ~from_addr
                 ~to_addrs:[(user.fullname, user.email)]
                 ~subject
                 ("This is an auto-generated message. "
                  ^ "Please do not reply to it.\n"
                  ^ "\n"
                  ^ "Your account is:\n"
                  ^ "\tUsername:\t" ^ name ^ "\n"
                  ^ "\tPassword:\t" ^ (match user.pwd with 
                                         | Some p -> p
                                         | _ -> "(NONE)")
                  ^ "\n"));
            true) ())
    (function _ -> Lwt.return false)

let create_unique_user =
  let digit s = s.[0] <- String.get "0123456789" (Random.int 10); s in
  let lock = Lwt_mutex.create () in
  fun ~name ~pwd ~fullname ~email ~groups ->
    let rec suffix n =
      Lwt.catch
        (fun () -> 
           get_user_by_name ~name:n >>= fun _ -> 
           suffix (n ^ (digit "X")))
        (function
           | Not_found -> 
               (create_user
                  ~name:n ~pwd ~fullname ~email ~groups >>= fun x -> 
                  Lwt.return (x, n))
           | e -> Lwt.fail e)
    in
    Lwt_mutex.lock lock >>= fun () ->
    suffix name >>= fun r ->
    Lwt_mutex.unlock lock;
    Lwt.return r

let update_user_data ~user 
    ?(pwd = user.pwd)
    ?(fullname = user.fullname) 
    ?(email = user.email)
    ?groups () =
  user.pwd <- pwd;
  user.fullname <- fullname;
  user.email <- email;
  User_sql.update_data
    ~id:user.id
    ~name:user.name
    ~password:pwd
    ~fullname
    ~email
    ?groups
    ()



let authenticate ~name ~pwd =
  Lwt.catch
    (fun () -> 
       get_user_by_name name >>= fun u -> 
       if u.pwd = (Some pwd) 
       then Lwt.return u
       else Lwt.fail BadPassword)
    (function
       | Not_found -> Lwt.fail NoSuchUser
       | e -> Lwt.fail e)


let in_group ~user ~group =
  let rec aux2 g = function
    | [] -> Lwt.return false
    | g2::l -> 
        aux g2 g >>= function
          | true -> Lwt.return true
          | false -> aux2 g l
  and aux u g =
    User_sql.get_groups u >>= fun gl ->
    if List.mem g gl
    then Lwt.return true
    else aux2 g gl
  in
  if (user = group) || (group = anonymous.id)
  then Lwt.return true
  else if user = anonymous.id
  then Lwt.return false
  else if user = admin.id
  then Lwt.return true
  else aux user group

let delete_user ~user =
  User_sql.delete_user user



(** {2 Session data} *)

let user_table: userdata option Eliom_sessions.persistent_table = 
  Eliom_sessions.create_persistent_table "ocsimore_user_table_v1"

type user_sd = 
    userdata Lwt.t (* Lazy.t not really useful to make it lazy here *)

(** The polytable key for retrieving user data inside session data *)
let user_key : user_sd Polytables.key = Polytables.make_key ()

let get_user_data ~sp =
  Eliom_sessions.get_persistent_session_data ~table:user_table ~sp ()
  >>= function
    | Eliom_sessions.Data (Some u) -> Lwt.return u
    | Eliom_sessions.Data None -> Lwt.return admin
    | _ -> Lwt.return anonymous

let get_user_sd ~sp ~sd =
  try
    Polytables.get ~table:sd ~key:user_key
  with Not_found -> 
    let ud = get_user_data ~sp in
    Polytables.set sd user_key ud;
    ud

let get_user_data ~sp ~sd = get_user_sd sp sd
          
let get_user_id ~sp ~sd = get_user_sd sp sd >>= fun u -> Lwt.return u.id
        
let get_user_name ~sp ~sd = get_user_sd sp sd >>= fun u -> Lwt.return u.name
        
let is_logged_on ~sp ~sd = 
  get_user_sd sp sd >>= fun u -> 
  Lwt.return (not (u == anonymous))


let anonymous_sd = 
  let sd = Polytables.create () in
  Polytables.set sd user_key (Lwt.return anonymous);
  sd

let set_session_data ~sp ~sd user =
  Polytables.set sd user_key (Lwt.return user);
  if user == admin
  then
    Eliom_sessions.set_persistent_session_data user_table sp None
  else
    Eliom_sessions.set_persistent_session_data user_table sp (Some user)
