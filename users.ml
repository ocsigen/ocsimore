(* The structure for users is a complete lattice, whose elements are
   sets of sets, ordered inclusion: the bottom element is the empty
   set (the anonymous user), the top element is the set of all sets
   (the admin user).

   In this model, users and groups are the same concept; we only
   distinguish, for practical matters, between "login enabled" users
   and "group only" users: the former has some (eventually void)
   password, the latter has not. "U is in group V" means, in fact,
   that there are some U1, U2, ..., Un such that U contains U1 and U1
   contains U2 and ... and Un contains V. *)

open Lwt

type userdata = 
    { id: int32;
      name: string;
      mutable pwd: string option;
      mutable fullname: string;
      mutable email: string }
      
module M = SetOfSets.Make(
  struct 
    type t = userdata 
    let compare u u' = Pervasives.compare u.name u'.name 
  end)

open M
open M.SSet
open Sql

exception Loop
exception UserExists
exception NotAllowed
exception BadPassword
exception NoSuchUser
exception Users_error of string

type user = M.SSet.elt

let update_permissions db ~user =
  User_sql.update_permissions 
    db ~name:user.data.name ~perm:(Marshal.to_string user.set [])

let get_user_by_name_from_db db ~name =
  User_sql.find_user db ~name () >>= fun (i, n, p, d, e, pm) -> 
  let data = { id = i; 
               name = n; 
               pwd = p; 
               fullname = d; 
               email = e } 
  in
  let user = { data = data; 
               set = match pm with
		 | None -> empty
		 | Some x -> (Marshal.from_string x 0) } 
  in
  return user

let create_anonymous db =
  catch
    (fun () -> get_user_by_name_from_db db ~name:"anonymous")
    (function
       | Not_found ->
	   (User_sql.new_user 
              db ~name:"anonymous" ~password:None
              ~fullname:"Anonymous" ~email:"" >>= fun i ->
	   let data = { id = i;
                        name = "anonymous"; 
                        pwd = None; 
                        fullname = "Anonymous"; 
                        email = "" }	
           in
	   Lwt.return { data = data; set = empty })
       | e -> Lwt.fail e)

let anonymous =
  Lwt_unix.run 
    (Lwt_pool.use Sql.pool (fun db -> create_anonymous db))


let create_admin db =
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
    (fun () -> get_user_by_name_from_db db ~name:"admin")
    (function
       | Not_found ->
           let pwd = ask_pwd () in
           let email = ask_email () in
	   (User_sql.new_user 
              db ~name:"admin" ~password:(Some pwd)
              ~fullname:"Admin" ~email:email >>= fun i ->
	   let data = { id = i;
                        name = "admin"; 
                        pwd = Some pwd; 
                        fullname = "Admin"; 
                        email = email }	
           in
	   Lwt.return { data = data; set = singleton anonymous })
       | e -> Lwt.fail e)



let admin =
  Lwt_unix.run 
    (Lwt_pool.use Sql.pool (fun db -> create_admin db))


let get_user_by_name db ~name =
  if name = "anonymous" 
  then return anonymous
  else
  if name = "admin" 
  then return admin
  else get_user_by_name_from_db db ~name

let create_user db ~name ~pwd ~fullname ~email =
  catch 
    (fun () -> get_user_by_name db ~name)
    (function 
       | Not_found ->
	   (User_sql.new_user db ~name ~password:pwd ~fullname ~email 
           >>= fun i ->
	   let data = { id = i;
                        name = name; 
                        pwd = pwd; 
                        fullname = fullname; 
                        email = email }	
           in
	   let user = { data = data; set = singleton anonymous } in
	   update_permissions db ~user >>= fun () -> 
           return user)
       | e -> fail e)



let generate_password () = 
  let chars = "0123456789"^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"^
    "abcdefghijklmnopqrstuvwxyz" in
  let pwd = String.create 8 in
    for i = 0 to 7 do
      pwd.[i] <- String.get chars (Random.int (10+2*26))
    done;
    Some pwd


let mail_password db ~name ~from_addr ~subject =
  catch
    (fun () -> 
       get_user_by_name db ~name >>= fun user -> 
       Lwt_preemptive.detach
	 (fun () -> 
            ignore(Netaddress.parse user.data.email);
            Netsendmail.sendmail
              ~mailer:"/usr/sbin/sendmail"
              (Netsendmail.compose
                 ~from_addr
                 ~to_addrs:[(user.data.fullname, user.data.email)]
                 ~subject
                 ("This is an auto-generated message. "
                  ^ "Please do not reply to it.\n"
                  ^ "\n"
                  ^ "Your account is:\n"
                  ^ "\tUsername:\t" ^ name ^ "\n"
                  ^ "\tPassword:\t" ^ (match user.data.pwd with Some p -> p
                                         | _ -> "(NONE)")
                  ^ "\n"));
            true) ())
    (function _ -> return false)

let create_unique_user =
  let digit s = s.[0] <- String.get "0123456789" (Random.int 10); s in
  let lock = Lwt_mutex.create () in
  fun db ~name ~pwd ~fullname ~email ->
    let rec suffix n =
      catch
        (fun () -> 
           get_user_by_name db ~name:n >>= fun _ -> 
           suffix (n ^ (digit "X")))
        (function
	   | Not_found -> 
               (create_user db ~name:n ~pwd ~fullname ~email >>= fun x -> 
                  Lwt.return (x, n))
           | e -> fail e)
    in
    Lwt_mutex.lock lock >>= fun () ->
    suffix name >>= fun r ->
    Lwt_mutex.unlock lock;
    Lwt.return r

let get_user_data ~user =
  let { id = i; name = n; pwd = p; fullname = d; email = e} = user.data in
  (i, n, p, d, e);;

let update_user_data db ~user =
  let d = user.data
  in fun ?(pwd = d.pwd) ?(fullname = d.fullname) ?(email = d.email) () ->
    d.pwd <- pwd;
    d.fullname <- fullname;
    d.email <- email;
    User_sql.update_data db
      ~id:user.data.id
      ~name:user.data.name
      ~password:pwd
      ~fullname
      ~email



let ( <-?- ) user1 user2 =
	mem user2 user1.set

let rec ( <-??- ) user1 user2 =
	user1 <-?- user2 || exists (fun elt -> elt <-??- user2) user1.set

let ( <--- ) user1 user2 =
	if user2 = user1 || user2 <-??- user1
	then raise Loop
	else user1.set <- add user2 user1.set

let ( <-/- ) user1 user2 =
	if user1 <-?- user2
	then user1.set <- union user2.set (remove user2 user1.set)
	else ()

let rec ( <-//- ) user1 user2 =
	user1 <-/- user2; iter (fun elt -> elt <-//- user2) user1.set

let rec update_permissions_cascade db ~user =
  Lwt_util.iter
    (fun elt -> update_permissions_cascade db ~user:elt)
    (elements user.set) >>= fun () ->
  update_permissions db ~user

let authenticate db ~name ~pwd =
	catch (fun () -> get_user_by_name db name >>=
	fun u -> if u.data.pwd = (Some pwd) then return u
	else fail BadPassword)
	(function
	| Not_found -> fail NoSuchUser
	| e -> fail e)

let delete_user db ~user =
	get_user_by_name db "admin" >>=
	fun admin -> admin <-//- user;
	update_permissions_cascade db ~user:admin

let in_group ~user ~group =
	user = group || user <-??- group

let add_group db ~user ~group =
	user <--- group;
	update_permissions db ~user:group
