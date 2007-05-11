(* The structure for users is a complete lattice, whose elements are
   sets of sets, ordered inclusion: the bottom element is the empty
   set (the anonymous user), the top element is the set of all sets
   (the root user).

   In this model, users and groups are the same concept; we only
   distinguish, for practical matters, between "login enabled" users
   and "group only" users: the former has some (eventually void)
   password, the latter has not. "U is in group V" means, in fact,
   that there are some U1, U2, ..., Un such that U contains U1 and U1
   contains U2 and ... and Un contains V. *)

open Lwt

type userdata = 
    {name: string;
     mutable pwd: string option;
     mutable desc: string;
     mutable email: string}
      
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

type user = elt 

(* The users structure (persistent data) *)
let global_users_container =


print_endline "---------------->avant run guc";
  Lwt_unix.run

    (
print_endline "---------------->run guc";

Persist.create "global_users_container" 
       (fun () -> 
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
	   let pwd1 = get_pwd "Enter new password for root: " in
	   let pwd2 = get_pwd "\nEnter root's password once again: "
	   in if pwd1 = pwd2 
	   then (print_endline "\nNew password registered."; pwd1) 
	   else (print_endline "\nPasswords do not match."; ask_pwd())
	       
         and ask_email () =
	   print_endline "\nEnter a valid e-mail address for root: ";
	   let email = input_line Pervasives.stdin in
	   print_endline ("\n'" ^ email ^ "': Confirm this address? (Y/N)");
	   match input_line Pervasives.stdin with
	   | "Y"|"y" -> print_endline "\n Thank you."; email
	   | _ -> print_endline "\n"; ask_email()
	         
         and anon = {data = {name = "";
			     pwd = None;
			     desc = "Anonymous user";
			     email = ""};
		     set = empty}
	     
         in {data = {name = "root";
		     pwd = Some (ask_pwd());
		     desc = "Administrator";
		     email = ask_email()};
	     set = singleton anon}))

;;print_endline "---------------->run guc fini";;
    
(* Get a user by name. Not exported in interface. *)
let getbyname name =
  let rec f u = function
    | None ->
	if u.data.name = name 
	then Some u
	else fold f u.set None
    | found -> found
  in
  f (Persist.get global_users_container) None


let root () = 
  match getbyname "root" with
  | Some u -> u
  | _ -> raise (Users_error "user root not created")

let anonymous () =
  match getbyname "" with
  | Some u -> u
  | _ -> raise (Users_error "user anonymous not created")


(* Does user1 own user2? *)
let ( <-?- ) user1 user2 =
  mem user2 user1.set

(* <-?- transitive closure *)
let rec ( <-??- ) user1 user2 =
  user1 <-?- user2 || exists (fun elt -> elt <-??- user2) user1.set
    
(* Insert user2 into user1.set. *)
let ( <--- ) user1 user2 = 
  if user2 = user1 || user2 <-??- user1
  then raise Loop
  else user1.set <- add user2 user1.set

(* Remove user2 from user1.set. *)
let ( <-/- ) user1 user2 =
  if user1 <-?- user2
  then user1.set <- union user2.set (remove user2 user1.set)
  else ()

(* Remove user2 from user1.set and from each u.set such that user1 <-??- u *)
let rec ( <-//- ) user1 user2 =
  user1 <-/- user2; iter (fun elt -> elt <-//- user2) user1.set


(* PUBLIC OPERATIONS ON USERS*)

let create_user ~name ~pwd ~desc ~email = 
  match getbyname name with
  | Some _ -> fail UserExists
  | None ->
      let data = {name = name; pwd = pwd; desc = desc; email = email} in
      let newuser = {data = data; set = singleton (anonymous ())} in
      root () <--- newuser;
      Persist.write_back global_users_container >>=
      (fun () -> return newuser)

let create_unique_user ~name ~pwd ~desc ~email = 
  let digit s = s.[0] <- String.get "0123456789" (Random.int 10); s in
  let rec suffix n = 
    match getbyname n with
    | Some _ -> suffix (n ^ (digit "X"))
    | None -> 
        create_user ~name:n ~pwd ~desc ~email >>= 
        (fun x -> return (x, n))
  in suffix name

let delete_user ~user =
  root () <-//- user;
  Persist.write_back global_users_container

let in_group ~user ~group =
  user = group || user <-??- group

let add_group ~user ~group =
  user <--- group;
  Persist.write_back global_users_container

let remove_group ~user ~group =
  user <-//- group;
  Persist.write_back global_users_container

let get_user_data ~user =
  let {name = n; pwd = p; desc = d; email = e} = user.data
  in (n, p, d, e)

let update_user_data ~user =
  let d = user.data 
  in fun ?(pwd = d.pwd) ?(desc = d.desc) ?(email = d.email) () ->
    d.pwd <- pwd; 
    d.desc <- desc; 
    d.email <- email; 
    Persist.write_back global_users_container
        
(* authentication function *)
let authenticate ~name ~pwd =
  match getbyname name with
  | Some user -> 
      if user.data.pwd = Some pwd 
      then user
      else raise BadPassword
  | None -> raise NoSuchUser


(* randomly generates an 8-chars alnum password *)
let generate_password () =
  let chars = "0123456789"^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"^
    "abcdefghijklmnopqrstuvwxyz" in
  let pwd = String.create 8 in
    for i = 0 to 7 do
      pwd.[i] <- String.get chars (Random.int (10+2*26))
    done;
    Some pwd

(* sends user data to the user's mail address *)
let mail_password ~name ~from_addr ~subject = 
  match getbyname name with
  | None -> return false 
  | Some user -> 
      let (_,pwd,desc,email) = get_user_data ~user in 
      catch
        (fun () ->
          Preemptive.detach
            (fun () ->
	      ignore(Netaddress.parse email);
	      Netsendmail.sendmail 
	        (Netsendmail.compose 
	           ~from_addr 
	           ~to_addrs:[(desc, email)] 
	           ~subject
	           ("This is an auto-generated message. "
	            ^ "Please do not reply to it.\n"
	            ^ "\n"
	            ^ "Your account is:\n"
	            ^ "\tUsername:\t" ^ name ^ "\n"
	            ^ "\tPassword:\t" ^ (match pwd with Some p -> p
                    | _ -> "(NONE)")
	            ^ "\n"));
	      true)
            ())
        (fun _ -> return false)



(* A functor for value protection. *)

module type T = sig  
  type t  
  val value: t  
  val group: user
end

module Protect = functor (A: T) ->
struct
  let f ~actor = 
    if in_group ~user:actor ~group:A.group
    then A.value
    else raise NotAllowed
end
    
(* Some functions available only to root *)

let get_user_by_name =
  let module M = Protect(
    struct
      type t = name:string -> user option
      let value = fun ~name -> getbyname name
      let group = root ()
    end)
  in M.f
		
let get_user_groups =
  let module M = Protect(
    struct
      type t = user:user -> user list
      let value = fun ~user -> elements user.set
      let group = root ()
    end)
  in M.f
