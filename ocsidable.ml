(** Ocsidable - Collaborative Sites for Ocsigen.

    Here is a small example of use: we define two forums, the former
    is moderated by users of group "forum1_mod", readable by everyone,
    and writable by registered users; the latter is not moderated and
    only users of group "developers" can read and post messages.

    Please look at the code and read comments. *)

(* Standard modules to be opened for Ocsigen *)
open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt

(* Users and groups creation.
   We use Sql.Persist functions to ensure it'll be executed once *)
let (forum1_mod, ocsigen_dev, registered_users, nobody) = 
  Sql.Persist.get
    (Sql.Persist.create "OCSIDABLE_EXAMPLE__groups"
       (fun () -> 
	  (Users.create_user "forum1_mod" None "Moderators of Forum #1" "",
	   Users.create_user "ocsigen_dev" None "Ocsigen developers" "",
	   Users.create_user "registered_users" None "All registered users" "",
	   Users.create_user "nobody" None "" "")))

let moder =
  Sql.Persist.get
    (Sql.Persist.create "OCSIDABLE_EXAMPLE__users"
       (fun () ->
	  Users.create_user 
	     ~name:"moder" 
	     ~pwd:(Some "") (* she'll change it later interactively *)
	     ~desc:"Miss Moderata Moderante"
	     ~email:""))

let _ = 
  Users.add_group ~user:moder ~group:forum1_mod; (* "moder" is a moderator *)
  Users.add_group ~user:moder ~group:ocsigen_dev; (* and she's a developer *)
  Users.add_group ~user:forum1_mod ~group:registered_users;
  Users.add_group ~user:ocsigen_dev ~group:registered_users

(* A main page for our site *)
let main = new_service ~url:[""] ~get_params:unit ()

(* Definition and registration of first forum *)
module rec Forum1: Forum.OUT = Forum.Make
  (struct
     let identifier = "FORUM1"
     let title = "OCaml"
     let descr = "Discussions about the OCaml language"
     let moderated = true
     let readable_by = Users.anonymous()
     let writable_by = registered_users
     let moderators = forum1_mod
     let url = ["forum1"]
     let exit_link = fun sp -> a main sp [pcdata "RETURN TO MAIN PAGE"] ()
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* Definition and registration of second forum *)
and Forum2: Forum.OUT = Forum.Make
  (struct
     let identifier = "FORUM2"
     let title = "Meta-Forum"
     let descr = "The Forum module of this site"
     let moderated = false
     let readable_by = ocsigen_dev
     let writable_by = ocsigen_dev
     let moderators = nobody
     let url = ["forum2"]
     let exit_link = fun sp -> a main sp [pcdata "RETURN TO MAIN PAGE"] ()
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* The unified session management *)
and S: SessionManager.OUT = SessionManager.Make
  (struct
     let url = ["users"]
     let default_groups = [registered_users]
     let exit_link = fun sp -> a main sp [pcdata "RETURN TO MAIN PAGE"] ()
     let login_actions sp sess = (Forum1.login_actions sp sess;
                                  Forum2.login_actions sp sess)
     let logout_actions sp = (Forum1.logout_actions sp;
                              Forum2.logout_actions sp)
     let registration_mail_from = ("Ocsigen","NO_REPLY@ocsigen.org")
     let registration_mail_subject = "Registration service"
   end)

(* Main page *)
let _ =
  register_service main 
    (fun sp () () -> return 
       (html 
          (head (title (pcdata "MAIN PAGE")) [])
          (body [p [a Forum1.srv_forum sp [pcdata "ENTER FORUM 1"] ();
                    br();
                    a Forum2.srv_forum sp [pcdata "ENTER FORUM 2"] ()]])))
