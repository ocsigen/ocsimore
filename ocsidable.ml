(** Ocsidable - Collaborative Sites for Ocsigen.

    Here is a small example of use: we define two forums, the former
    is moderated by users of group "forum1_mod", and is readable and
    writable by everyone; the latter is not moderated and only users
    of group "developers" can read and post messages.

    Please look at the code and read comments. *)

(* Standard modules to be opened for Ocsigen *)
open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt

(* Moderators group for Forum #1 *)
let forum1_mod = 
  Sql.Persist.get
    (Sql.Persist.create 
       "forum1_mod"
       (fun () -> Users.create_user 
          ~name:"forum1_mod" 
          ~pwd:None 
          ~desc:"Moderators group - Forum1"
          ~email:""))

(* A new user *)
let moder =
  Sql.Persist.get
    (Sql.Persist.create
       "moder"
       (fun () -> Users.create_user
          ~name:"moder"
          ~pwd:(Some "") (* we'll change it later interactively *)
	  ~desc:"Mr. Moderato Moderante, moderator."
          ~email:""))

(* "moder" is a moderator for Forum #1 *)
let _ = Users.add_group ~user:moder ~group:forum1_mod


(* The group of users for Forum #2 *)
let developers = 
  Sql.Persist.get
    (Sql.Persist.create 
       "developers"
       (fun () -> Users.create_user 
          ~name:"developers" 
          ~pwd:None 
          ~desc:"Developers group"
          ~email:""))

(* "moder" is a developer too *)
let _ = Users.add_group ~user:moder ~group:developers

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
     let writable_by = Users.anonymous()
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
     let readable_by = developers
     let writable_by = developers
     let moderators = developers
     let url = ["forum2"]
     let exit_link = fun sp -> a main sp [pcdata "RETURN TO MAIN PAGE"] ()
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* The unified session management *)
and S: SessionManager.OUT = SessionManager.Make
  (struct
     let url = ["users"]
     let default_groups = []
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
