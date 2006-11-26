open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt
open Sql
open Users

let forum1_mod = 
  Persist.get
    (Persist.create 
       "forum1_mod"
       (fun () -> create_user 
	  ~name:"forum1_mod" 
	  ~pwd:None 
	  ~desc:"Moderators group - Forum1"
	  ~email:""))

let developers = 
  Persist.get
    (Persist.create 
       "developers"
       (fun () -> create_user 
	  ~name:"developers" 
	  ~pwd:None 
	  ~desc:"Developers group"
	  ~email:""))

(* A main page for our site *)
let main = new_service ~url:[""] ~get_params:unit ()

(* Definition and registration of first forum *)
module rec Forum1: Forum.OUT = Forum.Make
  (struct
     let identifier = "FORUM1"
     let title = "OCaml"
     let descr = "Discussions about the OCaml language"
     let moderated = true
     let readable_by = anonymous()
     let writable_by = anonymous()
     let moderators = forum1_mod
     let url = ["forum1"]
     let exit_link = fun sp -> a main sp [pcdata "RETURN TO MAIN PAGE"] ()
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* Definition and registration of first forum *)
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
