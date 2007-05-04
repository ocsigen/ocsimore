(** Ocsimore - Collaborative Sites Tools for Ocsigen.

    Here is a small example of use: we define two forums, the former
    is moderated by users of group "forum1_mod", readable by everyone,
    and writable by registered users; the latter is not moderated and
    only users of group "developers" can read and post messages.  We
    also define a Wiki, readable by everyone and writable by
    registered users only.

    Please look at the code and read comments. *)

(* STANDARD MODULES TO BE OPENED FOR OCSIGEN *)
open XHTML.M
open Eliom
open Eliom.Xhtml
open Lwt

(* USERS AND GROUPS CREATION *)
let (forum1_mod, ocsigen_dev, registered_users, nobody) = 
  Sql.Persist.get
    (Sql.Persist.create "OCSIMORE_EXAMPLE__init"
       (fun () -> 

	  (* A USER *)
	  let moder = Users.create_user 
	    ~name:"moder" 
	    ~pwd:(Some "") (* she'll change it later interactively *)
	    ~desc:"Miss Moderata Moderante"
	    ~email:"" 

	  (* GROUPS *)
	  and forum1_mod =
	    Users.create_user "forum1_mod" None "Moderators of Forum #1" ""
	  and ocsigen_dev =
	    Users.create_user "ocsigen_dev" None "Ocsigen developers" ""
	  and registered_users =
	    Users.create_user "registered_users" None "Registered users" ""
	  and nobody =
	    Users.create_user "nobody" None "" ""
	  in

	    (* "moder" is a moderator and an Ocsigen developer *)
	    Users.add_group ~user:moder ~group:forum1_mod;
	    Users.add_group ~user:moder ~group:ocsigen_dev;
	    Users.add_group ~user:forum1_mod ~group:registered_users;
	    Users.add_group ~user:ocsigen_dev ~group:registered_users;
	    (forum1_mod, ocsigen_dev, registered_users, nobody)))


(* A MAIN PAGE FOR OUR SITE *)
module rec Main: sig 
  (* this is a safe module for a recursive module definition *)
  val login_actions: Eliom.server_params -> Users.user option -> unit
  val logout_actions: Eliom.server_params -> unit
  val get_back: Eliom.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
end = struct
  let srv_main = new_service ~url:[""] ~get_params:unit ()
  let page_main sess = fun sp () () -> return 
    (html 
       (head (title (pcdata "MAIN PAGE")) [])
       (body [S.mk_log_form sp sess;
	      h1 [pcdata "Ocsimore - Collaborative Sites Tools for Ocsigen"];
	      p [pcdata "A public (moderated) forum:";
		 br();
		 pcdata "everyone can read messages; registered users can \
                         post new ones; member of \"forum1_mod\" group are \
                         moderators.";
		 br();
		 a Forum1.srv_forum sp [pcdata "ENTER FORUM 1"] ()];
	      p [pcdata "A private forum:";
		 br();
		 pcdata "only members of \"ocsigen_dev\" group can read \
                         post messages.";
		 br();
		 a Forum2.srv_forum sp [pcdata "ENTER FORUM 2"] ()];
	      p [pcdata "A simple Wiki:";
		 br();
		 pcdata "readable by everyone, writable by registered users.";
		 br();
		 a MyWiki.srv_main sp [pcdata "ENTER WIKI"] ()];
	      p [pcdata "--- User 'moder' Password '' is in 'forum1_mod', \
                         'ocsigen_dev', 'registered_users' groups."]]))
  let login_actions sp sess = 
    register_for_session sp srv_main (page_main sess)
  let logout_actions sp = ()
  let get_back sp = a srv_main sp [pcdata "RETURN TO MAIN PAGE"] ()
  (* REGISTRATION OF MAIN PAGE *)
  let _ = register srv_main (page_main None)
end

(* DEFINITION AND REGISTRATION OF FIRST FORUM *)
and Forum1: Forum.OUT = Forum.Make
  (struct
     let identifier = "FORUM1"
     let title = "OCaml"
     let descr = "Discussions about the OCaml language"
     let moderated = true
     let readable_by = Users.anonymous()
     let writable_by = registered_users
     let moderators = forum1_mod
     let url = ["forum1"]
     let exit_link = Main.get_back
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* DEFINITION AND REGISTRATION OF SECOND FORUM *)
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
     let exit_link = Main.get_back
     let mk_log_form = S.mk_log_form
     let max_rows = 5l
   end: Forum.IN) 

(* THE WIKI *)
and MyWiki: Wiki.OUT = Wiki.Make
  (struct
     let identifier = "WIKI"
     let title = "My pretty Wiki"
     let descr = "Create your wikipages here"
     let readable_by = Users.anonymous()
     let writable_by = registered_users
     let url = ["mywiki";""]
     let exit_link = Main.get_back
     let mk_log_form = S.mk_log_form
   end: Wiki.IN) 

(* THE UNIFIED SESSION MANAGEMENT *)
and S: SessionManager.OUT = SessionManager.Make
  (struct
     let url = ["users"]
     let default_groups = [registered_users]
     let exit_link = Main.get_back
     let login_actions sp sess = 
       Main.login_actions sp sess;
       Forum1.login_actions sp sess;
       Forum2.login_actions sp sess;
       MyWiki.login_actions sp sess
     let logout_actions sp = 
       Main.logout_actions sp;
       Forum1.logout_actions sp;
       Forum2.logout_actions sp;
       MyWiki.logout_actions sp
     let registration_mail_from = ("Ocsigen","NO_REPLY@ocsigen.org")
     let registration_mail_subject = "Registration service"
   end)

