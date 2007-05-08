(** Ocsimore - Collaborative Sites Tools for Ocsigen.

    Here is a small example of use: we define two forums, the former
    is moderated by users of group "forum1_mod", readable by everyone,
    and writable by registered users; the latter is not moderated and
    only users of group "developers" can read and post messages.  We
    also define a Wiki, readable by everyone and writable by
    registered users only.

   In the future it will be possible to create all this dynamically
   from a Web page.
   
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



(* DEFINITION AND REGISTRATION OF FIRST FORUM *)
let rec forum1 = lazy (new Forum.makeforum
        ~foruminfo:{
         Forum.identifier = "FORUM1";
         Forum.title = "OCaml";
         Forum.descr = "Discussions about the OCaml language";
         Forum.moderated = true;
         Forum.readable_by = Users.anonymous();
         Forum.writable_by = registered_users;
         Forum.moderators = forum1_mod;
         Forum.url = ["forum1"];
         Forum.max_rows = 5l;
       }
        ~sessionmanager:(Lazy.force sessmag)
        ~container:container)


(* DEFINITION AND REGISTRATION OF SECOND FORUM *)
and forum2 = lazy (new Forum.makeforum
      ~foruminfo:{
       Forum.identifier = "FORUM2";
       Forum.title = "Meta-Forum";
       Forum.descr = "The Forum module of this site";
       Forum.moderated = false;
       Forum.readable_by = ocsigen_dev;
       Forum.writable_by = ocsigen_dev;
       Forum.moderators = nobody;
       Forum.url = ["forum2"];
       Forum.max_rows = 5l;
     }
      ~sessionmanager:(Lazy.force sessmag)
      ~container:container)

    
(* THE WIKI *)
and mywiki = lazy (new Wiki.makewiki
                     ~wikiinfo:{
                   Wiki.identifier = "WIKI";
                   Wiki.title = "My pretty Wiki";
                   Wiki.descr = "Create your wikipages here";
                   Wiki.readable_by = Users.anonymous();
                   Wiki.writable_by = registered_users;
                   Wiki.url = ["mywiki";""];
                 }
                     ~sessionmanager:(Lazy.force sessmag)
                     ~container:container)


(* THE UNIFIED SESSION MANAGEMENT *)
and sessmag = lazy (new SessionManager.makesessionmanager
      {
       SessionManager.url = ["users"];
       SessionManager.default_groups = [registered_users];
       SessionManager.login_actions = (fun sp sess -> return ());
       SessionManager.logout_actions = (fun sp -> return ());
       SessionManager.registration_mail_from = 
       ("Ocsigen","NO_REPLY@ocsigen.org");
       SessionManager.registration_mail_subject = "Registration service";
     }
      ~container:container)



(* A MAIN PAGE FOR OUR SITE *)
and container sp sess ~title:t l =
    return
      (html 
         (head (title (pcdata t)) [])
         (body l))

in 

let srv_main = new_service ~url:[""] ~get_params:unit () in

let page_main = fun sp () () -> 
  get_persistent_data SessionManager.user_table sp >>=
  (fun sess ->
    container
      sp
      ~title:"Ocsimore example"
      sess
      [(Lazy.force sessmag)#mk_log_form sp sess;
       h1 [pcdata "Ocsimore - Collaborative Sites Tools for Ocsigen"];
       p [pcdata "A public (moderated) forum:";
	  br();
	  pcdata "everyone can read messages; registered users can \
            post new ones; member of \"forum1_mod\" group are \
                moderators.";
		br();
	  a (Lazy.force forum1)#srv_forum sp [pcdata "ENTER FORUM 1"] ()];
       p [pcdata "A private forum:";
	  br();
	  pcdata "only members of \"ocsigen_dev\" group can read \
            post messages.";
	    br();
	  a (Lazy.force forum2)#srv_forum sp [pcdata "ENTER FORUM 2"] ()];
       p [pcdata "A simple Wiki:";
	  br();
	  pcdata "readable by everyone, writable by registered users.";
	  br();
	  a (Lazy.force mywiki)#srv_main sp [pcdata "ENTER WIKI"] ()];
       p [pcdata "--- User 'moder' Password '' is in 'forum1_mod', \
            'ocsigen_dev', 'registered_users' groups."]])
in   
         
ignore (Lazy.force sessmag);

       (* REGISTRATION OF MAIN PAGE *)
register srv_main page_main

