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


              
(* DEFINITION AND REGISTRATION OF FIRST FORUM *)
module rec Forum1 : sig val forum : Forum.forum end = struct
    let forum = new Forum.makeforum
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
        ~container:Main.container
end

(* DEFINITION AND REGISTRATION OF SECOND FORUM *)
and Forum2 : sig val forum : Forum.forum end  = struct
  let forum = new Forum.makeforum
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
      ~container:Main.container
end
    
(* THE WIKI *)
and Mywiki : sig val wiki : Wiki.wiki end  = struct
  let wiki = new Wiki.makewiki
      ~wikiinfo:{
       Wiki.identifier = "WIKI";
       Wiki.title = "My pretty Wiki";
       Wiki.descr = "Create your wikipages here";
       Wiki.readable_by = Users.anonymous();
       Wiki.writable_by = registered_users;
       Wiki.url = ["mywiki";""];
     }
      ~container:Main.container
end

(* THE UNIFIED SESSION MANAGEMENT *)
and S : sig val sessmag : SessionManager.sessionmanager end  = struct
  let sessmag = new SessionManager.makesessionmanager
      {
       SessionManager.url = ["users"];
       SessionManager.default_groups = [registered_users];
       SessionManager.login_actions = (fun sp sess ->
         Main.login_actions sp sess;
         Forum1.forum#login_actions sp sess;
         Forum2.forum#login_actions sp sess;
         Mywiki.wiki#login_actions sp sess);
       SessionManager.logout_actions = (fun sp ->
         Main.logout_actions sp;
         Forum1.forum#logout_actions sp;
         Forum2.forum#logout_actions sp;
         Mywiki.wiki#logout_actions sp);
       SessionManager.registration_mail_from = 
       ("Ocsigen","NO_REPLY@ocsigen.org");
       SessionManager.registration_mail_subject = "Registration service";
     }
      ~container:Main.container
end



(* A MAIN PAGE FOR OUR SITE *)
and Main : sig 
  val container : 
      Eliom.server_params -> Users.user option -> title:string -> 
        XHTML.M.block XHTML.M.elt list -> html Lwt.t
  val login_actions : Eliom.server_params -> Users.user option -> unit
  val logout_actions : Eliom.server_params -> unit
  val get_back : Eliom.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
 end = struct
  
  let container sp sess ~title:t l =
    return
      (html 
         (head (title (pcdata t)) [])
         (body l))

   let srv_main = new_service ~url:[""] ~get_params:unit ()

   let page_main = fun sp () () -> 
     get_persistent_data SessionManager.user_table sp >>=
     (fun sess ->
       container
         sp
         ~title:"Ocsimore example"
         sess
         [S.sessmag#mk_log_form sp sess;
	  h1 [pcdata "Ocsimore - Collaborative Sites Tools for Ocsigen"];
	  p [pcdata "A public (moderated) forum:";
	     br();
	     pcdata "everyone can read messages; registered users can \
               post new ones; member of \"forum1_mod\" group are \
                   moderators.";
		   br();
	     a Forum1.forum#srv_forum sp [pcdata "ENTER FORUM 1"] ()];
	  p [pcdata "A private forum:";
	     br();
	     pcdata "only members of \"ocsigen_dev\" group can read \
               post messages.";
	       br();
	     a Forum2.forum#srv_forum sp [pcdata "ENTER FORUM 2"] ()];
	  p [pcdata "A simple Wiki:";
	     br();
	     pcdata "readable by everyone, writable by registered users.";
	     br();
	     a Mywiki.wiki#srv_main sp [pcdata "ENTER WIKI"] ()];
	  p [pcdata "--- User 'moder' Password '' is in 'forum1_mod', \
               'ocsigen_dev', 'registered_users' groups."]])

   let login_actions sp sess = 
     register_for_session sp srv_main page_main

   let logout_actions sp = ()

   let get_back sp = a srv_main sp [pcdata "RETURN TO MAIN PAGE"] ()
       
       (* REGISTRATION OF MAIN PAGE *)
   let _ = register srv_main page_main

 end
