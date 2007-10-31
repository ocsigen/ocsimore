open Eliom
open Eliomduce.Xhtml
open Lwt

let main_service = new_service ~url:[""] ~get_params:unit ();;

let _ =
begin
  Lwt_unix.run
  (Sql.Persist.lwtcreate "omexample_store"
  (fun () -> Users.create_user "admin" (Some "nimda") "Administrator" "") >>=
  fun s -> return (Sql.Persist.get s) >>=
  fun admin ->
    let s = new SessionManager.sessionmanager
    ~sessionmanagerinfo:{
      SessionManager.url = ["users"];
      SessionManager.default_groups = [];
      SessionManager.login_actions =
        (fun sp sess -> return ());
      SessionManager.logout_actions =
        (fun sp -> return ());
      SessionManager.registration_mail_from =
        ("Registration","register@somewhere.net");
			SessionManager.registration_mail_subject = "Register"
    } in
    s#lwtinit >>=
    fun () -> return (s :> SessionManager.sessionmanager) >>=
  fun sessmag ->
		return (new Forum.forum
		~foruminfo:{
			Forum.url = ["forum1"];
			Forum.identifier = "forum1";
			Forum.title = "First Forum";
			Forum.descr = "An utterly nondescript forum";
			Forum.moderated = false;
			Forum.readable_by = Users.anonymous ();
			Forum.writable_by = Users.anonymous ();
			Forum.moderators = admin;
			Forum.max_rows = 5;
			Forum.arborescent = true
		}
		~sessionmanager:sessmag) >>=
	fun forum1 -> let main_page sp _ _ =
    begin
      get_persistent_data SessionManager.user_table sp >>=
			fun sess ->	forum1#display sp sess >>=
			fun forum1_box -> 
				return {{<html>[
					<head>[<title>{: "Yayness!" :}]
					<body>[
						{: sessmag#mk_log_form sp sess :}
						!{: forum1_box :}
					]]
				}}
    end in
  sessmag#register;
	forum1#register;
  register ~service:main_service main_page;
  return ())
end;;
