(** PostgreSQL database operations via PGOCaml library. *)

open Lwt_PGOCaml 
  (* SQL aggregate functions can sometimes return NULL values, but
     this is not the case for COUNT, that _always_ returns a non-NULL
     value. PGOCaml's nullability test fails here, as the type
     inferred for LWT_PGSQL(db) "SELECT COUNT(field) FROM YourTable" is
     'int64 option' and not 'int64', as expected. *)

open Lwt
open Ocsimorelib
open CalendarLib

(* let db = Lwt_PGOCaml.connect ~host:"courbet.kerguelen.org" ~database:"ocsimore" ~user:"ocsigen" () *)

type db_t = (string, bool) Hashtbl.t Lwt_PGOCaml.t

let connect () =
  Lwt_PGOCaml.connect ~database:"ocsimore" ~user:Ocsimore_config.user ();;

let uuid_of_conn db =
  Lwt_unix.run 
    (db >>= fun db -> 
     return (Lwt_PGOCaml.uuid_of_conn db))

type db_int_t = int32;;
type db_size_t = int64;;
type db_count_t = int64;;

let db_int_of_int = Int32.of_int;;
let db_size_of_int = Int64.of_int;;
let db_count_of_int = Int64.of_int;;
let db_int_of_string = Int32.of_string;;
let string_of_db_int = Int32.to_string;;
let int_of_db_int = Int32.to_int;;
let int_of_db_size = Int64.to_int;;
let int_of_db_count = Int64.to_int;;

  (* I could define here a functor to try to abstract the db
     structure, improving Vincent's Ocsicache.Make; but I need lots of
     specialized SQL queries and commands, so it's easy and clearer to
     define those ones directly. *)

(* USERS *)

let new_user db ~name ~password ~fullname ~email =
	Ocsigen_messages.debug2 "[Sql] new_user";
  begin_work db >>=
	fun _ -> (match password with
	| None -> LWT_PGSQL(db) "INSERT INTO users (login, fullname, email)\
		VALUES ($name, $fullname, $email)"
	| Some pwd -> LWT_PGSQL(db) "INSERT INTO users (login, password, fullname, email) VALUES ($name, $pwd, $fullname, $email)") >>=
	fun () -> serial4 db "users_id_seq" >>=
	fun frm_id -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] new_user: finish"; return frm_id;;

let find_user db ?id ?name () =
	Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_user" (Lwt_PGOCaml.uuid_of_conn db));
	(match (name, id) with
	| (Some n, Some i) -> LWT_PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE id = $i AND login = $n"
	| (None, Some i) -> LWT_PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE id = $i"
	| (Some n, None) -> LWT_PGSQL(db) "SELECT id, login, password, fullname, email, permissions FROM users WHERE login = $n"
	| (None, None) -> fail (Failure "Neither name nor id specified")) >>=
	fun res -> (match res with
	| [u] -> Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_user: return" (Lwt_PGOCaml.uuid_of_conn db)); return u
	| _ -> Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_user: fail with Not_found" (Lwt_PGOCaml.uuid_of_conn db)); fail Not_found);;

let update_permissions db ~name ~perm =
  Ocsigen_messages.debug2 (Printf.sprintf "[Sql] update_permissions [%s]" perm);
	begin_work db >>=
	fun _ -> find_user db ~name () >>=
	fun (id, _, _, _, _, _) -> LWT_PGSQL(db) "UPDATE users SET permissions = $perm WHERE id = $id" >>=
	fun () -> Ocsigen_messages.debug2 "[Sql] update_permissions: finish"; commit db >>=
  fun _ -> return ();;

let update_data db ~id ~name ~password ~fullname ~email =
	Ocsigen_messages.debug2 "[Sql] update_data";
	begin_work db >>=
	fun _ -> find_user db ~id ~name () >>=
	fun (id, _, _, _, _, _) -> (match password with
	| None -> LWT_PGSQL(db) "UPDATE users SET fullname = $fullname, email = $email WHERE id = $id"
	| Some pwd -> LWT_PGSQL(db) "UPDATE users SET password = $pwd, fullname = $fullname, email = $email WHERE id = $id") >>=
	fun () -> Ocsigen_messages.debug2 "[Sql] update_data: finish"; commit db >>=
  fun _ -> return ();;

(* FORUMS *)

  (* Lots of queries here take a ~frm_id parameter, even if other ones
     should be enough to determinate a primary key.  This has been
     done because of the need to match every query request against a
     forum's ACL.  TO BE DONE: A LAYER FOR ACCESS CONTROL *)


(* Used to restrict the recordsets *)
type role = Moderator | Author of db_int_t | Lurker of string | Unknown;;

type message_info =
	db_int_t * string * string * Calendar.t * bool * db_int_t option * string option;;
(* type 'a collection = List of 'a list | Forest of 'a tree list;; *)

let new_forum
    db ~title ~descr ~moderated ~arborescent ~reader ~writer
    ~moderator =
  (* inserts a new forum *)
  Ocsigen_messages.debug2 "[Sql] new_forum";
  begin_work db >>= fun _ -> 
  LWT_PGSQL(db) 
    "INSERT INTO forums (title, descr, moderated,
     arborescent, reader, writer, moderator) \
     VALUES ($title, $descr, $moderated, $arborescent, $reader, $writer,
     $moderator)" >>= fun () -> 
  serial4 db "forums_id_seq" >>= fun frm_id -> 
  commit db >>=	fun _ -> 
  Ocsigen_messages.debug2 "[Sql] new_forum: finish"; 
  return frm_id;;
        
let new_thread_and_message db ~frm_id ~author_id ~subject ~txt = 
  (* inserts a message starting a new thread; both thread and message
     will be hidden if forum is moderated *)
	Ocsigen_messages.debug2 "[Sql] new_thread_and_message";
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>= 
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> LWT_PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author_id) \
		VALUES ($frm_id, $subject, $hidden, $author_id)" >>=
	fun () -> serial4 db "threads_id_seq" >>=
	fun thr_id -> LWT_PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () ->	serial4 db "textdata_id_seq" >>=
	fun txt_id -> LWT_PGSQL(db) "SELECT MAX(tree_max) FROM messages \
		WHERE thr_id = $thr_id" >>=
	fun z -> (match z with
		| [x] -> (match x with
			| None -> return (db_int_of_int 0)
			| Some y -> return y)
		| _ -> return (db_int_of_int 0)) >>=
	fun db_max -> LWT_PGSQL(db) "INSERT INTO messages (author_id, thr_id, txt_id, hidden, tree_min, tree_max) \
		VALUES ($author_id, $thr_id, $txt_id, $hidden, $db_max + 1, $db_max + 2)" >>=
	fun () -> serial4 db "messages_id_seq" >>=
	fun msg_id -> commit db >>=
 	fun _ -> Ocsigen_messages.debug2 "[Sql] new_thread_and_message: finish";
	return (thr_id, msg_id);;
    
let new_thread_and_article db ~frm_id ~author_id ~subject ~txt =
	Ocsigen_messages.debug2 "[Sql] new_thread_and_article";
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> LWT_PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () -> serial4 db "textdata_id_seq" >>=
	fun txt_id -> LWT_PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author_id, article_id) \
		VALUES ($frm_id, $subject, $hidden, $author_id, $txt_id)" >>=
	fun () -> serial4 db "threads_id_seq" >>=
	fun thr_id -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] new_thread_and_article: finish"; return (thr_id, txt_id);;

let new_message db ~thr_id ?parent_id ~author_id ~txt ~sticky () = 
  (* inserts a message in an existing thread; message will be hidden
     if forum is moderated *)
	Ocsigen_messages.debug2 "[Sql] new_message"; 
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT moderated FROM forums,threads \
		WHERE threads.id = $thr_id \
		AND threads.frm_id = forums.id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> LWT_PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () -> serial4 db "textdata_id_seq" >>=
	fun txt_id -> LWT_PGSQL(db) "SELECT MAX(tree_max) FROM messages \
		WHERE thr_id = $thr_id" >>=
	fun z -> (match z with
		| [x] -> (match x with
			| None -> return (db_int_of_int 0)
			| Some y -> return y)
		| _ -> return (db_int_of_int 0)) >>=
	fun db_max -> 
		(match parent_id with
		| None -> LWT_PGSQL(db) "INSERT INTO messages (author_id, thr_id, txt_id, hidden, \
				sticky, tree_min, tree_max) \
				VALUES ($author_id, $thr_id, $txt_id, $hidden, $sticky, \
				$db_max + 1, $db_max + 2)" >>=
				fun () -> serial4 db "messages_id_seq"
		| Some pid -> LWT_PGSQL(db) "SELECT tree_min, tree_max \
				FROM messages WHERE id = $pid" >>=
				fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
				fun (db_min, db_max) -> LWT_PGSQL(db) "UPDATE messages SET tree_min = tree_min + 2, \
					tree_max = tree_max + 2 WHERE tree_min > $db_max" >>=
				fun () -> LWT_PGSQL(db) "UPDATE messages SET tree_max = tree_max + 2 \
					WHERE $db_min BETWEEN tree_min AND tree_max" >>=
				fun () -> LWT_PGSQL(db) "INSERT INTO messages (author_id, thr_id, txt_id, hidden, \
					sticky, tree_min, tree_max) \
					VALUES ($author_id, $thr_id, $txt_id, $hidden, $sticky, $db_max, \
					$db_max + 1)" >>=
				fun () -> serial4 db "messages_id_seq") >>=
	fun msg_id ->	commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] new_message: finish"; return msg_id;;

let forum_toggle_moderated db ~frm_id =
	(* toggle moderation status of a forum *)
	Ocsigen_messages.debug2 "[Sql] forum_toggle_moderated";
  LWT_PGSQL(db) "UPDATE forums SET moderated = NOT moderated WHERE id = $frm_id";;
    
let thread_toggle_hidden db ~frm_id ~thr_id =
	(* hides/shows a thread *)
	Ocsigen_messages.debug2 "[Sql] thread_toggle_hidden";
	LWT_PGSQL(db) "UPDATE threads SET hidden = NOT hidden \
 		WHERE id = $thr_id AND frm_id = $frm_id";;

let message_toggle_hidden db ~frm_id ~msg_id =
  (* hides/shows a message *)
	Ocsigen_messages.debug2 "[Sql] message_toggle_hidden";
	LWT_PGSQL(db) "UPDATE messages \
		SET hidden = NOT messages.hidden \
		FROM threads \
		WHERE messages.id = $msg_id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id";;

let message_toggle_sticky db ~frm_id ~msg_id =
	Ocsigen_messages.debug2 "[Sql] message_toggle_sticky";
	LWT_PGSQL(db) "UPDATE messages SET sticky = NOT messages.sticky \
		FROM threads WHERE messages.id = $msg_id AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id"

let find_forum db ?id ?title () =
	Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_forum (id: %s title: %s)"
		(Lwt_PGOCaml.uuid_of_conn db)
		(match id with None -> "none" | Some i -> string_of_db_int i)
		(match title with None -> "none" | Some s -> s));
	begin_work db >>=
	fun _ -> (match (title, id) with
	| (Some t, Some i) ->  LWT_PGSQL(db) "SELECT forums.id, title, r.login, w.login, m.login \
		FROM forums, users AS r, users AS w, users AS m \
		WHERE r.id = reader AND w.id = writer AND m.id = moderator \
		AND title = $t AND forums.id = $i"
	| (Some t, None) -> LWT_PGSQL(db) "SELECT forums.id, title, r.login, w.login, m.login \
		FROM forums, users AS r, users AS w, users AS m \
		WHERE r.id = reader AND w.id = writer AND m.id = moderator \
		AND title = $t"
	| (None, Some i) -> LWT_PGSQL(db) "SELECT forums.id, title, r.login, w.login, m.login \
		FROM forums, users AS r, users AS w, users AS m \
		WHERE r.id = reader AND w.id = writer AND m.id = moderator \
		AND forums.id = $i"
	| (None, None) -> fail (Failure "Neither title nor id specified")) >>=
	fun r -> commit db >>=
	fun _ -> (match r with
	| [x] -> Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_forum: return" (Lwt_PGOCaml.uuid_of_conn db)); return x
	| _ -> Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] find_forum: fail with Not_found" (Lwt_PGOCaml.uuid_of_conn db)); fail Not_found)

let get_forums_list db =
	Ocsigen_messages.debug2 "[Sql] get_forums_list";
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT id, title, descr, moderated, arborescent FROM forums" >>=
	fun r -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] get_forums_list: finish"; return r;;

let forum_get_data db ~frm_id ~role =
  (* returns id, title, description, mod status, number of shown/hidden
     threads and messages of a forum.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
	Ocsigen_messages.debug2 "[Sql] forum_get_data";
	begin_work db >>=
  fun _ -> LWT_PGSQL(db) "SELECT id, title, descr, moderated FROM forums \
		WHERE id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, title, description, moderated) -> 
		LWT_PGSQL(db) "SELECT COUNT(*) FROM threads \
		WHERE frm_id = $frm_id AND (NOT hidden)" >>=
	fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false) >>=
	fun n_shown_thr  -> LWT_PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
		WHERE threads.frm_id = $frm_id \
		AND messages.thr_id = threads.id \
		AND NOT (messages.hidden OR threads.hidden)" >>=
	fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false) >>=
	fun n_shown_msg -> (match role with	
		| Moderator -> (* counts all hidden stuff *)
			LWT_PGSQL(db) "SELECT COUNT(*) FROM threads \
				WHERE frm_id = $frm_id AND hidden" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false)
		| Author aid ->
			LWT_PGSQL(db) "SELECT COUNT(*) FROM threads \
				WHERE frm_id = $frm_id AND hidden \
				AND author_id = $aid" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false)
		| Unknown -> return 0) >>=
	fun n_hidden_thr -> (match role with
		| Moderator -> 
			LWT_PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE threads.frm_id = $frm_id \
				AND messages.thr_id = threads.id \
				AND (messages.hidden OR threads.hidden)" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false)
		| Author aid ->
			LWT_PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
 				WHERE threads.frm_id = $frm_id \
				AND messages.thr_id = threads.id \
				AND (messages.hidden OR threads.hidden) \
				AND messages.author_id = $aid" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_count x) | _ -> assert false)
		| Unknown -> return 0) >>=
	fun n_hidden_msg -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] forum_get_data: finish"; return 
      (id, title, description, moderated,
       n_shown_thr, n_hidden_thr, n_shown_msg, n_hidden_msg);;

let thread_get_nr_messages db ~thr_id ~role =
	Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages";
	begin_work db >>=
	fun _ -> (match role with
	| Moderator -> (* all messages *)
		LWT_PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))
	| Author aid -> (* all non-hidden messages AND hidden messages posted by a *)
	  LWT_PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id \
			AND messages.hidden = false OR messages.author_id = $aid" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))
	| Unknown -> (* all non-hidden messages *)
	  LWT_PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id \
			AND messages.hidden = false" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))) >>=
	fun n_msg -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] thread_get_nr_messages: finish"; return (int_of_db_count n_msg);;

let thread_get_data db (* ~frm_id *) ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
	Ocsigen_messages.debug2 (Printf.sprintf "[Sql] [%s] thread_get_data" (Lwt_PGOCaml.uuid_of_conn db));
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT t.id, subject, fullname, t.datetime, t.hidden, COALESCE (txt, '') \
		FROM users AS u, threads AS t LEFT OUTER JOIN textdata ON \
		(t.article_id = textdata.id) \
		WHERE t.id = $thr_id AND u.id = t.author_id" (* AND frm_id = $frm_id *) >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, subject, author_id, datetime, hidden, article) ->
		LWT_PGSQL(db) "SELECT COUNT(*) FROM messages \
		WHERE thr_id = $thr_id AND (NOT hidden)" >>=
	fun y -> (match y with
		| [Some x] -> return (int_of_db_count x)
		| _ -> assert false) >>=
 	fun n_shown_msg -> (match role with
		| Moderator -> (* counts all hidden messages *)
	    LWT_PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE messages.thr_id = $thr_id \
				AND threads.id = $thr_id \
				AND (messages.hidden OR threads.hidden)" >>=
	    fun y -> (match y with
				| [Some x] -> return (int_of_db_count x)
				| _ -> assert false)
 		| Author aid -> (* counts only hidden messages posted by her *)
	    LWT_PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE messages.thr_id = $thr_id \
				AND threads.id = $thr_id \
				AND (messages.hidden OR threads.hidden) \
				AND messages.author_id = $aid" >>=
	    fun y -> (match y with
				| [Some x] -> return (int_of_db_count x)
				| _ -> assert false)
		| Unknown -> (* nothing to be counted *) return 0) >>=
	fun n_hidden_msg -> commit db >>=
 	fun _ -> Ocsigen_messages.debug2 "[Sql] thread_get_data: finish"; return (id, subject, author_id, article, datetime, hidden, n_shown_msg, n_hidden_msg);;

let message_get_data db ~frm_id ~msg_id =
  (* returns id, text, author, datetime, hidden status of a message *)
	Ocsigen_messages.debug2 "[Sql] message_get_data";
	LWT_PGSQL(db) "SELECT messages.id, textdata.txt, fullname, \
		messages.datetime, messages.hidden \
		FROM messages, textdata, threads, users \
		WHERE messages.id = $msg_id \
		AND messages.txt_id = textdata.id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id \
		AND users.id = messages.author_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, text, author_id, datetime, hidden) ->
		Ocsigen_messages.debug2 "[Sql] message_get_data: finish";
		return (id, text, author_id, datetime, hidden);;
      
let thread_get_neighbours db ~frm_id ~thr_id ~role =
  (* returns None|Some id of prev & next thread in the same forum. *)
	Ocsigen_messages.debug2 "[Sql] thread_get_neighbours";
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT datetime FROM threads \
		WHERE id = $thr_id AND frm_id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun datetime -> (match role with
	| Moderator -> (* all kinds of threads *)
	    LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				ORDER BY datetime DESC LIMIT 1"
	| Author aid -> (* only shown threads, or hidden ones posted by her *)
	    LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				AND (author_id = $aid OR NOT hidden) \
				ORDER BY datetime DESC LIMIT 1"
 	| Unknown -> (* only shown threads *)
	    LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				AND (NOT hidden) \
				ORDER BY datetime DESC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun prev -> (match role with
	| Moderator ->
			LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
 				ORDER BY datetime ASC LIMIT 1"
	| Author aid ->
			LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
				AND (author_id = $aid OR NOT hidden) \
 				ORDER BY datetime ASC LIMIT 1"
	| Unknown ->
			LWT_PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
				AND (NOT hidden) \
				ORDER BY datetime ASC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun next -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] thread_get_neighbours: finish"; return (prev, next);;

let message_get_neighbours db ~frm_id ~msg_id ~role =
  (* returns None|Some id of prev & next message in the same
     thread. *)
	Ocsigen_messages.debug2 "[Sql] message_get_neighbours";
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT messages.thr_id, messages.datetime \
		FROM messages, threads \
		WHERE messages.id = $msg_id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (thr_id, datetime) ->  (match role with
	| Moderator -> (* all kinds of threads *)
	    LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				ORDER BY datetime DESC LIMIT 1"
	| Author aid -> (* only shown messages, or hidden ones posted by her *)
	    LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				AND (author_id = $aid OR NOT hidden) \
				ORDER BY datetime DESC LIMIT 1"
 	| Unknown -> (* only shown messages *)
	    LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				AND (NOT hidden) \
				ORDER BY datetime DESC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun prev -> (match role with
	| Moderator ->
			LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
 				ORDER BY datetime ASC LIMIT 1"
	| Author aid ->
			LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
				AND (author_id = $aid OR NOT hidden) \
 				ORDER BY datetime ASC LIMIT 1"
	| Unknown ->
			LWT_PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
				AND (NOT hidden) \
				ORDER BY datetime ASC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun next -> commit db >>=
	fun _ -> Ocsigen_messages.debug2 "[Sql] message_get_neighbours: finish"; return (prev, next);;

let forum_get_threads_list db ~frm_id ?offset ?limit ~role () =
  (* returns the threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
	Ocsigen_messages.debug2 "[Sql] forum_get_threads_list";
	let db_offset = match offset with
	| None -> db_size_of_int 0
	| Some x -> db_size_of_int x in
	match limit with
	| None -> 
		(match role with
		| Moderator -> LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id AND author_id = users.id \
				ORDER BY datetime DESC \
				OFFSET $db_offset"
		| Author aid ->
			LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id \
				AND (author_id = $aid OR NOT hidden) \
				AND users.id = author_id \
				ORDER BY datetime DESC \
				OFFSET $db_offset"
		| Unknown -> LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id AND users.id = author_id \
				AND NOT hidden \
				ORDER BY datetime DESC \
				OFFSET $db_offset")
	| Some x -> let db_limit = db_size_of_int x in
		(match role with
		| Moderator -> LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id AND users.id = author_id \
				ORDER BY datetime DESC \
				LIMIT $db_limit OFFSET $db_offset"
		| Author aid -> 
			LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id AND users.id = author_id \
 				AND (author_id = $aid OR NOT hidden) \
				ORDER BY datetime DESC \
				LIMIT $db_limit OFFSET $db_offset"
		| Unknown -> LWT_PGSQL(db) "SELECT threads.id, subject, fullname, datetime, hidden \
				FROM threads, users \
				WHERE frm_id = $frm_id AND users.id = author_id \
				AND NOT hidden \
				ORDER BY datetime DESC \
				LIMIT $db_limit OFFSET $db_offset");;

let rec forest_of (get_coords: 'a -> 'b) (l: 'a list): 'a tree list =
let rec get_children_of (min, max) l2 = 	
begin
	match l2 with
	| [] -> ([], [])
	| x::xs -> let (xmin, xmax) = get_coords x in
		if (xmin > min) && (xmax < max) then
		let (ch, rest) = get_children_of (xmin, xmax) xs in
			([Node (x, ch)], rest)
		else
			([], l2)
end in
begin
	match l with
	| [] -> []
	| x::xs -> let (ch, rest) = get_children_of (get_coords x) xs in
			(Node (x, ch))::(forest_of get_coords rest)
end;;

let rec cut f id =
function
|	[]  -> []
|	h::t -> if (f h) = id then [h] else h::(cut f id t);;

let thread_get_messages_with_text db ~thr_id ?offset ?limit ~role ?bottom () =
	Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text";
let db_offset = match offset with
| None -> db_size_of_int 0
| Some x -> db_size_of_int x in
match limit with
| None -> 
	begin_work db >>=
	fun _ -> (match role with 
	| Moderator ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE txt_id = textdata.id AND thr_id = $thr_id AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset" 
	| Author aid ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE (author_id = $aid OR NOT hidden) AND txt_id = textdata.id AND \
			thr_id = $thr_id AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset" 
	| Unknown ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
			AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset") >>=
	fun msg_l -> commit db >>=
	fun _ -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_) -> id) btm msg_l in
		Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text: finish";
			return final_msg_l
| Some x -> let db_limit = db_size_of_int x in
	begin_work db >>=
	fun _ -> (match role with 
	| Moderator ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE txt_id = textdata.id AND thr_id = $thr_id AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Author aid ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE (author_id = $aid OR NOT hidden) AND txt_id = textdata.id AND \
			thr_id = $thr_id AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Unknown ->
		LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky \
		FROM messages, textdata, users \
		WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
			AND users.id = author_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset") >>=
  fun msg_l -> commit db >>=
	fun _ -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_)->id) btm msg_l in
	Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text: finish";
			return final_msg_l;;

let thread_get_messages_with_text_forest db ~thr_id ?offset ?limit ?top ?bottom ~role () =
	Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text_forest";
	let db_offset = match offset with
	| None -> db_size_of_int 0
	| Some x -> db_size_of_int x in
	match limit with
	| None -> begin_work db >>=
		fun _ -> (match top with
		| None -> (LWT_PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
			FROM messages WHERE thr_id = $thr_id" >>=
			fun z -> return (match z with [(x,y)] -> (match x, y with Some x',Some y' -> (x',y')| _-> (Int32.zero,Int32.zero)) | _ -> (Int32.zero,Int32.zero)))
		| Some t -> (LWT_PGSQL(db) "SELECT tree_min, tree_max \
			FROM messages WHERE thr_id = $thr_id AND id = $t") >>=
			fun y -> (match y	with [x] -> return x | _ -> fail Not_found)) >>=
		fun (db_min, db_max) -> (match role with 
		| Moderator ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata, users \
			WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
			AND thr_id = $thr_id AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset" 
		| Author aid ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata, users \
			WHERE (author_id = $aid OR NOT hidden) AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
				AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset" 
		| Unknown ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata, users \
			WHERE NOT hidden AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
				AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset") >>=
		fun msg_l -> commit db >>=
		fun _ -> let final_msg_l = match bottom with
			| None -> msg_l
			| Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l in
		Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text_forest: finish";
      return (forest_of
			 	(fun (_,_,_,_,_,_,x,y)->(int_of_db_int x, int_of_db_int y))
				final_msg_l)
	| Some x -> let db_limit = db_size_of_int x in
    begin_work db >>=
		fun _ -> (match top with
		| None -> (LWT_PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
			FROM messages WHERE thr_id = $thr_id" >>=
			fun z -> return (match z with [(x,y)] -> (match x, y with Some x',Some y' -> (x',y')| _-> (Int32.zero,Int32.zero)) | _ -> (Int32.zero,Int32.zero)))
		| Some t -> (LWT_PGSQL(db) "SELECT tree_min, tree_max \
			FROM messages WHERE thr_id = $thr_id AND id = $t") >>=
			fun y -> (match y	with [x] -> return x | _ -> fail Not_found)) >>=
		fun (db_min, db_max) -> (match role with 
		| Moderator ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata, users \
			WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
			AND thr_id = $thr_id AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset" 
		| Author aid ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata, users \
			WHERE (author_id = $aid OR NOT hidden) AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
				AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset" 
		| Unknown ->
			LWT_PGSQL(db) "SELECT messages.id,txt,fullname,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata, users \
			WHERE NOT hidden AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
				AND users.id = author_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset") >>=
	fun msg_l -> commit db >>=
	fun _ -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l in
		Ocsigen_messages.debug2 "[Sql] thread_get_messages_with_text_forest: finish";
	return (forest_of
		(fun (_,_,_,_,_,_,x,y)->(int_of_db_int x, int_of_db_int y))
		final_msg_l);;

let get_latest_messages db ~frm_ids ~limit () =
	Ocsigen_messages.debug2 (Printf.sprintf "[Sql] get_latest_messages [%s]" (String.concat "," (List.map string_of_db_int frm_ids)));
let db_limit = db_size_of_int limit in
	LWT_PGSQL(db) "SELECT messages.id,txt,fullname \
	FROM messages, textdata, users \
	WHERE messages.txt_id = textdata.id AND \
	thr_id IN (SELECT id FROM threads WHERE frm_id IN $@frm_ids) AND
	NOT messages.hidden AND users.id = author_id \
	ORDER BY datetime DESC LIMIT $db_limit" >>=
	fun result -> Ocsigen_messages.debug2 "[Sql] get_latest_messages: finish"; return result;;

let new_wiki db ~title ~descr =
  (* inserts a new wiki *)
  begin_work db >>= fun _ ->
  LWT_PGSQL(db) "INSERT INTO wikis (title, descr) \
                 VALUES ($title, $descr)" >>= fun () ->
  serial4 db "wikis_id_seq" >>= fun wik_id ->
  commit db >>= fun _ ->
  return wik_id

(*
let new_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* inserts a new wikipage in an existing wiki; returns [None] if
     [~suffix] is already used in that wiki. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_id =
        (match 
          LWT_PGSQL(db) "SELECT id FROM wikipages \
            WHERE wik_id = $wik_id AND suffix = $suffix" 
        with
        | [] ->
	    LWT_PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
	    let txt_id = serial4 db "textdata_id_seq" in
	    LWT_PGSQL(db) "INSERT INTO wikipages \
              (wik_id, suffix, author, subject, txt_id) \
              VALUES ($wik_id,$suffix,$author,$subject,$txt_id)";
	      Some (serial4 db "wikipages_id_seq")
        | _ -> None) in
      commit db;
      wpg_id)
    ()

let add_or_change_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* updates, or inserts, a wikipage. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      (match
        LWT_PGSQL(db) "SELECT id, txt_id FROM wikipages \
          WHERE wik_id = $wik_id AND suffix = $suffix" 
      with
      | [(wpg_id,txt_id)] ->
	  LWT_PGSQL(db) "UPDATE textdata SET txt = $txt WHERE id = $txt_id";
	  LWT_PGSQL(db) "UPDATE wikipages \
            SET suffix = $suffix, author = $author, \
            subject = $subject \
            WHERE id = $wpg_id"
      | _ ->
	  LWT_PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
	  let txt_id = serial4 db "textdata_id_seq" in
	  LWT_PGSQL(db) "INSERT INTO wikipages \
            (wik_id, suffix, author, subject, txt_id) \
            VALUES ($wik_id,$suffix,$author,$subject,$txt_id)");
            commit db)
        ()

let wiki_get_data ~wik_id = 
  (* returns title, description, number of wikipages of a wiki. *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let (title, description) = 
        (match LWT_PGSQL(db) "SELECT title, descr FROM wikis WHERE id = $wik_id"
        with [x] -> x | _ -> assert false) in
      let n_pages = 
        (match LWT_PGSQL(db)
            "SELECT COUNT(*) FROM wikipages WHERE wik_id = $wik_id"
        with [Some x] -> x | _ -> assert false) in
      commit db;
      (title, description, int_of_db_size n_pages))
    ()

let wiki_get_pages_list ~wik_id =
  (* returns the list of wikipages *)
  Lwt_preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_l = LWT_PGSQL(db) "SELECT subject, suffix, author, datetime \
          FROM wikipages \
          WHERE wik_id = $wik_id \
          ORDER BY subject ASC" in
          commit db;
        wpg_l)
    ()
*)    

let wikibox_get_data db ~wiki ~id =
  (* returns subject, text, author, datetime of a wikipage; None if
     non-existant *)
  LWT_PGSQL(db) "SELECT comment, author, content, datetime \
                 FROM wikiboxes \
                 WHERE wiki_id = $wiki \
                 AND id = $id \
                 AND version = \
                     (SELECT max(version) \
                      FROM wikiboxes \
                      WHERE wiki_id = $wiki \
                      AND id = $id)" >>= 
  function
    | [] -> Lwt.return None
    | [x] -> Lwt.return (Some x)
    | x::_ -> 
        Ocsigen_messages.warning
          "Ocsimore: database error (Wiki_sql.wikipage_get_data)";
        Lwt.return (Some x)




(*
(* SERVICES *)
(*VVV PAS FINI !!!!!!!!!!!!!!! *)
let new_service db ~url =
  (* inserts a new service *)
  begin_work db >>=
  fun _ -> LWT_PGSQL(db) "INSERT INTO services (url) \
		VALUES ($url)" >>=
	fun () -> serial4 db "services_id_seq" >>=
	fun srv_id -> commit db >>=
	fun _ -> return srv_id;;

let list_services db =
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT url FROM services" >>=
	fun srv_l -> commit db >>=
	fun _ -> return srv_l;;

let get_service_parameters db ~url =
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT id FROM services WHERE url = $url" >>=
	fun x -> (match x with [id] -> return id | _ -> fail Not_found) >>=
	fun id -> LWT_PGSQL(db) "SELECT id, name FROM service_parameters \
		WHERE service_id = $id" >>=
	fun param_l -> commit db >>=
	fun _ -> return param_l;;

let add_parameter_to_service db ~url ~param_name =
	begin_work db >>=
	fun _ -> LWT_PGSQL(db) "SELECT id FROM services WHERE url = $url" >>=
	fun x -> (match x with [id] -> return id | _ -> fail Not_found) >>=
	fun id -> LWT_PGSQL(db) "INSERT INTO service_parameters \
		(service_id, name) VALUES \
		($id, $param_name)"  >>=
	fun () -> serial4 db "service_parameters_id_seq" >>=
	fun param_id -> commit db >>=
	fun _ -> return param_id;;
*)
