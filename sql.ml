(** PostgreSQL database operations via PGOCaml library. *)

open Lwt_PGOCaml 
  (* SQL aggregate functions can sometimes return NULL values, but
     this is not the case for COUNT, that _always_ returns a non-NULL
     value. PGOCaml's nullability test fails here, as the type
     inferred for PGSQL(db) "SELECT COUNT(field) FROM YourTable" is
     'int64 option' and not 'int64', as expected. *)

open Lwt
open Ocsimorelib

(* let db = Lwt_PGOCaml.connect ~host:"courbet.kerguelen.org" ~database:"ocsimore" ~user:"ocsigen" () *)

type db_t = (string, bool) Hashtbl.t Lwt_PGOCaml.t

module Persist = struct
  (* This is, mutatis mutandis, Vincent's Ocsipersist
     module. Thanks to him. *)

  type 'a t = string * 'a ref

  let lwtcreate db name default =
		Messages.debug "[Sql] lwtcreate";
		begin_work db >>= 
		fun () -> PGSQL(db) "SELECT value FROM globalstore WHERE key = $name" >>=
		fun res -> (match res with
		| [Some n] ->
				commit db >>=
				fun () -> return (Some (name, ref (Marshal.from_string n 0)))
		| _ ->
				commit db >>=
				fun () -> return None) >>=
    function
      | Some pv -> return pv
      | None -> default () >>=
          (fun def ->
                begin_work db >>=
	        fun () -> let v = Marshal.to_string def [] in 
	        PGSQL(db) "INSERT INTO globalstore VALUES ($name, $v)" >>=
          fun () -> commit db >>=
	        fun () -> return (name, ref def)
          )

  let create db name default =
    lwtcreate db name (fun () -> return (default ()))

  let get ((pvname, pvref): 'a t) = (!pvref: 'a)

  let set db (pvname, pvref) value =
    let v = Marshal.to_string value [] in
    pvref := value;
    PGSQL(db) "UPDATE globalstore SET value = $v WHERE key = $pvname"

  let write_back db (pv: 'a t) = set db pv (get pv)

end

let connect () =
	Messages.debug "[Sql] connect: start";
	Lwt_PGOCaml.connect ~host:"localhost" ~database:"ocsimore" ~user:"ocsigen" ();;

type db_int_t = int32;;
type db_size_t = int64;;

let db_int_of_int = Int32.of_int;;
let db_size_of_int = Int64.of_int;;
let db_int_of_string = Int32.of_string;;
let string_of_db_int = Int32.to_string;;
let int_of_db_int = Int32.to_int;;
let int_of_db_size = Int64.to_int;;

  (* I could define here a functor to try to abstract the db
     structure, improving Vincent's Ocsicache.Make; but I need lots of
     specialized SQL queries and commands, so it's easy and clearer to
     define those ones directly. *)


(* FORUMS *)

  (* Lots of queries here take a ~frm_id parameter, even if other ones
     should be enough to determinate a primary key.  This has been
     done because of the need to match every query request against a
     forum's ACL.  TO BE DONE: A LAYER FOR ACCESS CONTROL *)


(* Used to restrict the recordsets *)
type role = Moderator | Author of string | Unknown;;

type message_info =
	db_int_t * string * string * Calendar.t * bool * db_int_t option * string option;;
(* type 'a collection = List of 'a list | Forest of 'a tree list;; *)

let new_forum db ~title ~descr ~moderated =
  (* inserts a new forum *)
  begin_work db >>=
  fun () -> PGSQL(db) "INSERT INTO forums (title, descr, moderated) \
		VALUES ($title, $descr, $moderated)" >>=
	fun () -> serial4 db "forums_id_seq" >>=
	fun frm_id -> commit db >>=
	fun () -> return frm_id;;
        
let new_thread_and_message db ~frm_id ~author ~subject ~txt = 
  (* inserts a message starting a new thread; both thread and message
     will be hidden if forum is moderated *)
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>= 
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author) \
		VALUES ($frm_id, $subject, $hidden, $author)" >>=
	fun () -> serial4 db "threads_id_seq" >>=
	fun thr_id -> PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () ->	serial4 db "textdata_id_seq" >>=
	fun txt_id -> PGSQL(db) "SELECT MAX(tree_max) FROM messages \
		WHERE thr_id = $thr_id" >>=
	fun z -> (match z with
		| [x] -> (match x with
			| None -> return (db_int_of_int 0)
			| Some y -> return y)
		| _ -> return (db_int_of_int 0)) >>=
	fun db_max -> PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, tree_min, tree_max) \
		VALUES ($author, $thr_id, $txt_id, $hidden, $db_max + 1, $db_max + 2)" >>=
	fun () -> serial4 db "messages_id_seq" >>=
	fun msg_id -> commit db >>=
 	fun () -> return (thr_id, msg_id);;
    
let new_thread_and_article db ~frm_id ~author ~subject ~txt =
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () -> serial4 db "textdata_id_seq" >>=
	fun txt_id -> PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author, article_id) \
		VALUES ($frm_id, $subject, $hidden, $author, $txt_id)" >>=
	fun () -> serial4 db "threads_id_seq" >>=
	fun thr_id -> commit db >>=
	fun () -> return (thr_id, txt_id);;

let new_message db ~thr_id ?parent_id ~author ~txt ~sticky () = 
  (* inserts a message in an existing thread; message will be hidden
     if forum is moderated *)
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT moderated FROM forums,threads \
		WHERE threads.id = $thr_id \
		AND threads.frm_id = forums.id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun hidden -> PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)" >>=
	fun () -> serial4 db "textdata_id_seq" >>=
	fun txt_id -> PGSQL(db) "SELECT MAX(tree_max) FROM messages \
		WHERE thr_id = $thr_id" >>=
	fun z -> (match z with
		| [x] -> (match x with
			| None -> return (db_int_of_int 0)
			| Some y -> return y)
		| _ -> return (db_int_of_int 0)) >>=
	fun db_max -> 
		(match parent_id with
		| None -> PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, \
				sticky, tree_min, tree_max) \
				VALUES ($author, $thr_id, $txt_id, $hidden, $sticky, \
				$db_max + 1, $db_max + 2)" >>=
				fun () -> serial4 db "messages_id_seq"
		| Some pid -> PGSQL(db) "SELECT tree_min, tree_max \
				FROM messages WHERE id = $pid" >>=
				fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
				fun (db_min, db_max) -> PGSQL(db) "UPDATE messages SET tree_min = tree_min + 2, \
					tree_max = tree_max + 2 WHERE tree_min > $db_max" >>=
				fun () -> PGSQL(db) "UPDATE messages SET tree_max = tree_max + 2 \
					WHERE $db_min BETWEEN tree_min AND tree_max" >>=
				fun () -> PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, \
					sticky, tree_min, tree_max) \
					VALUES ($author, $thr_id, $txt_id, $hidden, $sticky, $db_max, \
					$db_max + 1)" >>=
				fun () -> serial4 db "messages_id_seq") >>=
	fun msg_id ->	commit db >>=
	fun () -> return msg_id;;

let forum_toggle_moderated db ~frm_id =
	(* toggle moderation status of a forum *)
  PGSQL(db) "UPDATE forums SET moderated = NOT moderated WHERE id = $frm_id";;
    
let thread_toggle_hidden db ~frm_id ~thr_id =
	(* hides/shows a thread *)
	PGSQL(db) "UPDATE threads SET hidden = NOT hidden \
 		WHERE id = $thr_id AND frm_id = $frm_id";;

let message_toggle_hidden db ~frm_id ~msg_id =
  (* hides/shows a message *)
	PGSQL(db) "UPDATE messages \
		SET hidden = NOT messages.hidden \
		FROM threads \
		WHERE messages.id = $msg_id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id";;

let message_toggle_sticky db ~frm_id ~msg_id =
	PGSQL(db) "UPDATE messages SET sticky = NOT messages.sticky \
		FROM threads WHERE messages.id = $msg_id AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id"

let forum_get_data db ~frm_id ~role =
  (* returns id, title, description, mod status, number of shown/hidden
     threads and messages of a forum.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
	begin_work db >>=
  fun () -> PGSQL(db) "SELECT id, title, descr, moderated FROM forums \
		WHERE id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, title, description, moderated) -> 
		PGSQL(db) "SELECT COUNT(*) FROM threads \
		WHERE frm_id = $frm_id AND (NOT hidden)" >>=
	fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false) >>=
	fun n_shown_thr  -> PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
		WHERE threads.frm_id = $frm_id \
		AND messages.thr_id = threads.id \
		AND NOT (messages.hidden OR threads.hidden)" >>=
	fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false) >>=
	fun n_shown_msg -> (match role with	
		| Moderator -> (* counts all hidden stuff *)
			PGSQL(db) "SELECT COUNT(*) FROM threads \
				WHERE frm_id = $frm_id AND hidden" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false)
		| Author a ->
			PGSQL(db) "SELECT COUNT(*) FROM threads \
				WHERE frm_id = $frm_id AND hidden \
				AND author = $a" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false)
		| Unknown -> return 0) >>=
	fun n_hidden_thr -> (match role with
		| Moderator -> 
			PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE threads.frm_id = $frm_id \
				AND messages.thr_id = threads.id \
				AND (messages.hidden OR threads.hidden)" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false)
		| Author a ->
			PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
 				WHERE threads.frm_id = $frm_id \
				AND messages.thr_id = threads.id \
				AND (messages.hidden OR threads.hidden) \
				AND messages.author = $a" >>=
			fun y -> (match y with [Some x] -> return (int_of_db_size x) | _ -> assert false)
		| Unknown -> return 0) >>=
	fun n_hidden_msg -> commit db >>=
	fun () -> return 
      (id, title, description, moderated,
       n_shown_thr, n_hidden_thr, n_shown_msg, n_hidden_msg);;

let thread_get_nr_messages db ~thr_id ~role =
	Messages.debug "[Sql] thread_get_nr_messages";
	begin_work db >>=
	fun () -> (match role with
	| Moderator -> (* all messages *)
		PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))
	| Author a -> (* all non-hidden messages AND hidden messages posted by a *)
	  PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id \
			AND messages.hidden = false OR messages.author = $a" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))
	| Unknown -> (* all non-hidden messages *)
	  PGSQL(db) "SELECT COUNT(*) FROM messages \
			WHERE messages.thr_id = $thr_id \
			AND messages.hidden = false" >>=
		fun y -> (match y with
			| [Some x] -> return x
			| _ -> fail (Failure "thread_get_nr_messages"))) >>=
	fun n_msg -> commit db >>=
	fun () -> Messages.debug "[Sql] thread_get_nr_messages: end"; return (int_of_db_size n_msg);;

let thread_get_data db (* ~frm_id *) ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT t.id, subject, t.author, t.datetime, t.hidden, COALESCE (txt, '') \
		FROM threads AS t LEFT OUTER JOIN textdata ON \
		(t.article_id = textdata.id) \
		WHERE t.id = $thr_id" (* AND frm_id = $frm_id *) >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, subject, author, datetime, hidden, article) ->
		PGSQL(db) "SELECT COUNT(*) FROM messages \
		WHERE thr_id = $thr_id AND (NOT hidden)" >>=
	fun y -> (match y with
		| [Some x] -> return (int_of_db_size x)
		| _ -> assert false) >>=
 	fun n_shown_msg -> (match role with
		| Moderator -> (* counts all hidden messages *)
	    PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE messages.thr_id = $thr_id \
				AND threads.id = $thr_id \
				AND (messages.hidden OR threads.hidden)" >>=
	    fun y -> (match y with
				| [Some x] -> return (int_of_db_size x)
				| _ -> assert false)
 		| Author a -> (* counts only hidden messages posted by her *)
	    PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
				WHERE messages.thr_id = $thr_id \
				AND threads.id = $thr_id \
				AND (messages.hidden OR threads.hidden) \
				AND messages.author = $a" >>=
	    fun y -> (match y with
				| [Some x] -> return (int_of_db_size x)
				| _ -> assert false)
		| Unknown -> (* nothing to be counted *) return 0) >>=
	fun n_hidden_msg -> commit db >>=
 	fun () -> return (id, subject, author, article, datetime, hidden, n_shown_msg, n_hidden_msg);;

let message_get_data db ~frm_id ~msg_id =
  (* returns id, text, author, datetime, hidden status of a message *)
	PGSQL(db) "SELECT messages.id, textdata.txt, messages.author, \
		messages.datetime, messages.hidden \
		FROM messages, textdata, threads \
		WHERE messages.id = $msg_id \
		AND messages.txt_id = textdata.id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (id, text, author, datetime, hidden) ->
		return (id, text, author, datetime, hidden);;
      
let thread_get_neighbours db ~frm_id ~thr_id ~role =
  (* returns None|Some id of prev & next thread in the same forum. *)
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT datetime FROM threads \
		WHERE id = $thr_id AND frm_id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun datetime -> (match role with
	| Moderator -> (* all kinds of threads *)
	    PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				ORDER BY datetime DESC LIMIT 1"
	| Author a -> (* only shown threads, or hidden ones posted by her *)
	    PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				AND (author = $a OR NOT hidden) \
				ORDER BY datetime DESC LIMIT 1"
 	| Unknown -> (* only shown threads *)
	    PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime < $datetime \
				AND (NOT hidden) \
				ORDER BY datetime DESC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun prev -> (match role with
	| Moderator ->
			PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
 				ORDER BY datetime ASC LIMIT 1"
	| Author a ->
			PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
				AND (author = $a OR NOT hidden) \
 				ORDER BY datetime ASC LIMIT 1"
	| Unknown ->
			PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
				AND datetime > $datetime \
				AND (NOT hidden) \
				ORDER BY datetime ASC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun next -> commit db >>=
	fun () -> return (prev, next);;

let message_get_neighbours db ~frm_id ~msg_id ~role =
  (* returns None|Some id of prev & next message in the same
     thread. *)
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT messages.thr_id, messages.datetime \
		FROM messages, threads \
		WHERE messages.id = $msg_id \
		AND messages.thr_id = threads.id \
		AND threads.frm_id = $frm_id" >>=
	fun y -> (match y with [x] -> return x | _ -> fail Not_found) >>=
	fun (thr_id, datetime) ->  (match role with
	| Moderator -> (* all kinds of threads *)
	    PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				ORDER BY datetime DESC LIMIT 1"
	| Author a -> (* only shown messages, or hidden ones posted by her *)
	    PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				AND (author = $a OR NOT hidden) \
				ORDER BY datetime DESC LIMIT 1"
 	| Unknown -> (* only shown messages *)
	    PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime < $datetime \
				AND (NOT hidden) \
				ORDER BY datetime DESC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun prev -> (match role with
	| Moderator ->
			PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
 				ORDER BY datetime ASC LIMIT 1"
	| Author a ->
			PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
				AND (author = $a OR NOT hidden) \
 				ORDER BY datetime ASC LIMIT 1"
	| Unknown ->
			PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
				AND datetime > $datetime \
				AND (NOT hidden) \
				ORDER BY datetime ASC LIMIT 1") >>=
	fun y -> return (match y with [x] -> Some x | _ -> None) >>=
	fun next -> commit db >>=
	fun () -> return (prev, next);;

let forum_get_threads_list db ~frm_id ?offset ?limit ~role () =
  (* returns the threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
	let db_offset = match offset with
	| None -> db_size_of_int 0
	| Some x -> db_size_of_int x in
	match limit with
	| None -> 
		(match role with
		| Moderator -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
				ORDER BY datetime DESC \
				OFFSET $db_offset"
		| Author a -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
				AND (author = $a OR NOT hidden) \
				ORDER BY datetime DESC \
				OFFSET $db_offset"
		| Unknown -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
				AND NOT hidden \
				ORDER BY datetime DESC \
				OFFSET $db_offset")
	| Some x -> let db_limit = db_size_of_int x in
		(match role with
		| Moderator -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
				ORDER BY datetime DESC \
				LIMIT $db_limit OFFSET $db_offset"
		| Author a -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
 				AND (author = $a OR NOT hidden) \
				ORDER BY datetime DESC \
				LIMIT $db_limit OFFSET $db_offset"
		| Unknown -> PGSQL(db) "SELECT id, subject, author, datetime, hidden \
				FROM threads \
				WHERE frm_id = $frm_id \
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
Messages.debug "[Sql] thread_get_messages_with_text";
let db_offset = match offset with
| None -> db_size_of_int 0
| Some x -> db_size_of_int x in
match limit with
| None -> 
	begin_work db >>=
	fun () -> (match role with 
	| Moderator ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset" 
	| Author a ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
			thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset" 
	| Unknown ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		OFFSET $db_offset") >>=
	fun msg_l -> commit db >>=
	fun () -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_) -> id) btm msg_l in
			Messages.debug "[Sql] thread_get_messages_with_text: end 1";
			return final_msg_l
| Some x -> let db_limit = db_size_of_int x in
	begin_work db >>=
	fun () -> (match role with 
	| Moderator ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Author a ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
			thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Unknown ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky \
		FROM messages, textdata \
		WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset") >>=
  fun msg_l -> commit db >>=
	fun () -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_)->id) btm msg_l in
			Messages.debug "[Sql] thread_get_messages_with_text: end 1";
			return final_msg_l;;

let thread_get_messages_with_text_forest db ~thr_id ?offset ?limit ?top ?bottom ~role () =
	let db_offset = match offset with
	| None -> db_size_of_int 0
	| Some x -> db_size_of_int x in
	match limit with
	| None -> begin_work db >>=
		fun () -> (match top with
		| None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
			FROM messages WHERE thr_id = $thr_id" >>=
			fun z -> return (match z with [(x,y)] -> (match x, y with Some x',Some y' -> (x',y')| _-> (Int32.zero,Int32.zero)) | _ -> (Int32.zero,Int32.zero)))
		| Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
			FROM messages WHERE thr_id = $thr_id AND id = $t") >>=
			fun y -> (match y	with [x] -> return x | _ -> fail Not_found)) >>=
		fun (db_min, db_max) -> (match role with 
		| Moderator ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata \
			WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
			AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset" 
		| Author a ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata \
			WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset" 
		| Unknown ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min, tree_max \
			FROM messages, textdata \
			WHERE NOT hidden AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			OFFSET $db_offset") >>=
		fun msg_l -> commit db >>=
		fun () -> let final_msg_l = match bottom with
			| None -> msg_l
			| Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l in
      return (forest_of
			 	(fun (_,_,_,_,_,_,x,y)->(int_of_db_int x, int_of_db_int y))
				final_msg_l)
	| Some x -> let db_limit = db_size_of_int x in
    begin_work db >>=
		fun () -> (match top with
		| None -> (PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
			FROM messages WHERE thr_id = $thr_id" >>=
			fun z -> return (match z with [(x,y)] -> (match x, y with Some x',Some y' -> (x',y')| _-> (Int32.zero,Int32.zero)) | _ -> (Int32.zero,Int32.zero)))
		| Some t -> (PGSQL(db) "SELECT tree_min, tree_max \
			FROM messages WHERE thr_id = $thr_id AND id = $t") >>=
			fun y -> (match y	with [x] -> return x | _ -> fail Not_found)) >>=
		fun (db_min, db_max) -> (match role with 
		| Moderator ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata \
			WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
			AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset" 
		| Author a ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata \
			WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset" 
		| Unknown ->
			PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			tree_min,tree_max \
			FROM messages, textdata \
			WHERE NOT hidden AND txt_id = textdata.id AND \
				(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
			ORDER BY sticky DESC, tree_min \
			LIMIT $db_limit OFFSET $db_offset") >>=
	fun msg_l -> commit db >>=
	fun () -> let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut (fun (id,_,_,_,_,_,_,_) -> id) btm msg_l in
	return (forest_of
		(fun (_,_,_,_,_,_,x,y)->(int_of_db_int x, int_of_db_int y))
		final_msg_l);;

let get_latest_messages db ~frm_ids ~limit () =
let db_limit = db_size_of_int limit in
	PGSQL(db) "SELECT messages.id,txt,author \
	FROM messages, textdata \
	WHERE messages.txt_id = textdata.id AND \
	thr_id IN (SELECT id FROM threads WHERE frm_id IN $@frm_ids) AND
	NOT messages.hidden \
	ORDER BY datetime DESC LIMIT $db_limit" >>=
	fun result -> return result;;

(* let new_wiki ~title ~descr =
  (* inserts a new wiki *)
  Preemptive.detach
    (fun () ->
      begin_work db;
      let wik_id = 
        (PGSQL(db) "INSERT INTO wikis (title, descr) \
           VALUES ($title, $descr)";
           serial4 db "wikis_id_seq") in 
      commit db;
      wik_id)
    ()

let new_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* inserts a new wikipage in an existing wiki; returns [None] if
     [~suffix] is already used in that wiki. *)
  Preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_id =
        (match 
          PGSQL(db) "SELECT id FROM wikipages \
            WHERE wik_id = $wik_id AND suffix = $suffix" 
        with
        | [] ->
	    PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
	    let txt_id = serial4 db "textdata_id_seq" in
	    PGSQL(db) "INSERT INTO wikipages \
              (wik_id, suffix, author, subject, txt_id) \
              VALUES ($wik_id,$suffix,$author,$subject,$txt_id)";
	      Some (serial4 db "wikipages_id_seq")
        | _ -> None) in
      commit db;
      wpg_id)
    ()

let add_or_change_wikipage ~wik_id ~suffix ~author ~subject ~txt = 
  (* updates, or inserts, a wikipage. *)
  Preemptive.detach
    (fun () ->
      begin_work db;
      (match
        PGSQL(db) "SELECT id, txt_id FROM wikipages \
          WHERE wik_id = $wik_id AND suffix = $suffix" 
      with
      | [(wpg_id,txt_id)] ->
	  PGSQL(db) "UPDATE textdata SET txt = $txt WHERE id = $txt_id";
	  PGSQL(db) "UPDATE wikipages \
            SET suffix = $suffix, author = $author, \
            subject = $subject \
            WHERE id = $wpg_id"
      | _ ->
	  PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
	  let txt_id = serial4 db "textdata_id_seq" in
	  PGSQL(db) "INSERT INTO wikipages \
            (wik_id, suffix, author, subject, txt_id) \
            VALUES ($wik_id,$suffix,$author,$subject,$txt_id)");
            commit db)
        ()

let wiki_get_data ~wik_id = 
  (* returns title, description, number of wikipages of a wiki. *)
  Preemptive.detach
    (fun () ->
      begin_work db;
      let (title, description) = 
        (match PGSQL(db) "SELECT title, descr FROM wikis WHERE id = $wik_id"
        with [x] -> x | _ -> assert false) in
      let n_pages = 
        (match PGSQL(db)
            "SELECT COUNT(*) FROM wikipages WHERE wik_id = $wik_id"
        with [Some x] -> x | _ -> assert false) in
      commit db;
      (title, description, int_of_db_size n_pages))
    ()

let wiki_get_pages_list ~wik_id =
  (* returns the list of wikipages *)
  Preemptive.detach
    (fun () ->
      begin_work db;
      let wpg_l = PGSQL(db) "SELECT subject, suffix, author, datetime \
          FROM wikipages \
          WHERE wik_id = $wik_id \
          ORDER BY subject ASC" in
          commit db;
        wpg_l)
    ()
    

let wikipage_get_data ~wik_id ~suffix =
  (* returns subject, text, author, datetime of a wikipage; None if
     non-existant *)
  Preemptive.detach
    (fun () ->
      let wpg_data =
        (match 
          PGSQL(db) "SELECT subject, txt, author, datetime \
            FROM wikipages, textdata \
            WHERE wik_id = $wik_id AND suffix = $suffix \
            AND txt_id = textdata.id"
        with [x] -> Some x | _ -> None) in
      wpg_data)
    () *)

(* SERVICES *)
let new_service db ~url =
  (* inserts a new service *)
  begin_work db >>=
  fun () -> PGSQL(db) "INSERT INTO services (url) \
		VALUES ($url)" >>=
	fun () -> serial4 db "services_id_seq" >>=
	fun srv_id -> commit db >>=
	fun () -> return srv_id;;

let list_services db =
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT url FROM services" >>=
	fun srv_l -> commit db >>=
	fun () -> return srv_l;;

let get_service_parameters db ~url =
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT id FROM services WHERE url = $url" >>=
	fun x -> (match x with [id] -> return id | _ -> fail Not_found) >>=
	fun id -> PGSQL(db) "SELECT id, name, type FROM service_parameters \
		WHERE service_id = $id" >>=
	fun param_l -> commit db >>=
	fun () -> return param_l;;

let add_parameter_to_service db ~url ~param_name ~param_type =
	begin_work db >>=
	fun () -> PGSQL(db) "SELECT id FROM services WHERE url = $url" >>=
	fun x -> (match x with [id] -> return id | _ -> fail Not_found) >>=
	fun id -> PGSQL(db) "INSERT INTO service_parameters \
		(service_id, name, type) VALUES \
		($id, $param_name, $param_type)"  >>=
	fun () -> serial4 db "service_parameters_id_seq" >>=
	fun param_id -> commit db >>=
	fun () -> return param_id;;
