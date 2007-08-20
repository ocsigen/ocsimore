(** PostgreSQL database operations via PGOCaml library. *)

open PGOCaml 
  (* SQL aggregate functions can sometimes return NULL values, but
     this is not the case for COUNT, that _always_ returns a non-NULL
     value. PGOCaml's nullability test fails here, as the type
     inferred for PGSQL(db) "SELECT COUNT(field) FROM YourTable" is
     'int64 option' and not 'int64', as expected. *)

open Lwt
open Preemptive

let db = connect ~database:"balat" ()

module Persist = struct
  (* This is, mutatis mutandis, Vincent's Ocsipersist
     module. Thanks to him. *)

  type 'a t = string * 'a ref

  let lwtcreate name default =
    detach
      (fun () ->
        begin_work(db);
        match PGSQL(db) 
            "SELECT value FROM globalstore WHERE key = $name" with
	| [Some n] -> commit db; Some (name, ref (Marshal.from_string n 0)) 
	  | _ -> commit db; None)
       () >>=
    function
      | Some pv -> return pv
      | None -> default () >>=
          (fun def ->
            detach
              (fun () ->
                begin_work(db);
	        let v = Marshal.to_string def [] in 
	        PGSQL(db) "INSERT INTO globalstore VALUES ($name, $v)";
                commit db;
	        (name, ref def)
              )
              ())

  let create name default =
    lwtcreate name (fun () -> return (default ()))

  let get ((pvname, pvref): 'a t) = (!pvref: 'a)

  let set (pvname, pvref) value =
    detach
      (fun () ->
        let v = Marshal.to_string value [] in
        pvref := value;
        PGSQL(db) "UPDATE globalstore SET value = $v WHERE key = $pvname")
      ()

  let write_back (pv: 'a t) = set pv (get pv)

end

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

type 'a tree = Node of 'a * ('a tree list);;
type 'a collection = List of 'a list | Forest of 'a tree list;;

let new_forum ~title ~descr ~moderated =
  (* inserts a new forum *)
  detach
    (fun () ->
      begin_work db;
      let frm_id = 
        (PGSQL(db) "INSERT INTO forums (title, descr, moderated) \
           VALUES ($title, $descr, $moderated)";
           serial4 db "forums_id_seq") in
      commit db;
      frm_id)
    ()
        
let new_thread_and_message ~frm_id ~author ~subject ~txt = 
  (* inserts a message starting a new thread; both thread and message
     will be hidden if forum is moderated *)
  detach
  (fun () ->
      begin_work db;
      let hidden = 
        (match 
          PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" 
        with [x] -> x | _ -> raise Not_found) in
      let thr_id = 
        (PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author) \
           VALUES ($frm_id, $subject, $hidden, $author)";
           serial4 db "threads_id_seq") in
      let txt_id = 
        (PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
         serial4 db "textdata_id_seq") in
      let msg_id = 
				let db_max = (match PGSQL(db) "SELECT MAX(tree_max) FROM messages \
				WHERE thr_id = $thr_id" with
					[x] -> (match x with None -> db_int_of_int 0 | Some y -> y)
						| _ -> db_int_of_int 0) in 
        (PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, tree_min, tree_max) \
           VALUES ($author, $thr_id, $txt_id, $hidden, $db_max + 1, $db_max + 2)";
           serial4 db "messages_id_seq") in
      commit db;
      (thr_id, msg_id))
    ()
    
let new_thread_and_article ~frm_id ~author ~subject ~txt =
	detach
		(fun () -> 
      begin_work db;
      let hidden = 
        (match 
          PGSQL(db) "SELECT moderated FROM forums WHERE id=$frm_id" 
        with [x] -> x | _ -> raise Not_found) in
      let txt_id = 
        (PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
         serial4 db "textdata_id_seq") in
      let thr_id = 
        (PGSQL(db) "INSERT INTO threads (frm_id, subject, hidden, author, article_id) \
           VALUES ($frm_id, $subject, $hidden, $author, $txt_id)";
           serial4 db "threads_id_seq") in
      commit db;
      (thr_id, txt_id))
    ();;

let new_message ~frm_id ~thr_id ?parent_id ~author ~txt ~sticky () = 
  (* inserts a message in an existing thread; message will be hidden
     if forum is moderated *)
  detach
    (fun () ->
      begin_work db;
      let hidden = 
        (match 
          PGSQL(db) "SELECT moderated FROM forums,threads \
            WHERE threads.frm_id = forums.id \
            AND threads.id = $thr_id \
            AND forums.id = $frm_id"
        with [x] -> x | _ -> raise Not_found) in
      let txt_id = 
        (PGSQL(db) "INSERT INTO textdata (txt) VALUES ($txt)";
         serial4 db "textdata_id_seq") in
      let msg_id = 
				match parent_id with
				| None ->
					let db_max = (match PGSQL(db) "SELECT MAX(tree_max) FROM messages \
					WHERE thr_id = $thr_id" with
						[x] -> (match x with None -> db_int_of_int 0 | Some y -> y)
							| _ -> db_int_of_int 0) in 
						(PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, \
						sticky, tree_min, tree_max) \
           VALUES ($author, $thr_id, $txt_id, $hidden, $sticky, \
					 $db_max + 1, $db_max + 2)";
           serial4 db "messages_id_seq")
				| Some pid ->
					let (db_min, db_max) = (match PGSQL(db) "SELECT tree_min, tree_max \
					FROM messages WHERE id = $pid" with
						[x] -> x | _ -> raise Not_found) in 
						(PGSQL(db) "UPDATE messages SET tree_min = tree_min + 2, \
							tree_max = tree_max + 2 WHERE tree_min > $db_max";
						PGSQL(db) "UPDATE messages SET tree_max = tree_max + 2 \
							WHERE $db_min BETWEEN tree_min AND tree_max";
						PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden, \
						sticky, tree_min, tree_max) \
						VALUES ($author, $thr_id, $txt_id, $hidden, $sticky, $db_max, \
						$db_max + 1)";
           serial4 db "messages_id_seq")
				in
      commit db;
      msg_id)
    ()

let forum_toggle_moderated ~frm_id =
  detach
    (fun () ->
      (* toggle moderation status of a forum *)
      PGSQL(db)
        "UPDATE forums SET moderated = NOT moderated WHERE id = $frm_id")
    ()
    
let thread_toggle_hidden ~frm_id ~thr_id =
  (* hides/shows a thread *)
  detach
    (fun () ->
      PGSQL(db) "UPDATE threads SET hidden = NOT hidden \
        WHERE id = $thr_id AND frm_id = $frm_id")
        ()

let message_toggle_hidden ~frm_id ~msg_id =
  (* hides/shows a message *)
  detach
    (fun () ->
      PGSQL(db) "UPDATE messages \
        SET hidden = NOT messages.hidden \
        FROM threads \
        WHERE messages.id = $msg_id \
        AND messages.thr_id = threads.id \
        AND threads.frm_id = $frm_id")
        ();;

let message_toggle_sticky ~frm_id ~msg_id =
	detach
	(fun () -> PGSQL(db) "UPDATE messages SET sticky = NOT messages.sticky \
	FROM threads WHERE messages.id = $msg_id AND messages.thr_id = threads.id \
	AND threads.frm_id = $frm_id") ();;

let forum_get_data ~frm_id ~role =
  (* returns id, title, description, mod status, number of shown/hidden
     threads and messages of a forum.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  detach
    (fun () ->
      begin_work db;
      let (id, title, description, moderated) = 
        (match 
          PGSQL(db) "SELECT id, title, descr, moderated FROM forums \
            WHERE id = $frm_id"
        with [x] -> x | _ -> raise Not_found) in
      let (n_shown_thr, n_shown_msg) = 
        ((match
	  PGSQL(db) "SELECT COUNT(*) FROM threads \
            WHERE frm_id = $frm_id AND (NOT hidden)"
        with [Some x] -> x | _ -> assert false),
         (match
	   PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
             WHERE threads.frm_id = $frm_id \
             AND messages.thr_id = threads.id \
             AND NOT (messages.hidden OR threads.hidden)"
         with [Some x] -> x | _ -> assert false)) in
      let (n_hidden_thr, n_hidden_msg) =
        (match role with
        | Moderator -> (* counts all hidden stuff *)
	    ((match
	      PGSQL(db) "SELECT COUNT(*) FROM threads \
                WHERE frm_id = $frm_id AND hidden"
	    with [Some x] -> x | _ -> assert false),
	     (match
	       PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                 WHERE threads.frm_id = $frm_id \
                 AND messages.thr_id = threads.id \
                 AND (messages.hidden OR threads.hidden)"
	     with [Some x] -> x | _ -> assert false))
        | Author a -> (* counts only hidden stuff posted by her *)
	    ((match
	      PGSQL(db) "SELECT COUNT(*) FROM threads \
                WHERE frm_id = $frm_id AND hidden \
                AND author = $a"
	    with [Some x] -> x | _ -> assert false),
	     (match
	       PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                 WHERE threads.frm_id = $frm_id \
                 AND messages.thr_id = threads.id \
                 AND (messages.hidden OR threads.hidden) \
                 AND messages.author = $a"
	     with [Some x] -> x | _ -> assert false))
        | Unknown -> (* nothing to be counted *)
	    (0L, 0L)) in
      commit db;
      (id, title, description, moderated,
       int_of_db_size n_shown_thr, int_of_db_size n_hidden_thr,
       int_of_db_size n_shown_msg, int_of_db_size n_hidden_msg))
    ()
        
let thread_get_data ~frm_id ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
	detach
	(fun () -> begin_work db;
	let (id, subject, author, datetime, hidden, article) =
		(match
			PGSQL(db) "SELECT t.id, subject, t.author, t.datetime, t.hidden, COALESCE (txt, '') \
			FROM threads AS t LEFT OUTER JOIN textdata ON \
			(t.article_id = textdata.id)\
			WHERE t.id = $thr_id AND frm_id = $frm_id"
		with [x] -> x | _ -> raise Not_found) in
 	let n_shown_msg = 
        int_of_db_size (match 
          PGSQL(db) "SELECT COUNT(*) FROM messages \
            WHERE thr_id = $thr_id AND (NOT hidden)"
        with [Some x] -> x | _ -> assert false) in
	let n_hidden_msg =
        int_of_db_size (match role with
        | Moderator -> (* counts all hidden messages *)
	    (match PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                WHERE messages.thr_id = $thr_id \
                AND threads.id = $thr_id \
                AND (messages.hidden OR threads.hidden)"
	    with [Some x] -> x | _ -> assert false)
        | Author a -> (* counts only hidden messages posted by her *)
	    (match PGSQL(db) "SELECT COUNT(*) FROM messages, threads \
                WHERE messages.thr_id = $thr_id \
                AND threads.id = $thr_id \
                AND (messages.hidden OR threads.hidden) \
                AND messages.author = $a"
	    with [Some x] -> x | _ -> assert false)
        | Unknown -> (* nothing to be counted *) 0L) in
      commit db;
      (id, subject, author, article, datetime, hidden, n_shown_msg, n_hidden_msg))
    ()
;;

let message_get_data ~frm_id ~msg_id =
  (* returns id, text, author, datetime, hidden status of a message *)
  detach
    (fun () ->
      let (id, text, author, datetime, hidden) =
        (match
          PGSQL(db) "SELECT messages.id, textdata.txt, messages.author, \
            messages.datetime, messages.hidden \
            FROM messages, textdata, threads \
            WHERE messages.id = $msg_id \
            AND messages.txt_id = textdata.id \
            AND messages.thr_id = threads.id \
            AND threads.frm_id = $frm_id" 
        with [x] -> x | _ -> raise Not_found) in
      (id, text, author, datetime, hidden))
    ()
;;
      
let thread_get_neighbours ~frm_id ~thr_id ~role =
  (* returns None|Some id of prev & next thread in the same forum. *)
  detach
    (fun () ->
      begin_work db;
      let datetime = 
        (match 
          PGSQL(db) "SELECT datetime FROM threads \
            WHERE id = $thr_id AND frm_id = $frm_id"
        with [x] -> x | _ -> raise Not_found) in
      let (prev, next) = 
        (match role with
        | Moderator -> (* all kinds of threads *)
	    (PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime < $datetime \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime > $datetime \
               ORDER BY datetime ASC \
               LIMIT 1")
        | Author a -> (* only shown threads, or hidden ones posted by her *)
	    (PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime < $datetime \
               AND (author = $a OR NOT hidden) \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime > $datetime \
               AND (author = $a OR NOT hidden) \
               ORDER BY datetime ASC \
               LIMIT 1")
        | Unknown -> (* only shown threads *)
	    (PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime < $datetime \
               AND (NOT hidden) \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM threads WHERE frm_id = $frm_id \
               AND datetime > $datetime \
               AND (NOT hidden) \
               ORDER BY datetime ASC \
               LIMIT 1")) in
               commit db;
             ((match prev with [x] -> Some x | _ -> None),
              (match next with [x] -> Some x | _ -> None)))
              ()

let message_get_neighbours ~frm_id ~msg_id ~role =
  (* returns None|Some id of prev & next message in the same
     thread. *)
  detach
    (fun () ->
      begin_work db;
      let (thr_id, datetime) = 
        (match
          PGSQL(db) "SELECT messages.thr_id, messages.datetime \
            FROM messages, threads \
            WHERE messages.id = $msg_id \
            AND messages.thr_id = threads.id \
            AND threads.frm_id = $frm_id"
        with [x] -> x | _ -> raise Not_found) in
      let (prev, next) = 
        (match role with
        | Moderator -> (* all kinds of messages *)
	    (PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime < $datetime \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime > $datetime \
               ORDER BY datetime ASC \
               LIMIT 1")
        | Author a -> (* only shown messages, or hidden ones posted by her *)
	    (PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime < $datetime \
               AND (author = $a OR NOT hidden) \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime > $datetime \
               AND (author = $a OR NOT hidden) \
               ORDER BY datetime ASC \
               LIMIT 1")
        | Unknown -> (* only shown messages *)
	    (PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime < $datetime \
               AND (NOT hidden) \
               ORDER BY datetime DESC \
               LIMIT 1",
	       PGSQL(db) "SELECT id FROM messages WHERE thr_id = $thr_id \
               AND datetime > $datetime \
               AND (NOT hidden) \
               ORDER BY datetime ASC \
               LIMIT 1")) in
               commit db;
             ((match prev with [x] -> Some x | _ -> None),
              (match next with [x] -> Some x | _ -> None)))
              ();;


let forum_get_threads_list ~frm_id ~offset ~limit ~role =
  (* returns the threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
	let db_offset = db_size_of_int offset
	and db_limit = db_size_of_int limit in
  detach
    (fun () ->
      let thr_l = 
        (match role with
        | Moderator ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              ORDER BY datetime DESC \
              LIMIT $db_limit OFFSET $db_offset"
        | Author a ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              AND (author = $a OR NOT hidden) \
              ORDER BY datetime DESC \
              LIMIT $db_limit OFFSET $db_offset"
        | Unknown ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              AND NOT hidden \
              ORDER BY datetime DESC \
              LIMIT $db_limit OFFSET $db_offset") in
              thr_l)
          ();;

let rec forest_of (get_size: 'a -> 'b) (z: 'a list): 'a tree list =
begin
	let rec forest_of_aux n l =
	(* let n_id = get_id n in *)
	begin
		match l with
		| [] -> ([], [])
		| (x::xs) ->
			(* let x_pid = get_parent_id x in
			if x_pid = (Some n_id) then *)
			if get_size x > 1 then (* not a leaf node *)
			let x_children, x_rest = forest_of_aux x xs in
			let n_children, n_rest = forest_of_aux n x_rest in
				((Node (x, x_children))::n_children, n_rest) 
			else
				([Node (x, [])], xs)
	end in	
	match z with
	| [] -> []
	| m::ms -> let (m_children, m_rest) = forest_of_aux m ms in
		begin
			if m_rest = [] then [Node (m, m_children)]
			else (Node (m, m_children))::(forest_of get_size m_rest)
		end
end;;


let rec cut id =
function
|	[]  -> []
|	h::t -> let (h_id,_,_,_,_,_,_) = h in
	if h_id = id then [h] else h::(cut id t);;


let thread_get_messages_with_text ~frm_id ~thr_id ~offset ~limit ~role ?bottom () =
let db_offset = db_size_of_int offset
and db_limit = db_size_of_int limit in
detach
(fun () ->
	begin_work db;
	let msg_l = (match role with 
	| Moderator ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			CAST(NULL AS INTEGER) \
		FROM messages, textdata \
		WHERE txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Author a ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			CAST(NULL AS INTEGER) \
		FROM messages, textdata \
		WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
			thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset" 
	| Unknown ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
			CAST(NULL AS INTEGER) \
		FROM messages, textdata \
		WHERE NOT hidden AND txt_id = textdata.id AND thr_id = $thr_id \
		ORDER BY sticky DESC, datetime \
		LIMIT $db_limit OFFSET $db_offset") in
    commit db;
		let final_msg_l = match bottom with
		| None -> msg_l
		| Some btm -> cut btm msg_l in
			List final_msg_l
	) ();;


let thread_get_messages_with_text_forest ~frm_id ~thr_id ~offset ~limit ?top ?bottom ~role () =
	let db_offset = db_size_of_int offset
	and db_limit = db_size_of_int limit in
  detach
    (fun () ->
      begin_work db;
			let (db_min, db_max) = match top with
			| None -> (match PGSQL(db) "SELECT MIN(tree_min), MAX(tree_max) \
					FROM messages WHERE thr_id = $thr_id"
				with [(x,y)] -> (match x, y with Some x',Some y' -> (x',y')| _->(Int32.zero,Int32.zero)) | _ -> (Int32.zero,Int32.zero))
			| Some t -> (match PGSQL(db) "SELECT tree_min, tree_max \
					FROM messages WHERE thr_id = $thr_id AND id = $t"
				with [x] -> x | _ -> raise Not_found) in
      let msg_l = 
			(match role with 
			| Moderator ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
		tree_max-tree_min \
		FROM messages, textdata \
		WHERE txt_id = textdata.id AND (tree_min BETWEEN $db_min AND $db_max) \
		AND thr_id = $thr_id \
		ORDER BY sticky DESC, tree_min \
		LIMIT $db_limit OFFSET $db_offset" 
			| Author a ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
		tree_max-tree_min \
		FROM messages, textdata \
		WHERE (author = $a OR NOT hidden) AND txt_id = textdata.id AND \
			(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
		ORDER BY sticky DESC, tree_min \
		LIMIT $db_limit OFFSET $db_offset" 
			| Unknown ->
		PGSQL(db) "SELECT messages.id,txt,author,datetime,hidden,sticky, \
		tree_max-tree_min \
		FROM messages, textdata \
		WHERE NOT hidden AND txt_id = textdata.id AND \
			(tree_min BETWEEN $db_min AND $db_max) AND thr_id = $thr_id \
		ORDER BY sticky DESC, tree_min \
		LIMIT $db_limit OFFSET $db_offset") in
              commit db;
							let final_msg_l = match bottom with
							| None -> msg_l
							| Some btm -> cut btm msg_l
							in
           Forest (forest_of
					 	(fun (_,_,_,_,_,_,x)->match x with None->1|Some i -> int_of_db_int i)
						final_msg_l))
          ();;

let new_wiki ~title ~descr =
  (* inserts a new wiki *)
  detach
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
  detach
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
  detach
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
  detach
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
  detach
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
  detach
    (fun () ->
      let wpg_data =
        (match 
          PGSQL(db) "SELECT subject, txt, author, datetime \
            FROM wikipages, textdata \
            WHERE wik_id = $wik_id AND suffix = $suffix \
            AND txt_id = textdata.id"
        with [x] -> Some x | _ -> None) in
      wpg_data)
    ()
