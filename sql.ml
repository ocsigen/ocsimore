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
type role = Moderator | Author of string | Unknown

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
        (PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden) \
           VALUES ($author, $thr_id, $txt_id, $hidden)";
           serial4 db "messages_id_seq") in
      commit db;
      (thr_id, msg_id))
    ()
        
let new_message ~frm_id ~thr_id ?parent_id ~author ~txt () = 
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
						(PGSQL(db) "INSERT INTO messages (author, thr_id, txt_id, hidden) \
           VALUES ($author, $thr_id, $txt_id, $hidden)";
           serial4 db "messages_id_seq")
				| Some pid ->
						(PGSQL(db) "INSERT INTO messages (author, thr_id, parent_id, \
						txt_id, hidden) VALUES ($author, $thr_id, $pid, $txt_id, $hidden)";
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
        ()

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
       n_shown_thr, n_hidden_thr,
       n_shown_msg, n_hidden_msg))
    ()
        
let thread_get_data ~frm_id ~thr_id ~role =
  (* returns id, subject, author, datetime, hidden status, number of
     shown/hidden messages of a thread.  NB: a message is counted as
     hidden if: 1) its hidden status is true, or 2) it is in a hidden
     thread. *)
  detach
    (fun () ->
      begin_work db;
      let (id, subject, author, datetime, hidden) =
        (match
          PGSQL(db) "SELECT id, subject, author, datetime, hidden \
            FROM threads WHERE id = $thr_id AND frm_id = $frm_id"
        with [x] -> x | _ -> raise Not_found) in
      let n_shown_msg = 
        (match 
          PGSQL(db) "SELECT COUNT(*) FROM messages \
            WHERE thr_id = $thr_id AND (NOT hidden)"
        with [Some x] -> x | _ -> assert false) in
      let n_hidden_msg =
        (match role with
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
      (id, subject, author, datetime, hidden, n_shown_msg, n_hidden_msg))
    ()

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
              ()

let forum_get_threads_list ~frm_id ~offset ~limit ~role =
  (* returns the threads list of a forum, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
  detach
    (fun () ->
      let thr_l = 
        (match role with
        | Moderator ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Author a ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              AND (author = $a OR NOT hidden) \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Unknown ->
	    PGSQL(db) "SELECT id, subject, author, datetime, hidden \
              FROM threads \
              WHERE frm_id = $frm_id \
              AND NOT hidden \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset") in
              thr_l)
          ()

let thread_get_messages_list ~frm_id ~thr_id ~offset ~limit ~role =
  (* returns the messages list of a thread, ordered cronologycally
     (latest first), with max [~limit] items and skipping first
     [~offset] rows. *)
  detach
    (fun () ->
      begin_work db;
      let _ = 
        (match PGSQL(db) "SELECT frm_id FROM threads \
            WHERE frm_id = $frm_id AND id = $thr_id"
        with [x] -> x | _ -> raise Not_found) in
      let msg_l = 
        (match role with
        | Moderator ->
	    PGSQL(db) "SELECT id, author, datetime, hidden \
              FROM messages \
              WHERE thr_id = $thr_id \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Author a ->
	    PGSQL(db) "SELECT id, author, datetime, hidden \
              FROM messages \
              WHERE thr_id = $thr_id AND (author = $a OR NOT hidden) \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Unknown ->
	    PGSQL(db) "SELECT id, author, datetime, hidden \
              FROM messages \
              WHERE thr_id = $thr_id AND NOT hidden \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset") in
              commit db;
            msg_l)
          ()

let thread_get_messages_with_text_list ~frm_id ~thr_id ~offset ~limit ~role =
  (* as above, but gets message texts too. *)
  detach
    (fun () ->
      begin_work db;
      let _ = 
        (match PGSQL(db) "SELECT frm_id FROM threads \
            WHERE frm_id = $frm_id AND id = $thr_id"
        with [x] -> x | _ -> raise Not_found) in
      let msg_l = 
        (match role with
        | Moderator ->
	    PGSQL(db) "SELECT messages.id, txt, author, datetime, hidden \
              FROM messages, textdata \
              WHERE thr_id = $thr_id AND txt_id = textdata.id \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Author a ->
	    PGSQL(db) "SELECT messages.id, txt, author, datetime, hidden \
              FROM messages, textdata \
              WHERE thr_id = $thr_id AND txt_id = textdata.id \
              AND (author = $a OR NOT hidden) \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset"
        | Unknown ->
	    PGSQL(db) "SELECT messages.id, txt, author, datetime, hidden \
              FROM messages, textdata \
              WHERE thr_id = $thr_id AND txt_id = textdata.id \
              AND NOT hidden \
              ORDER BY datetime DESC \
              LIMIT $limit OFFSET $offset") in
              msg_l)
          ()

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
      (title, description, n_pages))
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
