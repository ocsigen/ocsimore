(* open XHTML.M *)
open Eliommod
open Eliomparameters
open Eliomservices
open Eliomsessions
open Eliomduce.Xhtml
open Lwt
open Ocsimorelib

exception Unauthorized

type forum_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     moderated: bool;
     readable_by: Users.user;
     writable_by: Users.user;
     moderators: Users.user;
     url: string list;
     max_rows: int option;
		 arborescent: bool;
   }

class forum ~(db: Sql.db_t) ~(foruminfo: forum_in) =

(* creates the new forum once and gets its id *)
let forum_id = Sql.int_of_db_int (Lwt_unix.run
	(Sql.Persist.lwtcreate db foruminfo.identifier
	(fun () -> Sql.new_forum db ~title:foruminfo.title ~descr:foruminfo.descr ~moderated:foruminfo.moderated) >>=
	fun a -> return (Sql.Persist.get a))) in

object (self)

	method db = db

	method get_forum_id = forum_id

	method can_read =
	function
	| Data user -> Users.in_group ~user ~group:foruminfo.readable_by
	| _ -> foruminfo.readable_by = Users.anonymous ()

	method can_write =
	function
	| Data user -> Users.in_group ~user ~group:foruminfo.writable_by
	| _ -> foruminfo.writable_by = Users.anonymous ()

	method can_moderate = function
	| Data user -> Users.in_group ~user ~group:foruminfo.moderators
	| _ -> false (* there are no anonymous moderators *)
end
