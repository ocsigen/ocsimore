(* open XHTML.M *)
open Eliommod
open Eliomparameters
open Eliomservices
open Eliomsessions
open Eliomduce.Xhtml
open Lwt
open Users

exception Unauthorized

type forum_data =
{
	id: int;
	title: string;
	readable_by: Users.user;
	writable_by: Users.user;
	moderated_by: Users.user
}

let get_forum_by_id db id =
	Sql.find_forum db ~id:(Sql.db_int_of_int id) () >>=
	fun (id, title, r, w, m) -> get_user_by_name db ~name:r >>=
	fun read -> get_user_by_name db ~name:w >>=
	fun write -> get_user_by_name db ~name:m >>=
	fun moderate ->  
		return { id = Sql.int_of_db_int id; title = title; readable_by = read;
			writable_by = write; moderated_by = moderate
		}

let get_forum_by_name db title =
	Sql.find_forum db ~title () >>=
	fun (id, title, r, w, m) -> get_user_by_name db ~name:r >>=
	fun read -> get_user_by_name db ~name:w >>=
	fun write -> get_user_by_name db ~name:m >>=
	fun moderate -> 
		return { id = Sql.int_of_db_int id; title = title; readable_by = read;
			writable_by = write; moderated_by = moderate
		}

let can_read forum user =
	in_group user forum.readable_by

let can_write forum user =
	in_group user forum.writable_by

let can_moderate forum user =
	in_group user forum.moderated_by

let new_forum db ~title ~descr ~moderated ~arborescent ?reader ?writer
?moderator () =
begin
	let anon = Users.get_user_data Users.anonymous in
  let (r_id, _, _, _, _) = match reader with
  | None -> anon
	| Some u -> get_user_data u in
  let (w_id, _, _, _, _) = match writer with
  | None -> anon
	| Some u -> get_user_data u in
  let (m_id, _, _, _, _) = match moderator with
  | None -> anon
	| Some u -> get_user_data u in
		Sql.new_forum db title descr moderated arborescent (Sql.db_int_of_int r_id) (Sql.db_int_of_int w_id) (Sql.db_int_of_int m_id)
end;;

