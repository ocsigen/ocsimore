(* open XHTML.M *)
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Lwt
open Users

exception Unauthorized

type forum = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  readable_by: Users.user;
  writable_by: Users.user;
  moderated_by: Users.user
}

let get_forum_by_id db id =
  Forum_sql.find_forum db ~id ()
  >>= fun (id, title, descr, r, w, m) -> 
  get_user_by_name db ~name:r >>= fun read -> 
  get_user_by_name db ~name:w >>= fun write -> 
  get_user_by_name db ~name:m >>= fun moderate ->
  return { id = id; 
           title = title; 
           descr = descr;
           readable_by = read;
	   writable_by = write; 
           moderated_by = moderate
	 }

let get_forum_by_name db title =
  Forum_sql.find_forum db ~title () >>= fun (id, title, descr, r, w, m) -> 
  get_user_by_name db ~name:r >>= fun read -> 
  get_user_by_name db ~name:w >>= fun write -> 
  get_user_by_name db ~name:m >>= fun moderate ->
  return { id = id; 
           title = title; 
           descr = descr;
           readable_by = read;
	   writable_by = write; 
           moderated_by = moderate
	 }

let can_read forum user =
  in_group user forum.readable_by
    
let can_write forum user =
  in_group user forum.writable_by
    
let can_moderate forum user =
  in_group user forum.moderated_by

let create_forum
    db
    ~title
    ~descr
    ~moderated
    ~arborescent
    ?(reader = Users.anonymous)
    ?(writer = Users.anonymous)
    ?(moderator = Users.anonymous) (*VVV ??? *)
    () =
  catch
    (fun () -> get_forum_by_name db title)
    (function
       | Not_found -> 
           let (r_id, _, _, _, _) = Users.get_user_data reader in
           let (w_id, _, _, _, _) = Users.get_user_data writer in
           let (m_id, _, _, _, _) = Users.get_user_data moderator in
           Forum_sql.new_forum
             db title descr moderated arborescent
             r_id w_id m_id
             >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr;
                        readable_by = reader;
	                writable_by = writer; 
                        moderated_by = moderator
	              }
       | e -> fail e)

let get_role db sm (forum_id : Forum_sql.forum) =
  get_forum_by_id db forum_id >>= fun f -> 
  Lwt.return
    (match sm#get_user with
       | Data u -> 
	   if can_moderate f u
           then Forum_sql.Moderator
	   else if can_write f u && sm#is_logged_on
           then Forum_sql.Author sm#get_user_id
	   else if can_read f u && sm#is_logged_on
           then Forum_sql.Lurker sm#get_user_name
	   else Forum_sql.Unknown
       | _ -> Forum_sql.Unknown)

