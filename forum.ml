(* open XHTML.M *)
open Eliommod
open Eliom_parameters
open Eliom_services
open Eliom_sessions
open Eliom_duce.Xhtml
open Lwt
open Users

exception Unauthorized

type forum_info = {
  id: Forum_sql.forum;
  title: string;
  descr: string;
  moderated: bool;
  arborescent: bool;
  readable_by: Users.group;
  writable_by: Users.group;
  moderated_by: Users.group;
}

let get_forum_by_id id =
  Forum_sql.find_forum ~id ()
  >>= fun (id, title, descr, mo, a, r, w, m) -> 
  return { id = id; 
           title = title; 
           descr = descr;
           moderated = mo;
           arborescent = a;
           readable_by = r;
           writable_by = w; 
           moderated_by = m;
         }

let get_forum_by_name title =
  Forum_sql.find_forum ~title () >>= fun (id, title, descr, mo, a, r, w, m) -> 
  return { id = id; 
           title = title; 
           descr = descr;
           moderated = mo;
           arborescent = a;
           readable_by = r;
           writable_by = w; 
           moderated_by = m;
         }


let create_forum
    ~title
    ~descr
    ~moderated
    ~arborescent
    ?(reader = Users.anonymous_group)
    ?(writer = Users.anonymous_group)
    ?(moderator = Users.anonymous_group) (*VVV anonymous_group??? *)
    () =
  catch
    (fun () -> get_forum_by_name title)
    (function
       | Not_found -> 
           Forum_sql.new_forum
             title descr moderated arborescent 
             (Users.id_of_group reader) 
             (Users.id_of_group writer)
             (Users.id_of_group moderator)
             >>= fun id -> 
           Lwt.return { id = id; 
                        title = title; 
                        descr = descr;
                        moderated = moderated;
                        arborescent = arborescent;
                        readable_by = reader;
                        writable_by = writer; 
                        moderated_by = moderator
                      }
       | e -> fail e)

let can_read forum user =
  Users.in_group user forum.readable_by
    
let can_write forum user =
  Users.in_group user forum.writable_by
    
let can_moderate forum user =
  Users.in_group user forum.moderated_by

let get_role sm (forum_id : Forum_sql.forum) =
  get_forum_by_id forum_id >>= fun f -> 
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

