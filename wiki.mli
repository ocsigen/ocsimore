(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

type wiki = {
  id : Wiki_sql.wiki;
  title : string;
  descr : string;
  default_reader: Users.user;
  default_writer: Users.user;
  acl_enabled: bool;
}


(** Creates a new wiki or returns its id without modification
    if it already exists. *)
  val create_wiki :
    title:string ->
    descr:string ->
    ?acl_enabled:bool ->
    ?reader:Users.user -> ?writer:Users.user -> unit -> wiki Lwt.t

