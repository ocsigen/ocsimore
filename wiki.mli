(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

type wiki_data = {
  id : int;
  title : string;
  readable_by : Users.user;
  writable_by : Users.user;
}

val new_wiki : 
  Sql.db_t -> 
  title:string -> 
  ?reader:Users.user -> 
  ?writer:Users.user -> 
  unit -> 
  Sql.db_int_t Lwt.t
