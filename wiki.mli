(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

type wiki_data = {
  id : int;
  title : string;
}

val new_wiki : 
  Sql.db_t -> 
  title:string -> 
  descr:string -> 
  unit -> 
  Sql.db_int_t Lwt.t
