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

let new_wiki db ~title ~descr () =
  Wiki_sql.new_wiki db title descr


