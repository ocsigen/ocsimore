(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

let new_wiki db ~title ~descr ~moderated ~arborescent ?reader ?writer
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

