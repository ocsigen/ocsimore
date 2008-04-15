
let (>>=) = Lwt.bind

type wiki_data = {
  id: int;
  comment: string;
  author: Users.user option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

class wikibox_widget ~(parent: Session_manager.sessionmanager) =
object (self)
  inherit [int * int] Widget.parametrized_widget parent

  val div_class = "wikibox"

  val mutable data = None

  val db = Sql.connect ()

  method private set_data d = 
    data <- Some d
    
  method private retrieve_data (wiki_id0, wikibox_id) =
    let wiki_id = Sql.db_int_of_int wiki_id0 in
    let wikibox_id = Sql.db_int_of_int wikibox_id in
    db >>= fun db -> 
    Sql.wikibox_get_data db ~wiki:wiki_id ~id:wikibox_id >>= fun result ->
    match result with
      | None -> Lwt.return ()
      | Some (com, a, cont, d) ->
         Lwt.catch
           (fun () -> 
              Users.get_user_by_name db a >>= fun user ->
              Lwt.return (Some user))
           (function
              | Users.NoSuchUser -> Lwt.return None
              | e -> Lwt.fail e) >>= fun user ->
	 self#set_data
           { id = wiki_id0; 
             content = cont; 
             author = user; 
             datetime = d; 
             comment = com };
         Lwt.return ()

  method apply ~sp (wiki_id, message_id) =
    self#retrieve_data (wiki_id, message_id) >>= fun () -> 
    let content = match data with
      | Some data -> data.content 
      | None -> ""
    in
    Lwt.return
      {{ <div class={: div_class :}>
           {: content :}
	  }}
end;;
