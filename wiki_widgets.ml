
let (>>=) = Lwt.bind

type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.user option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

class wikibox_widget ~(parent: Session_manager.sessionmanager) =
object (self)
  inherit [Wiki_sql.wiki * int32] Widget.parametrized_widget parent

  val div_class = "wikibox"

  val mutable data = None

  method private set_data d = 
    data <- Some d
    
  method private retrieve_data (wiki_id, wikibox_id) =
    Wiki_sql.get_wikibox_data ~wiki:wiki_id ~id:wikibox_id >>= fun result ->
    match result with
      | None -> Lwt.return ()
      | Some (com, a, cont, d) ->
         Lwt.catch
           (fun () -> 
              Users.get_user_by_name a >>= fun user ->
              Lwt.return (Some user))
           (function
              | Users.NoSuchUser -> Lwt.return None
              | e -> Lwt.fail e) >>= fun user ->
	 self#set_data
           { wiki_id = wiki_id;
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
