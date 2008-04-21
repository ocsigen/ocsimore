val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.user option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

class wikibox_widget :
  parent:Session_manager.sessionmanager ->
  object
    method apply :
      sp:Eliom_sessions.server_params ->
      Wiki_sql.wiki * int32 -> Xhtmltypes_duce._div Lwt.t
    method name : string
    method private retrieve_data : Wiki_sql.wiki * int32 -> unit Lwt.t
  end
