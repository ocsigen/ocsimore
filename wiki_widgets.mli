val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
type wiki_data = {
  id : int;
  comment : string;
  author : Users.user option;
  content : string;
  datetime : CalendarLib.Calendar.t;
}
class wikibox_widget :
  parent:Session_manager.sessionmanager ->
  object
    val mutable data : wiki_data option
    val db : Sql.db_t Lwt.t
    val div_class : string
    method apply :
      sp:Eliom_sessions.server_params ->
      int * int -> Xhtmltypes_duce._div Lwt.t
    method name : string
    method private retrieve_data : int * int -> unit Lwt.t
    method private set_data : wiki_data -> unit
  end
