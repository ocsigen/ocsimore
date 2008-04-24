

type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.user option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

class wikibox :
  parent:Session_manager.sessionmanager ->
  [Wiki_sql.wiki * int32, wiki_data option] Widget.parametrized_div_widget_t

class editable_wikibox :
  parent:Session_manager.sessionmanager ->
  [Wiki_sql.wiki * int32, wiki_data option * Wiki_sql.role] 
    Widget.parametrized_div_widget_t

