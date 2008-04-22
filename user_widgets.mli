(** A widget for the login/logout box *)
class login_widget: parent:Session_manager.sessionmanager ->
object
  inherit [unit, unit, Xhtmltypes_duce._div] Widget.parametrized_widget

  method private retrieve_data : 'param_type -> 'data_type Lwt.t
      
  method apply : 
    sp:Eliom_sessions.server_params -> 
    'param_type -> Xhtmltypes_duce._div Lwt.t

end;;

