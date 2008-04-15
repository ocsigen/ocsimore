(** A widget for the login/logout box *)
class login_widget: parent:Session_manager.sessionmanager ->
object
	inherit [unit] Widget.parametrized_widget
end;;

