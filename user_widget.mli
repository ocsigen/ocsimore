(** A widget for the login/logout box *)
class login_widget: parent:SessionManager.sessionmanager ->
object
	inherit [unit] Widget.parametrized_widget
end;;

