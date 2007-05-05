val user_table : Users.user Eliom.persistent_table

type sessionmanager_in = 
    { url: string list;
      default_groups: Users.user list;
      login_actions: Eliom.server_params -> Users.user option -> unit;
      logout_actions: Eliom.server_params -> unit;
      registration_mail_from: string * string;
      registration_mail_subject: string;
  }

class type sessionmanager = object
  method mk_log_form: Eliom.server_params -> Users.user option -> 
    XHTML.M.block XHTML.M.elt
end

class makesessionmanager :
    sessionmanager_in ->
    exit_link:(Eliom.server_params -> Xhtmltypes.p_content XHTML.M.elt) ->
      sessionmanager

