type wiki_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     readable_by: Users.user;
     writable_by: Users.user;
     url: string list;
   }

class type wiki = object
  method srv_main :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
  method login_actions : Eliom.server_params -> Users.user option -> unit
  method logout_actions : Eliom.server_params -> unit
end


class makewiki :
  wiki_in ->
  exit_link:(Eliom.server_params -> 
    [< Xhtmltypes.div_content > `A ] XHTML.M.elt) ->
  mk_log_form:(Eliom.server_params ->
               Users.user option -> 
                 [< Xhtmltypes.body_content > `Form `Div `H1 `P ] 
                   XHTML.M.elt) ->
  wiki

