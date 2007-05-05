type forum_in = 
    {
     identifier: string;
     title: string;
     descr: string;
     moderated: bool;
     readable_by: Users.user;
     writable_by: Users.user;
     moderators: Users.user;
     url: string list;
     max_rows: int32;
   }

class type forum = object
  method srv_forum :
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
  method login_actions : Eliom.server_params -> Users.user option -> unit
  method logout_actions : Eliom.server_params -> unit
end

class makeforum :
  forum_in ->
  exit_link:(Eliom.server_params -> 
    [< Xhtmltypes.div_content > `A ] XHTML.M.elt) ->
  mk_log_form:(Eliom.server_params ->
               Users.user option -> 
                 [< Xhtmltypes.body_content > `Form `Div `H1 `P ] 
                   XHTML.M.elt) ->
                     forum

