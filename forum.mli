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
end

class makeforum :
  foruminfo:forum_in ->
    sessionmanager: SessionManager.sessionmanager ->
      container: 
        (Eliom.server_params -> Users.user option -> title:string -> 
          XHTML.M.block XHTML.M.elt list -> XHTML.M.html Lwt.t) ->
            forum

