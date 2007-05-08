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
end


class makewiki :
    wikiinfo:wiki_in ->
      sessionmanager: SessionManager.sessionmanager ->
        container: 
          (Eliom.server_params -> Users.user option -> title:string -> 
            XHTML.M.block XHTML.M.elt list -> XHTML.M.html Lwt.t) ->
              wiki

