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
	method container:
		Eliom.server_params -> Users.user option -> title:string ->
		{{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
  method srv_main:
      (unit, unit, Eliom.get_service_kind,
       [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
       [ `Registrable ])
      Eliom.service
end


val newwiki: wikiinfo:wiki_in -> sessionmanager: SessionManager.sessionmanager -> wiki Lwt.t

