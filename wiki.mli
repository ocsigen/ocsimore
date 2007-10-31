open Eliommod
open Eliomservices
open Eliomsessions

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
		server_params -> Users.user session_data -> title:string ->
		{{ Xhtml1_strict.blocks }} -> {{ Xhtml1_strict.html }} Lwt.t
  method srv_main:
      (unit, unit, get_service_kind,
       [ `WithoutSuffix ], unit, unit, [ `Registrable ])
      service
end


val newwiki: wikiinfo:wiki_in -> sessionmanager: SessionManager.sessionmanager -> wiki Lwt.t

