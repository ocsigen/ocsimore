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
  method box_forum :
      Users.user option ->
        Eliom.server_params -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_forum' :
      Users.user option ->
        Eliom.server_params ->
          int32 * int32 -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_newmessage :
      Users.user option ->
        int32 ->
          Eliom.server_params -> string -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_newthread :
      Users.user option ->
        Eliom.server_params ->
          string * string -> XHTML.M.block XHTML.M.elt list Lwt.t
  method box_thread' :
      Users.user option ->
        Eliom.server_params ->
          int32 * (int32 * int32) -> XHTML.M.block XHTML.M.elt list Lwt.t
  method page_forum :
      Eliom.server_params -> unit -> unit -> Eliom.Xhtml.page Lwt.t
  method page_forum' :
      Eliom.server_params ->
        int32 * int32 -> unit -> Eliom.Xhtml.page Lwt.t
  method page_newmessage :
      int32 ->
        Eliom.server_params -> unit -> string -> Eliom.Xhtml.page Lwt.t
  method page_newthread :
      Eliom.server_params ->
        unit -> string * string -> Eliom.Xhtml.page Lwt.t
  method page_thread :
      Eliom.server_params -> int32 -> unit -> Eliom.Xhtml.page Lwt.t
  method page_thread' :
      Eliom.server_params ->
        int32 * (int32 * int32) -> unit -> Eliom.Xhtml.page Lwt.t

end

val newforum :
  foruminfo:forum_in ->
    sessionmanager: SessionManager.sessionmanager ->
      container: 
        (Eliom.server_params -> Users.user option -> title:string -> 
          XHTML.M.block XHTML.M.elt list -> XHTML.M.html Lwt.t) ->
            forum Lwt.t

