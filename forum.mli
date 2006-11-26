module type IN = sig
  val identifier : string
  val title : string
  val descr : string
  val moderated : bool
  val readable_by : Users.user
  val writable_by : Users.user
  val moderators : Users.user
  val url : string list
  val exit_link : Ocsigen.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
  val mk_log_form :
    Ocsigen.server_params ->
    Users.auth option -> [> Xhtmltypes.form ] XHTML.M.elt
  val max_rows : int32
end

module type OUT = sig
  val srv_forum: (unit, unit, [ `Internal_Service of [ `Public_Service ] ],
		  [ `WithoutSuffix ], unit Ocsigen.param_name,
		  unit Ocsigen.param_name) Ocsigen.service
  val login_actions : Ocsigen.server_params -> Users.auth option -> unit
  val logout_actions : 'a -> unit
end

module Make :  functor (A : IN) -> OUT
