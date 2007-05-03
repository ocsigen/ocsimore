module type IN = sig
  val identifier : string
  val title : string
  val descr : string
  val readable_by : Users.user
  val writable_by : Users.user
  val url : string list
  val exit_link : Eliom.server_params -> [> Xhtmltypes.a ] XHTML.M.elt
  val mk_log_form :
    Eliom.server_params ->
    Users.auth option -> [> Xhtmltypes.form ] XHTML.M.elt
end

module type OUT = sig
  val srv_main :
    (unit, unit,
     [> `Attached of [> `Internal of [> `Service ] * [> `Get ] ] Eliom.a_s ],
     [ `WithoutSuffix ], unit Eliom.param_name, unit Eliom.param_name,
     [> `Registrable ])
    Eliom.service
  val login_actions : Eliom.server_params -> Users.auth option -> unit
  val logout_actions : Eliom.server_params -> unit
end

module Make : functor (A : IN) -> OUT
