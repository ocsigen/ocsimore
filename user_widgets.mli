(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Piero Furiesi
   @author Jaap Boender
   @author Vincent Balat
*)

(** A widget for the login/logout box *)
class login_widget: sessman:Session_manager.sessionmanager ->
object

  method display_login_widget :
    ?user_prompt:Ocamlduce.Utf8.repr ->
    ?pwd_prompt:Ocamlduce.Utf8.repr ->
    ?auth_error:Ocamlduce.Utf8.repr ->
    sp:Eliom_sessions.server_params -> 
    sd:Ocsimore_common.session_data -> 
    unit ->
    Xhtmltypes_duce._div Lwt.t

end;;

class login_widget_basic_user_creation 
  : sessman:Session_manager.sessionmanager ->
  (string * string) * string * User_sql.userid list ->
object
  inherit login_widget

  method srv_register: 
    (unit, 
     unit, 
     Eliom_services.get_service_kind, 
     [`WithoutSuffix],
     unit, 
     unit, 
     [`Registrable ]) Eliom_services.service

  method srv_reminder: 
    (unit, 
     unit, 
     Eliom_services.get_service_kind,
     [`WithoutSuffix], 
     unit, 
     unit,
     [`Registrable ]) Eliom_services.service
    
  method srv_edit: 
    (unit, 
     unit, 
     Eliom_services.get_service_kind, 
     [`WithoutSuffix], 
     unit, 
     unit,
     [`Registrable ]) Eliom_services.service
    
  method container: 
    sp:Eliom_sessions.server_params -> 
    sd:Ocsimore_common.session_data ->
    contents:Xhtmltypes_duce.blocks -> Xhtmltypes_duce.html Lwt.t

end
