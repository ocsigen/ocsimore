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
   @author Boris Yakobowski
*)

open User_sql.Types
open Forum_sql.Types

(** {2 Forum related groups} *)
val message_writers : forum_arg parameterized_group
val message_writers_notmod : forum_arg parameterized_group
val message_moderators : forum_arg parameterized_group
val message_deletors : forum_arg parameterized_group
val message_deletors_if_author : forum_arg parameterized_group
val message_sticky_makers : forum_arg parameterized_group
val message_readers  : forum_arg parameterized_group
val comment_writers : forum_arg parameterized_group
val comment_writers_notmod : forum_arg parameterized_group
val comment_moderators : forum_arg parameterized_group
val comment_deletors : forum_arg parameterized_group
val comment_deletors_if_author : forum_arg parameterized_group
val comment_sticky_makers : forum_arg parameterized_group
val comment_readers  : forum_arg parameterized_group
val writers : forum_arg parameterized_group
val writers_notmod : forum_arg parameterized_group
val moderators : forum_arg parameterized_group
val deletors : forum_arg parameterized_group
val deletors_if_author : forum_arg parameterized_group
val sticky_makers : forum_arg parameterized_group
val readers  : forum_arg parameterized_group
val forum_admin : forum_arg parameterized_group
val forum_visible : forum_arg parameterized_group

val forum_creators : user


(** {2 Forum creation} *)

(** Creates a new forum or returns its id without modification
    if it already exists. *)
val create_forum : 
  title:string -> 
  descr:string -> 
  ?arborescent:bool -> 
  unit ->
  Forum_sql.Types.forum_info Lwt.t

(** {2 Session data} *)

type role = 
    {
      message_writers : bool Lwt.t Lazy.t;
      message_writers_notmod : bool Lwt.t Lazy.t;
      message_moderators : bool Lwt.t Lazy.t;
      message_deletors : bool Lwt.t Lazy.t;
      message_deletors_if_author : bool Lwt.t Lazy.t;
      message_sticky_makers : bool Lwt.t Lazy.t;
      message_readers : bool Lwt.t Lazy.t;

      comment_writers : bool Lwt.t Lazy.t;
      comment_writers_notmod : bool Lwt.t Lazy.t;
      comment_moderators : bool Lwt.t Lazy.t;
      comment_deletors : bool Lwt.t Lazy.t;
      comment_deletors_if_author : bool Lwt.t Lazy.t;
      comment_sticky_makers : bool Lwt.t Lazy.t;
      comment_readers : bool Lwt.t Lazy.t;

      writers : bool Lwt.t Lazy.t;
      writers_notmod : bool Lwt.t Lazy.t;
      moderators : bool Lwt.t Lazy.t;
      deletors : bool Lwt.t Lazy.t;
      deletors_if_author : bool Lwt.t Lazy.t;
      sticky_makers : bool Lwt.t Lazy.t;
      readers : bool Lwt.t Lazy.t;

      forum_admin : bool Lwt.t Lazy.t;
    }

val get_role : 
  sp:Eliom_sessions.server_params ->
  Forum_sql.Types.forum -> role Lwt.t



(** {2 } *)
type forum_action_info =
  | Preview of ((Forum_sql.Types.forum * 
                   Forum_sql.Types.message option (* parent *)) * string)
  | Msg_creation_not_allowed of (Forum_sql.Types.forum * 
                                   Forum_sql.Types.message option (* parent *))

exception Forum_action_info of forum_action_info


(** {2 Eliom related values} *)

(** Eliom parameter type for forums *)
val eliom_forum :
  string -> (forum, [`WithoutSuffix], [`One of forum] Eliom_parameters.param_name) Eliom_parameters.params_type

(** Eliom parameter type for messages *)
val eliom_message :
  string -> (message, [`WithoutSuffix], [`One of message] Eliom_parameters.param_name) Eliom_parameters.params_type

(** Eliom input field for messages *)
val eliom_message_input : 
  ?a:Eliom_duce.Xhtml.input_attrib_t ->
  input_type: Eliom_duce.Xhtml.input_type_t ->
  ?name:[< Forum_sql.Types.message Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
  ?value:Forum_sql.Types.message -> unit -> Eliom_duce.Xhtml.input_elt

(** Eliom button for messages *)
val eliom_message_button :
  ?a:Eliom_duce.Xhtml.button_attrib_t ->
  name:[< Forum_sql.Types.message Eliom_parameters.setone ] Eliom_parameters.param_name ->
  value:Forum_sql.Types.message ->
  {{ [ Xhtmltypes_duce.button_content* ] }} ->
  Xhtmltypes_duce.button
