(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Universit� Paris Diderot - CNRS
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
   @author Vincent Balat
   @author Boris Yakobowski
*)

let ( ** ) = Eliom_parameters.prod
let ( >>= ) = Lwt.bind

let add_message_service =
  Eliom_services.new_post_coservice'
      ~keep_get_na_params:false
      ~name:"forum_add"
      ~post_params:
      (Eliom_parameters.string "actionname" **
         ((Eliom_parameters.sum
             (Eliom_parameters.int32 "parent")
             (Eliom_parameters.int32 "forum")) **
            (Eliom_parameters.opt (Eliom_parameters.string "subject") **
               Eliom_parameters.string "content")))
    ()


let _ = Eliom_predefmod.Any.register add_message_service
  (fun sp () (actionname, (parent, (subject, text))) ->

     let sd = Ocsimore_common.get_sd sp in
     (match parent with
        | Eliom_parameters.Inj2 forum_id -> (* new messages *)
            Lwt.return (forum_id, None)
        | Eliom_parameters.Inj1 parent_id -> (* comment *)
            (* We do not require the user to be allowed to read the message ... 
               (Forum_sql.get_message and not Forum_data.get_message) *)
            Forum_sql.get_message ~message_id:parent_id
            >>= fun (_, _, _, _, _, _, forum_id, _, _, _, _) ->
            Lwt.return (forum_id, Some parent_id))
       >>= fun (forum_id, parent_id) ->

       Forum.get_role sp sd forum_id >>= fun role ->

       if actionname = "save" 
       then
         (Lwt.catch
            (fun () ->
               Users.get_user_data sp sd >>= fun u ->
               Forum_data.new_message ~sp ~sd ~forum_id 
                 ~author_id:u.Users.id ?subject ?parent_id ~text ()
               >>= fun _ ->
               Eliom_predefmod.Redirection.send ~sp 
                 Eliom_services.void_hidden_coservice'
            )
            (function
               | Ocsimore_common.Permission_denied ->
                   Eliom_predefmod.Action.send ~sp 
                     [Ocsimore_common.Session_data sd;
                      Forum.Forum_action_info 
                        (Forum.Msg_creation_not_allowed (forum_id, parent_id))
                     ]
               | e -> Lwt.fail e)
         )
       else (* preview *)
         Eliom_predefmod.Action.send ~sp
           [Ocsimore_common.Session_data sd;
            Forum.Forum_action_info
              (Forum.Preview ((forum_id, parent_id), text))]
  )