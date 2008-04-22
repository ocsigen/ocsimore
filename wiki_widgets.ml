(* Ocsimore
 * Copyright (C) 2005 Piero Furiesi Jaap Boender Vincent Balat
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

let (>>=) = Lwt.bind

type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.user option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

class wikibox_widget ~(parent: Session_manager.sessionmanager) =
object (self)
  inherit [(Wiki_sql.wiki * int32), wiki_data option] 
    Widget.parametrized_div_widget parent

  val xhtml_class = "wikibox"
    
  method private retrieve_data (wiki_id, wikibox_id) =
    Wiki_sql.get_wikibox_data ~wiki:wiki_id ~id:wikibox_id >>= fun result ->
    match result with
      | None -> Lwt.return None
      | Some (com, a, cont, d) ->
         Lwt.catch
           (fun () -> 
              Users.get_user_by_name a >>= fun user ->
              Lwt.return (Some user))
           (function
              | Users.NoSuchUser -> Lwt.return None
              | e -> Lwt.fail e) >>= fun user ->
         Lwt.return
           (Some { wiki_id = wiki_id;
                   content = cont; 
                   author = user; 
                   datetime = d; 
                   comment = com })

  method apply ~sp (wiki_id, message_id) =
    self#retrieve_data (wiki_id, message_id) >>= fun data -> 
    let content = match data with
      | Some data -> data.content 
      | None -> ""
    in
    Lwt.return
      {{ <div class={: xhtml_class :}>
           {: content :}
	  }}

end;;
