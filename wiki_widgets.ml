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


let (>>=) = Lwt.bind

type wiki_data = {
  wiki_id: Wiki_sql.wiki;
  comment: string;
  author: Users.userdata option;
  content: string;
  datetime: CalendarLib.Calendar.t;
}

let retrieve_wikibox_data (wiki_id, wikibox_id) =
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


class wikibox ~(parent: Session_manager.sessionmanager) =
object (self)
  inherit [(Wiki_sql.wiki * int32), wiki_data option] 
    Widget.parametrized_div_widget parent

  val xhtml_class = "wikibox"
    
  method private retrieve_data ~sd a =
    retrieve_wikibox_data a

  method apply ~sp ~sd ~data:(wiki_id, message_id) =
    self#retrieve_data sd (wiki_id, message_id) >>= fun data -> 
    let content = match data with
      | Some data -> data.content 
      | None -> ""
    in
    Lwt.return
      {{ <div class={: xhtml_class :}>
           {: content :}
          }}

end;;


              let rec list_find_some f = function
                | [] -> None
                | a::l -> 
                    match f a with
                      | None -> list_find_some f l
                      | v -> v
                
  


class editable_wikibox ~(parent: Session_manager.sessionmanager) () =
object (self)

  inherit
    [Wiki_sql.wiki * int32, 
     wiki_data option * Wiki_sql.role,
     ?rows:int -> ?cols:int -> unit -> Xhtmltypes_duce._div Lwt.t] 
      Widget.parametrized_widget parent

  val ne_xhtml_class = "wikibox"

  val edit_xhtml_class = "wikibox editform"

  val xhtml_class = "wikibox editable"

  method private retrieve_data ~sd ((wiki_id, message_id) as a) =
    Wiki.get_role sd wiki_id message_id >>= fun role -> 
    retrieve_wikibox_data a >>= fun d ->
    Lwt.return (d, role)

  method apply ~sp ~sd 
    ~data:((wiki_id, message_id) as d) ?(rows=40) ?(cols=30) () =
    self#retrieve_data sd d >>= fun (data, role) -> 
    let content = match data with
      | Some data -> data.content 
      | None -> ""
    in
    let draw_form r
        (((wikiidname, boxidname), contentname),
         (addrn, (addwn, (addan, (delrn, (delwn, delan)))))) =
      let f =
        {{ <p>[
             {: Eliom_duce.Xhtml.int32_input
                ~input_type:{: "hidden" :} 
                ~name:wikiidname
                ~value:wiki_id () :}
               {: Eliom_duce.Xhtml.int32_input
                  ~input_type:{: "hidden" :} 
                  ~name:boxidname
                  ~value:message_id () :}
               {: Eliom_duce.Xhtml.textarea
                  ~name:contentname
                  ~rows
                  ~cols
                  ~value:{{ {: content :} }} () :}
             <br>[]
               {: Eliom_duce.Xhtml.string_input
                  ~input_type:{: "submit" :} 
                  ~value:"Submit" () :}
           ]
         }}
      in
      match r with
        | Some (r, w, a) ->
              {{ [f 
                  <p>[ 
                    'Users who can read this wiki box: ' 
                    !{: r :}
                    <br>[]
                      'Add readers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:addrn
                         () :}
                    <br>[]
                      'Remove readers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:delrn
                         () :}
                      <br>[]
                      'Users who can modify this wiki box: ' 
                      !{: w :}
                    <br>[]
                      'Add writers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:addwn
                         () :}
                    <br>[]
                      'Remove writers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:delwn
                         () :}
                    <br>[]
                      'Users who can administrate this wiki box: ' 
                      !{: a :}
                    <br>[]
                      'Add readers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:addan
                         () :}
                    <br>[]
                      'Remove readers: '
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "text" :}
                         ~name:delan
                         () :}
                    <br>[]
                  ]
                 ]
               }}
        | _ -> {{ [f] }}
    in
    match role with
      | Wiki_sql.Admin _
      | Wiki_sql.Author _ ->
          if Wiki.edit_in_progress ~sp d
          then
            (match role with
              | Wiki_sql.Admin _ ->
                  Wiki_sql.get_readers wiki_id message_id >>= fun readers ->
                  Wiki_sql.get_writers wiki_id message_id >>= fun writers ->
                  Wiki_sql.get_admins wiki_id message_id >>= fun admins ->
                  Lwt.return
                    (Some
                       ((List.fold_left 
                           (fun s r -> s^" "^Users.get_group_name r)
                           ""
                           readers),
                        (List.fold_left 
                           (fun s r -> s^" "^Users.get_group_name r)
                           ""
                           writers),
                        (List.fold_left 
                           (fun s r -> s^" "^Users.get_group_name r)
                           ""
                           admins)))
              | _ -> Lwt.return None) >>= fun res ->
            Lwt.return
              {{ <div class={: xhtml_class :}>
                   [ {:
                        Eliom_duce.Xhtml.post_form
                        parent#action_send_wikibox
                        sp (draw_form res) ()
                        :}
                   ]
               }}
          else
            let exns = Eliom_sessions.get_exn sp in
            let c = 
              {{ [ {: Eliom_duce.Xhtml.a 
                        parent#action_edit_wikibox
                        sp {{ "edit" }} d :}
                      <br>[]
                      !{: content :} ] }}
            in
            Lwt.return
              {{ <div class={: edit_xhtml_class :}>
                   {{
                      if
                        List.exists
                          (fun e -> 
                             match e with 
                               | Wiki.Operation_not_allowed (wiki, box) ->
                                   wiki = wiki_id && box = message_id
                               | _ -> false)
                          exns
                      then
                        {{ [<p>[ 'Operation not allowed' ] !c] }}
                      else
                        match
                          list_find_some
                            (fun e -> 
                               match e with 
                                 | Wiki.Action_failed (wiki, box, e)
                                     when wiki = wiki_id && box = message_id
                                       -> Some e
                                 | _ -> None)
                            exns
                        with
                          | Some e ->
                              {{ [ <p> [ 'Edit failed: ' 
                                         !{: (Printexc.to_string e) :} ] ] }}
                          | None -> c
                    }}
               }}
      | _ ->
          Lwt.return
            {{ <div class={: ne_xhtml_class :}>
                 {: content :}
             }}

end
