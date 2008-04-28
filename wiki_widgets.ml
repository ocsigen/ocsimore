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
let ( ** ) = Eliom_parameters.prod

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


class wikibox =
object (self)
  inherit [(Wiki_sql.wiki * int32), wiki_data option] 
    Widget.parametrized_div_widget

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
                
  


class editable_wikibox () =
  (* The registration must be done during site loading, nor before! *)
  
  let action_edit_wikibox =
    Eliom_predefmod.Actions.register_new_service' 
      ~name:"wiki_edit"
      ~get_params:((Eliom_parameters.int32 "wikiid") ** 
                     (Eliom_parameters.int32 "boxid"))
      (fun sp g () -> Lwt.return [Wiki.Editbox g])
  in
    
  let action_cancel =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~name:"cancel"
      ~post_params:Eliom_parameters.unit
      (fun sp () () -> Lwt.return [])
  in

  let action_send_wikibox =
    Eliom_predefmod.Actions.register_new_post_service' 
      ~name:"wiki_send"
      ~post_params:
      ((((Eliom_parameters.int32 "wikiid") ** 
           (Eliom_parameters.int32 "boxid")) ** 
          Eliom_parameters.string "content") **
         (Eliom_parameters.opt (Eliom_parameters.string "addreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "addwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "addadmin") **
            Eliom_parameters.opt (Eliom_parameters.string "delreaders") **
            Eliom_parameters.opt (Eliom_parameters.string "delwriters") **
            Eliom_parameters.opt (Eliom_parameters.string "deladmin")
         ))
      (fun sp () p -> 
         Eliom_sessions.get_persistent_session_data Users.user_table sp ()
           >>= fun sd -> 
         Wiki.save_wikibox sp sd p)
  in
(*  fun <other parameters if any> -> *)

object (self)
  
  inherit
    [Wiki_sql.wiki * int32, 
      wiki_data option * Wiki_sql.role,
      ?rows:int -> 
       ?cols:int -> 
       ?classe:string list -> 
       unit -> Xhtmltypes_duce._div Lwt.t] 
       Widget.parametrized_widget

   val ne_xhtml_class = "wikibox"
   val edit_xhtml_class = "wikibox editform"
   val xhtml_class = "wikibox editable"
   val edit_link_class = "editlink"

   method private retrieve_data ~sd ((wiki_id, message_id) as a) =
     Wiki.get_role sd wiki_id message_id >>= fun role -> 
     retrieve_wikibox_data a >>= fun d ->
     Lwt.return (d, role)

   method apply ~sp ~sd 
     ~data:((wiki_id, message_id) as d) ?(rows=40) ?(cols=30) ?(classe=[]) () =
     self#retrieve_data sd d >>= fun (data, role) -> 
     let classe = List.fold_left (fun s e -> s^" "^e) "" classe in
     let content = match data with
       | Some data -> data.content 
       | None -> ""
     in
     let draw_form r
         (((wikiidname, boxidname), contentname),
          (addrn, (addwn, (addan, (delrn, (delwn, delan)))))) =
       let f =
         {{ [
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
            ]
          }}
       in
       match r with
         | Some (r, w, a) ->
               {{ [<p>[!f 
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
                       {: Eliom_duce.Xhtml.string_input
                          ~input_type:{: "submit" :} 
                          ~value:"Submit" () :}
                     ]
                  ]
                }}
         | _ -> {{ [<p>[!f
                      {: Eliom_duce.Xhtml.string_input
                         ~input_type:{: "submit" :} 
                         ~value:"Submit" () :}
                   ]] }}
     in
     let draw_form2 () =
       {{ [<p>[ {: Eliom_duce.Xhtml.string_input
                   ~input_type:{: "submit" :} 
                   ~value:"Cancel" () :} ] ] }}
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
               {{ <div class={: edit_xhtml_class ^ classe :}>
                    [ {:
                         Eliom_duce.Xhtml.post_form
                         action_send_wikibox
                         sp (draw_form res) ()
                         :}
                        {:
                         Eliom_duce.Xhtml.post_form
                         action_cancel
                         sp draw_form2 ()
                         :}
                    ]
                }}
           else
             let exns = Eliom_sessions.get_exn sp in
             let c = 
               {{ [<p class={: edit_link_class :}>
                     [ {: Eliom_duce.Xhtml.a 
                          ~a:{{ { class={: edit_link_class :} } }}
                          ~service:action_edit_wikibox
                          ~sp {{ "edit" }} d :}]
                     !{: content :} ] }}
            in
            Lwt.return
              {{ <div class={: xhtml_class ^ classe :}>
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
            {{ <div class={: ne_xhtml_class ^ classe :}>
                 {: content :}
             }}

end
