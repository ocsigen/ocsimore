(** EXAMPLE *)

open Lwt
open Eliom_duce.Xhtml

let srv_main = 
  Eliom_services.new_service ~path:[""] ~get_params:Eliom_parameters.unit ()

class example_sessionmanager ~sessionmanagerinfo =
object (self)

  inherit Session_manager.sessionmanager ~sessionmanagerinfo
    
  method container ~sp ~sd ~contents =
    return {{
              <html>[
                <head>[
                  <title>"EXAMPLE - Preuves, Programmes et Systèmes"
                    {: css_link 
                       (make_uri
                          (Eliom_services.static_dir sp) 
                          sp ["example.css"]) () :}
                ]
                <body>[
                  !{: contents :}
                ]
              ]
            }}

end;;

let _ =
  Lwt_unix.run
    ((* creating a wiki: *)
     Wiki.create_wiki 
       ~title:"EXAMPLE site wiki" ~descr:""
       ~reader:Users.anonymous_group
       ~writer:Users.admin_group
       ~admin:Users.admin_group
       () >>= fun wiki ->

     (* Filling the first wikibox if it does not exist: *)
     (Wiki_sql.get_wikibox_data wiki.Wiki.id 1l 
     >>= function
       | Some _ -> Lwt.return ()
       | _ -> (Wiki.new_wikibox 
                wiki
                "admin"
                "First wikibox" 
                "Welcome!"
                () >>= fun _ ->
              Lwt.return ()))
     >>= fun () ->

     let example_sminfo = {
       Session_manager.url = ["users"];
       default_groups = [];
       administrator = Users.admin;
       login_actions = (fun sp sess -> return ());
       logout_actions = (fun sp -> return ());
       registration_mail_from = ("EXAMPLE", 
                                 "webmaster@example.jussieu.fr");
       registration_mail_subject = ("EXAMPLE")
     }
     in
     let example_sm = new example_sessionmanager example_sminfo in

     (* widgets creation: *)
     let myloginbox = new User_widgets.login_widget example_sm in
     let mywikibox = new Wiki_widgets.editable_wikibox () in
     (* all widgets created *)

     register
       srv_main
       (fun sp () () ->
          let sd = Ocsimore_common.create_sd () in
          myloginbox#apply ~sp ~sd ~data:() >>= fun login_box -> 
          mywikibox#editable_wikibox ~sp ~sd ~classe:["mainbox"] 
            ~cols:80 ~rows:30 ~data:(wiki.Wiki.id, 1l) ()
            >>= fun essai_wiki_box -> 
          example_sm#container ~sp ~sd
            ~contents:{{
                         [<div class="main">[
                               {: login_box :}
                               {: essai_wiki_box :}
                           ]
                         ]
                       }}
       );
       (*
       Session_manager.connect
         example_sm
         srv_add_message
         example_sm#container
         (fun (forum_id, (thread_id, start)) (txt, (parent_id, sticky)) ->
            [message_add#apply (forum_id, thread_id, parent_id, txt, sticky);
             thread#apply (forum_id, thread_id);
             message_forest#apply (forum_id, thread_id, None);
             message_navigation#apply (forum_id, thread_id, start, None);
             message_form#apply (forum_id, thread_id, None, start)]);
       *)
       return ()
    )
