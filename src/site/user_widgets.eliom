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


open Eliom_content
open User_sql.Types
open Ocsimore_lib
open Eliom_lib.Lwt_ops

let str_input ?a ?(value="") ?(visible=true) name =
  Html5.F.string_input ?a ~name ~value
    ~input_type:(if visible then `Text else `Hidden)
    ()
let passwd_input ?a ?(value="") name =
  Html5.F.string_input
    ?a
    ~input_type:`Password
    ~name
    ~value ()
let submit_input ?a value =
  Html5.F.string_input ?a ~input_type:`Submit ~value ()


class type user_widget_class = object
  method login_box_extension : Html5_types.div_content_fun Html5.F.elt list Lwt.t
  method display_roles :
    Eliom_registration.Block5.page Lwt.t
  method display_groups :
    Eliom_registration.Block5.page Lwt.t
  method display_users :
    Eliom_registration.Block5.page Lwt.t

  method display_users_settings :
    Eliom_registration.Block5.page Lwt.t
  method display_users_settings_done :
    unit ->
      (bool * (string * (string * (string * (string * bool))))) ->
      Eliom_registration.Block5.page Lwt.t

  method display_group :
    user -> Eliom_registration.Block5.page Lwt.t

  method display_login_widget :
    ?user_prompt:string ->
    ?pwd_prompt:string ->
    ?auth_error:string ->
    ?switchtohttps:string ->
    ?show_ext:bool ->
    unit ->
    Html5_types.div Html5.F.elt list Lwt.t

  method private display_logout_box :
    ?show_ext:bool ->
    User_sql.Types.userdata ->
    Html5_types.form_content Html5.F.elt list Lwt.t
  method display_logout_button :
    Html5_types.button_content Html5.F.elt list ->
    [> Html5_types.form ] Html5.F.elt Lwt.t
  method logout_uri : Wiki_syntax.href

  method user_link :
    string -> [`A of Html5_types.a_content | `Form] Html5.F.elt

  (** Helper forms to add and remove users from groups. If [show_edit]
      is false, no controls to edit the permissions are shown *)
  (** Form to add users to a group *)
  method form_edit_group:
    group:user ->
    text:Html5_types.flow5 Html5.F.elt list ->
    unit ->
    Html5_types.tbody_content Html5.F.elt Lwt.t

  (** Form to add an user to a group *)
  method form_edit_user:
    user:User_sql.Types.user ->
    text:Html5_types.flow5 Html5.F.elt list ->
    unit ->
    Html5_types.tbody_content Html5.F.elt Lwt.t

  method form_edit_awr: 'a.
    text_prefix:string ->
    grps:'a User_sql.Types.admin_writer_reader ->
    arg:'a Opaque.int32_t ->
    ?defaults:string * string * string ->
    unit ->
    (  Html5_types.tbody_content Html5.F.elt
     * Html5_types.tbody_content Html5.F.elt list) Lwt.t

  method status_text:
    Html5_types.form_content Html5.F.elt list Lwt.t

  method display_group_creation :
    ?err:string -> unit ->
    Eliom_registration.Block5.page Lwt.t

  method display_group_creation_done :
    unit ->
    string * string ->
    Eliom_registration.Block5.page Lwt.t

end

class type user_widget_user_creation_class = object
  method display_user_creation :
    ?err:string ->
    unit ->
    Eliom_registration.Block5.page Lwt.t
  method display_user_creation_done :
    name:string ->
    fullname:string ->
    email:string ->
    pwd:string*string ->
    Eliom_registration.Block5.page Lwt.t

  method login_box_extension : Html5_types.div_content_fun Html5.F.elt list Lwt.t
end



(** Widget for user login/logout/edition without addition of new users *)
class user_widget : user_widget_class =
object (self)

  val xhtml_class = "logbox"

  method private users_to_html ~users_in ~group users =
    let open Html5.F in
    List.map
      (fun user ->
        tr [
          td [self#user_link user.user_login];
          td (
            let in_group =
              List.mem user.user_login users_in
            in
            self#bt_remove_user_from_group
              ~group
              ~user:user.user_login
              ~remove:in_group
              ()
          );
        ]
      ) (Lazy.force users)

  method private users_title title' =
    let open Html5.F in
    tr ~a:[a_class ["user_menu_title"]] [
      td [pcdata title'];
      td [];
    ]

  method form_edit_group ~group ~text () =
    User_sql.all_users ()
    >>= fun users ->
    User_sql.users_in_group ~generic:false ~group
    >>= (Lwt_list.map_p User_sql.user_to_string)
    >>= fun users_in ->
    User_sql.user_to_string group
    >>= fun group ->
    self#users_edit_table ~text [
      Html5.F.table
        (self#users_title "Users")
        (self#users_to_html ~users_in ~group users.users
         @ [self#users_title "Groups"]
         @ self#users_to_html ~users_in ~group users.groups
        )
    ]

  method form_edit_user ~user ~text () =
    User_sql.all_users ()
    >>= fun users ->
    User_sql.groups_of_user ~user
    >>= (Lwt_list.map_p User_sql.user_to_string)
    >>= fun users_in ->
    User_sql.user_to_string user
    >>= fun user ->
    self#get_roles_table
      ~td_content:(fun group ->
        let in_group =
          List.mem group users_in
        in
        self#bt_remove_user_from_group
          ~group
          ~user
          ~remove:in_group
          ()
      ) ()
    >>= fun roles ->
    let roles =
      Hashtbl.fold (fun a b acc ->
        Html5.F.tr ~a:[Html5.F.a_class ["roles_title"]] [
          Html5.F.td [
            Html5.F.strong [Html5.F.pcdata a]
          ];
          Html5.F.td [];
        ] :: b @ acc
      ) roles []
    in
    (* YYY put back edition buttons if the user has enough rights, or if
       it is admin *)
    self#users_edit_table ~text [
      Html5.F.table
        (self#users_title "Groups")
        (self#users_to_html ~users_in ~group:user users.groups
         @ [self#users_title "Roles"]
         @ roles
        )
    ]

  method private users_edit_table ~text content =
    Lwt.return (
      Html5.F.tr [
        Html5.F.td ~a:[Html5.F.a_class ["role"]] text;
        Html5.F.td ~a:[Html5.F.a_class ["current_users"]] [
          Html5.F.div ~a:[Html5.F.a_class ["small_box"]] content
        ]
      ]
    )

  method form_edit_awr : 'a.
      text_prefix:_
    -> grps:'a User_sql.Types.admin_writer_reader
    -> arg:'a Opaque.int32_t
    -> ?defaults:_
    -> unit
    -> _ = fun
       ~text_prefix
       ~grps
       ~arg
       ?defaults
       () ->
    let aux grp text default =
      self#form_edit_group ~group:(grp $ arg) ~text
    and d1, d2, d3 = match defaults with
      | None -> "", "", ""
      | Some (d1, d2, d3) -> d1, d2, d3
    in
    aux grps.grp_admin
      [Html5.F.p [Html5.F.pcdata (text_prefix ^" administrators: ")]] d1 ()
                                                               >>= fun forma ->
    aux grps.grp_writer
      [Html5.F.p [Html5.F.pcdata (text_prefix ^" writers: ")]] d2 ()
                                                               >>= fun formw ->
    aux grps.grp_reader
      [Html5.F.p [Html5.F.pcdata (text_prefix ^" readers: ")]] d3 ()
                                                               >>= fun formr ->
    Lwt.return (formr, [formw; forma;])

  method private bt_remove_user_from_group ~group ~user ~remove () =
    let button_value = if remove then "Remove" else "Add" in
    let button =
      Html5.D.button ~button_type:`Submit
        [Html5.F.pcdata button_value]
    in
    Eliom_service.onload {{
      let remove = ref %remove in
      let button = Eliom_content.Html5.To_dom.of_button %button in
      ignore (
        Lwt_js_events.clicks button (fun _ ->
          let user = %user in
          let add = if !remove then "" else user in
          let rem = if !remove then user else "" in
          Eliom_client.call_caml_service
            ~service:%User_services.action_add_remove_users_from_group
            () ( %group, ( add, rem))
          >>= fun new_remove ->
          let button_value = if new_remove then "Remove" else "Add" in
          remove := new_remove;
          button##innerHTML <- Js.string button_value;
          Lwt.return ()
        )
      )
    }};
    [Html5.F.div ~a:[eliom_inline_class] [button]]

  method private login_box_aux
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ?(show_ext=true)
    error =
    if (Eliom_request_info.get_ssl ()) || not User_services.force_secure
    then begin
      lwt ext =
        if show_ext
        then self#login_box_extension
        else Lwt.return ([]: Html5_types.div_content_fun Html5.F.elt list)
      in
      let open Html5.F in
      Lwt.return (fun (usr, pwd) ->
        let user_input_id = fresh_id () in
        let password_input_id = fresh_id () in
        [div ~a:[a_class ["login_box"]]
           (table
             (tr
                [td [label ~a:[Raw.a_for user_input_id] [pcdata user_prompt]];
                 td [str_input ~a:[a_id user_input_id] usr]])
             ([tr [td [label ~a:[Raw.a_for password_input_id] [pcdata pwd_prompt]];
                   td [passwd_input ~a:[a_id password_input_id] pwd]];
               tr [td [];
                   td [submit_input "Login"]]])
            :: ext
            @ (if error then [pcdata auth_error] else [])
           )]
      )
    end
    else
      Lwt.return (fun _ ->
        [Html5.F.p
           [Html5.F.a ~service:Eliom_service.https_void_coservice'
              [Html5.F.pcdata switchtohttps] ()
           ]
        ]
      )

  method private display_logout_box ?(show_ext=true) u =
    let open Html5.F in
    let user_name =
      span ~a:[a_class ["user_name"]]
        [pcdata u.user_fullname]
    in
    Lwt.return
      [div ~a:[a_class ["login_box"]]
         [ pcdata "You are logged as ";
           if show_ext then
             Html5.F.a
               ~service:User_services.service_view_group
               [user_name]
               u.user_login
           else
             user_name ];
       submit_input ~a:[a_class ["logout"]] "logout" ]

  method private login_box_extension = Lwt.return []

  method display_logout_button
    : 'a. _ -> ([> Html5_types.form ] as 'a) Html5.F.elt Lwt.t =
    fun content ->
    Lwt.return
      (Html5.F.post_form ~a:[Html5.F.a_class ["logoutbutton"]]
         ~service:User_services.action_logout
         (fun () ->
            [Html5.F.p
               [Html5.F.button ~button_type:`Submit content]
            ]
         ) ()
      )
              (*
            {{ [<p>[
                   {: Eliom_duce.Html5.button ~button_type:{:"submit":}
                      {: [ <div class="ocsimore_button">content ] :}
                      (*VVV How to avoid the <div> here??? *)
                      :}] ] }}) ())
               *)

  method logout_uri =
    Wiki_syntax.Service_href
      (Wiki_syntax.service_href User_services.action_logout_get ())

  method display_login_widget ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ?show_ext () =
    lwt u = User.get_user_data () in
    lwt logged = User.is_logged_on () in
    lwt f =
      if logged then
       self#display_logout_box ?show_ext u >|= fun f ->
         Html5.F.post_form
           ~a:[Html5.F.a_class ["logbox"; "logged"]]
           ~service:User_services.action_logout (fun _ -> f) ()
     else
       let f_login error ~a =
         self#login_box_aux ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ?show_ext error >|= fun f ->
           Html5.F.post_form
              ~service:User_services.action_login ~a f ()
       in
       lwt login_errors = User_data.get_login_error () in
       if List.exists
          (fun e -> e = User.BadPassword || e = User.BadUser)
          login_errors
       then (* unsuccessful attempt *)
         f_login true ~a:[Html5.F.a_class ["logbox"; "error"]]
       else (* no login attempt yet *)
         f_login false ~a:[Html5.F.a_class ["logbox"; "notlogged"]]
    in
    Lwt.return [let open Html5.F in div ~a:[a_class [xhtml_class]] [f]]


  method user_link group =
    Html5.F.a ~service:User_services.service_view_group
      [Html5.F.pcdata group] group


  method display_group group =
    lwt gtype = User_sql.user_type group in
    let _, _, gtypedescr = match gtype with
      | `Role  -> ("Role",  "role",  "Description")
      | `User  -> ("User",  "user",  "Name"       )
      | `Group -> ("Group", "group", "Description")
    in
    lwt error =
      Ocsimore_common.get_action_failure () >|= function
        | None -> []
        | Some e -> (* YYY add error handler somewhere *)
            let msg = match e with
              | Ocsimore_common.Ok ->
                  "Operation performed"
              | Ocsimore_common.Permission_denied  ->
                  "Unable to perform operation, insufficient rights"
              | Failure s -> s
              | User.UnknownUser u ->
                  "Unknown user/group '" ^ u ^ "'"
              | _ -> "Error"
            in
            [Html5.F.p ~a:[Html5.F.a_class ["errmsg"]] [Html5.F.pcdata msg]]
    in
    (* Adding groups to the group *)
    lwt f1 =
      self#form_edit_group ~group
        ~text:[Html5.F.p ~a:[eliom_inline_class]
                 [Html5.F.strong
                    [Html5.F.pcdata "Members: "]
              ]]
        ()
    in
    (* Adding the group to groups *)
    lwt f2 =
      User_sql.user_to_string ~expand_param:false group
      >>= fun username ->
      self#form_edit_user ~user:group
        ~text:[Html5.F.p ~a:[Html5.F.a_class ["eliom_inline"]]
                  [Html5.F.strong
                      [Html5.F.pcdata (username ^ " is in: ")]
                  ]
        ]
        ()
    in
    lwt g = User_sql.get_user_data group in
    lwt can_change = User_data.can_change_user_data_by_user group in
    let edit =
      if can_change &&
        g.user_pwd <> Connect_forbidden &&
        g.user_pwd <> External_Auth
      then
        [Html5.F.post_form
           ~service:User_services.action_edit_user_data
           (fun (nuserid, (pwd, (pwd2, (desc, email)))) ->
              let open Html5.F in
              [table
                 (let id = fresh_id () in
                  tr [td [label ~a:[Raw.a_for id] [pcdata gtypedescr]];
                      td [str_input ~a:[a_id id] ~value:g.user_fullname desc]])
                 [(let id = fresh_id () in
                   tr [td [label ~a:[Raw.a_for id] [pcdata "e-mail adress"]];
                       td [str_input ~a:[a_id id] ~value:(unopt_str g.user_email) email]]);
                  (let id = fresh_id () in
                   tr [td [label ~a:[Raw.a_for id] [pcdata "Password"]];
                       td [passwd_input ~a:[a_id id] pwd]]);
                  tr [td [];
                      td [passwd_input pwd2]];
                  tr [td [submit_input "Confirm";
                          Html5.F.user_type_input
                            string_from_userid
                            ~input_type:`Hidden
                            ~name:nuserid
                            ~value:g.user_id ()] ] ] ])
           () ]
      else
        let open Html5.F in
        [p [strong [pcdata (gtypedescr ^ ": ")];
                    pcdata g.user_fullname]]
    in
      Lwt.return
        (  error
         @ let open Html5.F in [
           div ~a:[a_class ["user_block"]] edit;
           div ~a:[a_class ["user_block"]]
             (if gtype = `User
              then []
              else [table ~a:[a_class ["users_in_group"]] f1 []]);
           div ~a:[a_class ["user_block"]]
             [table ~a:[a_class ["users_in_group"]] f2 []];
         ])

  method display_users_settings =
    (* TODO: Disable dynamicaly if checkbox is false *)
    User_sql.get_users_settings () >>= (fun users_settings ->
      Lwt.return [
        Html5.F.post_form
          ~service: User_services.action_users_settings
          (fun (enable, (mail_from, (mail_addr, (mail_subject, (groups, non_admin))))) -> [
            Html5.F.table
              (Html5.F.tr [
                Html5.F.td [
                  Html5.F.label [
                    Html5.F.pcdata "Enable users creation:"
                  ]
                ];
                Html5.F.td [
                  Html5.F.bool_checkbox
                    ~checked: users_settings.User_sql.basicusercreation
                    ~name: enable ()
                ]
               ]
              ) [
                Html5.F.tr [
                  Html5.F.td [
                    Html5.F.label [
                      Html5.F.pcdata "Registration mail from:"
                    ]
                  ];
                  Html5.F.td [
                    str_input
                      ~value: users_settings.User_sql.registration_mail_from
                      mail_from
                  ]
                ];
                Html5.F.tr [
                  Html5.F.td [
                    Html5.F.label [
                      Html5.F.pcdata "Registration mail address:"
                    ]
                  ];
                  Html5.F.td [
                    str_input
                      ~value: users_settings.User_sql.registration_mail_addr
                      mail_addr
                  ]
                ];
                Html5.F.tr [
                  Html5.F.td [
                    Html5.F.label [
                      Html5.F.pcdata "Registration mail subject:"
                    ]
                  ];
                  Html5.F.td [
                    str_input
                      ~value: users_settings.User_sql.registration_mail_subject
                      mail_subject
                  ]
                ];
                Html5.F.tr [
                  Html5.F.td [
                    Html5.F.label [
                      Html5.F.pcdata "Groups:"
                    ]
                  ];
                  Html5.F.td [
                    str_input
                      ~value: users_settings.User_sql.groups
                      groups
                  ]
                ];
                Html5.F.tr [
                  Html5.F.td [
                    Html5.F.label [
                      Html5.F.pcdata "Non-admin can create user:"
                    ]
                  ];
                  Html5.F.td [
                    Html5.F.bool_checkbox
                      ~checked: users_settings.User_sql.non_admin_can_create
                      ~name: non_admin ()
                  ]
                ];
                Html5.F.tr [
                  Html5.F.td [
                    submit_input "Send"
                  ]
                ]
              ]
           ]
          ) ()
      ]
    )

  method display_users_settings_done () (basicusercreation,
                                         (registration_mail_from,
                                         (registration_mail_addr,
                                         (registration_mail_subject,
                                         (groups,
                                         non_admin_can_create))))) =
    let users_settings = {
      User_sql.basicusercreation = basicusercreation;
      registration_mail_from = registration_mail_from;
      registration_mail_addr = registration_mail_addr;
      registration_mail_subject = registration_mail_subject;
      groups = groups;
      non_admin_can_create = non_admin_can_create
    } in
    (if basicusercreation then (
      match registration_mail_from with
        | "" -> Lwt.fail (Failure "Missing registration_mail_from attribute")
        | _ -> (
          match registration_mail_addr with (* TODO: Match for non-valide email *)
            | "" -> Lwt.fail (Failure "Missing registration_mail_addr attribute")
            | _ ->
              Lwt.return ()
        )
     )
     else
        Lwt.return ()
    ) >>= (fun () -> User_sql.set_users_settings users_settings) >>= (fun () ->
      Lwt.return [Html5.F.p [Html5.F.pcdata "Done !"]]
     )

  method display_users =
    User_sql.all_users () >>= fun l ->
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login)
      (Lazy.force l.users) in
    self#display_users_groups ~show_auth:true ~l ~utype:`User
      >|= list_singleton

  method display_groups =
    lwt l =
      User_sql.all_users () >|= fun users ->
      List.sort
        (fun u1 u2 -> compare u1.user_login u2.user_login)
        (Lazy.force users.groups)
    in
    self#display_users_groups ~show_auth:false ~l ~utype:`Group
      >|= list_singleton

  (* Parameterized users *)
  method display_roles =
    self#get_roles_table ()
    >>= fun hashtbl ->
    let l = Hashtbl.fold (fun a b acc ->
      Html5.F.tr ~a:[Html5.F.a_class ["roles_title"]] [
        Html5.F.td [
          Html5.F.h3 [Html5.F.pcdata a]
        ];
        Html5.F.td [];
      ] :: b @ acc
    ) hashtbl [] in
    match l with
      | [] -> Lwt.return []
      | x::xs ->
        Lwt.return [
          Html5.F.table
            ~a:[Html5.F.a_class ["table_admin"]]
            x xs
        ]

  method private get_roles_table ?td_content () =
    lwt l =
      User_sql.all_users () >|= fun users ->
      List.sort
        (fun u1 u2 -> compare u1.user_login u2.user_login)
        (Lazy.force users.roles)
    in
    let line u short_name =
      let open Html5.F in
      (match u.user_kind with
        | `ParameterizedGroup param ->
          (match param with
            | Some { param_description = param; _ } ->
              (match param with
                | "login of the user" -> User_sql.get_users_login ()
                | "id of the forum" -> Forum_sql.get_forums_id ()
                | "id of the wiki which is a forum" -> Forum_sql.get_forums_wiki_id ()
                | "id of the message" -> Forum_sql.get_forum_messages_id ()
                | "id of the wiki" -> Wiki_sql.get_wikis_id ()
                | "id of the wikibox" -> Wiki_sql.get_wikiboxes_id ()
                | "id of the wikipage" -> Wiki_sql.get_wikipages_id ()
                | _ -> Lwt.fail
                  (Failure
                     ("Desciption \"" ^ param ^ "\" not describe a valid user")
                  )
              ) >|= (fun x -> Some (param, x))
            | None -> Lwt.fail
              (Failure
                 "Parametrized group without description"
              )
          )
        | _ -> Lwt.return None
      ) >>= (function
        | None ->
          let block = tr [
            td ~a:[a_class ["roles_tr"]] [
              strong [
                a ~service:User_services.service_view_group
                  [pcdata short_name] u.user_login;
              ];
            ];
            td (match td_content with
              | None -> [pcdata u.user_fullname]
              | Some f -> f u.user_login
            );
          ] in
          Lwt.return [block]
        | Some (param, p) ->
          let parametrize id = u.user_login ^ "(" ^ id ^ ")"
          in
          let link param id =
            let param_string =
              let title = Sql.getn param#title in
              match title with
                | None -> "id: " ^ id
                | Some title -> title ^ " (id: " ^ id ^ ")"
            in
            a ~service:User_services.service_view_group
              [strong [pcdata param_string]] (parametrize id);
          in
          let block_and_link =
            let lines =
              List.map
                (fun param ->
                  let id = Int32.to_string (Sql.get param#id) in
                  tr [
                    td ~a:[a_class ["roles_tr_prime"]] [link param id];
                    td (match td_content with
                      | None -> []
                      | Some f -> f (parametrize id)
                    );
                  ]
                )
                p
            in
            match lines with
              | [] -> [tr [td ~a:[a_class ["roles_tr_prime"]] [pcdata "(None)"]]]
              | _ -> lines
          in
          let block name = tr ~a:[a_class ["user_menu_title"]] [
            td ~a:[a_class ["roles_tr"]] [
              strong [pcdata (name ^ "(" ^ param ^ ")")]
            ];
            td [pcdata u.user_fullname]
          ] in
          Lwt.return (block short_name :: block_and_link)
       )
      in
      let module Pcre = Netstring_pcre in
      let regexp = Pcre.regexp "#([^.]+)\\.(.*)" in
      let hashtbl = Hashtbl.create 4 in
      Lwt_list.iter_s (fun arg ->
        let (group, short_name) =
          match Pcre.string_match regexp arg.user_login 0 with
            | None -> ("Other", "")
            | Some x ->
              try (String.capitalize (Pcre.matched_group x 1 arg.user_login),
                   Pcre.matched_group x 2 arg.user_login)
              with Not_found -> ("Other", "")
        in
        line arg short_name
        >|= fun item ->
        try
          let prev = Hashtbl.find hashtbl group in
          Hashtbl.replace hashtbl group (item @ prev)
        with Not_found ->
          Hashtbl.add hashtbl group item
      ) l
      >>= fun () ->
      Lwt.return hashtbl

  method private display_users_groups ~show_auth ~utype ~l =
    let line u =
      let l =
        Html5.F.a ~service:User_services.service_view_group
          [Page_site.icon ~path:"imgedit.png" ~text:"Details"]
          u.user_login
      in
      let aa =
        if show_auth
        then [Html5.F.td
                [Html5.F.pcdata
                   (match u.user_pwd with
                      | Connect_forbidden -> "group"
                      | Ocsimore_user_plain _
                      | Ocsimore_user_safe _
                      | Ocsimore_user_crypt _ -> "password"
                      | External_Auth -> "external"
                   )
                ]
             ]
        else []
      in
      Html5.F.tr
        ( [Html5.F.td ~a:[Html5.F.a_class ["userlogin"]]
              [Html5.F.strong [Html5.F.pcdata u.user_login]];
           Html5.F.td ~a:[Html5.F.a_class ["userdescr"]]
             [Html5.F.pcdata u.user_fullname]]
          @ aa
          @ [Html5.F.td [l]] )
    in
    let l = List.rev (List.fold_left (fun s arg -> line arg :: s) [] l) in
    Lwt.return Html5.F.(
      table ~a:[a_class ["table_admin"]]
        (tr
           (  th [pcdata "Login"];
            :: th [pcdata (match utype with | `User -> "Name" | `Group -> "Description")]
            :: (if show_auth
                then [th [pcdata "Authentication"]]
                else [])
            @ [th []]
            ))
         l
    )


  method status_text =
    self#display_login_widget
      ~user_prompt:"You are not currently logged in. Login:"
      ~pwd_prompt:"Password:"
      ~show_ext:false () >>= fun r ->
    Lwt.return r (*
    User.get_user_data sp >>= fun u ->
      if u.user_id <> User.anonymous then
        let u = Ocamlduce.Utf8.make u.user_login in
        self#display_logout_button ~sp {{ ['Logout'] }} >>= fun l ->
        Lwt.return {{ ['You are logged in as ' !u '. ' l ] }}
      else
        let l = Eliom_duce.Html5.a User_services.service_login sp {{ "Login" }} () in
        Lwt.return {{ ['You are not currently logged. ' l]  }} *)


  method display_group_creation ?(err="") () =
    Lwt.return
      Html5.F.([
        p [pcdata
             "You can use the form below to create a new Ocsimore \
              group. (A group is a special form of user that is not \
              authorized to log in.) Once this is done, you will \
              be able to add users into your group."];
        Html5.F.post_form
          ~service:User_services.action_create_new_group
          (fun (usr, desc) ->
             [table
                (tr [td [pcdata "group name (letters and digits only)"];
                     td [str_input usr]])
                [tr [td [pcdata "description"];
                     td [str_input desc]];
                 tr [td [submit_input "Create"]]]
             ])
          ();
        p [strong [pcdata err]]
      ])

  method display_group_creation_done () (name, descr) =
    try_lwt
      lwt groupid = User_data.create_group ~name ~descr in
      lwt group = User_sql.get_basicuser_data groupid in
      let open Html5.F in
      Lwt.return ([
        h2 [pcdata "Group created"];
        p [
          pcdata "You can now ";
          Html5.F.a
            ~service:User_services.service_view_group
            [pcdata "edit"] group.user_login;
          pcdata " your new group."
        ]
      ] : Eliom_registration.Block5.page)
    with
      | Failure err ->
          self#display_group_creation ~err ()
      | Ocsimore_common.Permission_denied ->
          Lwt.return Html5.F.([
            h2 [pcdata "Error"];
            p [pcdata "You cannot create new groups"];
          ])
end


(* The [services] argument should be changed from a tuple into a first-class
   module as soon as this is available in Caml *)
class user_widget_user_creation user_creation_options : user_widget_user_creation_class =
object (self)

  method login_box_extension =
    User_data.can_create_user ~options:user_creation_options >|= function
      | true ->
          [Html5.D.a
             ~service:User_services.service_create_new_user
             [Html5.F.pcdata "New user? Register now!" ] () ]
(*
            << <tr>
              <td colspan="2">
                <a href=$User_services.service_create_new_user$>
                  "New user? Register now!"
                </a>
              </td>
            </tr> >>
 *)
      | false -> []

  method display_user_creation ?(err="") () =
    let open Html5.F in
    User_data.can_create_user ~options:user_creation_options >|= function
      | true -> [
           p [pcdata "Use the form below to create a new \
                      Ocsimore user.";
              br ();
              pcdata "Note that users that authenticate \
                      through external means (NIS or PAM) are \
                      added automatically the first time they \
                      log in inside Ocsimore, and you do not \
                      need to create them";
           ];
           p [pcdata "Please fill in the following fields.";
              br ();
              pcdata "Be very careful to enter a valid e-mail \
                      address, as the confirmation url will be \
                      sent there.";
           ];
           Html5.D.post_form
             ~service:User_services.action_create_new_user
             (fun (usr,(desc,(email, (pass1, pass2)))) ->
                [table
                   (let id = fresh_id () in
                    tr [td [label ~a:[Raw.a_for id] [pcdata "Login name"]];
                        td [str_input ~a:[a_id id] usr];
                        td ~a:[a_class ["description"]]
                          [pcdata "letters and digits only"] ])
                   [(let id = fresh_id () in
                     tr [td [label ~a:[Raw.a_for id] [pcdata "Real name:"]];
                         td [str_input ~a:[a_id id] desc]]);
                    (let id = fresh_id () in
                     tr [td [label ~a:[Raw.a_for id] [pcdata "E-mail address:"]];
                         td [str_input ~a:[a_id id] email]]);
                    (let id = fresh_id () in
                     tr [td [label ~a:[Raw.a_for id] [pcdata "Password:"]];
                         td [passwd_input ~a:[a_id id] pass1]]);
                    tr [td [];
                        td [passwd_input pass2]];
                    tr [td [submit_input "Register"]];
                   ]
                ]
             ) ();
           p [strong [pcdata err]];
          ]
      | false ->
          [h2 [pcdata "Error"];
           p [pcdata "You are not allowed to create new users"];
          ]

  method display_user_creation_done ~name  ~fullname ~email ~pwd =
    try_lwt
       if fst pwd <> snd pwd then
         Lwt.fail (Failure "You must enter the same password twice")
       else
         User_services.create_user ~name ~fullname ~email ~pwd:(fst pwd)
           ~options:user_creation_options () >|= fun () ->
         let open Html5.F in
         [h2 [pcdata "User creation successful"];
          p [pcdata "You will receive an activation e-mail \
                     at the following address:";
             br ();
             em [pcdata email];
          ];
         ]
     with
       | Failure err ->
           self#display_user_creation ~err ()
       | Ocsimore_common.Permission_denied ->
           let open Html5.F in
           Lwt.return
             [h2 [pcdata "Error"];
              p [pcdata "You cannot create new users"];
             ]
end
