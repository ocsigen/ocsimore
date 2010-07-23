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



open User_sql.Types

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(*TODO: relocate*)
let eliom_inline_class = XHTML.M.a_class ["eliom_inline"]
let accept_charset_utf8 = XHTML.M.a_accept_charset "utf-8"
let unopt_str = function | None -> "" | Some s -> s


let str_input ?(value="") ?(visible=true) name =
  Eliom_predefmod.Xhtml.string_input ~name ~value
    ~input_type:(if visible then `Text else `Hidden)
    ()
let passwd_input ?(value="") name =
  Eliom_predefmod.Xhtml.string_input
    ~input_type:`Password
    ~name
    ~value ()
let submit_input value =
  Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value ()


class type user_widget_class = object
  method display_roles :
    sp:Eliom_sessions.server_params ->
    Eliom_predefmod.Blocks.page Lwt.t
  method display_groups :
    sp:Eliom_sessions.server_params ->
    Eliom_predefmod.Blocks.page Lwt.t
  method display_users :
    sp:Eliom_sessions.server_params ->
    Eliom_predefmod.Blocks.page Lwt.t

  method display_group :
    sp:Eliom_sessions.server_params ->
    user * string -> Eliom_predefmod.Blocks.page Lwt.t

  method display_login_widget :
    sp:Eliom_sessions.server_params ->
    ?user_prompt:string ->
    ?pwd_prompt:string ->
    ?auth_error:string ->
    ?switchtohttps:string ->
    ?show_ext:bool ->
    unit ->
    Xhtmltypes.form_content XHTML.M.elt Lwt.t

  method private display_logout_box :
    sp:Eliom_sessions.server_params ->
    ?show_ext:bool ->
    User_sql.Types.userdata ->
    Xhtmltypes.form_content XHTML.M.elt list Lwt.t
  method display_logout_button :
    sp:Eliom_sessions.server_params ->
    Xhtmltypes.button_content XHTML.M.elt list ->
    Xhtmltypes.form XHTML.M.elt Lwt.t
  method logout_uri :
    sp:Eliom_sessions.server_params -> XHTML.M.uri

  method user_link :
    sp:Eliom_sessions.server_params ->
    string -> [`A | `Form] XHTML.M.elt

  method user_list_to_xhtml :
    sp:Eliom_sessions.server_params ->
    ?hook:(sp:Eliom_sessions.server_params ->
          user:string ->
          [`A | `Form] XHTML.M.elt list Lwt.t) ->
    User_sql.Types.user list -> Xhtmltypes.block XHTML.M.elt Lwt.t


  (** Helper forms to add and remove users from groups. If [show_edit]
      is false, no controls to edit the permissions are shown *)
  (** Form to add users to a group *)
  method form_edit_group:
    sp:Eliom_sessions.server_params ->
    ?show_edit:bool ->
    ?default_add:string ->
    group:user ->
    text:Xhtmltypes.block XHTML.M.elt list ->
    unit ->
    Xhtmltypes.tbody_content XHTML.M.elt Lwt.t

  (** Form to add an user to a group *)
  method form_edit_user:
    sp:Eliom_sessions.server_params ->
    user:User_sql.Types.user ->
    text:Xhtmltypes.block XHTML.M.elt list ->
    unit ->
    Xhtmltypes.block XHTML.M.elt list Lwt.t

  method form_edit_awr: 'a.
    sp:Eliom_sessions.server_params ->
    text_prefix:string ->
    grps:'a User_sql.Types.admin_writer_reader ->
    arg:'a Opaque.int32_t ->
    ?defaults:string * string * string ->
    unit ->
    (  Xhtmltypes.tbody_content XHTML.M.elt
     * Xhtmltypes.tbody_content XHTML.M.elt list) Lwt.t

  method status_text:
    sp:Eliom_sessions.server_params ->
    Xhtmltypes.form_content XHTML.M.elt list Lwt.t

  method display_group_creation :
    ?err:string ->
    sp:Eliom_sessions.server_params ->
    Eliom_predefmod.Blocks.page Lwt.t

  method display_group_creation_done :
    Eliom_sessions.server_params ->
    unit ->
    string * string ->
    Eliom_predefmod.Blocks.page Lwt.t

end

class type user_widget_user_creation_class = object
  method display_user_creation :
    ?err:string ->
    sp:Eliom_sessions.server_params ->
    Eliom_predefmod.Blocks.page Lwt.t
  method display_user_creation_done :
    sp:Eliom_sessions.server_params ->
    name:string ->
    fullname:string ->
    email:string ->
    pwd:string*string ->
    Eliom_predefmod.Blocks.page Lwt.t
end



open Xform.XformLwt
open Ops


(** Widget for user login/logout/edition without addition of new users *)
class user_widget : user_widget_class =
object (self)

  val xhtml_class = "logbox"

  method form_edit_group ~sp ?(show_edit=false) ?(default_add="") ~group ~text () =
    (if show_edit then
       User.get_user_data sp                      >>= fun u ->
       let user = basic_user u.user_id in
       User_data.can_admin_group ~sp ~user ~group () >>= function
         | true ->
             User_sql.user_to_string group        >>= fun group ->
             Lwt.return
               ((fun ~sp ~user ->
                   Lwt.return
                     (self#bt_remove_user_from_group ~sp ~group ~user ())
                ),
                (self#form_add_user_to_group ~sp ~default_add ~group ())
               )
         | false ->
             Lwt.return ((fun ~sp:_ ~user:_ -> Lwt.return []), [])
     else
       Lwt.return ((fun ~sp:_ ~user:_ -> Lwt.return []), [])
    )                                             >>= fun (hook, add) ->

    User_sql.users_in_group ~generic:false ~group >>= fun users ->
    self#user_list_to_xhtml ~sp ~hook users       >>= fun members ->
    Lwt.return
      (XHTML.M.tr
         (XHTML.M.td ~a:[XHTML.M.a_class ["role"]] text)
         [XHTML.M.td ~a:[XHTML.M.a_class ["current_users"]] (members :: add)]
      )


  method form_edit_user ~sp ~user ~text () =
    (* YYY put back edition buttons if the user has enough rights, or if
       it is admin *)
    User_sql.groups_of_user ~user >>=
    self#user_list_to_xhtml ~sp   >>= fun members ->
    Lwt.return (text @ [members])

  method form_edit_awr : 'a.
       sp:_
    -> text_prefix:_
    -> grps:'a User_sql.Types.admin_writer_reader
    -> arg:'a Opaque.int32_t
    -> ?defaults:_
    -> unit
    -> _ = fun
       ~sp
       ~text_prefix
       ~grps
       ~arg
       ?defaults
       () ->
    let aux grp text default =
      self#form_edit_group ~sp ~group:(grp $ arg) ~text
        ~show_edit:true ~default_add:default
    and d1, d2, d3 = match defaults with
      | None -> "", "", ""
      | Some (d1, d2, d3) -> d1, d2, d3
    in
    aux grps.grp_admin
      [XHTML.M.p [XHTML.M.pcdata (text_prefix ^" administrators: ")]] d1 ()
                                                               >>= fun forma ->
    aux grps.grp_writer
      [XHTML.M.p [XHTML.M.pcdata (text_prefix ^" writers: ")]] d2 ()
                                                               >>= fun formw ->
    aux grps.grp_reader
      [XHTML.M.p [XHTML.M.pcdata (text_prefix ^" readers: ")]] d3 ()
                                                               >>= fun formr ->
    Lwt.return (formr, [formw; forma;])

  method private bt_remove_user_from_group ~sp ~group ~user ?(text="Remove") () =
    let str_input = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      [ XHTML.M.div ~a:[eliom_inline_class]
          [ str_input ~value:group gname;
            str_input ~value:user remname;
            str_input addname;
            Eliom_predefmod.Xhtml.button ~button_type:`Submit
              [XHTML.M.pcdata text];
          ]
      ]
    in
    [Eliom_predefmod.Xhtml.post_form
       ~a:[eliom_inline_class; accept_charset_utf8 ]
       ~service:User_services.action_add_remove_users_from_group ~sp mform ()]

  method private form_add_user_to_group ~sp ~group ?(default_add="") ?(text="Add") () =
    let str_input' = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      [XHTML.M.div ~a:[eliom_inline_class]
         [str_input' ~value:group gname;
          str_input' remname;
          str_input ~value:default_add addname ;
          Eliom_predefmod.Xhtml.button ~button_type:`Submit
            [XHTML.M.pcdata text];
         ]
      ]
    in
    [Eliom_predefmod.Xhtml.post_form
       ~a:[eliom_inline_class; accept_charset_utf8]
      ~service:User_services.action_add_remove_users_from_group ~sp mform ()]

  method user_list_to_xhtml ~sp ?hook l = match l with
    | [] -> Lwt.return
              (XHTML.M.p [XHTML.M.em [XHTML.M.pcdata "(currently no user)"]])
    | e :: es ->
        let hook = match hook with
          | None -> (fun _ -> Lwt.return [])
          | Some h -> (fun user -> h ~sp ~user)
        in
        let convert u =
          User_sql.user_to_string ~expand_param:true u >>= fun user ->
          hook user                                    >|= fun hooked ->
          XHTML.M.li ((self#user_link ~sp user) :: hooked)
        in
        convert e >>= fun e ->
        Lwt_list.fold_left_s
          (fun s u -> convert u >|= fun r -> r :: s)
          [e] es
        >|= List.rev >|= function
        | r::rs -> XHTML.M.ul ~a:[XHTML.M.a_class ["user_list"]] r rs
        | [] -> (assert false) (*TODO: change function sa that no assertion is needed*)


  method private login_box_aux
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ?(show_ext=true)
    ~sp error =
    if (Eliom_sessions.get_ssl sp) || not User_services.force_secure
    then begin
      (if show_ext
       then self#login_box_extension ~sp
       else Lwt.return ([]: [`Tr] XHTML.M.elt list)
      ) >>= fun ext ->
      Lwt.return (fun (usr, pwd) ->
        [XHTML.M.table ~a:[XHTML.M.a_class ["login_box"]]
           (XHTML.M.tr
              (XHTML.M.td [XHTML.M.pcdata user_prompt])
              [XHTML.M.td [str_input usr]])
           (   XHTML.M.tr
                 (XHTML.M.td [XHTML.M.pcdata pwd_prompt])
                 [XHTML.M.td [passwd_input pwd]]
            :: XHTML.M.tr
                 (XHTML.M.td [submit_input "Login"])
                 []
            :: ext
             @  (if error
                then [XHTML.M.tr (XHTML.M.td ~a:[XHTML.M.a_colspan 2]
                                    [XHTML.M.pcdata auth_error]) []
                ]
                else []
               )
           )
        ]
      )
    end
    else
      Lwt.return (fun _ ->
        [XHTML.M.p
           [Eliom_predefmod.Xhtml.a Eliom_services.https_void_coservice'
              sp [XHTML.M.pcdata switchtohttps] ()
           ]
        ]
      )

  method private display_logout_box ~sp ?(show_ext=true) u =
    (if show_ext
     then self#logout_box_extension ~sp
     else Lwt.return []) >>= fun ext ->
    Lwt.return [XHTML.M.table ~a:[XHTML.M.a_class["login_box"]]
                  (XHTML.M.tr
                     (XHTML.M.td
                        [XHTML.M.pcdata (Printf.sprintf "You are logged as %s"
                                           u.user_fullname)])
                     []
                  )
                  (XHTML.M.tr
                     (XHTML.M.td
                        [submit_input "logout"]) []
                   :: ext)
    ]

  method private login_box_extension ~sp:_ = Lwt.return []

  method private logout_box_extension ~sp =
    User.get_user_data sp >|= fun ud ->
    [XHTML.M.tr
       (XHTML.M.td
          [Eliom_predefmod.Xhtml.a
             User_services.service_view_group sp
             [XHTML.M.pcdata "Manage your account"] ud.user_login
          ]
       )
       []
    ]

  method display_logout_button ~sp content =
    Lwt.return
      (Eliom_predefmod.Xhtml.post_form ~a:[XHTML.M.a_class ["logoutbutton"]]
         ~sp ~service:User_services.action_logout
         (fun () ->
            [XHTML.M.p
               [Eliom_predefmod.Xhtml.button ~button_type:`Submit content]
            ]
         ) ()
      )
              (*
            {{ [<p>[
                   {: Eliom_duce.Xhtml.button ~button_type:{:"submit":}
                      {: [ <div class="ocsimore_button">content ] :}
                      (*VVV How to avoid the <div> here??? *)
                      :}] ] }}) ())
               *)

  method logout_uri ~sp =
    Eliom_predefmod.Xhtml.make_uri
      ~service:User_services.action_logout_get
      ~sp
      ()


  method display_login_widget ~sp ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ?(show_ext=true) () =
    User.get_user_data sp >>= fun u ->
    User.is_logged_on sp >>= fun logged ->
    (if logged then
       self#display_logout_box ~sp ~show_ext u >>= fun f ->
       Lwt.return (
         Eliom_predefmod.Xhtml.post_form
           ~a:[XHTML.M.a_class ["logbox"; "logged"]]
           ~service:User_services.action_logout ~sp (fun _ -> f) ())

     else
       let f_login error ~a =
         self#login_box_aux ?user_prompt ?pwd_prompt ?auth_error
           ?switchtohttps ~sp ~show_ext error
         >>= fun f ->
         Lwt.return
           (Eliom_predefmod.Xhtml.post_form
              ~service:User_services.action_login ~sp ~a f ())
       in
       if List.exists
          (fun e -> e = User.BadPassword || e = User.BadUser)
          (User_data.get_login_error ~sp)
       then (* unsuccessful attempt *)
         f_login true ~a:[XHTML.M.a_class ["logbox"; "error"]]
       else (* no login attempt yet *)
         f_login false ~a:[XHTML.M.a_class ["logbox"; "notlogged"]]
    ) >>= fun f ->
    Lwt.return (XHTML.M.div ~a:[XHTML.M.a_class [xhtml_class]] [f])


  method user_link ~sp group =
    Eliom_predefmod.Xhtml.a ~service:User_services.service_view_group ~sp
      [XHTML.M.pcdata group] group


  method display_group ~sp (group, g) =
    User_sql.user_type group >>= fun gtype ->
    let ctext, text, gtypedescr = match gtype with
      | `Role  -> ("Role",  "role",  "Description")
      | `User  -> ("User",  "user",  "Name"       )
      | `Group -> ("Group", "group", "Description")
    in
    let error = match Ocsimore_common.get_action_failure sp with
      | None -> []
      | Some e -> (* YYY add error handler somewhere *)
          let msg = match e with
            | Ocsimore_common.Ok -> "Operation performed"
            | Ocsimore_common.Permission_denied  ->
                "Unable to perform operation, insufficient rights"
            | Failure s -> s
            | User.UnknownUser u -> "Unknown user/group '" ^ u ^ "'"
            | _ -> "Error"
          in
          [XHTML.M.p ~a:[XHTML.M.a_class ["errmsg"]] [XHTML.M.pcdata msg]]
    in
    let head =
      XHTML.M.h1
        [XHTML.M.pcdata ctext;
         XHTML.M.pcdata ("'" ^ g ^ "'"); (*Weird...*)
        ]
    in

    (* Adding groups to the group *)
    self#form_edit_group ~sp ~show_edit:true ~group
      ~text:[XHTML.M.p ~a:[eliom_inline_class]
               [XHTML.M.strong
                  [XHTML.M.pcdata ("Current users/groups in this "^ text ^": ")]
            ]]
      ()
    >>= fun f1  ->

    (* Adding the group to groups *)
    self#form_edit_user ~sp ~user:group
      ~text:[XHTML.M.p ~a:[XHTML.M.a_class ["eliom_inline"]]
               [XHTML.M.strong
                  [XHTML.M.pcdata ("Current groups/roles in which the " ^ text ^
                                   "is: ")
                  ]
               ]
      ]
      ()
    >>= fun f2  ->

    User_sql.get_user_data group >>= fun g ->
    User_data.can_change_user_data_by_user sp group >>= fun can_change ->
    let edit =
      if can_change &&
        g.user_pwd <> Connect_forbidden &&
        g.user_pwd <> External_Auth
      then
        [Eliom_predefmod.Xhtml.post_form
           ~service:User_services.action_edit_user_data ~sp
           (fun (nuserid, (pwd, (pwd2, (desc, email)))) ->
              [XHTML.M.table
                 (XHTML.M.tr
                    (XHTML.M.td
                       [XHTML.M.strong [XHTML.M.pcdata (gtypedescr ^ ": ")]])
                    [XHTML.M.td [str_input ~value:g.user_fullname desc]]
                 )
                 [XHTML.M.tr
                    (XHTML.M.td [XHTML.M.pcdata "e-mail adress: "])
                    [XHTML.M.td [str_input ~value:(unopt_str g.user_email)
                                   email]];
                  XHTML.M.tr
                    (XHTML.M.td [passwd_input pwd ])
                    [XHTML.M.td [passwd_input pwd2]];
                  XHTML.M.tr
                    (XHTML.M.td [submit_input "Confirm";
                                 Eliom_predefmod.Xhtml.user_type_input
                                   string_from_userid
                                   ~input_type:`Hidden
                                   ~name:nuserid
                                   ~value:g.user_id ()
                                ])
                    []
                 ]
              ]
           )
           ()
        ]
      else
        [XHTML.M.p [XHTML.M.strong [XHTML.M.pcdata (gtypedescr ^ ": ")];
                    XHTML.M.pcdata g.user_fullname]
        ]
    in
      Lwt.return
        (   head
         :: error
         @[ XHTML.M.div ~a:[XHTML.M.a_class ["user_block"]] edit;
            XHTML.M.div ~a:[XHTML.M.a_class ["user_block"]]
              [XHTML.M.table ~a:[XHTML.M.a_class ["users_in_group"]] f1 []];
            XHTML.M.div ~a:[XHTML.M.a_class ["user_block"]] f2;
        ])


  method display_users ~sp =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u; user_pwd = a} ->
                           u = `BasicUser && a <> Connect_forbidden ) l in
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    self#display_users_groups ~show_auth:true ~sp ~l ~utype:`User >>= fun r ->

    Lwt.return [XHTML.M.h1 [XHTML.M.pcdata "Existing users"]; r]

  method display_groups ~sp =
    User_sql.all_groups () >>= fun l ->
    let l =
      List.filter
        (fun {user_kind = u; user_pwd = a} ->
           u = `BasicUser && a = Connect_forbidden )
        l
    in
    let l =
      List.sort
        (fun u1 u2 -> compare u1.user_login u2.user_login)
        l
    in

    self#display_users_groups ~show_auth:false ~sp ~l ~utype:`Group >>= fun r ->

    Lwt.return [XHTML.M.h1 [XHTML.M.pcdata "Existing groups"]; r]


  (* Parameterized users *)
  method display_roles ~sp =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u} -> u <> `BasicUser) l in
    let l = List.sort (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    let hd, tl = match l with
      | hd :: tl -> (hd, tl)
      | _ -> (assert false) (*YYY: some groups always exist*)
    in
    let line u =
      let p = match u.user_kind with
        | `ParameterizedGroup param ->
            let p = match param with
              | Some { param_description = param } -> param
              | None -> "param"
            in
            [XHTML.M.em [XHTML.M.pcdata ("(" ^ p ^ ")")]]
        | _ -> []
      in
      XHTML.M.tr
        (XHTML.M.td [XHTML.M.strong (XHTML.M.pcdata u.user_login :: p)])
        [XHTML.M.td [XHTML.M.pcdata u.user_fullname]]
    in
    let l1 = List.rev (List.fold_left (fun s arg -> line arg :: s) [] tl) in
    let t1 = XHTML.M.table ~a:[XHTML.M.a_class ["table_admin"]] (line hd) l1 in

    let form name =
      [XHTML.M.p
         [Eliom_predefmod.Xhtml.string_input ~name ~input_type:`Text ();
          Eliom_predefmod.Xhtml.button ~button_type:`Submit
            [XHTML.M.pcdata "Edit this role"];
         ]
      ]
    in
    let f =
      Eliom_predefmod.Xhtml.get_form ~a:[accept_charset_utf8]
        ~service:User_services.service_view_group ~sp form
    in
    let msg2 =
      "Choose one group, and enter it (including its parameter if needed) below"
    in
    Lwt.return
      [XHTML.M.h1 [XHTML.M.pcdata "Roles"];
       t1;
       XHTML.M.p [XHTML.M.pcdata msg2];
       f
      ]

  method private display_users_groups ~show_auth ~sp ~utype ~l =
    let line2 u =
      let l =
        Eliom_predefmod.Xhtml.a ~service:User_services.service_view_group ~sp
          [Page_site.icon ~sp ~path:"imgedit.png" ~text:"Details"]
          u.user_login
      in
      let aa =
        if show_auth
        then [XHTML.M.td
                [XHTML.M.pcdata
                   (match u.user_pwd with
                      | Connect_forbidden -> "group"
                      | Ocsimore_user_plain _
                      | Ocsimore_user_crypt _ -> "password"
                      | External_Auth -> "external"
                   )
                ]
             ]
        else []
      in
      XHTML.M.tr
        (XHTML.M.td ~a:[XHTML.M.a_class ["userlogin"]]
           [XHTML.M.strong [XHTML.M.pcdata u.user_login]])
        (   XHTML.M.td ~a:[XHTML.M.a_class ["userdescr"]]
              [XHTML.M.pcdata u.user_fullname]
         :: aa
          @ [XHTML.M.td [l]]
        )
    in
    let l = List.rev (List.fold_left (fun s arg -> line2 arg :: s) [] l) in
    Lwt.return
      (XHTML.M.table ~a:[XHTML.M.a_class ["table_admin"]]
         (XHTML.M.tr
            (XHTML.M.th [XHTML.M.pcdata "Login"])
            (   XHTML.M.th
                  [XHTML.M.pcdata (match utype with
                                     | `User -> "Name"
                                     | `Group -> "Description"
                                  )
                  ]
             :: (if show_auth
                 then [XHTML.M.th [XHTML.M.pcdata "Authentication"]]
                 else []
             )
            )
         )
         l
      )


  method status_text ~sp =
    self#display_login_widget ~sp
      ~user_prompt:"You are not currently logged in. Login:"
      ~pwd_prompt:"Password:"
      ~show_ext:false () >>= fun r ->
    Lwt.return [r] (*
    User.get_user_data sp >>= fun u ->
      if u.user_id <> User.anonymous then
        let u = Ocamlduce.Utf8.make u.user_login in
        self#display_logout_button ~sp {{ ['Logout'] }} >>= fun l ->
        Lwt.return {{ ['You are logged in as ' !u '. ' l ] }}
      else
        let l = Eliom_duce.Xhtml.a User_services.service_login sp {{ "Login" }} () in
        Lwt.return {{ ['You are not currently logged. ' l]  }} *)


  method display_group_creation ?(err="") ~sp =
    User_data.can_create_group ~sp >|= function
      | true ->
          [XHTML.M.h1 [XHTML.M.pcdata "Group creation"];
           XHTML.M.p [XHTML.M.pcdata
                        "You can use the form below to create a new Ocsimore \
                         group. (A group is a special form of user that is not \
                         authorized to log in.) Once this is done, you will \
                         be able to add users into your group."];
           XHTML.M.h2 [XHTML.M.pcdata "Create a new group"];
           Eliom_predefmod.Xhtml.post_form ~sp
             ~service:User_services.action_create_new_group
             (fun (usr, desc) ->
                [XHTML.M.table
                   (XHTML.M.tr
                      (XHTML.M.td [XHTML.M.pcdata "group name (letters and \
                                                   digits only)"])
                      [XHTML.M.td [str_input usr]])
                   [XHTML.M.tr
                      (XHTML.M.td [XHTML.M.pcdata "description"])
                      [XHTML.M.td [str_input desc]];
                    XHTML.M.tr
                      (XHTML.M.td [submit_input "Create"])
                      []
                   ]
                ]
             )
             ();
           XHTML.M.p [XHTML.M.strong [XHTML.M.pcdata err]]
          ]
      | false ->
          [XHTML.M.h1 [XHTML.M.pcdata "Error"];
           XHTML.M.p [XHTML.M.pcdata "You are not allowed to create new groups"]
          ]

  method display_group_creation_done sp () (name, descr) =
    Lwt.catch
      (fun () ->
         User_data.create_group ~sp ~name ~descr >>= fun groupid ->
         User_sql.get_basicuser_data groupid >>= fun group ->
         Lwt.return
           [XHTML.M.h1 [XHTML.M.pcdata "Group created"];
            XHTML.M.p [XHTML.M.pcdata "You can now ";
                       Eliom_predefmod.Xhtml.a ~sp
                         ~service:User_services.service_view_group
                         [XHTML.M.pcdata "edit"] group.user_login;
                       XHTML.M.pcdata " your new group."
                      ];
           ]
      )
      (function
         | Failure err -> self#display_group_creation ~err ~sp
         | Ocsimore_common.Permission_denied ->
             Lwt.return
               [XHTML.M.h1 [XHTML.M.pcdata "Error"];
                XHTML.M.p [XHTML.M.pcdata "You cannot create new groups"];
               ]
         | e -> Lwt.fail e)

end


(* The [services] argument should be changed from a tuple into a first-class
   module as soon as this is available in Caml *)
class user_widget_user_creation user_creation_options : user_widget_user_creation_class =
object (self)

  method private login_box_extension ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >|= function
      | true ->
          [XHTML.M.tr
             (XHTML.M.td ~a:[XHTML.M.a_colspan 2]
                [Eliom_predefmod.Xhtml.a
                   User_services.service_create_new_user
                   sp [XHTML.M.pcdata "New user? Register now!" ] ()
                ]
             )
             []
          ]
      | false -> []

  method display_user_creation ?(err="") ~sp =
    User_data.can_create_user ~sp ~options:user_creation_options >|= function
      | true ->
          [XHTML.M.h1 [XHTML.M.pcdata "User creation"];
           XHTML.M.p [XHTML.M.pcdata "Use the form below to create a new \
                                       Ocsimore user.";
                      XHTML.M.br ();
                      XHTML.M.pcdata "Note that users that authenticate \
                                      through external means (NIS or PAM) are \
                                      added automatically the first time they \
                                      log in inside Ocsimore, and you do not \
                                      need to create them";
                     ];
           XHTML.M.h2 [XHTML.M.pcdata "Create a new user"];
           XHTML.M.p [XHTML.M.pcdata "Please fill in the following fields.";
                      XHTML.M.br ();
                      XHTML.M.pcdata "Be very careful to enter a valid e-mail \
                                      address, as the confirmation url will be \
                                      sent there.";
                     ];
           Eliom_predefmod.Xhtml.post_form ~sp
             ~service:User_services.action_create_new_user
             (fun (usr,(desc,(email, (pass1, pass2)))) ->
                [XHTML.M.table
                   (XHTML.M.tr
                      (XHTML.M.td
                         [XHTML.M.pcdata "login name: (letters & digits only)"])
                      [XHTML.M.td [str_input usr]]
                   )
                   [XHTML.M.tr
                      (XHTML.M.td [XHTML.M.pcdata "real name:"])
                      [XHTML.M.td [str_input desc]];
                    XHTML.M.tr
                      (XHTML.M.td [XHTML.M.pcdata "e-mail address:"])
                      [XHTML.M.td [str_input email]];
                    XHTML.M.tr
                      (XHTML.M.td [XHTML.M.pcdata "password:"])
                      [XHTML.M.td [passwd_input pass1; passwd_input pass2]];
                    XHTML.M.tr (XHTML.M.td [submit_input "Register"]) [];
                   ]
                ]
             ) ();
           XHTML.M.p [XHTML.M.strong [XHTML.M.pcdata err]];
          ]
      | false ->
          [XHTML.M.h1 [XHTML.M.pcdata "Error"];
           XHTML.M.p [XHTML.M.pcdata "You are not allowed to create new users"];
          ]

  method display_user_creation_done ~sp ~name  ~fullname ~email ~pwd =
    Lwt.catch
      (fun () ->
         if fst pwd <> snd pwd then
           Lwt.fail (Failure "You must enter the same password twice")
         else
           User_services.create_user ~sp ~name ~fullname ~email ~pwd:(fst pwd)
             ~options:user_creation_options () >|= fun () ->
           [XHTML.M.h1 [XHTML.M.pcdata "User creation successful"];
            XHTML.M.p [XHTML.M.pcdata "You will receive an activation e-mail \
                                       at the following address:";
                       XHTML.M.br ();
                       XHTML.M.em [XHTML.M.pcdata email];
                      ];
           ]
      )
      (function
         | Failure err -> self#display_user_creation ~err ~sp
         | Ocsimore_common.Permission_denied ->
             Lwt.return
               [XHTML.M.h1 [XHTML.M.pcdata "Error"];
                XHTML.M.p [XHTML.M.pcdata "You cannot create new users"];
               ]
         | e -> Lwt.fail e)
end
