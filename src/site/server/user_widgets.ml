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


open Eliom_pervasives
open User_sql.Types

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(*TODO: relocate*)
let eliom_inline_class = HTML5.M.a_class ["eliom_inline"]
let accept_charset_utf8 = HTML5.M.a_accept_charset ["utf-8"]
let unopt_str = function | None -> "" | Some s -> s


let str_input ?(value="") ?(visible=true) name =
  Eliom_output.Html5.string_input ~name ~value
    ~input_type:(if visible then `Text else `Hidden)
    ()
let passwd_input ?(value="") name =
  Eliom_output.Html5.string_input
    ~input_type:`Password
    ~name
    ~value ()
let submit_input value =
  Eliom_output.Html5.string_input ~input_type:`Submit ~value ()


class type user_widget_class = object
  method display_roles :
    Eliom_output.Blocks5.page Lwt.t
  method display_groups :
    Eliom_output.Blocks5.page Lwt.t
  method display_users :
    Eliom_output.Blocks5.page Lwt.t

  method display_group :
    user * string -> Eliom_output.Blocks5.page Lwt.t

  method display_login_widget :
    ?user_prompt:string ->
    ?pwd_prompt:string ->
    ?auth_error:string ->
    ?switchtohttps:string ->
    ?show_ext:bool ->
    unit ->
    HTML5_types.form_content HTML5.M.elt Lwt.t

  method private display_logout_box :
    ?show_ext:bool ->
    User_sql.Types.userdata ->
    HTML5_types.form_content HTML5.M.elt list Lwt.t
  method display_logout_button :
    HTML5_types.button_content HTML5.M.elt list ->
    HTML5_types.form HTML5.M.elt Lwt.t
  method logout_uri : Wiki_syntax.href

  method user_link :
    string -> [`A of HTML5_types.a_content | `Form] HTML5.M.elt

  method user_list_to_xhtml :
    ?hook:(user:string ->
          [`A of HTML5_types.a_content | `Form] HTML5.M.elt list Lwt.t) ->
    User_sql.Types.user list -> HTML5_types.flow5 HTML5.M.elt Lwt.t


  (** Helper forms to add and remove users from groups. If [show_edit]
      is false, no controls to edit the permissions are shown *)
  (** Form to add users to a group *)
  method form_edit_group:
    ?show_edit:bool ->
    ?default_add:string ->
    group:user ->
    text:HTML5_types.flow5 HTML5.M.elt list ->
    unit ->
    HTML5_types.tbody_content HTML5.M.elt Lwt.t

  (** Form to add an user to a group *)
  method form_edit_user:
    user:User_sql.Types.user ->
    text:HTML5_types.flow5 HTML5.M.elt list ->
    unit ->
    HTML5_types.flow5 HTML5.M.elt list Lwt.t

  method form_edit_awr: 'a.
    text_prefix:string ->
    grps:'a User_sql.Types.admin_writer_reader ->
    arg:'a Opaque.int32_t ->
    ?defaults:string * string * string ->
    unit ->
    (  HTML5_types.tbody_content HTML5.M.elt
     * HTML5_types.tbody_content HTML5.M.elt list) Lwt.t

  method status_text:
    HTML5_types.form_content HTML5.M.elt list Lwt.t

  method display_group_creation :
    ?err:string -> unit ->
    Eliom_output.Blocks5.page Lwt.t

  method display_group_creation_done :
    unit ->
    string * string ->
    Eliom_output.Blocks5.page Lwt.t

end

class type user_widget_user_creation_class = object
  method display_user_creation :
    ?err:string ->
    unit ->
    Eliom_output.Blocks5.page Lwt.t
  method display_user_creation_done :
    name:string ->
    fullname:string ->
    email:string ->
    pwd:string*string ->
    Eliom_output.Blocks5.page Lwt.t
end



open Xform.XformLwt
open Ops


(** Widget for user login/logout/edition without addition of new users *)
class user_widget : user_widget_class =
object (self)

  val xhtml_class = "logbox"

  method form_edit_group ?(show_edit=false) ?(default_add="") ~group ~text () =
    (if show_edit then
       User.get_user_data ()                      >>= fun u ->
       let user = basic_user u.user_id in
       User_data.can_admin_group ~user ~group () >>= function
         | true ->
             User_sql.user_to_string group        >>= fun group ->
             Lwt.return
               ((fun ~user ->
                   Lwt.return
                     (self#bt_remove_user_from_group ~group ~user ())
                ),
                (self#form_add_user_to_group ~default_add ~group ())
               )
         | false ->
             Lwt.return ((fun ~user:_ -> Lwt.return []), [])
     else
       Lwt.return ((fun ~user:_ -> Lwt.return []), [])
    )                                             >>= fun (hook, add) ->

    User_sql.users_in_group ~generic:false ~group >>= fun users ->
    self#user_list_to_xhtml ~hook users       >>= fun members ->
    Lwt.return
      (HTML5.M.tr
         [HTML5.M.td ~a:[HTML5.M.a_class ["role"]] text;
	  HTML5.M.td ~a:[HTML5.M.a_class ["current_users"]] (members :: add)]
      )


  method form_edit_user ~user ~text () =
    (* YYY put back edition buttons if the user has enough rights, or if
       it is admin *)
    lwt groups = User_sql.groups_of_user ~user in
    lwt members = self#user_list_to_xhtml groups in
    Lwt.return (text @ [members])

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
        ~show_edit:true ~default_add:default
    and d1, d2, d3 = match defaults with
      | None -> "", "", ""
      | Some (d1, d2, d3) -> d1, d2, d3
    in
    aux grps.grp_admin
      [HTML5.M.p [HTML5.M.pcdata (text_prefix ^" administrators: ")]] d1 ()
                                                               >>= fun forma ->
    aux grps.grp_writer
      [HTML5.M.p [HTML5.M.pcdata (text_prefix ^" writers: ")]] d2 ()
                                                               >>= fun formw ->
    aux grps.grp_reader
      [HTML5.M.p [HTML5.M.pcdata (text_prefix ^" readers: ")]] d3 ()
                                                               >>= fun formr ->
    Lwt.return (formr, [formw; forma;])

  method private bt_remove_user_from_group ~group ~user ?(text="Remove") () =
    let str_input = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      [ HTML5.M.div ~a:[eliom_inline_class]
          [ str_input ~value:group gname;
            str_input ~value:user remname;
            str_input addname;
            Eliom_output.Html5.button ~button_type:`Submit
              [HTML5.M.pcdata text];
          ]
      ]
    in
    [Eliom_output.Html5.post_form
       ~a:[eliom_inline_class; accept_charset_utf8 ]
       ~service:User_services.action_add_remove_users_from_group mform ()]

  method private form_add_user_to_group ~group ?(default_add="") ?(text="Add") () =
    let str_input' = str_input ~visible:false in
    let mform (gname, (addname, remname)) =
      [HTML5.M.div ~a:[eliom_inline_class]
         [str_input' ~value:group gname;
          str_input' remname;
          str_input ~value:default_add addname ;
          Eliom_output.Html5.button ~button_type:`Submit
            [HTML5.M.pcdata text];
         ]
      ]
    in
    [Eliom_output.Html5.post_form
       ~a:[eliom_inline_class; accept_charset_utf8]
      ~service:User_services.action_add_remove_users_from_group mform ()]

  method user_list_to_xhtml ?hook l = match l with
    | [] -> Lwt.return
              (HTML5.M.p [HTML5.M.em [HTML5.M.pcdata "(currently no user)"]])
    | e :: es ->
        let hook = match hook with
          | None -> (fun _ -> Lwt.return [])
          | Some h -> (fun user -> h ~user)
        in
        let convert u =
          User_sql.user_to_string ~expand_param:true u >>= fun user ->
          hook user                                    >|= fun hooked ->
          HTML5.M.li ((self#user_link user) :: hooked)
        in
        convert e >>= fun e ->
        Lwt_list.fold_left_s
          (fun s u -> convert u >|= fun r -> r :: s)
          [e] es
        >|= fun l -> HTML5.M.ul ~a:[HTML5.M.a_class ["user_list"]] l


  method private login_box_aux
    ?(user_prompt= "login:")
    ?(pwd_prompt= "password:")
    ?(auth_error= "Wrong login or password")
    ?(switchtohttps= "Click here to switch to https and login")
    ?(show_ext=true)
    error =
    if (Eliom_request_info.get_ssl ()) || not User_services.force_secure
    then begin
      (if show_ext
       then self#login_box_extension
       else Lwt.return ([]: [`Tr] HTML5.M.elt list)
      ) >>= fun ext ->
      Lwt.return (fun (usr, pwd) ->
        [HTML5.M.table ~a:[HTML5.M.a_class ["login_box"]]
           (HTML5.M.tr
              [HTML5.M.td [HTML5.M.pcdata user_prompt];
               HTML5.M.td [str_input usr]])
           (   HTML5.M.tr
                 [HTML5.M.td [HTML5.M.pcdata pwd_prompt];
                  HTML5.M.td [passwd_input pwd]]
            :: HTML5.M.tr
                 [HTML5.M.td [submit_input "Login"]]
            :: ext
             @  (if error
                then [HTML5.M.tr [HTML5.M.td ~a:[HTML5.M.a_colspan 2]
                                    [HTML5.M.pcdata auth_error]]
                ]
                else []
               )
           )
        ]
      )
    end
    else
      Lwt.return (fun _ ->
        [HTML5.M.p
           [Eliom_output.Html5.a Eliom_services.https_void_coservice'
              [HTML5.M.pcdata switchtohttps] ()
           ]
        ]
      )

  method private display_logout_box ?(show_ext=true) u =
    (if show_ext
     then self#logout_box_extension
     else Lwt.return []) >>= fun ext ->
    Lwt.return [HTML5.M.table ~a:[HTML5.M.a_class["login_box"]]
                  (HTML5.M.tr
                     [HTML5.M.td
                         [HTML5.M.pcdata (Printf.sprintf "You are logged as %s"
                                            u.user_fullname)]]
                  )
                  (HTML5.M.tr
                     [HTML5.M.td
                         [submit_input "logout"]]
                   :: ext)
    ]

  method private login_box_extension = Lwt.return []

  method private logout_box_extension =
    User.get_user_data () >|= fun ud ->
    [HTML5.M.tr
       [HTML5.M.td
          [Eliom_output.Html5.a
             User_services.service_view_group
             [HTML5.M.pcdata "Manage your account"] ud.user_login
          ]]
    ]

  method display_logout_button content =
    Lwt.return
      (Eliom_output.Html5.post_form ~a:[HTML5.M.a_class ["logoutbutton"]]
         ~service:User_services.action_logout
         (fun () ->
            [HTML5.M.p
               [Eliom_output.Html5.button ~button_type:`Submit content]
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
      (Wiki_syntax.service_href (User_services.action_logout_get:>('a,'b) Wiki_syntax.wiki_service) ())

  method display_login_widget ?user_prompt ?pwd_prompt ?auth_error ?switchtohttps ?(show_ext=true) () =
    User.get_user_data () >>= fun u ->
    User.is_logged_on () >>= fun logged ->
    (if logged then
       self#display_logout_box ~show_ext u >>= fun f ->
       Lwt.return (
         Eliom_output.Html5.post_form
           ~a:[HTML5.M.a_class ["logbox"; "logged"]]
           ~service:User_services.action_logout (fun _ -> f) ())

     else
       let f_login error ~a =
         self#login_box_aux ?user_prompt ?pwd_prompt ?auth_error
           ?switchtohttps ~show_ext error
         >>= fun f ->
         Lwt.return
           (Eliom_output.Html5.post_form
              ~service:User_services.action_login ~a f ())
       in
       if List.exists
          (fun e -> e = User.BadPassword || e = User.BadUser)
          (User_data.get_login_error ())
       then (* unsuccessful attempt *)
         f_login true ~a:[HTML5.M.a_class ["logbox"; "error"]]
       else (* no login attempt yet *)
         f_login false ~a:[HTML5.M.a_class ["logbox"; "notlogged"]]
    ) >>= fun f ->
    Lwt.return (HTML5.M.div ~a:[HTML5.M.a_class [xhtml_class]] [f])


  method user_link group =
    Eliom_output.Html5.a ~service:User_services.service_view_group
      [HTML5.M.pcdata group] group


  method display_group (group, g) =
    User_sql.user_type group >>= fun gtype ->
    let ctext, text, gtypedescr = match gtype with
      | `Role  -> ("Role",  "role",  "Description")
      | `User  -> ("User",  "user",  "Name"       )
      | `Group -> ("Group", "group", "Description")
    in
    let error = match Ocsimore_common.get_action_failure () with
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
          [HTML5.M.p ~a:[HTML5.M.a_class ["errmsg"]] [HTML5.M.pcdata msg]]
    in
    let head =
      HTML5.M.h1
        [HTML5.M.pcdata ctext;
         HTML5.M.pcdata ("'" ^ g ^ "'"); (*Weird...*)
        ]
    in

    (* Adding groups to the group *)
    self#form_edit_group ~show_edit:true ~group
      ~text:[HTML5.M.p ~a:[eliom_inline_class]
               [HTML5.M.strong
                  [HTML5.M.pcdata ("Current users/groups in this "^ text ^": ")]
            ]]
      ()
    >>= fun f1  ->

    (* Adding the group to groups *)
    self#form_edit_user ~user:group
      ~text:[HTML5.M.p ~a:[HTML5.M.a_class ["eliom_inline"]]
               [HTML5.M.strong
                  [HTML5.M.pcdata ("Current groups/roles in which the " ^ text ^
                                   "is: ")
                  ]
               ]
      ]
      ()
    >>= fun f2  ->

    User_sql.get_user_data group >>= fun g ->
    User_data.can_change_user_data_by_user group >>= fun can_change ->
    let edit =
      if can_change &&
        g.user_pwd <> Connect_forbidden &&
        g.user_pwd <> External_Auth
      then
        [Eliom_output.Html5.post_form
           ~service:User_services.action_edit_user_data
           (fun (nuserid, (pwd, (pwd2, (desc, email)))) ->
              [HTML5.M.table
                 (HTML5.M.tr
                    [HTML5.M.td
                       [HTML5.M.strong [HTML5.M.pcdata (gtypedescr ^ ": ")]];
                    HTML5.M.td [str_input ~value:g.user_fullname desc]]
                 )
                 [HTML5.M.tr
                    [HTML5.M.td [HTML5.M.pcdata "e-mail adress: "];
                     HTML5.M.td [str_input ~value:(unopt_str g.user_email)
                                   email]];
                  HTML5.M.tr
                    [HTML5.M.td [passwd_input pwd ];
                     HTML5.M.td [passwd_input pwd2]];
                  HTML5.M.tr
                    [HTML5.M.td [submit_input "Confirm";
                                 Eliom_output.Html5.user_type_input
                                   string_from_userid
                                   ~input_type:`Hidden
                                   ~name:nuserid
                                   ~value:g.user_id ()
                                ]
		    ]
                 ]
              ]
           )
           ()
        ]
      else
        [HTML5.M.p [HTML5.M.strong [HTML5.M.pcdata (gtypedescr ^ ": ")];
                    HTML5.M.pcdata g.user_fullname]
        ]
    in
      Lwt.return
        (   head
         :: error
         @[ HTML5.M.div ~a:[HTML5.M.a_class ["user_block"]] edit;
            HTML5.M.div ~a:[HTML5.M.a_class ["user_block"]]
              [HTML5.M.table ~a:[HTML5.M.a_class ["users_in_group"]] f1 []];
            HTML5.M.div ~a:[HTML5.M.a_class ["user_block"]] f2;
        ])


  method display_users =
    User_sql.all_groups () >>= fun l ->
    let l = List.filter (fun {user_kind = u; user_pwd = a} ->
                           u = `BasicUser && a <> Connect_forbidden ) l in
    let l = List.sort
      (fun u1 u2 -> compare u1.user_login u2.user_login) l in

    self#display_users_groups ~show_auth:true ~l ~utype:`User >>= fun r ->

    Lwt.return [HTML5.M.h1 [HTML5.M.pcdata "Existing users"]; r]

  method display_groups =
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

    self#display_users_groups ~show_auth:false ~l ~utype:`Group >>= fun r ->

    Lwt.return [HTML5.M.h1 [HTML5.M.pcdata "Existing groups"]; r]


  (* Parameterized users *)
  method display_roles =
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
            [HTML5.M.em [HTML5.M.pcdata ("(" ^ p ^ ")")]]
        | _ -> []
      in
      HTML5.M.tr
        [HTML5.M.td [HTML5.M.strong (HTML5.M.pcdata u.user_login :: p)];
         HTML5.M.td [HTML5.M.pcdata u.user_fullname]]
    in
    let l1 = List.rev (List.fold_left (fun s arg -> line arg :: s) [] tl) in
    let t1 = HTML5.M.table ~a:[HTML5.M.a_class ["table_admin"]] (line hd) l1 in

    let form name =
      [HTML5.M.p
         [Eliom_output.Html5.string_input ~name ~input_type:`Text ();
          Eliom_output.Html5.button ~button_type:`Submit
            [HTML5.M.pcdata "Edit this role"];
         ]
      ]
    in
    let f =
      Eliom_output.Html5.get_form ~a:[accept_charset_utf8]
        ~service:User_services.service_view_group form
    in
    let msg2 =
      "Choose one group, and enter it (including its parameter if needed) below"
    in
    Lwt.return
      [HTML5.M.h1 [HTML5.M.pcdata "Roles"];
       t1;
       HTML5.M.p [HTML5.M.pcdata msg2];
       f
      ]

  method private display_users_groups ~show_auth ~utype ~l =
    let line2 u =
      let l =
        Eliom_output.Html5.a ~service:User_services.service_view_group
          [Page_site.icon ~path:"imgedit.png" ~text:"Details"]
          u.user_login
      in
      let aa =
        if show_auth
        then [HTML5.M.td
                [HTML5.M.pcdata
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
      HTML5.M.tr
        ( [HTML5.M.td ~a:[HTML5.M.a_class ["userlogin"]]
              [HTML5.M.strong [HTML5.M.pcdata u.user_login]];
           HTML5.M.td ~a:[HTML5.M.a_class ["userdescr"]]
             [HTML5.M.pcdata u.user_fullname]]
          @ aa
          @ [HTML5.M.td [l]] )
    in
    let l = List.rev (List.fold_left (fun s arg -> line2 arg :: s) [] l) in
    Lwt.return
      (HTML5.M.table ~a:[HTML5.M.a_class ["table_admin"]]
         (HTML5.M.tr
            ([HTML5.M.th [HTML5.M.pcdata "Login"];
             HTML5.M.th
               [HTML5.M.pcdata (match utype with
                 | `User -> "Name"
                 | `Group -> "Description")]]
             @ (if show_auth
               then [HTML5.M.th [HTML5.M.pcdata "Authentication"]]
               else [])
            )
         )
         l
      )


  method status_text =
    self#display_login_widget
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
        let l = Eliom_duce.Html5.a User_services.service_login sp {{ "Login" }} () in
        Lwt.return {{ ['You are not currently logged. ' l]  }} *)


  method display_group_creation ?(err="") () =
    User_data.can_create_group () >|= function
      | true ->
          [HTML5.M.h1 [HTML5.M.pcdata "Group creation"];
           HTML5.M.p [HTML5.M.pcdata
                        "You can use the form below to create a new Ocsimore \
                         group. (A group is a special form of user that is not \
                         authorized to log in.) Once this is done, you will \
                         be able to add users into your group."];
           HTML5.M.h2 [HTML5.M.pcdata "Create a new group"];
           Eliom_output.Html5.post_form
             ~service:User_services.action_create_new_group
             (fun (usr, desc) ->
                [HTML5.M.table
                   (HTML5.M.tr
                      [HTML5.M.td [HTML5.M.pcdata "group name (letters and \
                                                   digits only)"];
                       HTML5.M.td [str_input usr]])
                   [HTML5.M.tr
                      [HTML5.M.td [HTML5.M.pcdata "description"];
                       HTML5.M.td [str_input desc]];
                    HTML5.M.tr
                      [HTML5.M.td [submit_input "Create"]]
                   ]
                ]
             )
             ();
           HTML5.M.p [HTML5.M.strong [HTML5.M.pcdata err]]
          ]
      | false ->
          [HTML5.M.h1 [HTML5.M.pcdata "Error"];
           HTML5.M.p [HTML5.M.pcdata "You are not allowed to create new groups"]
          ]

  method display_group_creation_done () (name, descr) =
    Lwt.catch
      (fun () ->
         User_data.create_group ~name ~descr >>= fun groupid ->
         User_sql.get_basicuser_data groupid >>= fun group ->
         Lwt.return
           [HTML5.M.h1 [HTML5.M.pcdata "Group created"];
            HTML5.M.p [HTML5.M.pcdata "You can now ";
                       Eliom_output.Html5.a
                         ~service:User_services.service_view_group
                         [HTML5.M.pcdata "edit"] group.user_login;
                       HTML5.M.pcdata " your new group."
                      ];
           ]
      )
      (function
         | Failure err -> self#display_group_creation ~err ()
         | Ocsimore_common.Permission_denied ->
             Lwt.return
               [HTML5.M.h1 [HTML5.M.pcdata "Error"];
                HTML5.M.p [HTML5.M.pcdata "You cannot create new groups"];
               ]
         | e -> Lwt.fail e)

end


(* The [services] argument should be changed from a tuple into a first-class
   module as soon as this is available in Caml *)
class user_widget_user_creation user_creation_options : user_widget_user_creation_class =
object (self)

  method private login_box_extension =
    User_data.can_create_user ~options:user_creation_options >|= function
      | true ->
          [HTML5.M.tr
             [HTML5.M.td ~a:[HTML5.M.a_colspan 2]
                [Eliom_output.Html5.a
                   User_services.service_create_new_user
                   [HTML5.M.pcdata "New user? Register now!" ] ()
                ]
             ]
          ]
      | false -> []

  method display_user_creation ?(err="") () =
    User_data.can_create_user ~options:user_creation_options >|= function
      | true ->
          [HTML5.M.h1 [HTML5.M.pcdata "User creation"];
           HTML5.M.p [HTML5.M.pcdata "Use the form below to create a new \
                                       Ocsimore user.";
                      HTML5.M.br ();
                      HTML5.M.pcdata "Note that users that authenticate \
                                      through external means (NIS or PAM) are \
                                      added automatically the first time they \
                                      log in inside Ocsimore, and you do not \
                                      need to create them";
                     ];
           HTML5.M.h2 [HTML5.M.pcdata "Create a new user"];
           HTML5.M.p [HTML5.M.pcdata "Please fill in the following fields.";
                      HTML5.M.br ();
                      HTML5.M.pcdata "Be very careful to enter a valid e-mail \
                                      address, as the confirmation url will be \
                                      sent there.";
                     ];
           Eliom_output.Html5.post_form
             ~service:User_services.action_create_new_user
             (fun (usr,(desc,(email, (pass1, pass2)))) ->
                [HTML5.M.table
                   (HTML5.M.tr
                      [HTML5.M.td
                         [HTML5.M.pcdata "login name: (letters & digits only)"];
                       HTML5.M.td [str_input usr]]
                   )
                   [HTML5.M.tr
                      [HTML5.M.td [HTML5.M.pcdata "real name:"];
                       HTML5.M.td [str_input desc]];
                    HTML5.M.tr
                      [HTML5.M.td [HTML5.M.pcdata "e-mail address:"];
                       HTML5.M.td [str_input email]];
                    HTML5.M.tr
                      [HTML5.M.td [HTML5.M.pcdata "password:"];
                       HTML5.M.td [passwd_input pass1; passwd_input pass2]];
                    HTML5.M.tr [HTML5.M.td [submit_input "Register"]];
                   ]
                ]
             ) ();
           HTML5.M.p [HTML5.M.strong [HTML5.M.pcdata err]];
          ]
      | false ->
          [HTML5.M.h1 [HTML5.M.pcdata "Error"];
           HTML5.M.p [HTML5.M.pcdata "You are not allowed to create new users"];
          ]

  method display_user_creation_done ~name  ~fullname ~email ~pwd =
    Lwt.catch
      (fun () ->
         if fst pwd <> snd pwd then
           Lwt.fail (Failure "You must enter the same password twice")
         else
           User_services.create_user ~name ~fullname ~email ~pwd:(fst pwd)
             ~options:user_creation_options () >|= fun () ->
           [HTML5.M.h1 [HTML5.M.pcdata "User creation successful"];
            HTML5.M.p [HTML5.M.pcdata "You will receive an activation e-mail \
                                       at the following address:";
                       HTML5.M.br ();
                       HTML5.M.em [HTML5.M.pcdata email];
                      ];
           ]
      )
      (function
         | Failure err -> self#display_user_creation ~err ()
         | Ocsimore_common.Permission_denied ->
             Lwt.return
               [HTML5.M.h1 [HTML5.M.pcdata "Error"];
                HTML5.M.p [HTML5.M.pcdata "You cannot create new users"];
               ]
         | e -> Lwt.fail e)
end
