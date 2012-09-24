open Lwt

(*XXX Can we share with Eliom_atom ?*)

let get_etag c = Some (Digest.to_hex (Digest.string c))

open Ocsigen_http_frame

let result_of_content c =
  let c = Icalendar.calendar c in
  let md5 = get_etag c in
  let default_result = default_result () in
    {default_result with
     res_content_length = Some (Int64.of_int (String.length c));
     res_content_type = Some "text/calendar";
     res_etag = md5;
     res_headers= Http_headers.dyn_headers;
     res_stream =
        (Ocsigen_stream.make
           (fun () ->
              Ocsigen_stream.cont c
                (fun () -> Ocsigen_stream.empty None)),
         None)
   }

module Reg_ = struct
  type page = Icalendar.t
  type options
  let send ?options ?(cookies=[]) ?charset 
      ?code ?content_type ?headers ~sp content =
    let r = result_of_content content in
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= (match code with
                      | None -> 200
                      | Some c -> c);
         res_charset= (match charset with
                         | None ->
                             Some (Eliom_sessions.get_config_default_charset sp)
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers -> 
                             Http_headers.with_defaults headers r.res_headers
                      );
      }
end

include Eliom_mkreg.MakeRegister (Reg_)
