
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
                (fun () -> Ocsigen_stream.empty None))
   }

module Reg_ = struct
  type page = Icalendar.t
  type options
  let send ?options ?(cookies=[]) ?charset ?code ~sp content =
    Lwt.return (Eliom_services.EliomResult (result_of_content content))
end

include Eliom_mkreg.MakeRegister (Reg_)
