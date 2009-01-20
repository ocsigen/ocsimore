
let get_etag c = Some (Digest.to_hex (Digest.string c))

open Ocsigen_http_frame

let result_of_content (url, id, el) =
  let c = Atom_feed.simple_feed url id el in
  let md5 = get_etag c in
  let default_result = default_result () in
    {default_result with
     res_content_length = Some (Int64.of_int (String.length c));
     res_content_type = Some "application/atom+xml";
     res_etag = md5;
     res_headers= Http_headers.dyn_headers;
     res_stream =
         Ocsigen_stream.make
           (fun () ->
              Ocsigen_stream.cont c
                (fun () -> Ocsigen_stream.empty None))
   }

module Reg_ = struct
  type page = (string * Atom_feed.identity * Atom_feed.entry list)
  type options
  let send ?options ?(cookies=[]) ?charset ?code ~sp content =
    Lwt.return (Eliom_services.EliomResult (result_of_content content))
end

include Eliom_mkreg.MakeRegister (Reg_)
