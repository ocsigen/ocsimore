
let get_etag c = Some (Digest.to_hex (Digest.string c))

open Ocsigen_http_frame

let result_of_content (url, id, el) headers =
  let c = Atom_feed.simple_feed url id el in
  let md5 = get_etag c in
  let dr = default_result () in
    {dr with
     res_content_length = Some (Int64.of_int (String.length c));
     res_content_type = Some "application/atom+xml";
     res_etag = md5;
     res_headers= (match headers with
                         | None -> dr.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers dr.res_headers
                      );
     res_stream =
        (Ocsigen_stream.make
            (fun () ->
               Ocsigen_stream.cont c
                 (fun () -> Ocsigen_stream.empty None)), None)
   }

module Reg_ = struct
  type page = (string * Atom_feed.identity * Atom_feed.entry list)
  type options
  let send ?options:_ ?(cookies=[]) ?charset:_ ?code:_ ?content_type:_ ?headers ~sp:_ content =
    Lwt.return (result_of_content content headers)
end

include Eliom_mkreg.MakeRegister (Reg_)
