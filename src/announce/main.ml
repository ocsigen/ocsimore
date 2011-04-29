(*-*-coding: utf-8;-*-*)

module P = Eliom_parameters
module M = Eliom_duce.Xhtml
let (>>=) = Lwt.(>>=)
let str = Ocamlduce.Utf8.make

let _ =
  M.register_new_service
    ~path:[""]
    ~get_params:P.unit
    (fun sp () () ->
       Lwt.return
         {{<html xmlns="http://www.w3.org/1999/xhtml">[
             {:Common.head sp "":}
             <body>[<h1>{:str "":}
                    <ul>[
                      <li>[{:M.a ~service:Agenda.main ~sp (str "Agenda") ():}]
                      <li>[{:M.a Seminaire.groupes sp (str "Exposés") ():}]]]
           ]}})
