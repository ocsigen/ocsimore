open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Lwt

let wiki = new_service
  ~url:["wiki"]
  ~prefix:false
  ~get_params:unit
  ()

let wikisuffix = new_service
  ~url:["wiki"]
  ~prefix:true
  ~get_params:suffix_only
  ()

let myform = fun sfx ->
  [p [pcdata "enter text:";
      string_input sfx;
      submit_input "ok"]]

let _ = 
  register_service 
    wiki 
    (fun sp () () -> return
       (html
          (head (title (pcdata "prova")) [])
          (body [get_form wikisuffix sp myform;
                 p[a wikisuffix sp [pcdata "Clik here"] ("a/b/   c/d")]])));
  register_service
    wikisuffix
    (fun sp sfx () -> return
       (html
          (head (title (pcdata "prova")) [])
          (body [h1[pcdata "Wikisuffix:"];
                 pre[pcdata ("suffix is:" ^ sfx)];
		 p[a wiki sp [pcdata "Main"] ()]])))
  
