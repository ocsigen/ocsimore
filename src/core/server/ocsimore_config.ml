let opt s = if s = "" then None else Some s

let db_user = ref ""
let db_name = ref ""
let db_host = ref (opt "")
let db_port = ref (Eliom_lib.Option.map int_of_string None)
let db_unix_domain_socket_dir = ref (opt "")
let db_password = ref (opt "")

let admin_dir = ref "ocsimoreadmin"

let mailer = ref "/usr/sbin/sendmail"

let application_name = ref "ocsimore"

let aggregate_css = ref true

let wiki_headings_backref = ref true

let dyngroupstobecreated = ref ([] : (string * Simplexmlparser.xml) list)
