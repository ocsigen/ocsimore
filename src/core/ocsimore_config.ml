let db_user = ref "ocsimore"
let db_name = ref "ocsimore"

let password = ref (try Sys.getenv "PGPASSWORD" with Not_found -> "")

let dyngroupstobecreated = ref ([] : (string * Simplexmlparser.xml) list)
