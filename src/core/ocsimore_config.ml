let user = "ocsimore"

let password = ref (try Sys.getenv "PGPASSWORD" with Not_found -> "")

let dyngroupstobecreated = ref ([] : (string * Simplexmlparser.xml) list)
