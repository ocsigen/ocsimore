(* XXX : extend... *)


type messages = {
  (* Input : name of the user of the wiki *)
  wikiperso_wikidescr: (string -> string, unit, string) format;

  page_does_not_exist: string;
}


let messages_english = {
  wikiperso_wikidescr = "Personal wiki of %s";

  page_does_not_exist = "That page does not exist";
}

let messages_french = {
  wikiperso_wikidescr = "Wiki personnel de %s";

  page_does_not_exist = "Cette page n'existe pas";
}


let messages = ref messages_english
