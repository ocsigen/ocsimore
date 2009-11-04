(* YYY : extend... *)


type messages = {
  (* Input : name of the user of the wiki *)
  wikiperso_wikidescr: (string -> string, unit, string) format;

  page_does_not_exist: string;

  wikitext_edition_conflict1: string;
  wikitext_edition_conflict2: string;
  css_edition_conflict: string;
}


let messages_english = {
  wikiperso_wikidescr = "Personal wiki of %s";

  page_does_not_exist = "That page does not exist";

  wikitext_edition_conflict1 =
    "the content of this wikibox has been updated since you started editing \
     it. The preview and the code below reflect your modifications, not the \
     current wiki version.";

  wikitext_edition_conflict2 =
    "If you save your changes, you will overwrite the updated version of \
     the page currently in the wiki.";

  css_edition_conflict =
    "this CSS has been updated since you started editing it. If you save \
     your changes, you will overwrite the version of the CSS currently in \
     the wiki.";
}

let messages_french = {
  wikiperso_wikidescr = "Wiki personnel de %s";

  page_does_not_exist = "Cette page n'existe pas";

  wikitext_edition_conflict1 =
    "Le contenu de cette wikibox a changé depuis que vous avez commencé à la \
     modifier. L'aperçu et le code ci-dessous montrent vos modifications, \
     et non la version actuellement dans le wiki.";

  wikitext_edition_conflict2 =
    "Si vous sauvegardez vos changements, vous écraserez la version de la \
     boite se trouvant actuellement dans le wiki.";

  css_edition_conflict =
    "cette CSS a été éditée depuis que vous avez commencé à la modifier. \
     Si vous sauvegardez vos changements, vous écraserez la version actuelle \
     de la CSS.";
}


let messages = ref messages_english
