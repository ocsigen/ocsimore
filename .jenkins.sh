opam pin ocsimore .
EDITOR="sed -i s/0.6.1/dev/" opam pin macaque 'https://github.com/ocsigen/macaque.git#master'
opam pin eliom https://github.com/ocsigen/eliom.git#serv_typ
opam pin tyxml https://github.com/ocsigen/tyxml.git
opam pin ocsigenserver https://github.com/ocsigen/ocsigenserver.git
opam pin js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git
opam install --deps-only ocsimore
opam install --verbose ocsimore
opam remove --verbose ocsimore
