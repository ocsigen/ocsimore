
opam pin add --no-action ocsimore .
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git#master'
opam pin add --no-action js_of_ocaml 'https://github.com/ocsigen/js_of_ocaml.git#master'
opam pin add --no-action eliom 'https://github.com/ocsigen/eliom.git#master'
EDITOR="sed -i s/0.6.1/dev/" opam pin add --no-action macaque 'https://github.com/ocsigen/macaque.git#master'
opam install --deps-only ocsimore
opam install --verbose ocsimore

do_build_doc () {
  rm -rf _build/api.wikidocdir
  make wiki-api
  cp -Rf _build/api.wikidocdir/*.wiki ${API_DIR}/
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}/
}

do_remove () {
  opam remove --verbose ocsimore
}
