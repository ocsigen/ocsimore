opam pin ocsimore .
opam pin macaque 'https://github.com/ocsigen/macaque.git#master'
opam install --deps-only ocsimore
opam install --verbose ocsimore
opam remove --verbose ocsimore
