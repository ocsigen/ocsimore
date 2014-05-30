opam pin ocsimore .
EDITOR="sed -i s/0.6.1/dev/" opam pin macaque 'https://github.com/ocsigen/macaque.git#master'
opam install --deps-only ocsimore
opam install --verbose ocsimore
opam remove --verbose ocsimore
