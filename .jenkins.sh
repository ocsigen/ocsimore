opam pin add --no-action ocsimore .
opam pin add --no-action ocsigenserver 'https://github.com/ocsigen/ocsigenserver.git#master'
opam pin add --no-action eliom 'https://github.com/ocsigen/macaque.git#master'
EDITOR="sed -i s/0.6.1/dev/" opam pin add --no-action macaque 'https://github.com/ocsigen/macaque.git#master'
opam install --deps-only ocsimore
opam install --verbose ocsimore
opam remove --verbose ocsimore
