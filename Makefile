# OASIS_START
# DO NOT EDIT (digest: 46f8bd9984975bd4727bed22d0876cd2)

SETUP = ./setup.exe

build: setup.data $(SETUP)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data $(SETUP) build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data $(SETUP) build
	$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	$(SETUP) -all $(ALLFLAGS)

install: setup.data $(SETUP)
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data $(SETUP)
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data $(SETUP)
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP)
	$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	$(RM) $(SETUP)

setup.data: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: $(SETUP)
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.exe: setup.ml
	ocamlfind ocamlopt -o $@ $< || ocamlfind ocamlc -o $@ $< || true
	$(RM) setup.cmi setup.cmo setup.cmx setup.o

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

LOG_DIR = ./local/var/log
LIB_DIR = ./local/var/lib

run: install-data $(LOG_DIR) $(LIB_DIR)
	CAML_LD_LIBRARY_PATH="${CAML_LD_LIBRARY_PATH}:_build/src/core" ocsigenserver -c local/etc/ocsigen/ocsimore.conf -v

restart: install-data
	echo restart > /tmp/cpipe

STATIC_DIR = ./local/var/www/static/

.PHONY:
install-data: ${STATIC_DIR}
	cp ./_build/src/site/client/ocsimore.js $<

${STATIC_DIR}:
	mkdir -p $@

${LOG_DIR}:
	mkdir -p $@

${LIB_DIR}:
	mkdir -p $@
