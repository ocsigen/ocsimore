OCSIMORE_SRC = setOfSets.ml users.ml ocsimorelib.ml sessionManager.ml forum.ml \
	wikiparser.ml wiki.ml
OCSIMORE_MLI = forum.mli ocsimorelib.mli sessionManager.mli setOfSets.mli \
	users.mli wiki.mli wikiparser.mli
OCSIMORE_CMO = $(OCSIMORE_SRC:.ml=.cmo)
OCSIMORE_CMI = $(OCSIMORE_MLI:.mli=.cmi)


EXTLIB = $(shell ocamlfind query extlib)/extLib.cma
PCRE  = $(shell ocamlfind query pcre)/pcre.cma
UNIX = $(shell ocamlfind query unix)/unix.cma
STR = $(shell ocamlfind query str)/str.cma
CALENDAR = $(shell ocamlfind query calendar)/calendar.cma
CSV = $(shell ocamlfind query csv)/csv.cma
PGOCAML_EXT = $(STR) $(EXTLIB) $(PCRE) $(UNIX) $(CALENDAR) $(CSV) \
	$(shell ocamlfind query pgocaml)/pgocaml.cma \
	$(shell ocamlfind query pgocaml)/pa_pgsql.cmo
PP = -pp "camlp4orf $(PGOCAML_EXT) -loc loc"

PACKAGES = -package calendar,ocsigen,pgocaml

.PHONY: all depend clean

all: ocsimore.cma

ocsimore.cma: $(OCSIMORE_CMO) sql.cmo $(OCSIMORE_CMI) sql.cmi
	ocamlfind ocamlc -o $@ -a sql.cmo $(OCSIMORE_CMO) 

sql.cmo: sql.ml
	PGHOST=$(HOST) PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamlc $(PACKAGES) $(PP) -c $<

%.cmo: %.ml
	ocamlducefind ocamlc $(PACKAGES) -c $<	

%.cmi: %.mli
	ocamlducefind ocamlc $(PACKAGES) -c $<	

depend:
	ocamlducefind ocamldep $(OCSIMORE_SRC) $(OCSIMORE_MLI) > .depend
	PGHOST=$(HOST) PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamldep $(PP) sql.ml sql.mli >> .depend

clean:
	rm -f $(OCSIMORE_CMO) $(OCSIMORE_CMI) sql.cmo sql.cmi ocsimore.cma

include .depend
