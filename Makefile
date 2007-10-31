OCSIMORE_SRC = setOfSets.ml users.ml services.ml sessionManager.ml \
	ocsimorelib.ml widget.ml forumWidget.ml forum.ml # wikiparser.ml wiki.ml
OCSIMORE_MLI = widget.mli forumWidget.mli forum.mli \
	sessionManager.mli setOfSets.mli services.mli users.mli \
	# wiki.mli wikiparser.mli
OCSIMORE_CMO = $(OCSIMORE_SRC:.ml=.cmo)
OCSIMORE_CMI = $(OCSIMORE_MLI:.mli=.cmi)

HOST = localhost
USER = ocsigen
DATABASE = ocsimore

EXTLIB = $(shell ocamlfind query extlib)/extLib.cma
PCRE  = $(shell ocamlfind query pcre)/pcre.cma
UNIX = $(shell ocamlfind query unix)/unix.cma
STR = $(shell ocamlfind query str)/str.cma
CALENDAR = $(shell ocamlfind query calendar)/calendar.cma
CSV = $(shell ocamlfind query csv)/csv.cma
#PP = -pp "camlp4orf $(PGOCAML_EXT) -loc loc"
PP=-syntax camlp4o

PACKAGES = -package calendar,pgocaml,pgocaml.statements,ocsigen
LINKPKG = -package calendar,ocsigen,pgocaml

.PHONY: all depend clean

all: ocsimore.cma

doc:
	ocamlducefind ocamldoc $(PACKAGES) -html -d html $(OCSIMORE_MLI)

ocsimore.cma: $(OCSIMORE_CMO) sql.cmo sql.cmi $(OCSIMORE_CMI)
	ocamlducefind ocamlc -o $@ -a sql.cmo $(OCSIMORE_CMO) 

sql.cmo: sql.ml
	PGHOST=$(HOST) PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamlc $(PACKAGES) $(PP) -c $<

print_sql:
	PGHOST=$(HOST) PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	camlp4o.byte \
	$(shell ocamlc -where)/str.cma \
	-I +pcre $(shell ocamlfind query pcre)/pcre.cma \
	-I +extlib $(shell ocamlfind query extlib)/extLib.cma \
	-I +calendar $(shell ocamlfind query calendar)/calendar.cma \
	-I +csv $(shell ocamlfind query csv)/csv.cma \
	-I +ssl $(shell ocamlfind query ssl)/ssl.cma \
	-I +lwt $(shell ocamlfind query lwt)/lwt.cma \
	$(shell ocamlfind query pgocaml)/pgocaml.cma \
	$(shell ocamlfind query pgocaml)/pa_pgsql.cmo pr_o.cmo sql.ml

%.cmo: %.ml
	ocamlducefind ocamlc $(PACKAGES) -c $<	

%.cmi: %.mli
	ocamlducefind ocamlc $(PACKAGES) -c $<	

depend:
	ocamlducefind ocamldep $(OCSIMORE_SRC) $(OCSIMORE_MLI) > .depend
	PGHOST=$(HOST) PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamldep $(PACKAGES) $(PP) sql.ml sql.mli >> .depend

clean:
	rm -f $(OCSIMORE_CMO) $(OCSIMORE_CMI) sql.cmo sql.cmi ocsimore.cma

include .depend
