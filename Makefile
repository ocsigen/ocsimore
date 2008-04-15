include Makefile.config

OCSIMORE_SRC1 = ocsimore_config.ml
OCSIMORE_SRC2 = setOfSets.ml users.ml \
	forum.ml sessionManager.ml \
	ocsimorelib.ml widget.ml user_widget.ml forumWidget.ml \
	wikiparser.ml wiki.ml
# services.ml
OCSIMORE_MLI = widget.mli forumWidget.mli forum.mli \
	sessionManager.mli setOfSets.mli users.mli user_widget.mli \
	wiki.mli wikiparser.mli
#services.mli 
OCSIMORE_CMO = $(OCSIMORE_SRC1:.ml=.cmo) sql.cmo $(OCSIMORE_SRC2:.ml=.cmo)
OCSIMORE_CMI = $(OCSIMORE_MLI:.mli=.cmi)

#HOST = localhost
DATABASE = ocsimore
CAMLP4O = camlp4o 
#camlp4o.byte


EXTLIB = $(shell ocamlfind query extlib)/extLib.cma
PCRE  = $(shell ocamlfind query pcre)/pcre.cma
UNIX = $(shell ocamlfind query unix)/unix.cma
STR = $(shell ocamlfind query str)/str.cma
CALENDAR = $(shell ocamlfind query calendar)/calendarLib.cmo
CSV = $(shell ocamlfind query csv)/csv.cma
PP = -pp "$(CAMLP4O) -I $(shell ocamlfind query extlib) \
	-I $(shell ocamlfind query pcre) \
	-I $(shell ocamlfind query calendar) \
	-I $(shell ocamlfind query csv) \
	-I $(shell ocamlfind query ssl) \
	-I $(shell ocamlfind query lwt) \
	-I $(shell ocamlfind query threads)/threads \
	-I $(shell ocamlfind query pgocaml) \
	extLib.cma pcre.cma str.cma calendarLib.cmo csv.cma ssl.cma threads.cma lwt.cma \
	pgocaml.cma lwt_pa_pgsql.cmo -loc loc"
#PP=-syntax camlp4o

PACKAGES = -package calendar,lwt,pgocaml,pgocaml.statements,ocsigen
LINKPKG = -package calendar,lwt,ocsigen,pgocaml

.PHONY: all depend clean

all: createdb.sql ocsimore.cma

doc:
	ocamlducefind ocamldoc $(PACKAGES) -html -d html $(OCSIMORE_MLI)

ocsimore.cma: $(OCSIMORE_CMO) $(OCSIMORE_CMI)
	ocamlducefind ocamlc -thread -o $@ -a $(OCSIMORE_CMO) 

sql.cmo: sql.ml
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamlc -verbose -thread $(PACKAGES) $(PP) -c $<

print_sql:
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	$(CAMLP4O) \
	$(shell ocamlc -where)/str.cma \
	-I +threads $(shell ocamlfind query threads)/threads/threads.cma \
	-I +pcre $(shell ocamlfind query pcre)/pcre.cma \
	-I +extlib $(shell ocamlfind query extlib)/extLib.cma \
	-I +calendar $(shell ocamlfind query calendar)/calendarLib.cmo \
	-I +csv $(shell ocamlfind query csv)/csv.cma \
	-I +ssl $(shell ocamlfind query ssl)/ssl.cma \
	-I +lwt $(shell ocamlfind query lwt)/lwt.cma \
	$(shell ocamlfind query pgocaml)/pgocaml.cma \
	$(shell ocamlfind query pgocaml)/lwt_pa_pgsql.cmo pr_o.cmo sql.ml

ocsimore_config.ml: ocsimore_config.ml.in
	sed "s/%%USER%%/$(USER)/g" ocsimore_config.ml.in > ocsimore_config.ml

createdb.sql: createdb.sql.in
	sed "s/%%USER%%/$(USER)/g" createdb.sql.in > createdb.sql


%.cmo: %.ml
	ocamlducefind ocamlc -thread $(PACKAGES) -c $<	

%.cmi: %.mli
	ocamlducefind ocamlc -thread $(PACKAGES) -c $<	

depend:
	ocamlducefind ocamldep $(OCSIMORE_SRC1) $(OCSIMORE_SRC2) $(OCSIMORE_MLI) > .depend
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamldep $(PACKAGES) $(PP) sql.ml sql.mli >> .depend

clean:
	rm -f $(OCSIMORE_CMO) $(OCSIMORE_CMI) sql.cmo sql.cmi ocsimore.cma createdb.sql ocsimore_config.ml

include .depend
