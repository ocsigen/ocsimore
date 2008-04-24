include Makefile.config

# cannot use ocamlduce with camlp4 :-(
OCSIMORE_SRC1 = ocsimore_config.ml
OCSIMORE_SRC2 = users.ml
OCSIMORE_SRC3 = forum.ml session_manager.ml \
	ocsimorelib.ml widget.ml user_widgets.ml forum_widgets.ml \
	wiki_parser.ml wiki.ml wiki_widgets.ml

OCSIMORE_SQL1 = sql.ml user_sql.ml
OCSIMORE_SQL2 = forum_sql.ml wiki_sql.ml

OCSIMORE_SRC = $(OCSIMORE_SRC1) $(OCSIMORE_SQL1) $(OCSIMORE_SRC2) \
               $(OCSIMORE_SQL2) $(OCSIMORE_SRC3)

OCSIMORE_SRC_NOSQL = $(OCSIMORE_SRC1) $(OCSIMORE_SRC2) $(OCSIMORE_SRC3)

OCSIMORE_SQL = $(OCSIMORE_SQL1) $(OCSIMORE_SQL2)

# services.ml
OCSIMORE_MLI = widget.mli forum_widgets.mli forum.mli \
	session_manager.mli setOfSets.mli users.mli user_widgets.mli \
	wiki.mli wiki_parser.mli wiki_widgets.mli
#services.mli 
OCSIMORE_CMO = $(OCSIMORE_SRC:.ml=.cmo)
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
	pgocaml.cma pa_pgsql.cmo -loc loc"
#PP=-syntax camlp4o

PACKAGES = -package calendar,lwt,pgocaml,pgocaml.statements,ocsigen
LINKPKG = -package calendar,lwt,ocsigen,pgocaml

.PHONY: all depend clean

all: sql ocsimore.cma

doc:
	ocamlducefind ocamldoc $(PACKAGES) -html -d html $(OCSIMORE_MLI)

ocsimore.cma: $(OCSIMORE_CMO) $(OCSIMORE_CMI)
	ocamlducefind ocamlc -thread -o $@ -a $(OCSIMORE_CMO) 

sql.cmo: sql.ml
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamlc -verbose -thread $(PACKAGES) $(PP) -c $<

%sql.cmo: %sql.ml
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
	$(shell ocamlfind query pgocaml)/pa_pgsql.cmo pr_o.cmo sql.ml

ocsimore_config.ml: ocsimore_config.ml.in
	sed "s/%%USER%%/$(USER)/g" ocsimore_config.ml.in > ocsimore_config.ml

sql: createdb.sql.in user_createdb.sql.in forum_createdb.sql.in wiki_createdb.sql.in
	sed "s/%%USER%%/$(USER)/g" createdb.sql.in > createdb.sql
	sed "s/%%USER%%/$(USER)/g" user_createdb.sql.in > user_createdb.sql
	sed "s/%%USER%%/$(USER)/g" forum_createdb.sql.in > forum_createdb.sql
	sed "s/%%USER%%/$(USER)/g" wiki_createdb.sql.in > wiki_createdb.sql


%.cmo: %.ml
	ocamlducefind ocamlc -thread $(PACKAGES) -c $<	

%.cmi: %.mli
	ocamlducefind ocamlc -thread $(PACKAGES) -c $<	

depend:
	ocamlducefind ocamldep $(OCSIMORE_SRC_NOSQL) $(OCSIMORE_MLI) > .depend
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) \
	ocamlfind ocamldep $(PACKAGES) $(PP) $(OCSIMORE_SQL) $(OCSIMORE_SQL:.ml=.mli) >> .depend

clean:
	rm -f *.cmo *.cmi *.cma *.sql ocsimore_config.ml

include .depend
