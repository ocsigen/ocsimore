include Makefile.config

ifeq "$(PAM)" "YES"
PAM= ocsimore_pam.ml
PAMPACKAGE=,pam
else
PAM=
PAMPACKAGE=
endif

CAMLC = ocamlc -w v -g -dtypes -I nis_chkpwd
CAMLLEX = ocamllex

# cannot use ocamlduce with camlp4 :-(
OCSIMORE_SRC1 = opaque.ml polytables.ml cache.ml language.ml	\
        ocsimore_config.ml parse_config.ml ocsimore_common.ml	\
        ocsimore_lib.ml
OCSIMORE_SRC2 = user_cache.ml users.ml
OCSIMORE_SRC3 = forum.ml ocsimore_nis.ml session_manager.ml widget.ml \
	wikicreole.ml wiki_filter.ml wiki_syntax.ml \
	wiki.ml wiki_widgets.ml \
        user_widgets.ml forum_widgets.ml \
        dyngroups.ml ocsimore_wikibox.ml $(PAM)

OCSIMORE_OTHER_SRC = ocsisite.ml ocsicreatewiki.ml wikiperso.ml

OCSIMORE_SQL1 = sql.ml user_sql.ml
OCSIMORE_SQL2 = forum_sql.ml wiki_sql.ml

OCSIMORE_SRC = $(OCSIMORE_SRC1) $(OCSIMORE_SQL1) $(OCSIMORE_SRC2) \
               $(OCSIMORE_SQL2) $(OCSIMORE_SRC3)

OCSIMORE_SRC_NOSQL = $(OCSIMORE_SRC1) $(OCSIMORE_SRC2) $(OCSIMORE_SRC3)

OCSIMORE_SQL = $(OCSIMORE_SQL1) $(OCSIMORE_SQL2)

# services.ml
OCSIMORE_MLI = $(OCSIMORE_SRC:.ml=.mli)
#widget.mli forum_widgets.mli forum.mli \
#	session_manager.mli setOfSets.mli users.mli user_widgets.mli \
#	wiki.mli wiki_parser.mli wiki_widgets.mli
#services.mli 
OCSIMORE_CMO = $(OCSIMORE_SRC:.ml=.cmo)
OCSIMORE_CMI = $(OCSIMORE_MLI:.mli=.cmi)
OCSIMORE_OTHER_CMO = $(OCSIMORE_OTHER_SRC:.ml=.cmo)

#HOST = localhost
DATABASE=ocsimore
CAMLP4O=camlp4o 
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

PACKAGES = -package calendar,lwt,pgocaml,pgocaml.statements,ocsigen$(PAMPACKAGE)
#LINKPKG = -package calendar,lwt,ocsigen,pgocaml

.PHONY: all depend clean

all: nis_chkpwd_ ocsimore.cma $(OCSIMORE_OTHER_CMO)

doc:
	ocamlducefind ocamldoc $(PACKAGES) -html -d html $(OCSIMORE_MLI)

nis_chkpwd_:
	make -C nis_chkpwd

ocsimore.cma: $(OCSIMORE_CMO) $(OCSIMORE_CMI)
	ocamlducefind $(CAMLC) -a -thread -o $@ \
          nis_chkpwd/nis_chkpwd.cma $(OCSIMORE_CMO) -cclib -lnis_chkpwd 

sql.cmo: sql.ml
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	ocamlfind $(CAMLC) -verbose -thread $(PACKAGES) $(PP) -c $<

%sql.cmo: %sql.ml
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	ocamlfind $(CAMLC) -verbose -thread $(PACKAGES) $(PP) -c $<

print_sql:
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(CAMLP4O) \
	$(shell $(CAMLC) -where)/str.cma \
	-I +threads $(shell ocamlfind query threads)/threads/threads.cma \
	-I +pcre $(shell ocamlfind query pcre)/pcre.cma \
	-I +extlib $(shell ocamlfind query extlib)/extLib.cma \
	-I +calendar $(shell ocamlfind query calendar)/calendarLib.cmo \
	-I +csv $(shell ocamlfind query csv)/csv.cma \
	-I +ssl $(shell ocamlfind query ssl)/ssl.cma \
	-I +lwt $(shell ocamlfind query lwt)/lwt.cma \
	$(shell ocamlfind query pgocaml)/pgocaml.cma \
	$(shell ocamlfind query pgocaml)/pa_pgsql.cmo pr_o.cmo sql.ml



%.cmo: %.ml
	ocamlducefind $(CAMLC) -thread $(PACKAGES) -c $<	

%.cmi: %.mli
	ocamlducefind $(CAMLC) -thread $(PACKAGES) -c $<	

wikicreole.ml: wikicreole.cmi

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	$(CAMLLEX) $<

.mll.ml:
	$(CAMLLEX) $<


depend:
	ocamlducefind ocamldep $(OCSIMORE_SRC_NOSQL) $(OCSIMORE_OTHER_SRC) $(OCSIMORE_MLI) > .depend
#	PGHOST=$(HOST) 
	PGUSER=$(USER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	ocamlfind ocamldep wikicreole.mll wikicreole.mli $(PACKAGES) $(PP) $(OCSIMORE_SQL) $(OCSIMORE_SQL:.ml=.mli) >> .depend

clean:
	rm -f *.cmo *.cmi *.cma *.annot wikicreole.ml
	make -C nis_chkpwd clean


include .depend
