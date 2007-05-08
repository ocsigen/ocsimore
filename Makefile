### PostgreSQL/PGOCaml configuration
#
# The PGOCaml library requires a database connection at COMPILE TIME, so
# please check your PostgreSQL configuration and ensure that access to
# the database has been granted to the user which will compile Bugsigen,
# and to the Ocsigen server process too.
#
# Check/edit these settings:
#
# POSTGRESQL ENVIRONMENT VARIABLES
export PGUSER = balat
export PGDATABASE = balat
# See PostgreSQL doc, section 28.11, for other vars; example:
# export PGHOST=db.mydomain.com
# export PGPORT=5432
#
# FULL PATH TO THE PGOCAML SOURCE FILES
PGOCAML_SRC = /home/balat/src/pgocaml-0.6
#
### PostgreSQL/PGOCaml configuration section ends here


CSV_EXT =
#	  -I /home/vincent/src/ocaml-csv-1.1.6 csv.cma
PGSQL_EXT = ./pa_pgsql.cma
XHTML_EXT = $(shell ocamlfind query ocsigen)/xhtmlsyntax.cma
PP = -pp "camlp4o $(CSV_EXT) $(PGSQL_EXT) $(XHTML_EXT) -loc loc"

PKG = -package calendar,pgocaml,ocsigen,str 
LIB = 

ifeq "$(DEBUG)" "YES"
FLAGS = -w Aelty -dtypes -g -thread -linkall
else
FLAGS = -w y -thread -linkall
endif

OCAMLC = ocamlfind ocamlc $(PKG) $(FLAGS) $(LIB)

IMPLEM = sql.ml setOfSets.ml users.ml ocsimorelib.ml sessionManager.ml \
	 wikiparser.ml wiki.ml forum.ml

INTERF = $(IMPLEM:.ml=.mli)
OBJECT = $(IMPLEM:.ml=.cmo)
TARGET = ocsimore.cma

DOC = ./html



.PHONY: all depend doc clean

all: depend $(TARGET) ocsimoreexample.cmo

depend: $(PGSQL_EXT)
	ocamlfind ocamldep $(PP) $(LIB) $(INTERF) $(IMPLEM) > .depend

doc:
	[ -d $(DOC) ] || mkdir $(DOC)
	ocamlfind ocamldoc $(PKG) -html -d $(DOC) $(INTERF)

clean:
	-rm -f *.cm? *.annot *.ml4 $(DOC)/* *~ *.mlj

%.cmo: %.ml
	$(OCAMLC) $(PP) $(LIB) -c $<

%.cmi: %.mli
	$(OCAMLC) $(LIB) -c $<

# Dumps inferred interface to a .mlj file
%.mlj: %.ml
	$(OCAMLC) $(PP) $(LIB) -i $< >$@

$(TARGET): $(OBJECT)
	$(OCAMLC) $(PP) $(LIB) -o $@ -a $^

$(PGSQL_EXT): $(PGSQL_EXT:.cma=.ml4)
	ocamlfind ocamlc \
		-package camlp4,extlib,pcre,calendar,pgocaml -linkpkg \
		-pp "camlp4o pa_extend.cmo q_MLast.cmo -loc loc -impl" \
		-o $@ -a -impl $<

$(PGSQL_EXT:.cma=.ml4):
	-ln -s $(PGOCAML_SRC)/$@

-include .depend

