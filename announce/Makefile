include ../Makefile.config

PGOPTS=PGUSER=$(USER) PGPASSWORD=$(PASSWORD)
PSQL=$(PGOPTS) psql
LINK_PKG=ocsigen,pgocaml
COMP_PKG=ocsigen,pgocaml
COMP_FLAGS=-thread -I ..

BINARIES=website
WEBSITE=xform.cmo \
	atom_feed.cmo eliom_atom.cmo \
	icalendar.cmo eliom_icalendar.cmo \
	common_sql.cmo common.cmo \
	event_sql.cmo event.cmo \
	seminaire_sql.cmo seminaire.cmo \
	agenda_sql.cmo agenda.cmo \
	main.cmo
LOAD=load_sql.cmo load.cmo

OCAMLDUCEDEP=ocamlducefind ocamldep -package $(COMP_PKG)
OCAMLDEP=ocamlfind ocamldep -package $(COMP_PKG),pgocaml.statements -syntax camlp4o

all: $(BINARIES)

load.cma: $(LOAD)
	ocamlducefind ocamlc $(COMPFLAGS) -a -o $@ $^

load: load.cma
	$(PGOPTS) CAML_LD_LIBRARY_PATH=../nis_chkpwd ocsigen -c load_ocsigen.conf

website.cma: $(WEBSITE)
	ocamlducefind ocamlc $(COMPFLAGS) -a -o $@ $^
#	-echo reload > /tmp/cpipe

website: website.cma

realclean::
	rm -f $(BINARIES)

.PHONY: drop-db make-db

drop-db:
	-echo "DROP SCHEMA announcement CASCADE;" | $(PSQL)

make-db:
	$(PSQL) < tables.sql

#####

clean::
	find . -regex ".*\\.\(cm[oix]\|o\)" | xargs rm -f
realclean:: clean

%_sql.cmo: %_sql.ml
	$(PGOPTS) ocamlfind ocamlc -package $(COMP_PKG),pgocaml.statements $(COMP_FLAGS) -syntax camlp4o -c $<

%.cmo: %.ml
	ocamlducefind ocamlc -package $(COMP_PKG) $(COMP_FLAGS) -c $<

%.cmi: %.mli
	ocamlducefind ocamlc -package $(COMP_PKG) $(COMP_FLAGS) -c $<

ocsigen: website.cma
	mkdir -p ./var/log ./var/lib
	$(PGOPTS) CAML_LD_LIBRARY_PATH=../nis_chkpwd ocsigen -c ocsigen.conf &

depend:
	(find . -regex ".*\\.mli?" \! -regex ".*_sql\\.mli?"| xargs \
	 $(OCAMLDUCEDEP) $(DEPFLAGS) $$i; \
	 find . -regex ".*_sql\\.mli?"| $(PGOPTS) xargs \
	 $(OCAMLDEP) $(DEPFLAGS) $$i) \
	 > .depend

include .depend
