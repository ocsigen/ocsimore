include ../Makefile.config

PGOPTS=PGUSER=$(USER) PGPASSWORD=$(PASSWORD)
PSQL=$(PGOPTS) psql
LINK_PKG=ocsigen,pgocaml
COMP_PKG=ocsigen,pgocaml
COMP_FLAGS=-thread -I .. -dtypes

BINARIES=$(NAME).cma load.cma
WEBSITE=xform.cmo \
	atom_feed.cmo eliom_atom.cmo \
	icalendar.cmo eliom_icalendar.cmo \
	common_sql.cmo common.cmo \
	event_sql.cmo event.cmo \
	seminaire_sql.cmo seminaire.cmo \
	agenda_sql.cmo agenda.cmo \
	main.cmo
NAME=announce
LOAD=load_sql.cmo load.cmo

OCAMLDUCEDEP=ocamlducefind ocamldep -package $(COMP_PKG)
OCAMLDEP=ocamlfind ocamldep -package $(COMP_PKG),pgocaml.statements -syntax camlp4o

all: $(BINARIES)

load: load.cma
	$(PGOPTS) CAML_LD_LIBRARY_PATH=../nis_chkpwd ocsigen -c load_ocsigen.conf

load.cma: $(LOAD)
	ocamlducefind ocamlc $(COMPFLAGS) -a -o $@ $^

$(NAME).cma: $(WEBSITE)
	ocamlducefind ocamlc $(COMPFLAGS) -a -o $@ $^

realclean::
	rm -f $(BINARIES)

.PHONY: drop-db

drop-db:
	-echo "DROP SCHEMA announcement CASCADE;" | $(PSQL)

reload: $(NAME).cma
	-[ -p /tmp/cpipe ] && echo reload > /tmp/cpipe

ocsigen: $(NAME).cma
	mkdir -p ./var/log ./var/lib
	$(PGOPTS) CAML_LD_LIBRARY_PATH=../nis_chkpwd ocsigen -c ocsigen.conf &

darcs-send:
	darcs send --sign --to "nobody@ocsigen.org" --sendmail-command='curl -o /tmp/webdarcs.log -F patch=@- -F repo=ocsimore http://www.ocsigen.org/webdarcs/ %<'
	cat /tmp/webdarcs.log

#####

clean::
	find . -regex ".*\\.\(cm[oixa]\|o\)" | xargs rm -f
realclean:: clean

%_sql.cmo: %_sql.ml
	$(PGOPTS) ocamlfind ocamlc -package $(COMP_PKG),pgocaml.statements $(COMP_FLAGS) -syntax camlp4o -c $<

%.cmo: %.ml
	ocamlducefind ocamlc -package $(COMP_PKG) $(COMP_FLAGS) -c $<

%.cmi: %.mli
	ocamlducefind ocamlc -package $(COMP_PKG) $(COMP_FLAGS) -c $<

depend:
	(find . -regex ".*\\.mli?" \! -regex ".*_sql\\.mli?"| xargs \
	 $(OCAMLDUCEDEP) $(DEPFLAGS) $$i; \
	 find . -regex ".*_sql\\.mli?"| $(PGOPTS) xargs \
	 $(OCAMLDEP) $(DEPFLAGS) $$i) \
	 > .depend

include .depend
