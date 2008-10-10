OCAMLFIND = ocamlfind
OCAMLC = $(OCAMLFIND) ocamlc -package lwt
OCAMLMKLIB = ocamlmklib
OCAML = ocaml
NAME = nis_chkpwd

all: $(NAME).cma $(NAME).top

$(NAME).cma: $(NAME)_stubs.o $(NAME).cmo
	$(OCAMLMKLIB) str.cma -o $(NAME) -oc $(NAME) -lcrypt $^

%.o: %.c
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

$(NAME).top: $(NAME).cma
	echo "#!/bin/sh" > $@
	echo "CAML_LD_LIBRARY_PATH=$(PWD) exec $(OCAML) $(shell $(OCAMLFIND) query -i-format lwt) str.cma unix.cma lwt.cma $(NAME).cma" >> $@
	chmod +x $@

clean:
	rm -f *.cm* *.o *.so *~ *.a *.top

dist: clean
	tar -C .. -cjf ../$(NAME)-$(shell date -u +"%Y%m%d").tar.bz2 $(notdir $(PWD))

$(NAME).cmo: $(NAME).ml $(NAME).cmi
