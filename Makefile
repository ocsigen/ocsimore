# Ocsigen
# http://www.ocsigen.org/
# Makefile
# Copyright (C) 2009 Stéphane GlonduVincent Balat
# Laboratoire PPS - CNRS Université Paris Diderot
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, with linking exception;
# either version 2.1 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

include Makefile.config

OCAMLBUILD := ocamlbuild -X nis_chkpwd $(DISPLAYFLAG)
MYOCAMLFIND := _build/myocamlfind.byte
TARGETS := ocsimore.otarget
OBROWSERDIR := $(shell ocamlfind query obrowser)
ELIOMOBROWSERDIR := $(shell ocamlfind query ocsigen.ext.eliom_obrowser)

all: ocsimore.mllib check_db nis_chkpwd_ ocamlbuild static/ocsimore_client.uue static/vm.js static/eliom_obrowser.js

nis_chkpwd_:
	make -C nis_chkpwd
	mkdir -p _build
	cp nis_chkpwd/nis_chkpwd.{cma,cmi} _build

check_db:
	./update-db.sh

ocamlbuild: $(MYOCAMLFIND)
	PGUSER=$(USER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(OCAMLBUILD) $(TARGETS)

$(MYOCAMLFIND):
	$(OCAMLBUILD) -no-plugin $(subst _build/,,$@)

ocsimore.mllib: ocsimore.mllib.IN
	cp -f ocsimore.mllib.IN ocsimore.mllib
	if [ $(PAM) = YES ]; then echo Ocsimore_pam >> ocsimore.mllib; fi

static/ocsimore_client.uue:
	CAMLLIB=$(OBROWSERDIR) ocamlc -o ocsimore_client $(ELIOMOBROWSERDIR)/eliom_obrowser.cmo _build/forum/forum_client.cmo
	uuencode ocsimore_client stdout > static/ocsimore_client.uue

static/vm.js: $(OBROWSERDIR)/vm.js
	cp -f $(OBROWSERDIR)/vm.js static

static/eliom_obrowser.js: $(ELIOMOBROWSERDIR)/eliom_obrowser.js
	cp -f $(ELIOMOBROWSERDIR)/eliom_obrowser.js static


clean:
	rm -Rf _build
	make -C nis_chkpwd clean

.PHONY: all ocamlbuild clean check_db ocsimore.mllib nis_chkpwd_

SHELL=bash