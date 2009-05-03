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

VERSION :=$(shell head -n 1 VERSION)
DESTDIR :=$(shell ocamlfind printconf destdir)

OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild -X nis_chkpwd $(DISPLAYFLAG)
MYOCAMLFIND := _build/myocamlfind.byte
TARGETS := ocsimore.otarget
OBROWSERDIR := $(shell ocamlfind query obrowser)
ELIOMOBROWSERDIR := $(shell ocamlfind query ocsigen.ext.eliom_obrowser)

TOINSTALL := files/META \
             _build/ocsimore.cma _build/ocsisite.cmo \
             _build/ocsicreatewiki.cmo _build/wikiperso.cmo \
             _build/announce/announce.cma \
             _build/forum/ocsicreateforum.cmo _build/forum/forum.cma \
             _build/forum/forum_site.cmo

STATICFILES := static/vm.js static/eliom_obrowser.js static/ocsimore.js \
	static/ocsimore_client.uue static/ocsiwikistyle.css \
	static/creole_cheat_sheet.png

all: ocsimore.mllib check_db nis_chkpwd_ ocamlbuild static/ocsimore_client.uue static/vm.js static/eliom_obrowser.js files/META files/META.ocsimore ocsimore.conf ocsimore.conf.local etc/ocsigen/ocsimorepassword

nis_chkpwd_:
	make -C nis_chkpwd
	mkdir -p _build
	cp nis_chkpwd/nis_chkpwd.{cma,cmi} _build

check_db:
	./update-db.sh

ocamlbuild: $(MYOCAMLFIND)
	PGUSER=$(DBUSER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
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

files/META.ocsimore: files/META.in VERSION
	echo directory = \"$(SRC)/_build\" > $@
	sed "s/_VERSION_/$(VERSION)/" < $< | \
	sed "s%\"forum\" (%\"forum\" (\n  directory = \"$(SRC)/_build/forum\"%" >> $@

files/META: files/META.in VERSION
	sed "s/_VERSION_/$(VERSION)/" < $< > $@

etc/ocsigen/ocsimorepassword:
	echo $(PASSWORD) > etc/ocsigen/ocsimorepassword

install: all
	mkdir -p $(STATICDIR)
	mkdir -p /var/log/ocsimore
	mkdir -p /var/lib/ocsimore
	chown $(USER):$(GROUP) /var/lib/ocsimore
	chown $(USER):$(GROUP) /var/log/ocsimore
	$(OCAMLFIND) install ocsimore $(TOINSTALL)
	cp -f ocsimore.conf /etc/ocsigen/ocsimore.conf.sample
	chmod a+r /etc/ocsigen/ocsimore.conf.sample
	[ -f /etc/ocsigen/ocsimore.conf ] || \
	{ cp ocsimore.conf /etc/ocsigen/ocsimore.conf; \
	  chmod a+r /etc/ocsigen/ocsimore.conf; }
	cp -f $(STATICFILES) $(STATICDIR)
	chown $(USER):$(GROUP) $(STATICDIR)/*
	echo $(PASSWORD) > /etc/ocsigen/ocsimorepassword
	chown $(USER):$(GROUP) /etc/ocsigen/ocsimorepassword
	chmod 600 /etc/ocsigen/ocsimorepassword
	cp nis_chkpwd/dllnis_chkpwd.so $(DESTDIR)/stublibs
	chmod 664 $(DESTDIR)/stublibs/dllnis_chkpwd.so

ocsimore.conf: ocsimore.conf.in
	cat $< | \
	sed "s%_OCSIMOREDIR_%$(DESTDIR)\/ocsimore%g" | \
	sed "s%_USER_%$(USER)%g" | \
	sed "s%_GROUP_%$(GROUP)%g" | \
	sed "s%_STATICDIR_%$(STATICDIR)%g" > $@

ocsimore.conf.local: ocsimore.conf.in
	cat $< | \
	sed "s%_OCSIMOREDIR_%$(SRC)/files%g" | \
	sed "s%<user>_USER_</user>%%g" | \
	sed "s%<group>_GROUP_</group>%%g" | \
	sed "s%>/var%>$(SRC)/var%g" | \
	sed "s%name=\"/etc%name=\"$(SRC)/etc%g" | \
	sed "s%_STATICDIR_%$(SRC)/static%g" > $@

uninstall:
	$(OCAMLFIND) remove ocsimore

clean:
	rm -Rf _build
	make -C nis_chkpwd clean

.PHONY: all ocamlbuild clean check_db ocsimore.mllib nis_chkpwd_

SHELL=bash