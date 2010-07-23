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
DISPLAYFLAG := -classic-display

OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild -X nis_chkpwd $(DISPLAYFLAG) -j $(NBCPU)

MYOCAMLFIND := _build/myocamlfind.byte
TARGETS := ocsimore.otarget
LWTDIR := $(shell ocamlfind query lwt)
JSOFOCAMLDIR := $(shell ocamlfind query js_of_ocaml)
ELIOMCLIENTDIR := $(shell ocamlfind query ocsigen.eliom_client)
PAELIOMCLIENTSYNTAX := $(shell ocamlfind query ocsigen.eliom_client_syntax)

#VVV Faire le tri dans les cmis � installer !!!
TOINSTALL := files/META \
             _build/ocsimore.cma _build/page_site.cmo \
             _build/wikiperso.cmo \
             _build/ocsi_user.cma \
             _build/ocsi_wiki.cma \
             _build/wiki_client.js \
             _build/dyngroups.cmi \
             _build/language.cmi \
             _build/ocsimore_common.cmi \
             _build/ocsimore_lib.cmi \
             _build/ocsimore_nis.cmi \
             _build/page_site.cmi \
             _build/wiki_site.cmi \
             _build/opaque.cmi \
             _build/parse_config.cmi \
             _build/sql.cmi \
             _build/user.cmi \
             _build/user_data.cmi \
             _build/user_ext.cmi \
             _build/user_external_auth.cmi \
             _build/user_services.cmi \
             _build/user_site.cmi \
             _build/user_sql.cmi \
             _build/user_widgets.cmi \
             _build/widget.cmi \
             _build/wiki_client.cmi \
             _build/wiki.cmi \
             _build/wikicreole.cmi \
             _build/wiki_data.cmi \
             _build/wiki_ext.cmi \
             _build/wiki_models.cmi \
             _build/wikiperso.cmi \
             _build/wiki_self_services.cmi \
             _build/wiki_services.cmi \
             _build/wiki_sql.cmi \
             _build/wiki_syntax.cmi \
             _build/wiki_types.cmi \
             _build/wiki_widgets.cmi \
             _build/wiki_widgets_interface.cmi \
             _build/xform.cmi
#             _build/forum/forum_client.js \
#             _build/forum/forum.cmi \
#             _build/forum/forum_data.cmi \
#             _build/forum/forum_services.cmi \
#             _build/forum/forum_site.cmi \
#             _build/forum/forum_types.cmi \
#             _build/forum/forum_sql.cmi \
#             _build/forum/forum_widgets.cmi \
#             _build/forum/forum_wikiext.cmi \
#             _build/ocsimore_pam.cmi
#             _build/announce/announce.cma \
#             _build/forum/ocsicreateforum.cmo _build/forum/forum.cma \
#             _build/forum/ocsi_forum.cma \
#             _build/forum/forum_client.cmo \


STATICFILES := static/creole_cheat_sheet.png \
	static/ocsiwikistyle.css \
	static/ocsiforumstyle.css \
	static/ocsiadmin.css
#	static/vm.js \
#	static/eliom_obrowser.js \
#	static/ocsimore_client.uue \

all: $(MYOCAMLFIND) nis_chkpwd_ ocamlbuild files/META files/META.ocsimore ocsimore.conf ocsimore.conf.local etc/ocsigen/ocsimorepassword

nis_chkpwd_:
	$(MAKE) -C nis_chkpwd
	mkdir -p _build
	cp nis_chkpwd/nis_chkpwd.{cma,cmi} _build

updatedb: $(MYOCAMLFIND)
	PGUSER=$(DBUSER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(OCAMLBUILD) updatedb_sql.byte
	PGPASSWORD=$(PASSWORD) ./updatedb_sql.byte

ocamlbuild: $(MYOCAMLFIND) updatedb
	PGUSER=$(DBUSER) PGDATABASE=$(DATABASE) PGPASSWORD=$(PASSWORD) \
	$(OCAMLBUILD) $(TARGETS)

$(MYOCAMLFIND): myocamlfind.ml
	$(OCAMLBUILD) -no-plugin $(subst _build/,,$@)

files/META.ocsimore: files/META.in VERSION
	echo directory = \"$(SRC)/_build\" > $@
	sed "s/_VERSION_/$(VERSION)/" < $< | \
	sed "s%\"forum\" (%\"forum\" (\n  directory = \"$(SRC)/_build/forum\"%" >> $@

files/META: files/META.in VERSION
	sed "s/_VERSION_/$(VERSION)/" < $< > $@

etc/ocsigen/ocsimorepassword:
	echo $(PASSWORD) > etc/ocsigen/ocsimorepassword

wiki_client.js: wiki.cmo

#forum_client.js: forum.cmo

install:
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
	$(MAKE) -C nis_chkpwd clean
	rm -rf wiki_client.ml
	rm -rf forum/forum_client.ml

.PHONY: all clean nis_chkpwd_

SHELL=bash
