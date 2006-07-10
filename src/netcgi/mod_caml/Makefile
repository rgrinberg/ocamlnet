# mod_caml
#
# Minimal compilation of mod_caml needed by Netcgi_mod

# $Id: Makefile,v 1.2 2005/10/23 21:06:17 chris_77 Exp $

PACKAGE	:= $(shell grep "name" ../META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
VERSION := $(shell grep "version" ../META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PACKAGE := $(PACKAGE)_mod

include ../Makefile.conf
include Makefile.conf

TMPDIR ?= /tmp
export TMPDIR

OCAMLCINCS := -I $(PCRELIBDIR)
ALL_CMOS := apache.cmo mod_caml_config.cmo mod_caml.cmo

OCAMLCFLAGS 	= -w s -dtypes $(OCAMLCINCS)
OCAMLOPTFLAGS 	= -w s $(OCAMLCINCS)
ALL_CMAS += dynlink.cma unix.cma str.cma pcre.cma


all: config.h mod_caml_config.ml 500mod_caml.info mod_caml.so

config.h: config.h.in
	$(SED) -e 's,@APACHELIBDIR@,$(APACHELIBDIR),g' \
	-e 's,@PACKAGE@,$(PACKAGE),g' \
	-e 's,@VERSION@,$(VERSION),g' \
	-e 's,@APACHE_MAJOR@,$(APACHE_MAJOR),g' \
	< $< > $@

mod_caml_config.ml: mod_caml_config.ml.in
	$(SED) -e 's,@APACHELIBDIR@,$(APACHELIBDIR),g' \
	-e 's,@OCAMLLIBDIR@,$(OCAMLLIBDIR),g' \
	-e 's,@OCAMLAPACHEDIR@,$(OCAMLAPACHEDIR),g' \
	-e 's,@PACKAGE@,$(PACKAGE),g' \
	-e 's,@VERSION@,$(VERSION),g' \
	-e 's,@APACHE_MAJOR@,$(APACHE_MAJOR),g' \
	< $< > $@

500mod_caml.info: 500mod_caml.info.in
	$(SED) -e 's,@APACHELIBDIR@,$(APACHELIBDIR),g' < $< > $@

mod_caml.so: mod_caml_c.o apache_c.o wrappers.o $(ALL_CMOS)
	$(OCAMLC)  -o $@ -linkall -custom $(OCAMLCFLAGS) $(ALL_CMAS) $^ \
	  -cclib '$(CFLAGS) $(LDFLAGS_SHLIB) $(OCAMLLIBS)'


#Installation
install: 
	if [ ! -d $(DESTDIR)$(APACHELIBDIR) ]; then \
		$(INSTALL) -m 0755 -d $(DESTDIR)$(APACHELIBDIR); fi
	if [ ! -d $(DESTDIR)$(OCAMLAPACHEDIR) ]; then \
		$(INSTALL) -m 0755 -d $(DESTDIR)$(OCAMLAPACHEDIR); fi
	$(INSTALL) -c -m 0644 mod_caml_config.cmi $(DESTDIR)$(OCAMLAPACHEDIR)
	$(INSTALL) -c -m 0644 apache.mli apache.cmi $(DESTDIR)$(OCAMLAPACHEDIR)
	$(INSTALL) -c -m 0644 mod_caml.mli mod_caml.cmi \
		$(DESTDIR)$(OCAMLAPACHEDIR)
	$(INSTALL) -c -m 0644 mod_caml.so      $(DESTDIR)$(APACHELIBDIR)
	$(INSTALL) -c -m 0644 500mod_caml.info $(DESTDIR)$(APACHELIBDIR)

# General rules
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

dep:	.depend
depend: .depend

.depend: $(wilcard *.ml) $(wildcard *.mli)
	rm -f .depend
	ocamldep *.mli *.ml > $@

include .depend


# Cleaning
clean:
	rm -f *~ *.bak core *.o *.lo *.slo
	rm -f *.cm{i,o,x,a} *.so *.a *.annot
	rm -f 500mod_caml.info
	rm -f mod_caml_config.ml config.h

distclean:
	rm -rf .depend html/
