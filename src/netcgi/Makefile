# $Id: Makefile,v 1.15 2005/10/19 18:54:48 chris_77 Exp $

PACKAGE	:= $(shell grep "name" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
VERSION := $(shell grep "version" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
ARCHIVE := $(shell grep "archive(byte)" META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")
XARCHIVE := $(shell grep "archive(native)" META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")

MODCAMLDIR := mod_caml/

include Makefile.conf

PP	   = #-pp "camlp4o pa_macro.cmo"

INCLUDE		= -I $(PCRE) -I $(NETSTRING) -I $(MODCAMLDIR)
OCAMLC_FLAGS	= -g -dtypes $(INCLUDE)
OCAMLOPT_FLAGS := $(INCLUDE)

OCAMLDOC_GEN = -html
#OCAMLDOC_GEN = -i $(HOME)/Software/OCaml/doc-generators -g odoc_fhtml.cmo
OCAMLDOC_FLAGS := -I $(NETSTRING) -I $(MODCAMLDIR) $(OCAMLDOC_GEN)


DOCFILES   = netcgi.mli netcgi_cgi.mli netcgi_fcgi.mli netcgi_ajp.mli \
  netcgi_mod.mli netcgi_scgi.mli \
  netcgi_test.mli netcgi_common.mli
# netcgi_compat.mli

DISTFILES  = INSTALL LICENSE META Makefile README \
  $(wildcard *.ml) $(wildcard *.mli) examples/Makefile \
  $(wildcard examples/*.ml) $(wildcard examples/*.mli) tests/Makefile \
  $(wildcard tests/*.ml) $(wildcard tests/*.mli) $(wildcard tests/*.tex)

# Files in the correct order to build the netcgi library
NETCGI	   = netcgi_common.ml netcgi.ml \
  netcgi_cgi.ml netcgi_fcgi.ml netcgi_ajp.ml netcgi_scgi.ml \
  netcgi_test.ml netcgi_dbi.ml
# netcgi_compat.ml

EXTRA_LIBS = netcgi_modtpl.ml

# We want netcgi_mod.cma to be as small as possible as it will be
# loaded into Apache.  It is not compiled in native code because
# Dynlink is used for loading.
NETCGI_MOD= netcgi_common.ml netcgi.ml netcgi_mod.ml \
  netcgi_dbi.ml

PKG_TARBALL  = ocaml-$(PACKAGE)-$(VERSION).tar.bz2


default: all


######################################################################

ML_FILES  := $(wildcard *.ml)
MLI_FILES := $(wildcard *.mli)
CMI_FILES := $(addsuffix .cmi,$(basename $(MLI_FILES)))

BYTE_OBJS := $(if $(ML_FILES),$(PACKAGE).cmo $(BYTE_OBJS),)
OPT_OBJS  := $(if $(ML_FILES),$(PACKAGE).cmx $(OPT_OBJS),)

#DOCFILES  += $(ML_FILES) $(MLI_FILES)
PUBFILES  += $(DOCFILES) README

PKGS = $(shell grep "requires" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGS_CMA  += $(addsuffix .cma, $(PKGS))
PKGS_CMXA += $(addsuffix .cmxa, $(PKGS))

export OCAMLPATH = ..

.PHONY: all opt byte ex
all: $(CMI_FILES) byte opt
byte: $(ARCHIVE) $(EXTRA_LIBS:.ml=.cma)
opt: $(XARCHIVE) $(EXTRA_LIBS:.ml=.cmxa)
ex: examples
ps: netcgi.ps

netcgi.cma: $(NETCGI:.ml=.cmo)
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) $^

netcgi.cmxa: $(NETCGI:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $(OCAMLOPT_FLAGS) $^

netcgi_mod.cma: $(NETCGI_MOD:.ml=.cmo) $(MODCAMLDIR)/apache.cmi \
	  $(MODCAMLDIR)/mod_caml.cmi
	$(OCAMLC) $(OCAMLC_FLAGS) -a -o $@ $(filter-out %.cmi,$^)

# Compile mod_caml
netcgi_mod.cmi: netcgi_mod.mli $(MODCAMLDIR)/apache.cmi

$(MODCAMLDIR)/%:
	cd $(MODCAMLDIR); $(MAKE)



TAGS: $(ML_FILES)
	$(OCAMLTAGS) $^

# Compile HTML documentation
DOC_DIR=doc
DOC_CSS=ocamldoc.css
doc: $(DOCFILES) $(CMI_FILES)
	@if [ -n "$(DOCFILES)" ] ; then \
	    if [ ! -x $(DOC_DIR) ] ; then mkdir $(DOC_DIR) ; fi ; \
	    if [ -r "$(DOC_CSS)" ] ; then \
	        CSS_STYLE="-css-style $(DOC_CSS)" ; \
	        $(LN) ../$(DOC_CSS) $(DOC_DIR)/$(DOC_CSS) ; \
	    fi; \
	    $(OCAMLDOC) -v -d $(DOC_DIR) -colorize-code -stars \
		 $(OCAMLDOC_FLAGS) $$CSS_STYLE $(DOCFILES) ; \
	fi

# (Un)installation
.PHONY: install uninstall
install: all
	ocamlfind remove $(PACKAGE); \
	[ -f "$(XARCHIVE)" ] && \
	extra="$(XARCHIVE) $(basename $(XARCHIVE)).a"; \
	ocamlfind install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PACKAGE) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META $$extra

installbyte:
	ocamlfind remove $(PACKAGE); \
	ocamlfind install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PACKAGE) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META


uninstall:
	ocamlfind remove $(PACKAGE)

# Make the examples
.PHONY: examples
examples: all
	cd examples/; $(MAKE)
examples-byte: byte
	cd examples/; $(MAKE) byte


# Make the tests
.PHONY: tests
tests: all
	cd tests; $(MAKE) all

# Dependency graph (for developers)
.PHONY: netcgi.ps
netcgi.ps: $(DOCFILES) $(NETCGI:.ml=.cmo) netcgi_mod.cmo
	$(OCAMLDOC) -o netcgi.dot -I $(NETSTRING) -I $(MOD_CAML) -dot \
		$(DOCFILES) $(NETCGI) netcgi_mod.ml
	dot -Tps netcgi.dot > $@
	rm -f netcgi.dot


# Make.bat -- easy compilation on win32
Make.bat:
	$(MAKE) clean
#	Filter out all "make" messages
	$(MAKE) all | grep --invert-match "make" > $@

# Make a tarball
.PHONY: dist
dist: $(DISTFILES) Make.bat
	@ if [ -z "$(PACKAGE)" ]; then echo "PACKAGE not defined"; exit 1; fi
	@ if [ -z "$(VERSION)" ]; then \
		echo "VERSION not defined"; exit 1; fi
	mkdir $(PACKAGE)-$(VERSION) ; \
	mv Make.bat $(PACKAGE)-$(VERSION); \
	cp -r $(DISTFILES) $(PACKAGE)-$(VERSION)/; \
	tar --exclude "CVS" --exclude ".cvsignore" --exclude "*~" \
	   --exclude "*.cm{i,x,o,xa}" --exclude "*.o" \
	  -jcvf $(PKG_TARBALL) $(PACKAGE)-$(VERSION); \
	rm -rf $(PACKAGE)-$(VERSION)

# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $<


.PHONY: depend
depend: .depend
.depend: $(wildcard *.ml) $(wildcard *.mli) $(wildcard test/*.ml)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp gmon.out
	rm -f *.html *.ps
	rm -f Make.bat $(PACKAGE)-$(VERSION).tar.bz2
	rm -rf $(DOC_DIR)
	cd tests/; $(MAKE) clean
	cd examples/; $(MAKE) clean
	cd $(MODCAMLDIR); $(MAKE) clean

distclean: clean
	rm -f config.status config.cache config.log TAGS .depend
