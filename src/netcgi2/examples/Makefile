# $Id: Makefile,v 1.4 2005/09/04 22:10:24 chris_77 Exp $

include ../Makefile.conf

PP		= #-pp "camlp4o pa_macro.cmo"
INCLUDES	= -I $(PCRE) -I $(NETSTRING) -I .. $(CRYPTOKIT_INC)
LIBS_CMA	= str.cma pcre.cma unix.cma netstring.cma $(CRYPTOKIT_LIBS)
OCAMLC_FLAGS	= -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -dtypes $(INCLUDES)


PROGRAMS=add.ml filemanager_cgi.ml filemanager_fcgi.ml filemanager_mod.cma \
  filemanager_scgi.ml icfp2001.ml


######################################################################

.PHONY: all opt byte
all: byte opt
# Extensions choosen for M$win
byte: $(PROGRAMS:.ml=.exe)
opt: $(PROGRAMS:.ml=.com)
mod_caml: $(PROGRAMS:.ml=.cmo)

filemanager_cgi.exe: filemanager.cmo filemanager_cgi.ml
filemanager_fcgi.exe: filemanager.cmo filemanager_fcgi.ml
filemanager_ajp.exe: filemanager.cmo filemanager_ajp.ml
filemanager_scgi.exe: filemanager.cmo filemanager_scgi.ml

filemanager_cgi.com: filemanager.cmx filemanager_cgi.ml
filemanager_fcgi.com: filemanager.cmx filemanager_fcgi.ml
filemanager_ajp.com: filemanager.cmx filemanager_ajp.ml
filemanager_scgi.com: filemanager.cmx filemanager_scgi.ml
filemanager.cmx: ../netcgi.cmxa

filemanager_mod.cma: filemanager.cmo filemanager_mod.ml
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $^


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

%.exe: ../netcgi.cma %.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)
%.com: ../netcgi.cmxa %.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMA:.cma=.cmxa) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)


.PHONY: depend
depend: .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.com


distclean: clean
	rm -f config.status config.cache config.log
