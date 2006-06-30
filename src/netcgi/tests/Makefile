# $Id: Makefile,v 1.6 2005/09/04 20:57:51 chris_77 Exp $

include ../Makefile.conf

PDFLATEX	= pdflatex

PP		= #-pp "camlp4o pa_macro.cmo"

INCLUDES	= -I .. -I +pcre -I $(NETSTRING)
LIBS_CMA	= pcre.cma unix.cma netstring.cma netcgi.cma
OCAMLC_FLAGS	= -g -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -p $(INCLUDES)

PROGRAMS = args_cgi.ml args_fcgi.ml args_mod.cma args_scgi.ml args_ajp.ml \
  browsertest.ml fdf_cgi.ml fdf_form.pdf

export OCAMLPATH = ..

######################################################################

.PHONY: all opt byte
all: $(CMI_FILES) byte opt
byte: $(PROGRAMS:.ml=.exe)
opt: $(filter-out %.cma, $(PROGRAMS:.ml=.com))

args_cgi.exe:  args_common.cmo args_cgi.ml 
args_fcgi.exe: args_common.cmo args_fcgi.ml 
args_scgi.exe: args_common.cmo args_fcgi.ml 
args_ajp.exe:  args_common.cmo args_ajp.ml 

args_cgi.com:  args_common.cmx args_cgi.ml 
args_fcgi.com: args_common.cmx args_fcgi.ml 
args_scgi.com: args_common.cmx args_fcgi.ml 
args_ajp.com:  args_common.cmx args_ajp.ml 

fdf_cgi.com: fdf_cgi.ml
fdf_cgi.exe: fdf_cgi.ml
fdf_form.pdf: fdf_form.tex
	$(PDFLATEX) $<

# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli ../netcgi.cma
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $<

%.exe: %.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) $(filter %.cmo, $^) $(filter %.ml, $^)

%.com: %.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMA:.cma=.cmxa) $(filter %.cmx, $^) $(filter %.ml, $^)



.PHONY: depend dep
dep: .depend
depend: .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.opt
	rm -f *.aux *.log *.out *.pdf
	find . -name '*.com' -type f -perm -u=x -exec rm -f {} \;
	find . -name '*.exe' -type f -perm -u=x -exec rm -f {} \;

distclean: clean
	rm -f config.status config.cache config.log
