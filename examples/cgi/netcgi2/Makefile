# $Id: Makefile,v 1.4 2005/09/04 22:10:24 chris_77 Exp $
TOP_DIR=../../..

include $(TOP_DIR)/Makefile.rules

INCLUDES += $(INC_NETSTRING) $(INC_NETSYS) $(INC_NETCGI2) $(INC_NETCGI2_APACHE)

LIBS_CMA	= str.cma pcre.cma unix.cma netstring.cma $(CRYPTOKIT_LIBS)
OCAMLC_FLAGS	= -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -dtypes $(INCLUDES)


PROGRAMS=counter.ml add.ml \
  filemanager_cgi.ml filemanager_fcgi.ml filemanager_mod.cma \
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
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) $^



%.exe: %.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)
%.com: %.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMA:.cma=.cmxa) \
	  $(filter-out %.ml, $^) $(filter %.ml, $^)

include depend

clean::
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.com
