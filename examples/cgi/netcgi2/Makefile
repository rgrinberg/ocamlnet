# $Id: Makefile,v 1.4 2005/09/04 22:10:24 chris_77 Exp $
TOP_DIR=../../..

include $(TOP_DIR)/Makefile.rules

INCLUDES += $(INC_NETSTRING) $(INC_NETSYS) $(INC_NETCGI2) $(INC_NETCGI2_APACHE)

# Tell where you cryptokit module is (leave blank otherwise):
CRYPTOKIT_LIBS=
#CRYPTOKIT_LIBS= nums.cma cryptokit.cma
INCLUDES += -I $(shell ocamlc -where)/cryptokit

LIBS_CMA	= str.cma pcre.cma unix.cma netsys.cma netstring.cma \
  $(CRYPTOKIT_LIBS)
OCAMLC_FLAGS	= -dtypes $(INCLUDES)
OCAMLOPT_FLAGS	= -dtypes $(INCLUDES)


PROGRAMS=counter_cgi.ml add_cgi.ml

ifneq "$(CRYPTOKIT_LIBS)" ""
PROGRAMS += filemanager_cgi.ml filemanager_fcgi.ml filemanager_scgi.ml \
  icfp2001.ml
endif

ifneq "$(APACHE_MAJOR)" ""
PROGRAMS += counter_apache.cma add_apache.cma
ifneq "$(CRYPTOKIT_LIBS)" ""
PROGRAMS += filemanager_apache.cma
endif
endif

######################################################################

.PHONY: all opt byte
all: byte opt
# Extensions choosen for M$win
byte: $(PROGRAMS:.ml=.exe)
opt: $(PROGRAMS:.ml=.com)
mod: $(MOD_PROGRAMS)

counter_cgi.exe: counter.cmo counter_cgi.ml
add_cgi.exe: add.cmo add_cgi.ml
filemanager_cgi.exe: filemanager.cmo filemanager_cgi.ml
filemanager_fcgi.exe: filemanager.cmo filemanager_fcgi.ml
filemanager_ajp.exe: filemanager.cmo filemanager_ajp.ml
filemanager_scgi.exe: filemanager.cmo filemanager_scgi.ml

counter_cgi.com: counter.cmx counter_cgi.ml
add_cgi.com: add.cmx add_cgi.ml
filemanager_cgi.com: filemanager.cmx filemanager_cgi.ml
filemanager_fcgi.com: filemanager.cmx filemanager_fcgi.ml
filemanager_ajp.com: filemanager.cmx filemanager_ajp.ml
filemanager_scgi.com: filemanager.cmx filemanager_scgi.ml
filemanager.cmx: ../netcgi.cmxa

counter_apache.cma: counter.cmo counter_apache.ml
add_apache.cma: add.cmo add_apache.ml
filemanager_apache.cma: filemanager.cmo filemanager_apache.ml



%.exe: %.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $(LIBS_CMA) \
	  netcgi.cma $(filter-out %.ml, $^) $(filter %.ml, $^)
%.com: %.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $(LIBS_CMA:.cma=.cmxa) \
	  netcgi.cmxa $(filter-out %.ml, $^) $(filter %.ml, $^)
%.cma: %.ml
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) $^


include depend

clean::
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp *.exe *.com
