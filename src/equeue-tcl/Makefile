TOP_DIR=../..
include $(TOP_DIR)/Makefile.rules

EQUEUE_CMO = uq_tcl.cmo
EQUEUE_CMX = $(EQUEUE_CMO:.cmo=.cmx)
EQUEUE_CMI = $(EQUEUE_CMO:.cmo=.cmi)

OCAMLC_OPTIONS = -I ../equeue
OCAMLOPT_OPTIONS = -I ../equeue

.PHONY: all
all: equeue_tcl.cma

.PHONY: opt
opt: equeue_tcl.cmxa

equeue_tcl.cma: $(EQUEUE_CMO) tclqueue.o
	$(OCAMLMKLIB) -oc tclqueue -o equeue_tcl \
		$(EQUEUE_TCL_LIBS) $(EQUEUE_CMO) tclqueue.o

equeue_tcl.cmxa: $(EQUEUE_CMX) tclqueue.o
	$(OCAMLMKLIB) -oc tclqueue -o equeue_tcl \
		$(EQUEUE_TCL_LIBS) $(EQUEUE_CMX) tclqueue.o

tclqueue.o: tclqueue.h tclqueue.c
	$(OCAMLC) $(EQUEUE_TCL_DEFS) -c tclqueue.c

.PHONY: clean
clean:
	rm -f $(CLEAN_LIST)

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f META depend

.PHONY: install
install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a dll* META` && \
	$(OCAMLFIND) install equeue-tcl $$files

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove equeue-tcl

htmldoc.dump:
	ocamldoc -dump htmldoc.dump -stars -I ../equeue *.mli

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

-include depend
