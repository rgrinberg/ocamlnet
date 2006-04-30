TOP_DIR=../..
include $(TOP_DIR)/Makefile.rules

include ./Makefile.gtk
# Sets GTK_PKG, GTK_VERSION, ARCHIVE, FL_NAME

EQUEUE_CMO = uq_gtk.cmo
EQUEUE_CMX = $(EQUEUE_CMO:.cmo=.cmx)
EQUEUE_CMI = $(EQUEUE_CMO:.cmo=.cmi)

GTK_DEFINES += -ppopt -D$(GTK_VERSION)

OCAMLC_OPTIONS = -I ../equeue -syntax camlp4o $(GTK_DEFINES)
OCAMLOPT_OPTIONS = -I ../equeue -syntax camlp4o $(GTK_DEFINES)
PACKAGES = camlp4.macro $(GTK_PKG)

.PHONY: all
all: $(ARCHIVE).cma

.PHONY: opt
opt: $(ARCHIVE).cmxa

$(ARCHIVE).cma: $(EQUEUE_CMO)
	$(OCAMLC) -a -o $(ARCHIVE).cma $(EQUEUE_CMO)

$(ARCHIVE).cmxa: $(EQUEUE_CMX)
	$(OCAMLOPT) -a -o $(ARCHIVE).cmxa $(EQUEUE_CMX)

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
	$(OCAMLFIND) install $(FL_NAME) $$files

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove $(FL_NAME)

htmldoc.dump:
	ocamldoc -dump htmldoc.dump -stars -I ../equeue *.mli

depend: *.ml *.mli
	$(OCAMLFIND) ocamldep -syntax camlp4o -package camlp4.macro *.ml *.mli >depend

-include depend
