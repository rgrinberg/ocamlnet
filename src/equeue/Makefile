TOP_DIR=../..
include $(TOP_DIR)/Makefile.rules

EQUEUE_CMO = equeue.cmo unixqueue.cmo uq_engines.cmo uq_socks5.cmo
EQUEUE_CMX = $(EQUEUE_CMO:.cmo=.cmx)

.PHONY: all
all: equeue.cma unixqueue_mt.cmo

.PHONY: opt
opt: equeue.cmxa $(MT_NATIVE_P:yes=unixqueue_mt.cmx)

equeue.cma: $(EQUEUE_CMO)
	$(OCAMLC) -a -o equeue.cma $(EQUEUE_CMO)

equeue.cmxa: $(EQUEUE_CMX)
	$(OCAMLOPT) -a -o equeue.cmxa $(EQUEUE_CMX)

#unixqueue.ml: unixqueue.mlp
#	$(IFDEF) unixqueue.mlp

unixqueue_mt.cmo: unixqueue_mt.cmi unixqueue_mt.ml
	$(OCAMLC) -c $(MT_BYTE) unixqueue_mt.ml

unixqueue_mt.cmx: unixqueue_mt.cmi unixqueue_mt.ml
	$(OCAMLOPT) -c $(MT_NATIVE) unixqueue_mt.ml

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
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a unixqueue_mt.cmo unixqueue_mt.cmx unixqueue_mt.o META` && \
	$(OCAMLFIND) install equeue $$files

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove equeue


htmldoc.dump:
	ocamldoc -dump htmldoc.dump -stars *.mli

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

-include depend
