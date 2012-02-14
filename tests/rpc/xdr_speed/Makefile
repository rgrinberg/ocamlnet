TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

test: all
	./prototest all

read_int32: read_int32.ml
	ocamlc -ccopt -O2 -ccopt -S -c read_int32_help.c
	ocamlc -ccopt -O2 -c read_int32_help.c
	OCAMLPATH=$(TOP_DIR)/src/ ocamlfind opt -o read_int32 -package rpc,bigarray -linkpkg -S -inline 5 read_int32_help.o read_int32.ml

read_int32_c: read_int32_c.c
	gcc -O2 -S read_int32_c.c
	gcc -O2 -o read_int32_c read_int32_c.c

alloc_nativeint: alloc_nativeint.ml
	ocamlopt -o alloc_nativeint unix.cmxa -S alloc_nativeint.ml

alloc_string: alloc_string.ml
	ocamlopt -o alloc_string unix.cmxa -S alloc_string.ml

clean::
	rm -f $(CLEAN_LIST) protospeed_byte protospeed_opt
	rm -f proto_aux.ml proto_aux.mli
	rm -f proto_clnt.ml proto_clnt.mli
	rm -f proto_srv.ml proto_srv.mli
	rm -f *.astamp *.cstamp *.sstamp

CLEAN: clean

distclean:: clean
#	rm -f META
