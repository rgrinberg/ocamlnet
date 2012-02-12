RPCGEN = ../../../src/rpc-generator/ocamlrpcgen -aux $(RPCGEN_EXTRA)
RUN = CAML_LD_LIBRARY_PATH=../../../src/netsys:../../../src/netstring \
      OCAMLRUNPARAM=b=1

.PHONY: all build test

all: build
	$(MAKE) test

build:
	$(MAKE) clean
	$(MAKE) convtest1 convtest1_bigint convtest2 convtest3 convtest4 convtest4_bigint

test:
	$(RUN) ./convtest1
	$(RUN) ./convtest1_bigint
	$(RUN) ./convtest2
	$(RUN) ./convtest3
	$(RUN) ./convtest4
	$(RUN) ./convtest4_bigint


convtest1: convtest1.ml convtest1_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest1 -package rpc -linkpkg -g \
		convtest1_aux.mli convtest1_aux.ml convtest1.ml

convtest1_aux.ml: convtest1.xdr
	$(RPCGEN) convtest1.xdr

convtest1_bigint: convtest1_bigint.ml convtest1_bigint_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest1_bigint -package rpc -linkpkg -g \
		convtest1_bigint_aux.mli convtest1_bigint_aux.ml convtest1_bigint.ml

convtest1_bigint_aux.ml: convtest1.xdr
	ln -s convtest1.xdr convtest1_bigint.xdr
	$(RPCGEN) -int int32 -hyper int64 convtest1_bigint.xdr

convtest2: convtest2.ml convtest2_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest2 -package rpc -linkpkg -g \
		convtest2_aux.mli convtest2_aux.ml convtest2.ml

convtest2_aux.ml: convtest2.xdr
	$(RPCGEN) convtest2.xdr

convtest3: convtest3.ml convtest3_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest3 -package rpc -linkpkg -g \
		convtest3_aux.mli convtest3_aux.ml convtest3.ml

convtest3_aux.ml: convtest3.xdr
	$(RPCGEN) convtest3.xdr

convtest4: convtest4.ml convtest4_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest4 -package rpc -linkpkg -g \
		convtest4_aux.mli convtest4_aux.ml convtest4.ml

convtest4_aux.ml: convtest4.xdr
	$(RPCGEN) convtest4.xdr

convtest4_bigint: convtest4_bigint.ml convtest4_bigint_aux.ml
	OCAMLPATH=../../../src \
		ocamlfind ocamlc -o convtest4_bigint -package rpc -linkpkg -g \
		convtest4_bigint_aux.mli convtest4_bigint_aux.ml convtest4_bigint.ml

convtest4_bigint_aux.ml: convtest4.xdr
	ln -s convtest4.xdr convtest4_bigint.xdr
	$(RPCGEN) -int int32 -hyper int64 convtest4_bigint.xdr

.PHONY: clean
clean:
	rm -f *~
	rm -f *_aux.mli *_aux.ml *_clnt.ml *_srv.ml *.cmi *.cmo 
	rm -f convtest1 convtest2 convtest3 convtest4
	rm -f convtest1_bigint convtest1_bigint.xdr
	rm -f convtest4_bigint convtest4_bigint.xdr

.PHONY: CLEAN
CLEAN: clean
