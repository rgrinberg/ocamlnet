TOP_DIR=../..

PKGNAME  = netmulticore
OBJECTS  = netmcore.cmo netmcore_camlbox.cmo netmcore_util.cmo \
	   netmcore_mempool.cmo netmcore_heap.cmo
DOBJECTS = netmcore.mli netmcore_camlbox.mli netmcore_mempool.mli \
	   netmcore_heap.mli

INCLUDES += $(INC_EQUEUE) $(INC_RPC) $(INC_NETSTRING) $(INC_NETSYS) \
            $(INC_NETPLEX) $(INC_NETCAMLBOX)


include $(TOP_DIR)/Makefile.rules

t_toploop: t_toploop.ml $(PKGNAME).cma
	OCAMLPATH=.. \
	ocamlfind ocamlc -g -o t_toploop \
	   toplevellib.cma $(PKGNAME).cma t_toploop.ml \
	   -package "equeue,rpc,netstring,netsys,netplex,netcamlbox" \
	   -linkpkg -linkall

t_mempool: t_mempool.ml $(PKGNAME).cma
	OCAMLPATH=.. \
	ocamlfind ocamlc -g -o t_mempool \
	   $(PKGNAME).cma t_mempool.ml \
	   -package "equeue,rpc,netstring,netsys,netplex,netcamlbox" \
	   -linkpkg



include depend
