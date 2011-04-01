TOP_DIR=../..

PKGNAME  = netmulticore
OBJECTS  = netmcore.cmo netmcore_camlbox.cmo netmcore_util.cmo \
	   netmcore_mempool.cmo
DOBJECTS = netmcore.mli netmcore_camlbox.mli netmcore_mempool.mli

INCLUDES += $(INC_EQUEUE) $(INC_RPC) $(INC_NETSTRING) $(INC_NETSYS) \
            $(INC_NETPLEX) $(INC_NETCAMLBOX)


include $(TOP_DIR)/Makefile.rules

t_toploop: t_toploop.ml $(PKGNAME).cma
	OCAMLPATH=.. \
	ocamlfind ocamlc -g -o t_toploop \
	   toplevellib.cma $(PKGNAME).cma t_toploop.ml \
	   -package "equeue,rpc,netstring,netsys,netplex,netcamlbox" \
	   -linkpkg -linkall


include depend
