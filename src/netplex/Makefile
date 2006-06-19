TOP_DIR=../..

PKGNAME  = netplex
OBJECTS  = netplex_mp.cmo netplex_log.cmo \
	   netplex_ctrl_aux.cmo netplex_ctrl_clnt.cmo netplex_ctrl_srv.cmo \
	   netplex_container.cmo netplex_sockserv.cmo netplex_workload.cmo \
	   netplex_controller.cmo netplex_config.cmo \
           netplex_main.cmo rpc_netplex.cmo
DOBJECTS = netplex_types.mli netplex_mp.mli netplex_mt.mli netplex_log.mli \
	   netplex_controller.mli netplex_container.mli \
	   netplex_sockserv.mli netplex_workload.mli netplex_main.mli \
	   netplex_config.mli rpc_netplex.mli

INCLUDES += -I ../equeue -I ../rpc -I ../shell -I ../netstring
# Note: Instead of shell use unix-exts

ALL_EXTRA = netplex-admin$(EXEC_SUFFIX) netplex-packlist netplex_mt.cmo
OPT_EXTRA = netplex_mt.cmx
INSTALL_EXTRA = netplex-packlist netplex_mt.cmo netplex_mt.cmx netplex_mt.o

PP_OPTIONS = -syntax camlp4o -package camlp4

OCAMLC_OPTIONS_FOR_netplex_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netplex_mt.ml = $(THREAD)

INSTOTHER = install-netplex-admin
PACKLIST = netplex-packlist

include $(TOP_DIR)/Makefile.rules

netplex-admin$(EXEC_SUFFIX): netplex.cma netplex_admin.cmo
	$(OCAMLC) -o netplex-admin -custom \
		unix.cma equeue.cma netstring.cma rpc.cma shell.cma \
		netplex.cma netplex_admin.cmo

.PHONY: install-netplex-admin uninstall-netplex-admin

install-netplex-admin:
	mkdir -p $(DESTDIR)$(BINDIR)
	cp netplex-admin$(EXEC_SUFFIX) $(DESTDIR)$(BINDIR)
	@echo "Installed netplex-admin$(EXEC_SUFFIX) into $(DESTDIR)$(BINDIR)"

netplex-packlist:
	echo $(BINDIR)/netplex-admin >netplex-packlist

include depend
