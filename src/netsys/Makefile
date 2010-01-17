TOP_DIR=../..

include Makefile.conf

PKGNAME  = netsys
OBJECTS  = netsys_conf.cmo netsys_types.cmo \
           netlog.cmo netexn.cmo netsys_impl_util.cmo \
	   netsys_signal.cmo \
	   netsys_posix.cmo netsys_win32.cmo netsys.cmo \
	   netsys_pollset.cmo netsys_pollset_posix.cmo \
	   netsys_pollset_win32.cmo netsys_pollset_generic.cmo \
	   netsys_mem.cmo netsys_xdr.cmo
COBJECTS = netsys_c.o netsys_c_win32.o netsys_c_htab.o netsys_c_queue.o \
           netsys_c_mem.o netsys_c_xdr.o
DOBJECTS = netsys.mli netsys_posix.mli netsys_pollset.mli netlog.mli \
           netexn.mli netsys_win32.mli netsys_pollset_posix.mli \
           netsys_pollset_win32.mli netsys_pollset_generic.mli \
           netsys_signal.mli netsys_oothr.mli netsys_xdr.mli \
	   netsys_types.mli netsys_mem.mli
LINK_OPTIONS = $(NETSYS_LINK_OPTIONS)

ALL_EXTRA = netsys_oothr.cma netsys_oothr_mt_init.cmo
ALLMT_EXTRA = netsys_oothr_mt_init.cmo
OPT_EXTRA = netsys_oothr.cmxa 
OPTMT_EXTRA = netsys_oothr_mt_init.cmx

OCAMLC_OPTIONS_FOR_netsys_oothr_mt.mli = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt.ml = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt_init.mli = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt_init.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netsys_oothr_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netsys_oothr_mt_init.ml = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_c_xdr.c = -ccopt -O

include $(TOP_DIR)/Makefile.rules

netsys_types.ml: netsys_types.mli
	echo '(* WARNING! THIS IS A COPY OF NETSYS_TYPES.MLI! *)' \
	    >netsys_types.ml
	cat netsys_types.mli >> netsys_types.ml

netsys_types.cmo: netsys_types.cmi
netsys_types.cmx: netsys_types.cmi

CLEAN_LIST += netsys_types.ml

netsys_oothr.cma: netsys_oothr.cmo netsys_oothr_mt.cmo
	$(OCAMLC) -o netsys_oothr.cma -a \
		netsys_oothr.cmo netsys_oothr_mt.cmo


netsys_oothr.cmxa: netsys_oothr.cmx netsys_oothr-$(MT_TYPE)
	extra=""; \
	if [ -f  netsys_oothr_mt.cmx ]; then extra="netsys_oothr_mt.cmx"; fi; \
	$(OCAMLOPT) -o netsys_oothr.cmxa -a netsys_oothr.cmx $$extra

.PHONY: netsys_oothr-vm netsys_oothr-posix
netsys_oothr-vm:

netsys_oothr-posix: netsys_oothr_mt.cmx


clean::
	rm -f configtests/*.o configtests/*.err
	rm -f configtests/posix_shm

include depend
