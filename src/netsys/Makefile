TOP_DIR=../..

include Makefile.conf

PKGNAME  = netsys
OBJECTS  = netsys_util.cmo netsys_oothr.cmo \
	   netsys.cmo netsys_pollset.cmo netsys_pollset_posix.cmo \
	   netsys_win32.cmo netsys_pollset_win32.cmo
COBJECTS = netsys_c.o netsys_c_win32.o
DOBJECTS = netsys.mli netsys_pollset.mli
LINK_OPTIONS = $(NETSYS_LINK_OPTIONS)

ALLMT_EXTRA = netsys_oothr_mt.cmo netsys_oothr_mt_init.cmo
OPTMT_EXTRA = netsys_oothr_mt.cmx netsys_oothr_mt_init.cmx

OCAMLC_OPTIONS_FOR_netsys_oothr_mt.mli = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt.ml = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt_init.mli = $(THREAD)
OCAMLC_OPTIONS_FOR_netsys_oothr_mt_init.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netsys_oothr_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_netsys_oothr_mt_init.ml = $(THREAD)


include $(TOP_DIR)/Makefile.rules

clean::
	rm -f configtests/*.o configtests/*.err
	rm -f configtests/posix_shm

include depend
