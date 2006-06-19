TOP_DIR=../..

PKGNAME  = shell
OBJECTS  = unix_exts.cmo shell_sys.cmo shell.cmo shell_uq.cmo 
COBJECTS = unix_exts_c.o
DOBJECTS = unix_exts.mli shell_sys.mli shell.mli shell_uq.mli

INCLUDES += $(INC_EQUEUE)

ALLMT_EXTRA = shell_mt.cmo
OPTMT_EXTRA = shell_mt.cmx

OCAMLC_OPTIONS_FOR_shell_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_shell_mt.ml = $(THREAD)

include $(TOP_DIR)/Makefile.rules

include depend
