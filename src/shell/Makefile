TOP_DIR=../..

PKGNAME  = shell
OBJECTS  = shell_sys.cmo shell.cmo shell_uq.cmo 
DOBJECTS = shell_sys.mli shell.mli shell_uq.mli shell_mt.mli \
	   shell_intro.txt

INCLUDES += $(INC_EQUEUE) $(INC_NETSYS)

ALLMT_EXTRA = shell_mt.cmo
OPTMT_EXTRA = shell_mt.cmx

OCAMLC_OPTIONS_FOR_shell_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_shell_mt.ml = $(THREAD)

include $(TOP_DIR)/Makefile.rules

include depend
