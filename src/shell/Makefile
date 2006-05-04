TOP_DIR=../..

PKGNAME  = shell
OBJECTS  = unix_exts.cmo shell_sys.cmo shell.cmo shell_uq.cmo 
COBJECTS = unix_exts_c.o
DOBJECTS = unix_exts.mli shell_sys.mli shell.mli shell_uq.mli

INCLUDES += -I ../equeue

include $(TOP_DIR)/Makefile.rules

include depend
