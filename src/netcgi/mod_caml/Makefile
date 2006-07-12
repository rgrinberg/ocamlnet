TOP_DIR=../../..

OBJECTS  = apache.cmo mod_caml.cmo mod_caml.so

DOBJECTS = apache.mli mod_caml.mli

include $(TOP_DIR)/Makefile.conf

mod_caml.so: mod_caml_c.o apache_c.o wrappers.o
	$(OCAMLC) -o $@ -linkall -custom \
	  dynlink.cma unix.cma str.cma pcre.cma $^ \
	  -cclib '$(APACHE_CFLAGS) $(APACHE_LDFLAGS_SHLIB) $(APACHE_OCAMLLIBS)'

%.o: %.c
	$(APACHE_CC) $(APACHE_CFLAGS) -I $(APACHE_OCAMLLIBDIR) -c $<


include $(TOP_DIR)/Makefile.rules

CLEAN_LIST += config.h mod_caml_config.ml 500mod_caml.info mod_caml.so

include depend
