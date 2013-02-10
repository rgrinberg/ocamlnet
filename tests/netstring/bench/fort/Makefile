# Copyright (c) 2001 Patrick Doane.
# For conditions of distribution and use, see copyright notice in LICENSE. *)

include Makefile.config

OCAMLC     := ocamlc
OCAMLMKTOP := ocamlmktop

# FORT top-level interpreter
NAME    := fort
VERSION := 0.4.2
OBJECTS := fort.cmo fortmain.cmo
LIBS    := unix

# Rules

EXECUTABLE := $(NAME)$(EXEC_SUFFIX)

$(EXECUTABLE): $(OBJECTS)
	$(OCAMLMKTOP) -linkall -o $@ $(LIBS:%=%.cma) $(OBJECTS)

install: $(EXECUTABLE) $(OBJECTS)
	mkdir -p $(LIBDIR)
	cp fort.cmi $(OBJECTS) $(LIBDIR)
	mkdir -p $(BINDIR)
	cp $(EXECUTABLE) $(BINDIR)

clean:
	rm -f *.cm[iox] *.obj

distclean: clean
	rm -f $(EXECUTABLE) *~

release: distclean
	cd ../releases; \
	cp -r ../fort $(NAME)-$(VERSION); \
	gtar czvf $(NAME)-$(VERSION).tar.gz --exclude='CVS' --exclude='Makefile.config' $(NAME)-$(VERSION); \
	rm -r $(NAME)-$(VERSION)

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	$(OCAMLC) -c -I +compiler-libs $<

.ml.cmo:
	$(OCAMLC) -c -I +compiler-libs $<

# Dependencies

fort.cmo: fort.cmi 
fortmain.cmo: fort.cmi