TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

all:
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

depend:
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META depend

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a dll* META` && \
	$(OCAMLFIND) install shell $$files

uninstall:
	$(OCAMLFIND) remove shell

HTML_TARGET?=html

.PHONY: htmldoc
htmldoc:
	ocamldoc -html -d $(HTML_TARGET) -stars -t "Shell Reference Manual" \
		-I ../equeue \
		shell.mli shell_sys.mli shell_uq.mli unix_exts.mli

