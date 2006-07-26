TOP_DIR=..

.PHONY: doc
doc: html-doc html-doc-cgi

include $(TOP_DIR)/Makefile.rules

.PHONY: html-doc
html-doc:
	rm -rf html-main
	mkdir -p html-main
	rm -f loadopts.tmp
	for p in $(PKGLIST); do \
		[ "$$p" != "cgi" ] || continue; \
		[ "$$p" != "equeue-gtk1" ] || continue; \
		echo "-load ../src/$$p/ocamldoc.dump" >>loadopts.tmp; \
	done
	$(OCAMLDOC) -d html-main -stars -t "Ocamlnet 2 Reference Manual" \
		-html -css-style style.css \
		-intro intro.txt \
		`cat loadopts.tmp`
	cp style.css html-main

.PHONY: html-doc-cgi
html-doc-cgi:
	rm -rf html-main/cgi
	mkdir -p html-main/cgi
	$(OCAMLDOC) -d html-main/cgi -stars -t "Ocamlnet 2 Reference Manual (cgi add-on)" \
		-html -css-style style.css \
		-load ../src/cgi/ocamldoc.dump \
		-intro cgi_intro.txt
	cp style.css html-main/cgi

clean-doc::
	rm -rf html-main loadopts.tmp

clean:: clean-doc
