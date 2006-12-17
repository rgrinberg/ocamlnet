TOP_DIR=..

.PHONY: doc
doc: html-doc html-doc-netcgi1

include $(TOP_DIR)/Makefile.rules

.PHONY: html-doc
html-doc:
	rm -rf html-main
	mkdir -p html-main
	rm -f loadopts.tmp
	pkglist="$$(cd ..; echo src/*/ocamldoc.dump | xargs -n 1 dirname | xargs -n 1 basename )"; \
	for p in $$pkglist; do \
		[ "$$p" != "cgi" ] || continue; \
		[ "$$p" != "netcgi1" ] || continue; \
		[ "$$p" != "nethttpd-for-netcgi1" ] || continue; \
		[ "$$p" != "equeue-gtk1" ] || continue; \
		[ "$$p" != "equeue-gtk2" ] || continue; \
		[ "$$p" != "netcgi-apache" ] || continue; \
		echo "-load ../src/$$p/ocamldoc.dump" >>loadopts.tmp; \
	done
	$(OCAMLDOC) -d html-main -stars -t "Ocamlnet 2 Reference Manual" \
		-html -css-style style.css \
		-intro intro.txt \
		`cat loadopts.tmp`
	cp style.css html-main

.PHONY: html-doc-netcgi1
html-doc-netcgi1:
	rm -rf html-main/netcgi1
	mkdir -p html-main/netcgi1
	$(OCAMLDOC) -d html-main/netcgi1 -stars -t "Ocamlnet 2 Reference Manual (netcgi1 add-on)" \
		-html -css-style style.css \
		-load ../src/netcgi1/ocamldoc.dump \
		-intro netcgi1_intro.txt
	cp style.css html-main/netcgi1

clean-doc::
	rm -rf html-main loadopts.tmp

clean:: clean-doc
