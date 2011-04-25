TOP_DIR=..

.PHONY: doc
doc: html-doc

include $(TOP_DIR)/Makefile.rules

.PHONY: html-doc
html-doc:
	rm -rf html-main
	mkdir -p html-main
	rm -f loadopts.tmp
	pkglist="$$(cd ..; echo src/*/ocamldoc.dump | xargs -n 1 dirname | xargs -n 1 basename )"; \
	for p in $$pkglist; do \
		[ "$$p" != "equeue-gtk1" ] || continue; \
		echo "-load ../src/$$p/ocamldoc.dump" >>loadopts.tmp; \
	done
	$(OCAMLDOC) -d html-main -stars -t "Ocamlnet 3 Reference Manual" \
		-html -css-style style.css \
		-intro intro.txt intro.txt \
		platform.txt foreword.txt \
               `cat loadopts.tmp`
	cp style.css html-main
	cp html-main/index.html html-main/Intro.html

clean-doc::
	rm -rf html-main loadopts.tmp

clean:: clean-doc
