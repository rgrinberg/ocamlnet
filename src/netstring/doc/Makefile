.PHONY: all
all: README INSTALL ABOUT-FINDLIB

README: README.xml common.xml config.xml
	readme -text README.xml >README

INSTALL: INSTALL.xml common.xml config.xml
	readme -text INSTALL.xml >INSTALL

ABOUT-FINDLIB: ABOUT-FINDLIB.xml common.xml config.xml
	readme -text ABOUT-FINDLIB.xml >ABOUT-FINDLIB

.PHONY: clean
clean:

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~

