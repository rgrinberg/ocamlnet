- Move "make install" to Makefile.pre so install/uninstall is driven
  by the same file.

- Document Makefile and Makefile.pre

- "make install", "make uninstall", "make generate", and "make clean"
  should be propagated from Makefile to Makefile.pre

- make depend should be called first
