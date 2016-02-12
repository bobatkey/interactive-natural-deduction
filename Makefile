.DEFAULT_GOAL := all

######################################################################

SRCDIR := src
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all test clean

all: natural-deduction.js

natural-deduction.js: src/_build/byte_bin/natural-deduction
	js_of_ocaml -o $@ $<

clean:
	rm -rf $(BUILDDIRS)
	rm natural-deduction.js
