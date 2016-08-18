.DEFAULT_GOAL := all

######################################################################

SRCDIR := src
include build-support/OCamlSrcs.makefile

SRCDIR := lib_mvc
include build-support/OCamlSrcs.makefile

SRCDIR := canvas_test
include build-support/OCamlSrcs.makefile

######################################################################
.PHONY: all test clean

all: natural-deduction.js

natural-deduction.js: src/_build/byte_bin/natural-deduction
	js_of_ocaml -o $@ $<

canvas_test.js: canvas_test/_build/byte_bin/canvas_test
	js_of_ocaml -o $@ $<

clean:
	rm -rf $(BUILDDIRS)
	rm -f natural-deduction.js
	rm -f canvas_test.js
