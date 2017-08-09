.DEFAULT_GOAL := all

######################################################################
.PHONY: all

all: natural-deduction.js

natural-deduction.js: src/*.ml
	jbuilder build src/Main.bc.js
	cp _build/default/src/Main.bc.js $@
