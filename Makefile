.DEFAULT_GOAL := all

######################################################################
.PHONY: all clean

all: natural-deduction.js

natural-deduction.js: src/*.ml
	jbuilder build src/Main.bc.js
	cp _build/default/src/Main.bc.js $@

clean:
	rm -f natural-deduction.js
	jbuilder clean
