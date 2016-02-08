######################################################################
# Generic OCaml build instructions; uses .merlin file to get the list
# of required ocamlfind packages.

# FIXME: is there any way to trigger a rebuild if the ocamlfind packages
# change? can ocamlfind packages be hashed?

# FIXME: abort if there are .ml(i) files that would be overwritten by
# ocamllex or menhir [this might be fixed now by the static pattern
# rules: there will be two rules for the same .cmo/.cmx file. but this
# only gives a warning.]

# FIXME: ought to warn about LIB lines in .mllib files, and other
# syntax errors in the .merlin, *.mllib and *.mlbin files. In general,
# a checker for .merlin files' consistency.

# FIXME: when to include -no-alias-deps? In general, how to use module
# aliases effectively for building libraries?
#   https://sympa.inria.fr/sympa/arc/caml-list/2015-02/msg00024.html

# FIXME: how to make sure pre-processors are compiled before starting
# to compute dependencies of other objects:
#  test/_build/blah.ml.d needs src/_build/byte_bin/ppx_blah, but,
#  src/_build/ppx_blah.mlbin.d won't be built or included in the
#  Makefile in time, so the dependencies for ppx_blah will be
#  incorrect and the running of the preprocessor will fail.
# Pretty sure this is unfixable without reinvoking make manually
#
# Proper fix: compute rule-generation-time dependencies between source
# directories; and process in phases to be able to get the ruleset
# together for the preprocessors before starting to compute the
# ruleset for the rest of the code.


# FIXME: could define everything here as a massive 'define' and then
# use a foreach loop+eval to generate the rules for every source
# directory in a list.

######################################################################
ifndef SRCDIR
$(error SRCDIR must be set)
endif

BUILDDIR := $(SRCDIR)/_build

ifndef BUILDDIRS
BUILDDIRS := $(BUILDDIR)
else
BUILDDIRS := $(BUILDDIRS) $(BUILDDIR)
endif

BYTE_BINDIR    := $(BUILDDIR)/byte_bin
NATIVE_BINDIR  := $(BUILDDIR)/native_bin

######################################################################
# OCAML_PRODUCTS contains that could possibly be generated from the
# current source files in all source directories
ifndef OCAML_PRODUCTS
OCAML_PRODUCTS :=
endif

######################################################################
# Gather compiler options from the .merlin file

# FIXME: if the .merlin file doesn't exist then use some sensible
# defaults

# OCAML_LIBS     := $(shell cat $(SRCDIR)/.merlin | grep ^PKG | sed -E 's/^PKG +//')
# OCAML_PKGS_OPT := $(foreach lib,$(OCAML_LIBS),-package $(lib))

# MERLIN_FLAGS := $(shell cat $(SRCDIR)/.merlin | grep ^FLG | cut -d' ' -f2- | grep -v '^ *-ppx')
# PPX_BINARIES :=\
#   $(foreach ppx,$(shell cat $(SRCDIR)/.merlin | egrep '^FLG +-ppx +' | sed -E 's/^FLG +-ppx +//'),$(shell realpath -m --relative-to=. $(SRCDIR)/$(ppx)))

# MERLIN_BINDIRS := $(shell cat $(SRCDIR)/.merlin | grep ^B | cut -d ' ' -f2- | grep -v '_build')
# BINDIRS_OPT    := -I $(BUILDDIR) $(foreach dir,$(MERLIN_BINDIRS),-I $(shell realpath -m --relative-to=. $(SRCDIR)/$(dir)))

# MERLIN_SRCDIRS := $(shell cat $(SRCDIR)/.merlin | grep ^S | cut -d ' ' -f2- | grep -v '\.')
# SRCDIRS_OPT    := -I $(SRCDIR) $(foreach dir,$(MERLIN_SRCDIRS),-I $(shell realpath -m --relative-to=. $(SRCDIR)/$(dir)))

# OCAMLC_FLAGS  := $(MERLIN_FLAGS) \
#                  $(foreach ppx,$(PPX_BINARIES),-ppx $(ppx)) \
#                  -no-alias-deps

OCAMLDEP_FLAGS := $(shell build-support/of_merlin -ocamldep-flags $(SRCDIR)/.merlin)
OCAMLC_FLAGS   := $(shell build-support/of_merlin -ocamlc-flags $(SRCDIR)/.merlin) -no-alias-deps -g
PPX_BINARIES   := $(shell build-support/of_merlin -ppx-bins $(SRCDIR)/.merlin)
SRCDIRS_OPT    := $(shell build-support/of_merlin -src-dirs $(SRCDIR)/.merlin)
BINDIRS_OPT    := $(shell build-support/of_merlin -bin-dirs $(SRCDIR)/.merlin)

# or ocamlfind build-support/run $(SRCDIR)/.merlin ocamlc ...
# or adjust ocamlfind to take an options file...
# and write a new version of mlbindep
# .. only problem: still need to extract PPX_BINARIES from the .merlin file... (not sure this works right anyway...)

######################################################################
# Construct command lines for all the compiler executables to be used

# FIXME: weird bug in ocamlfind: if -only-show is used it seems to run
# these commands and also output the command line. If there are .cma
# or .cmxa files mentioned (e.g., via -linkpkg), then it will generate
# an a.out file

OCAMLDEP      := ocamlfind ocamldep $(SRCDIRS_OPT) $(OCAMLDEP_FLAGS) -ml-synonym .mll -ml-synonym .mly -mli-synonym .mly
MLBINDEP      := build-support/mlbindep $(SRCDIRS_OPT)
OCAMLC        := ocamlfind ocamlc   $(BINDIRS_OPT) $(OCAMLC_FLAGS)
OCAMLOPT      := ocamlfind ocamlopt $(BINDIRS_OPT) $(OCAMLC_FLAGS)
OCAMLC_LINK   := ocamlfind ocamlc   $(BINDIRS_OPT) $(OCAMLC_FLAGS) -linkpkg
OCAMLOPT_LINK := ocamlfind ocamlopt $(BINDIRS_OPT) $(OCAMLC_FLAGS) -linkpkg

######################################################################
$(BUILDDIR):
	@mkdir -p $@

$(BYTE_BINDIR):
	@mkdir -p $@

$(NATIVE_BINDIR):
	@mkdir -p $@

######################################################################
# gather the set of all source files, which may affect the dependency
# generation for other source files.

# for each source file:
#   generate a corresponding .d file with a rule for each possible primary build output of that file.
#   extend OCAML_PRODUCTS with a list of all primary and secondary outputs of that file

# SOURCE_DIRECTORIES := $(shell grep '^S ' < $(SRCDIR)/.merlin | cut -b3-)
# RELEVANT_SOURCES   := $(sort $(foreach d,$(SOURCE_DIRECTORIES),$(wildcard $(d)/*.mli $(d)/*.ml $(d)/*.mly $(d)/*.mll $(d)/*.mllib $(d)/*.mlbin)))

# .PHONY: always
# $(BUILDDIR)/relevant-sources.fileset: always | $(BUILDDIR)
# 	@echo $(RELEVANT_SOURCES) | cmp -s - $@ || echo '$(RELEVANT_SOURCES)' > $@

# OCAML_SOURCES     := $(wildcard $(SRCDIR)/*.ml $(SRCDIR)/*.mli $(SRCDIR)/*.mll $(SRCDIR)/*.mly $(SRCDIR)/*.mllib $(SRCDIR)/*.mlbin)
# OCAML_RULES_FILES := $(patsubst %,%.d,$(OCAML_SOURCES))
# $(OCAML_RULES_FILES): $(BUILDDIR)/%.d: $(SRCDIR)/% $(SRCDIR)/.merlin $(BUILDDIR)/relevant-sources.fileset | $(BUILDDIR)
# 	@echo Generating rules for $<
# 	@build-support/rulegen $(SRCDIR)/.merlin $< $@

# -include $(OCAML_RULES_FILES)

######################################################################
## .ml files
OCAML_ML_SRCS  := $(wildcard $(SRCDIR)/*.ml)
OCAML_ML_CMOS  := $(patsubst $(SRCDIR)/%.ml,$(BUILDDIR)/%.cmo,$(OCAML_ML_SRCS))
OCAML_ML_CMXS  := $(patsubst $(SRCDIR)/%.ml,$(BUILDDIR)/%.cmx,$(OCAML_ML_SRCS))
OCAML_ML_DEPS  := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_ML_SRCS))
# .o and .cmi files are possible 'side' consequences of compiling a .ml file
OCAML_PRODUCTS += \
    $(OCAML_ML_CMOS) \
    $(patsubst $(SRCDIR)/%.ml,$(BUILDDIR)/%.cmi,$(OCAML_ML_SRCS)) \
    $(patsubst $(SRCDIR)/%.ml,$(BUILDDIR)/%.o,$(OCAML_ML_SRCS)) \
    $(OCAML_ML_CMXS) \
    $(OCAML_ML_DEPS)

# FIXME: this ought to depend on the *set* of .mli files in all the
# source directories, otherwise the dependencies will not be
# recomputed if a .mli is removed.
$(OCAML_ML_DEPS): private OCAMLDEP := $(OCAMLDEP)
$(OCAML_ML_DEPS): $(BUILDDIR)/%.ml.d: $(SRCDIR)/%.ml $(SRCDIR)/.merlin | $(BUILDDIR)
	@echo Generating dependencies for $<
	@$(OCAMLDEP) $< | sed -E "s#([A-Za-z_0-9]+.cm)#_build/\1#g" > $@

-include $(OCAML_ML_DEPS)

# FIXME: write these into the .d file, effectively caching the rules for this file?
# if these are written into the .d file, then we can make the targets depend on that file, rather than the whole of .merlin
$(OCAML_ML_CMOS): private OCAMLC := $(OCAMLC)
$(OCAML_ML_CMOS): $(BUILDDIR)/%.cmo: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(bytecode)'
	@$(OCAMLC) -o $@ -c $<

$(OCAML_ML_CMXS): private OCAMLOPT := $(OCAMLOPT)
$(OCAML_ML_CMXS): $(BUILDDIR)/%.cmx: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(native)'
	@$(OCAMLOPT) -o $@ -c $<

######################################################################
## .mli files
OCAML_MLI_SRCS := $(wildcard $(SRCDIR)/*.mli)
OCAML_MLI_CMIS := $(patsubst $(SRCDIR)/%.mli,$(BUILDDIR)/%.cmi,$(OCAML_MLI_SRCS))
OCAML_MLI_DEPS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_MLI_SRCS))
OCAML_PRODUCTS += $(OCAML_MLI_CMIS) $(OCAML_MLI_DEPS)

$(OCAML_MLI_DEPS): private $(OCAMLDEP) := $(OCAMLDEP)
$(OCAML_MLI_DEPS): $(BUILDDIR)/%.mli.d: $(SRCDIR)/%.mli $(SRCDIR)/.merlin | $(BUILDDIR)
	@echo Generating dependencies for $<
	@$(OCAMLDEP) $< | sed -E "s#([A-Za-z_0-9]+.cm)#_build/\1#g" > $@

-include $(OCAML_MLI_DEPS)

$(OCAML_MLI_CMIS): private OCAMLC := $(OCAMLC)
$(OCAML_MLI_CMIS): $(BUILDDIR)/%.cmi: $(SRCDIR)/%.mli $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling interface $*
	@$(OCAMLC) -o $@ -c $<

######################################################################
## .mll files
OCAML_LEX_SRCS := $(wildcard $(SRCDIR)/*.mll)
OCAML_LEX_MLS  := $(patsubst $(SRCDIR)/%.mll,$(SRCDIR)/%.ml,$(OCAML_LEX_SRCS))
OCAML_LEX_CMOS := $(patsubst $(SRCDIR)/%.mll,$(BUILDDIR)/%.cmo,$(OCAML_LEX_SRCS))
OCAML_LEX_CMXS := $(patsubst $(SRCDIR)/%.mll,$(BUILDDIR)/%.cmx,$(OCAML_LEX_SRCS))
OCAML_LEX_DEPS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_LEX_SRCS))
OCAML_PRODUCTS += \
    $(OCAML_LEX_CMOS) \
    $(patsubst $(SRCDIR)/%.mll,$(BUILDDIR)/%.cmi,$(OCAML_LEX_SRCS))  \
    $(patsubst $(SRCDIR)/%.mll,$(BUILDDIR)/%.o,$(OCAML_LEX_SRCS))    \
    $(OCAML_LEX_CMXS) \
    $(OCAML_LEX_DEPS)

# FIXME: error if there is a name.ml for any name.mll

$(OCAML_LEX_DEPS): private OCAMLDEP := $(OCAMLDEP)
$(OCAML_LEX_DEPS): $(BUILDDIR)/%.mll.d: $(SRCDIR)/%.mll $(SRCDIR)/.merlin | $(BUILDDIR)
	@$(OCAMLDEP) -pp 'ocamllex -q -o /dev/fd/1' $< | sed -E "s#([A-Za-z_0-9]+.cm)#_build/\1#g" > $@

-include $(OCAML_LEX_DEPS)

.INTERMEDIATE: \
  $(patsubst $(SRCDIR)/%.mll,$(SRCDIR)/%.ml,$(OCAML_LEX_SRCS))

$(OCAML_LEX_MLS): $(SRCDIR)/%.ml: $(SRCDIR)/%.mll
	@echo Generating lexer $*
	@ocamllex -q $< -o $@

# Need these rules to compile the outputs of ocamllex, because the
# static pattern rules for .ml files above don't know about the output
# of ocamllex. Same for menhir below.

$(OCAML_LEX_CMOS): private OCAMLC := $(OCAMLC)
$(OCAML_LEX_CMOS): $(BUILDDIR)/%.cmo: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(bytecode)'
	@$(OCAMLC) -o $@ -c $<

$(OCAML_LEX_CMXS): private OCAMLOPT := $(OCAMLOPT)
$(OCAML_LEX_CMXS): $(BUILDDIR)/%.cmx: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(native)'
	@$(OCAMLOPT) -o $@ -c $<

######################################################################
## .mly files, using menhir
OCAML_MENHIR_SRCS := $(wildcard $(SRCDIR)/*.mly)
OCAML_MENHIR_CMOS := $(patsubst $(SRCDIR)/%.mly,$(BUILDDIR)/%.cmo,$(OCAML_MENHIR_SRCS))
OCAML_MENHIR_CMXS := $(patsubst $(SRCDIR)/%.mly,$(BUILDDIR)/%.cmx,$(OCAML_MENHIR_SRCS))
OCAML_MENHIR_CMIS := $(patsubst $(SRCDIR)/%.mly,$(BUILDDIR)/%.cmi,$(OCAML_MENHIR_SRCS))
OCAML_MENHIR_DEPS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_MENHIR_SRCS))
OCAML_PRODUCTS    += \
    $(OCAML_MENHIR_CMOS) \
    $(OCAML_MENHIR_CMIS) \
    $(patsubst $(SRCDIR)/%.mly,$(BUILDDIR)/%.o,$(OCAML_MENHIR_SRCS)) \
    $(OCAML_MENHIR_CMXS) \
    $(OCAML_MENHIR_DEPS)

# FIXME: error if there is a name.mli or name.ml for any name.mly

$(OCAML_MENHIR_DEPS): private OCAMLDEP := $(OCAMLDEP)
$(OCAML_MENHIR_DEPS): $(BUILDDIR)/%.mly.d: $(SRCDIR)/%.mly $(SRCDIR)/.merlin | $(BUILDDIR)
	@menhir --depend --ocamldep '$(OCAMLDEP)' $< | sed -E "s#([A-Za-z_0-9]+.cm)#_build/\1#g" > $@

-include $(OCAML_MENHIR_DEPS)

# Mark Menhir's output as intermediate, so it gets deleted after a build
.INTERMEDIATE: \
  $(patsubst $(SRCDIR)/%.mly,$(SRCDIR)/%.ml,$(OCAML_MENHIR_SRCS)) \
  $(patsubst $(SRCDIR)/%.mly,$(SRCDIR)/%.mli,$(OCAML_MENHIR_SRCS))

# FIXME: can't use static pattern rules here, because then make will
# treat the .mli and .ml rules as completely separate and run menhir
# twice.
$(SRCDIR)/%.mli $(SRCDIR)/%.ml: private OCAMLC := $(OCAMLC)
$(SRCDIR)/%.mli $(SRCDIR)/%.ml: $(SRCDIR)/%.mly $(PPX_BINARIES)
	@echo Generating parser $*
	@menhir --infer --ocamlc '$(OCAMLC)' $<

$(OCAML_MENHIR_CMOS): private OCAMLC := $(OCAMLC)
$(OCAML_MENHIR_CMOS): $(BUILDDIR)/%.cmo: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(bytecode)'
	@$(OCAMLC) -o $@ -c $<

$(OCAML_MENHIR_CMXS): private OCAMLOPT := $(OCAMLOPT)
$(OCAML_MENHIR_CMXS): $(BUILDDIR)/%.cmx: $(SRCDIR)/%.ml $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling module $* '(native)'
	@$(OCAMLOPT) -o $@ -c $<

$(OCAML_MENHIR_CMIS): private OCAMLC := $(OCAMLC)
$(OCAML_MENHIR_CMIS): $(BUILDDIR)/%.cmi: $(SRCDIR)/%.mli $(SRCDIR)/.merlin $(PPX_BINARIES) | $(BUILDDIR) ocaml-tidy
	@echo Compiling interface $*
	@$(OCAMLC) -o $@ -c $<

######################################################################
## .mllib files
OCAML_LIB_SRCS  := $(wildcard $(SRCDIR)/*.mllib)
OCAML_LIB_CMAS  := $(patsubst $(SRCDIR)/%.mllib,$(BUILDDIR)/%.cma,$(OCAML_LIB_SRCS))
OCAML_LIB_CMXAS := $(patsubst $(SRCDIR)/%.mllib,$(BUILDDIR)/%.cmxa,$(OCAML_LIB_SRCS))
OCAML_LIB_DEPS  := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_LIB_SRCS))
OCAML_PRODUCTS  += \
    $(OCAML_LIB_CMAS) \
    $(OCAML_LIB_CMXAS) \
    $(patsubst $(SRCDIR)/%.mllib,$(BUILDDIR)/%.a,$(OCAML_LIB_SRCS))    \
    $(OCAML_LIB_DEPS)

# FIXME: use private target-specific variable to add linker options
# FIXME: what about lower-case .cmo files? This needs to be fixed in mlbindep

# FIXME: this ought to depend on the set of possible .ml,.mll,.mly
# files in the source directories
$(OCAML_LIB_DEPS): private MLBINDEP := $(MLBINDEP)
$(OCAML_LIB_DEPS): $(BUILDDIR)/%.mllib.d: $(SRCDIR)/%.mllib $(SRCDIR)/.merlin | $(BUILDDIR)
	@echo Generating dependencies for $<
	@$(MLBINDEP) $< > $@

-include $(OCAML_LIB_DEPS)

$(OCAML_LIB_CMAS): private OCAMLC := $(OCAMLC)
$(OCAML_LIB_CMAS): $(BUILDDIR)/%.cma: $(SRCDIR)/%.mllib $(SRCDIR)/.merlin | $(BUILDDIR) ocaml-tidy
	@echo Compiling library $* '(bytecode)'
	@$(OCAMLC) -a $(filter %.cmo,$+) -o $@

$(OCAML_LIB_CMXAS): private OCAMLOPT := $(OCAMLOPT)
$(OCAML_LIB_CMXAS): $(BUILDDIR)/%.cmxa: $(SRCDIR)/%.mllib $(SRCDIR)/.merlin | $(BUILDDIR) ocaml-tidy
	@echo Compiling library $* '(native)'
	@$(OCAMLOPT) -a $(filter %.cmx,$+) -o $@

######################################################################
## .mlbin files
OCAML_BIN_SRCS    := $(wildcard $(SRCDIR)/*.mlbin)
OCAML_BIN_BYTES   := $(patsubst $(SRCDIR)/%.mlbin,$(BYTE_BINDIR)/%,$(OCAML_BIN_SRCS))
OCAML_BIN_NATIVES := $(patsubst $(SRCDIR)/%.mlbin,$(NATIVE_BINDIR)/%,$(OCAML_BIN_SRCS))
OCAML_BIN_DEPS    := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.d,$(OCAML_BIN_SRCS))
OCAML_PRODUCTS    += $(OCAML_BIN_BYTES) $(OCAML_BIN_NATIVES) $(OCAML_BIN_DEPS)

# mlbindep could spit out:
# test/_build/byte_bin/test: lib/_build/generalities.cma test/_build/test.cmo | test/_build/byte_bin ocaml-tidy
#       @echo Linking executable '$(notdir $@)' '(bytecode)'
#       @$(OCAMLC_LINK) $(filter %.cmo %.cma,$+) -o $@
# test/_build/native_bin/test: \
#   lib/_build/generalities.cmxa \
#   test/_build/test.cmx
#
# where the command line is generated from the .merlin file

$(OCAML_BIN_DEPS): private MLBINDEP := $(MLBINDEP)
$(OCAML_BIN_DEPS): $(BUILDDIR)/%.mlbin.d: $(SRCDIR)/%.mlbin $(SRCDIR)/.merlin | $(BUILDDIR)
	@echo Generating dependencies for $<
	@$(MLBINDEP) $< > $@

-include $(OCAML_BIN_DEPS)

$(OCAML_BIN_BYTES): private OCAMLC_LINK := $(OCAMLC_LINK)
$(OCAML_BIN_BYTES): $(BYTE_BINDIR)/%: $(SRCDIR)/%.mlbin $(SRCDIR)/.merlin | $(BYTE_BINDIR) ocaml-tidy
	@echo Linking executable '$(notdir $@)' '(bytecode)'
	@$(OCAMLC_LINK) $(filter %.cmo %.cma,$+) -o $@

$(OCAML_BIN_NATIVES): private OCAMLOPT_LINK := $(OCAMLOPT_LINK)
$(OCAML_BIN_NATIVES): $(NATIVE_BINDIR)/%: $(SRCDIR)/%.mlbin $(SRCDIR)/.merlin | $(NATIVE_BINDIR) ocaml-tidy
	@echo Linking executable '$(notdir $@)' '(native)'
	@$(OCAMLOPT_LINK) $(filter %.cmx %.cmxa,$+) -o $@

OCAML_BIN_JS := $(patsubst $(SRCDIR)/%.mlbin,$(BUILDDIR)/%.js,$(OCAML_BIN_SRCS))
$(OCAML_BIN_JS): $(BUILDDIR)/%.js: $(BYTE_BINDIR)/% | $(BULIDDIR)
	js_of_ocaml --opt 2 $< -o $@

######################################################################
ifndef OCAML_TIDY
OCAML_TIDY := 1
ocaml-tidy:
	@for f in $(filter-out $(sort $(OCAML_PRODUCTS)),$(shell find $(BUILDDIRS) -type f)); \
	do \
	  echo Removing $$f '(no matching source file)'; \
	  rm $$f; \
	done
endif
