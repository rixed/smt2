# vim:ft=make
LEX = ocamllex
YACC = ocamlyacc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep
OCAMLOPTFLAGS = -annot
bin_dir ?= $(shell opam config var bin)

ifdef NDEBUG
OCAMLOPTFLAGS += -noassert -O2
STRIP_BIN = 1
else
OCAMLOPTFLAGS += -g
endif

all: smt2.cmxa smt2-tool

.SUFFIXES: .ml .mli .mll .mly .cmx .cmo .cmi

.PHONY: clean dep install uninstall reinstall

dep:
	$(RM) .depend
	$(MAKE) .depend

SMT2_SOURCES = \
	Smt2Types.ml \
	Smt2Parser.ml \
	Smt2Lexer.ml

SOURCES = \
	$(SMT2_SOURCES) \
	Smt2Tool.ml

.depend: $(SOURCES)
	@echo '== Generating dependencies'
	@$(OCAMLDEP) $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@
	@echo >> $@
	@echo '# Those are statically added by Makefile:' >> $@
	@echo 'Smt2Parser.cmi: Smt2Types.cmx' >> $@

include .depend


# Build

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

Smt2Lexer.ml: Smt2Lexer.mll
	$(LEX) $<

Smt2Parser.ml: Smt2Parser.mly
	$(YACC) $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -o $@

smt2.cmxa: $(SMT2_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $^ -o $@

smt2-tool: smt2.cmxa Smt2Tool.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg $^ -o $@


# Install

install: install-bin install-lib

install-lib: \
		META \
		$(filter %.cmi, $(SMT2_SOURCES:.ml=.cmi)) \
		$(filter %.cmx, $(SMT2_SOURCES:.ml=.cmx)) \
		smt2.cmxa \
		smt2.a
	@echo '== Installing OCaml libs'
	ocamlfind install smt2 $^

install-bin: smt2-tool
	@echo '== Installing $^ in $(bin_dir)'
	install -d $(bin_dir)
	install $^ $(bin_dir)/
ifdef STRIP_BIN
	for f in $^; do \
	   strip $(bin_dir)/$$(basename $$f) || true ;\
	done
endif

uninstall:
	@echo '== Uninstalling'
	ocamlfind remove smt2
	$(RM) $(bin_dir)/smt2-tool

reinstall:
	$(MAKE) uninstall && \
	$(MAKE) install



# Clean

# Deletes all transient productions
clean:
	$(RM) Smt2Types.cmx Smt2Types.cmo Smt2Types.cmi Smt2Types.cmt Smt2Types.o
	$(RM) Smt2Lexer.cmx Smt2Lexer.cmo Smt2Lexer.cmi Smt2Lexer.cmt Smt2Lexer.o Smt2Lexer.ml
	$(RM) Smt2Parser.cmx Smt2Parser.cmo Smt2Parser.cmi Smt2Parser.cmt Smt2Parser.o Smt2Parser.ml Smt2Parser.mli
	$(RM) Smt2Tool.cmx Smt2Tool.cmo Smt2Tool.cmi Smt2Tool.cmt Smt2Tool.o

# Also deletes final productions
cleaner: clean
	$(RM) smt2-tool smt2.cmxa smt2.a
