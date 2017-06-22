include $(shell ocamlc -where)/Makefile.config

PROG = bumpvsn
PKGS = batteries,re

.PHONY: clean install uninstall

$(PROG)$(EXE): $(PROG).ml
	ocamlfind ocamlopt -O2 -o $@ -package $(PKGS) -linkpkg $<
	strip $@

clean:
	$(RM) $(PROG)$(EXE) $(PROG).cmi $(PROG).cmx $(PROG).o

# TODO
install: $(PROG)$(EXE)

# TODO
uninstall: $(PROG)$(EXE)
