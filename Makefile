
OBJS=interp.cmo
FLAGS=-annot -g -package compiler-libs.common -linkpkg
OCAML=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmx:
	$(OCAMLOPT) $(FLAGS) -c $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

interp: $(OBJS)
	$(OCAML) $(FLAGS) -linkpkg -o $@ $<

interpopt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(FLAGS) -linkpkg -o $@ $<