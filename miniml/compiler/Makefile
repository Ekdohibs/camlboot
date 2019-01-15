OBJS=lexer.cmo parser.cmo compile.cmo driver.cmo
GENERATED=lexer.ml parser.ml parser.mli
FLAGS=-annot -g
MENHIR_FLAGS=-v --infer --ocamlc "ocamlc $(FLAGS)"
OCAML=ocamlc

all: miniml

.PHONY: miniml
miniml: $(OBJS)
	$(OCAML) $(FLAGS) -o $@ $(OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	$(OCAML) $(FLAGS) -c  $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir $(MENHIR_FLAGS) $<

.mly.mli:
	menhir $(MENHIR_FLAGS) $<

parser.mly: ast.cmi

clean:
	rm -f *.cm[iox] *.o *.annot *~ miniml $(GENERATED)
	rm -f parser.output parser.automaton parser.conflicts
	rm -f .depend

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
