S=cmo
OBJS=interp.$S
FLAGS=-annot -g -package compiler-libs.common -linkpkg
OCAML=ocamlfind ocamlc

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.ml.cmx:
	$(OCAML) $(FLAGS) -c $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

interp: $(OBJS)
	$(OCAML) $(FLAGS) -linkpkg -o $@ $(OBJS)
