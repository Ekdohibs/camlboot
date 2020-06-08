
OBJS=interp.cmo
FLAGS=-package compiler-libs.common -linkpkg
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
CC=cc

mod.a: wrap.c wrap.ml
	$(OCAMLOPT) $(FLAGS) -output-obj -o mlwrap.o wrap.ml
	$(OCAMLC) -c wrap.c
	cp `$(OCAMLC) -where`/libasmrun.a mod.a && chmod +w mod.a
	ar r mod.a mlwrap.o wrap.o

interp: mod.a main.c
	cc -o interp -I `$(OCAMLC) -where` main.c mod.a -lm -ldl

clean:
	rm -f *.a *.o *.cmx *.cmi
