interp: interp.c
	gcc -I ~/Projects/ocaml/byterun/ interp.c ~/Projects/ocaml/byterun/prims.o ~/Projects/ocaml/byterun/libcamlrun.a -o interp -lm -ldl

interp.c: out.ml
	../compiler/miniml out.ml > interp.c
