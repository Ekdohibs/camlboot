interp: interp.c main.c std.h
	gcc -g -I ~/Projects/ocaml/byterun/ interp.c ~/Projects/ocaml/byterun/libcamlrun.a -o interp -lm -ldl

interp.c: out.ml
	../compiler/miniml out.ml > interp.c
