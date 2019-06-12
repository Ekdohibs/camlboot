OCAMLLEX=../../ocaml-src/byterun/ocamlrun ../../ocaml-src/boot/ocamllex
OCAMLYACC=../../ocaml-src/yacc/ocamlyacc

interp: interp.c main.c std.h
	gcc -g -O2 -I ../../ocaml-src/byterun/ interp.c ../../ocaml-src/byterun/libcamlrun.a -o interp -lm -ldl

interp.c: out.ml
	../compiler/miniml out.ml > interp.c

out.ml: buffer.ml lexing.ml parsing.ml misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml docstrings.ml longident.ml parsetree.mli ast_helper.ml parser.ml lexer.ml interp.ml
	./genfile.sh

lexer.ml: lexer.mll
	$(OCAMLLEX) $<
	sed -i "s/lexbuf.Lexing.refill_buff/Lexing.refill_buff/" $@

parser.ml: parser.mly
	$(OCAMLYACC) $<