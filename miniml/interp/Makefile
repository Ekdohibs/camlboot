OCAMLLEX=../../ocaml-src/byterun/ocamlrun ../../ocaml-src/boot/ocamllex
OCAMLYACC=../../ocaml-src/yacc/ocamlyacc

INTERPOBJS=int32.ml int64.ml nativeint.ml seq.ml char.ml bytes.ml string.ml digest.ml marshal.ml array.ml list.ml hashtbl.ml map.ml set.ml buffer.ml format.ml printf.ml arg.ml gc.ml filename.ml lexing.ml parsing.ml misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml longident.ml parsetree.mli docstrings.ml ast_helper.ml parser.ml lexer.ml parse.ml ../../interpreter/conf.ml ../../interpreter/data.ml ../../interpreter/envir.ml ../../interpreter/runtime_lib.ml ../../interpreter/runtime_base.ml ../../interpreter/eval.ml ../../interpreter/runtime_stdlib.ml ../../interpreter/runtime_compiler.ml ../../interpreter/primitives.ml ../../interpreter/interp.ml

interp.byte: std.ml $(INTERPOBJS)
	guile ../compiler/compile.scm std.ml --open Std $(INTERPOBJS) -o $@

lexer.ml: lexer.mll
	$(OCAMLLEX) $<
	#sed -i "s/lexbuf.Lexing.refill_buff/Lexing.refill_buff/" $@

parser.ml: parser.mly
	$(OCAMLYACC) $<