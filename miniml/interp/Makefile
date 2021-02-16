MINIML=guile ../compiler/compile.scm std.ml --open Std

OCAMLRUN=../../ocaml-src/byterun/ocamlrun
OCAMLLEX=../../ocaml-src/byterun/ocamlrun ../../ocaml-src/boot/ocamllex
OCAMLYACC=../../ocaml-src/yacc/ocamlyacc

COMMONOBJS=int32.ml int64.ml nativeint.ml seq.ml char.ml bytes.ml string.ml digest.ml marshal.ml array.ml list.ml stack.ml hashtbl.ml map.ml set.ml buffer.ml format.ml printf.ml arg.ml gc.ml filename.ml lexing.ml parsing.ml
INTERPOBJS=$(COMMONOBJS) misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml longident.ml parsetree.mli docstrings.ml ast_helper.ml parser.ml lexer.ml parse.ml ../../interpreter/conf.ml ../../interpreter/data.ml ../../interpreter/envir.ml ../../interpreter/runtime_lib.ml ../../interpreter/runtime_base.ml ../../interpreter/eval.ml ../../interpreter/runtime_stdlib.ml ../../interpreter/runtime_compiler.ml ../../interpreter/primitives.ml ../../interpreter/interp.ml
LEXOBJS1=../../ocaml-src/lex/cset.ml ../../ocaml-src/lex/syntax.ml ../../ocaml-src/lex/parser.ml
LEXOBJS2=../../ocaml-src/lex/table.ml ../../ocaml-src/lex/lexgen.ml ../../ocaml-src/lex/compact.ml ../../ocaml-src/lex/common.ml ../../ocaml-src/lex/output.ml ../../ocaml-src/lex/outputbis.ml ../../ocaml-src/lex/main.ml

LEXBOOTOBJS=$(COMMONOBJS) $(LEXOBJS1) ../../lex/lexer.ml $(LEXOBJS2)
LEXOBJS=$(COMMONOBJS) $(LEXOBJS1) lex/lexer.ml $(LEXOBJS2)

%.byte: std.ml

lex.boot.byte: $(LEXBOOTOBJS)
	$(MINIML) $(LEXBOOTOBJS) -o $@

lex/lexer.ml: lex.boot.byte ../../ocaml-src/lex/lexer.mll
	$(OCAMLRUN) lex.boot.byte ../../ocaml-src/lex/lexer.mll -o $@

lex.byte: $(LEXOBJS)
	$(MINIML) $(LEXOBJS) -o $@

make_opcodes.ml: lex.byte ../../ocaml-src/tools/make_opcodes.mll
	$(OCAMLRUN) lex.byte ../../ocaml-src/tools/make_opcodes.mll -o make_opcodes.ml

make_opcodes.byte: $(COMMONOBJS) make_opcodes.ml
	$(MINIML) $(COMMONOBJS) make_opcodes.ml -o $@

cvt_emit.ml: lex.byte ../../ocaml-src/tools/cvt_emit.mll
	$(OCAMLRUN) lex.byte ../../ocaml-src/tools/cvt_emit.mll -o cvt_emit.ml

cvt_emit.byte: $(COMMONOBJS) cvt_emit.ml
	$(MINIML) $(COMMONOBJS) cvt_emit.ml -o $@

interp.byte: $(INTERPOBJS)
	$(MINIML) $(INTERPOBJS) -o $@

lexer.ml: lex.byte lexer.mll
	$(OCAMLRUN) lex.byte lexer.mll

parser.ml: parser.mly
	$(OCAMLYACC) $<