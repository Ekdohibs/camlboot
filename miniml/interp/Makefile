TIMED=../../timed.sh

MINIML=guile ../compiler/compile.scm std_miniml_prefix.ml --open Std_miniml_prefix std.ml --open Std std_miniml.ml --open Std_miniml

OCAMLRUN=../../ocaml-src/byterun/ocamlrun
OCAMLLEX=../../ocaml-src/byterun/ocamlrun ../../ocaml-src/boot/ocamllex
OCAMLYACC=../../ocaml-src/yacc/ocamlyacc

COMMONOBJS=int32.ml int64.ml nativeint.ml seq.ml char.ml bytes.ml string.ml digest.ml marshal.ml array.ml list.ml stack.ml hashtbl.ml map.ml set.ml buffer.ml format.ml printf.ml arg.ml gc.ml filename.ml lexing.ml parsing.ml
PARSEOBJS=misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml longident.ml parsetree.mli docstrings.ml ast_helper.ml parser.ml lexer.ml parse.ml
INTERPOBJS=$(COMMONOBJS) $(PARSEOBJS) ../../interpreter/conf.ml ../../interpreter/data.ml ../../interpreter/envir.ml ../../interpreter/runtime_lib.ml ../../interpreter/runtime_base.ml ../../interpreter/eval.ml ../../interpreter/runtime_stdlib.ml ../../interpreter/runtime_compiler.ml ../../interpreter/primitives.ml ../../interpreter/interp.ml
LEXOBJS1=../../ocaml-src/lex/cset.ml ../../ocaml-src/lex/syntax.ml ../../ocaml-src/lex/parser.ml
LEXOBJS2=../../ocaml-src/lex/table.ml ../../ocaml-src/lex/lexgen.ml ../../ocaml-src/lex/compact.ml ../../ocaml-src/lex/common.ml ../../ocaml-src/lex/output.ml ../../ocaml-src/lex/outputbis.ml ../../ocaml-src/lex/main.ml
DEPENDOBJS=$(COMMONOBJS) $(PARSEOBJS) depend.ml makedepend.ml

LEXBOOTOBJS=$(COMMONOBJS) $(LEXOBJS1) ../../lex/lexer.ml $(LEXOBJS2)
LEXOBJS=$(COMMONOBJS) $(LEXOBJS1) lex/lexer.ml $(LEXOBJS2)

OPTCOMPILE=./interp -nopervasives -nostdlib -w -20-21 -I ../../interpreter

%.byte: std.ml

lex.boot.byte: $(LEXBOOTOBJS)
	$(MINIML) $(LEXBOOTOBJS) -o $@

lex/lexer.ml: lex.boot.byte ../../ocaml-src/lex/lexer.mll
	mkdir -p lex/
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

makedepend.byte: $(DEPENDOBJS)
	$(MINIML) $(DEPENDOBJS) -o $@

#interpopt.opt: $(INTERPOBJS) interp.byte
#	./genfileopt.sh
#	@echo "Compiling interpopt.opt, this make take a while..."
#	$(TIMED) ./interp -nopervasives -nostdlib -w -20-21 ../../ocaml-src/asmrun/libasmrun.a interpopt.ml -o interpopt.opt -ccopt "-lm -lpthread -ldl"

INTERPCMX=$(filter %.cmx,$(INTERPOBJS:.ml=.cmx))
interpopt.opt: interp.byte stdopt.cmx $(INTERPCMX) stdopt_exit.cmx
	$(OPTCOMPILE) ../../ocaml-src/asmrun/libasmrun.a $(filter %.cmx, $^) -o $@ -ccopt "-lm -lpthread -ldl"

lexer.ml: lex.byte lexer.mll
	$(OCAMLRUN) lex.byte lexer.mll

parser.ml: parser.mly
	$(OCAMLYACC) $<

%.cmi: interp.byte
%.cmx: interp.byte

stdopt.cmi: stdopt.mli
	$(OPTCOMPILE) -c $<

stdopt.cmx: stdopt.ml stdopt.cmi
	$(OPTCOMPILE) -c $<

%.cmi: %.mli stdopt.cmi
	$(OPTCOMPILE) -open Stdopt -c $<

%.cmx: %.ml stdopt.cmx
	$(OPTCOMPILE) -open Stdopt -c $<

stdopt.ml: std_opt_prefix.ml std.ml
	cat $^ > $@

stdopt.mli: std_opt_prefix.mli std.mli
	cat $^ > $@

stdopt_exit.mli:
	echo > $@

stdopt_exit.ml:
	echo "let () = __atexit ()" > $@



include .depend


.PHONY: depend
depend: makedepend.byte stdopt.ml stdopt.mli stdopt_exit.ml stdopt_exit.mli lexer.ml parser.ml
	./depend.sh -native -I ../../interpreter $(INTERPOBJS) $(INTERPOBJS:.ml=.mli) stdopt.ml stdopt.mli stdopt_exit.ml stdopt_exit.mli > .depend
