#!/usr/bin/env bash
files=( int32.ml int64.ml nativeint.ml seq.ml char.ml bytes.ml string.ml digest.ml marshal.ml array.ml list.ml stack.ml hashtbl.ml map.ml set.ml buffer.ml format.ml printf.ml arg.ml gc.ml filename.ml lexing.ml parsing.ml misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml longident.ml parsetree.mli docstrings.ml ast_helper.ml parser.ml lexer.ml parse.ml ../../interpreter/conf.ml ../../interpreter/data.ml ../../interpreter/envir.ml ../../interpreter/runtime_lib.ml ../../interpreter/runtime_base.ml ../../interpreter/eval.ml ../../interpreter/runtime_stdlib.ml ../../interpreter/runtime_compiler.ml ../../interpreter/primitives.ml ../../interpreter/interp.ml )
modules=( Int32 Int64 Nativeint Seq Char Bytes String Digest Marshal Array List Stack Hashtbl Map Set Buffer Format Printf Arg Gc Filename Lexing Parsing Misc Clflags Location Asttypes Warnings Syntaxerr Longident Parsetree Docstrings Ast_helper Parser Lexer Parse Conf Data Envir Runtime_lib Runtime_base Eval Runtime_stdlib Runtime_compiler Primitives Interp )
out=interpopt.ml
cat std_opt_prefix.ml > $out
cat std.ml >> $out
for i in "${!files[@]}"; do
  f=${files[$i]}
  m=${modules[$i]}
  echo "module $m = struct" >> $out
  echo "# 1 \"$f\"" >> $out
  cat $f >> $out
  echo "# $(($(wc -l < $out) + 2)) \"$out\"" >> $out
  echo "end" >> $out
  echo >> $out
done
echo >> $out
echo "let () = __atexit ()" >> $out