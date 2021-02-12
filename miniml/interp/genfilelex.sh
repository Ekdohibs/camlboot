#!/usr/bin/env bash
files=( int64.ml nativeint.ml seq.ml char.ml bytes.ml string.ml digest.ml marshal.ml array.ml list.ml stack.ml hashtbl.ml map.ml set.ml buffer.ml format.ml printf.ml arg.ml gc.ml filename.ml lexing.ml parsing.ml ../../ocaml-src/lex/cset.ml ../../ocaml-src/lex/syntax.ml ../../ocaml-src/lex/parser.ml ../../ocaml-src/lex/lexer.ml ../../ocaml-src/lex/table.ml ../../ocaml-src/lex/lexgen.ml ../../ocaml-src/lex/compact.ml ../../ocaml-src/lex/common.ml ../../ocaml-src/lex/output.ml ../../ocaml-src/lex/outputbis.ml ../../ocaml-src/lex/main.ml )
modules=( Int64 Nativeint Seq Char Bytes String Digest Marshal Array List Stack Hashtbl Map Set Buffer Format Printf Arg Gc Filename Lexing Parsing Cset Syntax Parser Lexer Table Lexgen Compact Common Output Outputbis Main )
out=outlex.ml
cat std.ml > $out
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
camlboot_path_esc=$(realpath "$(dirname "$0")"/../.. | sed 's_/_\\/_g')
sed -i "s#%CAMLBOOT_PATH%#$camlboot_path_esc#" $out
sed -i "s/lexbuf.Lexing.refill_buff/Lexing.refill_buff/" $out