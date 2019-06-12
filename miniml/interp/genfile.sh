#!/usr/bin/env bash
files=( buffer.ml lexing.ml parsing.ml misc.ml clflags.ml location.ml asttypes.mli warnings.ml syntaxerr.ml docstrings.ml longident.ml parsetree.mli ast_helper.ml parser.ml lexer.ml interp.ml )
modules=( Buffer Lexing Parsing Misc Clflags Location Asttypes Warnings Syntaxerr Docstrings Longident Parsetree Ast_helper Parser Lexer Interp )
out=out.ml
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
  camlboot_path_esc=$(realpath "$(dirname "$0")"/../.. | sed 's_/_\\/_g')
  sed -i "s#%CAMLBOOT_PATH%#$camlboot_path_esc#" $out
done