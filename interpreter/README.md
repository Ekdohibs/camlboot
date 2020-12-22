# OCaml interpreter

This directory contains an interpreter for OCaml, written in miniml. It aims to be as correct as possible while only using the untyped representation of the program.

It has three main modes:
- `OCAMLINTERP_COMMAND=ocamlc ./interp`, which will interpret the compiler sources to get a replacement for `ocamlc`,
- `OCAMLINTERP_COMMAND=ocamlopt ./interp`, which will interpret the compiler sources to get a replacement for `ocamlopt`,
- `OCAMLINTERP_COMMAND=files ./interp [list of files]`, which will interpret the list of files given as argument.

For now, it is written in a subset of OCaml a bit larger than miniml, but we are working on making it compatible with miniml by avoiding the use of unnecessary features and adding other features to miniml.
