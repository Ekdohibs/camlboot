# OCaml interpreter

This directory contains an interpreter for OCaml, written in miniml. It aims to be as correct as possible while only using the untyped representation of the program.

It has three main modes:
- `./interp ocamlc`, which will interpret the compiler sources to get a replacement for `ocamlc`,
- `./interp ocamlopt`, which will interpret the compiler sources to get a replacement for `ocamlopt`,
- `./interp files [list of files]`, which will interpret the list of files given as argument.

For now, it is written in a subset of OCaml a bit larger than miniml, but we are working on making it compatible with miniml by avoiding the use of unnecessary features and adding other features to miniml.
