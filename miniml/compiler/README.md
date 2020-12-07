# miniml compiler

The file `compile.scm` is a single-file compiler for the miniml language, written in Scheme. It targets the OCaml bytecode for the version 4.07.0 of the OCaml compiler.

It is used as such:
```bash
$ guile compile.scm file.ml -o file.byte
```
The file `file.byte` will then contain executable OCaml bytecode, which can be run using `ocamlrun` from version 4.07.0 of the OCaml compiler (in this repository's `ocaml-src/` submodule).

It can only process a single file of input; in case you wish to use multiple files, you must first bundle them all into a single `.ml` file, putting the contents of file `x.ml` between `module X = struct`/`end` delimiters.

All OCaml external functions can be used with `external` declarations, but primitives work differently: they call a single bytecode instruction, with the instruction number specified in the external declaration; see `hello.ml` for examples.

`miniml` is completely untyped, and error reporting during compilation is very limited. However, since syntax and semantics are almost completely compatible with OCaml, in most cases you can just use OCaml to find the syntax or type error.

