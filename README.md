# camlboot: An OCaml bootstrap experiment

camlboot is an experiment on the boostraping of the OCaml compiler. It is composed of:

- An interpreter of OCaml, in the directory `interpreter/`, which is able to interpret the OCaml compiler. This interpreter is written in a subset of OCaml called miniml, for which a compiler is available as part of the experiment.
- A compiler for miniml, in the directory `miniml/compiler/`. This compiler compiles miniml to OCaml bytecode, which is then executed by the OCaml runtime. It is written in scheme (more specifically, guile), since the objective is to bootstrap OCaml.
- A handwritten lexer for the bootstrapping of ocamllex, in the directory `lex/`. This lexer is able to perform the lexing of ocamllex's own `lexer.mll`, the first step towards the bootstrap of ocamllex, and then OCaml.
