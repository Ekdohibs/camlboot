#!/usr/bin/env bash
r=$(dirname $0)
root=$r/../..
$root/_boot/byterun/ocamlrun $r/lex.byte "$@"