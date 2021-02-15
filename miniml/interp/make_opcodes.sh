#!/usr/bin/env bash
r=$(dirname $0)
root=$r/../..
$root/_boot/byterun/ocamlrun $r/make_opcodes.byte "$@"