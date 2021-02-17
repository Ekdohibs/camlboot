#!/usr/bin/env bash
r=$(dirname $0)
root=$r/../..
$root/ocaml-src/byterun/ocamlrun $r/cvt_emit.byte "$@"