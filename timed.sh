#!/usr/bin/env bash

r=$(dirname $0)
echo >> "$r/timings"
echo "$@" >> "$r/timings"
{ time { "$@" 2>&3; }; } 3>&2 2>> "$r/timings"
