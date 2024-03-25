#!/usr/bin/env bash

CC=""
target=$1

target_dir=${target%.ml}


rm $target_dir/tygraphs/*
rm $target_dir/svg_types/*
mkdir -p $target_dir/tygraphs
mkdir -p $target_dir/svg_types
ODEBUG=true ./ocamlopt.opt $CC -I stdlib/ $target  -i -dump-dir $target_dir/tygraphs/
cd $target_dir/tygraphs
for i in *.dot
do
   dot -Tsvg $i > ../svg_types/${i/dot/svg}
done
