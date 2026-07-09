#!/bin/bash

Rscript -e "source('examples/test-layouts.R')"

tree examples/official

mkdir -p examples/diffs

for f in examples/official/layout-*.html;
    do base=$(basename "$f" | sed 's/layout-/layout-/');
    out=examples/diffs/${base%.html}.diff
    echo "--- comparing $f vs examples/r/$base -> $out";
    diff -u "$f" "examples/shiny/$base" > "$out" || true;
    echo "wrote $out";
done

tree examples/diffs
