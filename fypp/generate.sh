#!/bin/sh
# Regenerates the sources in src from the fypp templates in this directory. Run from anywhere; needs fypp.
set -e
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root/fypp"
find . -name '*.fypp' ! -name common.fypp | while read -r template; do
  name=${template#./}
  fypp --no-folding -I . "$name" "../src/${name%.fypp}.f90"
done
