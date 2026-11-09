#!/bin/sh
# Regenerates the Fortran sources from the fypp templates in this directory: fypp/test/<path>.fypp becomes
# test/<path>.f90, and any other fypp/<path>.fypp becomes src/<path>.f90. Run from anywhere; needs fypp.
set -e
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root/fypp"
find . -name '*.fypp' ! -name common.fypp | while read -r template; do
  name=${template#./}
  case "$name" in
    test/*) out="../${name%.fypp}.f90" ;;
    *)      out="../src/${name%.fypp}.f90" ;;
  esac
  fypp --no-folding -I . "$name" "$out"
done
