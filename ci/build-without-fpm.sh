#!/bin/bash
# Builds the library and the test suite without fpm, the way README.md describes, and runs the suite.
#
#   ci/build-without-fpm.sh <compiler> <preprocessor flag> <build directory> [compiler flags...]
#   ci/build-without-fpm.sh gfortran -cpp build/nofpm -O2
#
# Run it from the repository root. The library is build/nofpm/libIO-Fortran-Library.a, with its .mod files beside it.
set -euo pipefail

fc=$1; pp=$2; out=$3; shift 3; flags=("$@")
root=$(pwd)
inc=(-I"$root/include")

rm -rf "$out" && mkdir -p "$out" && cd "$out"

# The library: the module first, then io_strings, which uses it, then the submodules, which use both.
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -c "$root/src/modules/io_fortran_lib_mod.f90"
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -c "$root/src/modules/io_strings_mod.f90"
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -c "$root"/src/io_fortran_lib/*.f90
ar rcs libIO-Fortran-Library.a ./*.o

# The tests: test_values and test_support, then the case modules that use them, then the driver.
mkdir -p tests && cd tests
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -I.. -c "$root/test/suite/test_values.f90" "$root/test/suite/test_support.f90"
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -I.. -c "$root"/test/suite/*_cases.f90
"$fc" "$pp" "${flags[@]}" "${inc[@]}" -I.. "$root/test/tests.f90" ./*.o ../libIO-Fortran-Library.a -o tests

# The suite writes to test/output, relative to the repository root.
cd "$root"
"$out/tests/tests"
