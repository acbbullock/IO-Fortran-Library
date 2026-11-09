# IO-Fortran-Library

[![CI](https://github.com/acbbullock/IO-Fortran-Library/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/acbbullock/IO-Fortran-Library/actions/workflows/ci.yml)
[![Documentation](https://github.com/acbbullock/IO-Fortran-Library/actions/workflows/docs.yml/badge.svg?branch=main)](https://acbbullock.github.io/IO-Fortran-Library/)
[![Latest release](https://img.shields.io/github/v/release/acbbullock/IO-Fortran-Library)](https://github.com/acbbullock/IO-Fortran-Library/releases/latest)
[![License: MIT](https://img.shields.io/github/license/acbbullock/IO-Fortran-Library)](LICENCE)
[![Fortran 2018](https://img.shields.io/badge/Fortran-2018-734f96)](https://fortran-lang.org)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24068278.svg)](https://doi.org/10.5281/zenodo.24068278)

A Fortran library for reading and writing text, CSV, binary and NumPy
`.npy` and `.npz` files, converting between numbers and text, and working with
strings through a `String` type. It is a single module, `io_fortran_lib`,
written in standard Fortran 2018 with no dependencies. The routines
accept all intrinsic numeric types and kinds (`int8` to `int64`, and
`real32` to `real128` for `real` and `complex` where the compiler has
them), and arrays up to rank 15.

## Quick start

Add the library to your [fpm](https://fpm.fortran-lang.org) project's
`fpm.toml`:

```toml
[dependencies]
IO-Fortran-Library = { git="https://github.com/acbbullock/IO-Fortran-Library", tag="v2.1.1" }
```

Then `use io_fortran_lib`. This program writes an array to a CSV file and
reads it back, does the same with a NumPy file, and builds a line of text
from a number. It is [`example/quickstart.f90`](example/quickstart.f90),
so `fpm run --example quickstart` runs it from a clone of this repository:

<!-- This block must match example/quickstart.f90; CI checks it. -->
```fortran
program quickstart
    use io_fortran_lib, only: String, str, to_file, from_file, aprint
    implicit none (type, external)

    real              :: x(3,2) = reshape([1.0, 2.5, -3.0, 4.0, 5.5, 6.25], [3,2])
    real, allocatable :: y(:,:), z(:,:)
    type(String)      :: line

    call to_file(x, file="x.csv", header=["x"])     ! A header row x1,x2, then three rows of data
    call from_file("x.csv", into=y, header=.true.)  ! y is allocated with shape [3,2]
    call aprint(y, fmt="f", decimals=2)

    call to_file(x, file="x.npy")                   ! In Python: x = numpy.load("x.npy")
    call from_file("x.npy", into=z)                 ! The shape comes from the file

    line = String("x(2,1) = "//str(x(2,1), fmt="f", decimals=2))
    write(*,"(a)") line%as_str()                     ! x(2,1) = 2.50
    write(*,*) "Read back exactly: ", all(y == x) .and. all(z == x)
end program quickstart
```

## Features

* **Delimited text files.** `to_file` and `from_file` write and read
  numeric, `logical` and text arrays as CSV or any other delimiter, with
  an optional header.
  Values are written in scientific (`e`), fixed (`f`) or hexadecimal (`z`)
  form, all of which read back exactly, with a `.` or `,` decimal mark.
  For tables of mixed types, `read_file` and `write_file` read and write
  a whole file as an array of `String` cells, handling quoted cells.
* **Binary and NumPy files.** Raw binary `.dat` and `.bin` files, and
  NumPy `.npy` files, including `logical` arrays as `bool`, that
  `numpy.load` reads and `numpy.save` writes. A `.npy` file carries its
  own shape, kind and byte order. A `.npz` archive holds several arrays
  by name, as `numpy.savez` writes them.
* **Numbers and text.** `str`, `String` and `cast` convert between numbers
  and text in either direction, in any of the formats above, and report
  text that is not a valid number instead of returning a wrong value.
* **A `String` type.** Operators for concatenation and for removing or
  repeating text, `split` and `join`, `replace`, `count` and `trim`,
  case conversion, `starts_with`, `ends_with` and `find`, ordering
  comparisons, and reading a file whole or a line at a time. The
  `io_strings` module provides the same operations for `character`
  values, with the names used by the Fortran standard library.
* **Output.** `aprint` prints an array as a table, and `echo` appends
  text to a log file.
* **Errors that are not silent.** A failing call stops the program with a
  message, unless it is given `stat` (and optionally `errmsg`) to handle
  the error itself.

## Documentation

The [documentation](https://acbbullock.github.io/IO-Fortran-Library/)
has a [reference](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/index.html)
for every interface,
[tutorials](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Examples/index.html)
with complete programs, and
[notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/index.html)
on file formats, error handling and compilers. The tutorial programs are
in [`example`](example), where CI builds and runs them.

## Compilers

The library is tested with `gfortran` 13 to 16, `ifx` 2025.3 and 2026, and
`flang` 23 on Linux (x86-64), with `gfortran` 14 and `flang` 23 on ARM64
Linux, `gfortran` 13 on big-endian s390x Linux, `gfortran` 16 and `flang` 23
on macOS (Apple Silicon), `gfortran` 15 on macOS (Intel), and `gfortran` 16
and `ifx` 2026 on Windows. See the [CI workflow](.github/workflows/ci.yml) for details,
and the
[compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html)
for known compiler bugs, including one in `gfortran` on Windows that can
crash formatted I/O and how to avoid it.

`nvfortran` (NVIDIA HPC SDK) and AMD's AOCC `flang` are not supported. They
lack parts of Fortran 2018 that the library relies on, most of all
`select rank`, as well as arrays above rank 7. A separate
[workflow](.github/workflows/experimental.yml) runs them weekly, along with
LFortran, to show when a new release can build the library.

The `real128` routines are enabled on `ifx`, and on `gfortran` targets with
`REAL(16)`. They are left out on other compilers, including `flang`. To
override this detection, compile with `-DIOFL_HAS_R128` or `-DIOFL_NO_R128`.

`fpm` handles the preprocessor, include path and compile order for you.
To compile the sources by other means, add `-cpp` (`gfortran`, `flang`) or
`-fpp` (`ifx`), add `include` to the include path, and compile
`src/modules/io_fortran_lib_mod.f90`, then `src/modules/io_strings_mod.f90`,
then `src/io_fortran_lib/*.f90`. [`ci/build-without-fpm.sh`](ci/build-without-fpm.sh)
does exactly this, and CI runs it.

## Contributing

Bug reports, questions and pull requests are welcome. Run the tests with
`fpm test`. [CONTRIBUTING.md](CONTRIBUTING.md) explains the test suite,
the generated sources and the conventions the code follows. For questions
and ideas, use
[Discussions](https://github.com/acbbullock/IO-Fortran-Library/discussions);
for bugs, open an
[issue](https://github.com/acbbullock/IO-Fortran-Library/issues/new/choose).

## Changelog

[CHANGELOG.md](CHANGELOG.md) has the notes for every release. Version 2.1
added NumPy `.npy` files, `IOFL_`-prefixed names for the exported
constants and error codes, and two fixes for values written with
`fmt="e"` and `fmt="f"`.

## Citing

If you use the library in published work, please cite it. Each release
is archived on Zenodo, and
[10.5281/zenodo.24068278](https://doi.org/10.5281/zenodo.24068278) always
refers to the latest version; Zenodo lists a DOI for each version too.
GitHub's "Cite this repository" button, from [CITATION.cff](CITATION.cff),
gives the reference in APA and BibTeX form.

## License

All source code referenced is distributed under the
[MIT license](https://github.com/acbbullock/IO-Fortran-Library/blob/main/LICENCE).

## Contact

For bug fixes or feature requests, feel free to open an issue at the
[project repository](https://github.com/acbbullock/IO-Fortran-Library/issues)
or contact [acb.bullock@gmail.com](mailto:acb.bullock@gmail.com).
