# Changelog

The changes in each release of IO-Fortran-Library. Versions follow
[semantic versioning](https://semver.org): a minor release such as 2.1 is
compatible with the 2.0 code that uses it, and a major release lists its
breaking changes first. The
[releases page](https://github.com/acbbullock/IO-Fortran-Library/releases)
has the same notes, along with the pre-releases.

## Unreleased

Work toward 2.3, tracked in the [2.3 milestone](https://github.com/acbbullock/IO-Fortran-Library/milestone/4).

### Changed

* With gfortran, writing and reading raw binary (`.dat`) and NumPy (`.npy`, `.npz`) files is 3–5 times as fast, and
  now matches plain unformatted stream I/O. A contiguous array is transferred as one block, which gfortran didn't do
  for an array of any rank (#93).

## [2.2.0](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v2.2.0) - 2026-11-09

New file formats and types: `.tsv` files, NumPy `.npz` archives, and `logical`, `String` and `character` arrays.
`from_file` returns a header's column names, and there are new string operations, also as functions on
`character` values in a new module, `io_strings`. CI adds ARM64, big-endian s390x, a build without fpm, test
coverage and benchmarks. Code written for 2.1 works unchanged. The work is tracked in the
[2.2 milestone](https://github.com/acbbullock/IO-Fortran-Library/milestone/3).

### Added

* `.tsv` files. `tsv` is a text file extension, and a `.tsv` file is separated by tabs unless `delim` (or
  `column_separator` for `read_file` and `write_file`) says otherwise (#70).
* `column_names` for `from_file`, which returns a text file's header row as a `String` array, one name per column,
  and implies `header=.true.` (#71).
* `String` methods `to_upper`, `to_lower`, `starts_with`, `ends_with` and `find`, and an optional `chars` for
  `trim` and `trim_inplace`, which removes the given characters instead of whitespace (#72).
* A module, `io_strings`, with the same operations as functions on `character` and `String` values: `to_upper`,
  `to_lower`, `starts_with`, `ends_with`, `find` and `trim(string, chars)`, named and called as in the Fortran
  standard library. They are kept out of `io_fortran_lib`, so that a program using it without an `only` list can
  still define procedures with these names (#72).
* `to_file` and `from_file` for `logical` arrays. Text files hold `T` and `F`, which list-directed input reads, or `1`
  and `0` with `fmt="bit"`; reading accepts any form list-directed input does, such as `.true.`. `.npy` files store
  them as NumPy's one-byte `bool`. Raw binary files are refused, since compilers store `logical` values differently
  (#73).
* `to_file` for `String` and `character` arrays, and `from_file` into `String` arrays, for text files of rank 1 or
  2. Cells are quoted and unquoted as `write_file` and `read_file` do (#73).
* NumPy `.npz` archives, as `numpy.savez` writes them. `to_file` and `from_file` take the array's name in a new
  `name` argument: `call to_file(x, file="data.npz", name="x")` adds `x` to the archive, or replaces the array of
  that name, and `call from_file("data.npz", into=x, name="x")` reads it. A new subroutine, `npz_names`, lists the
  arrays in an archive. Archives of 4 GiB and more use ZIP64 records. Compressed archives, from
  `numpy.savez_compressed`, can't be read, and give an error that says so (#74).
* Benchmarks against plain Fortran: `benchmarks/benchmark-compare.f90` times writing and reading CSV, raw binary,
  `.npy` and `.npz` files, converting numbers to and from text, splitting a CSV file into cells, and building a
  string, each against plain Fortran that does the same work. The Benchmarking page gives the results with gfortran,
  ifx and flang, and CI runs it at a quarter of the size. The slower cases are filed as #91 to #95 (#77).

### Changed

* The submodules are renamed with an `io_` prefix (`io_file`, `io_text`, `io_npy` and so on), and `fpm.toml` sets
  `module-naming = "io"`, which the fpm package registry requires. Programs only `use io_fortran_lib`, so nothing
  changes for them (#66).

### Fixed

* Reading a `.npy` file checks that its data fits in the file before allocating the array, so a damaged header gives
  an error instead of asking for a huge allocation (#74).
* The `to_file` reference said a rank 1 array's header could have `size(x)` names, and mentioned a `dim` argument
  that no longer exists. A rank 1 array is written as one column, with one name (#76).

### Tested platforms

* CI now runs the tests on ARM64 Linux, with `gfortran` and `flang`, and on big-endian s390x Linux, where a `.npy`
  file and a `.npz` archive written there are checked with NumPy. It also builds the library without fpm, with
  `gfortran` and `ifx`, as the README describes (#78, #79, #81).
* CI measures test coverage with `gfortran --coverage` and `gcovr`, and lists the error checks that no test reaches.
  Cases were added for the ones that can be reached: invalid ranks, locales, formats, delimiters and header sizes,
  a header with no data, and files that can't be opened (#76).
* `nvfortran` and AOCC `flang` lack `select rank` and arrays above rank 7, so they can't compile the library and are
  not supported. A separate weekly workflow runs them, and LFortran, to show when that changes (#75, #80, #82).

## [2.1.1](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v2.1.1) - 2026-11-09

The library's code is the same as in 2.1.0. This release fixes and extends the documentation and examples, and is
the first to be archived on Zenodo, with a DOI for citation.

### Added

* An `example` directory: the tutorial programs, a quick start and a NumPy `.npy` example, run with
  `fpm run --example <name>`. CI builds and runs them all, and the tutorials include them, so the documented code
  always compiles.
* A section on NumPy `.npy` files in the binary-files tutorial.
* `CITATION.cff`, so GitHub and Zenodo can give a citation for the library.
* A description, homepage, categories and keywords in `fpm.toml`.

### Changed

* The README is shorter and starts with a quick start and a list of features. The release notes moved to this
  file.

### Fixed

* Two tutorial programs did not compile with `gfortran` or `ifx`, because each was named like a procedure inside
  it. The compiler check in the string tutorial tested for compiler versions from 2024.

## [2.1.0](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v2.1.0) - 2026-11-09

Version 2.1 is compatible with 2.0: code that compiles against 2.0 compiles unchanged and gives the same results, except where noted under Fixed.

### Added

* `IOFL_`-prefixed names for the character constants: `IOFL_NL`, `IOFL_SPACE`, `IOFL_CR`, `IOFL_LF`, `IOFL_TAB`, `IOFL_FF`, `IOFL_VT`, `IOFL_BELL` and `IOFL_NUL`. The old names such as `CR`, `LF` and `TAB` stay, but they clash with variable names in a program that uses the module without an `only` list, so the documentation now uses the new names.
* NumPy `.npy` files, selected by the `npy` extension or `format="npy"`. `to_file` writes them and `from_file` reads them, taking the shape from the file, so `data_shape` is not needed and the wrong kind or rank is an error rather than garbage. Files from `numpy.save`, in C order or either byte order, read correctly. `real128` is not supported, since NumPy has no IEEE quad type.
* The error codes are exported as `IOFL_READ_ERR`, `IOFL_WRITE_ERR`, `IOFL_ALLOC_ERR` and `IOFL_ARG_ERR`, with the same values as before, so `stat` can be tested by name.

### Changed

* The `data` directory is gone, so a project that depends on the library no longer downloads 4.4 MB of test data. The tests generate their data, and the tutorials say where to get the files they use.
* CI uses fpm 0.13, which builds with `flang` under that name and enables its preprocessor, so the tests no longer need `flang-new` or `--flag -cpp`. fpm 0.12 still works.

### Known issue

* With `gfortran` on Windows, any program that does a lot of formatted I/O can crash at random inside `setlocale`, because libgfortran restores the numeric locale from a string the C runtime has already freed ([GCC bug 128183](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=128183)). The library's conversions do formatted I/O, so they are affected, but so is any `write` or `read` with a format. The [compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html) give a short C file that avoids it, which the tests now use.

### Fixed

* `fmt="e"` and `fmt="f"` dropped the sign of negative zero, writing `0.0e+000` for `-0.0`, so it read back as positive zero. Complex values with a negative zero imaginary part were also written as `1.0e+000+-0.0e+000j`. `fmt="z"` was already correct.
* `to_file` without `decimals` wrote text with at most 150 decimals, so with `fmt="f"` values below 1e-150 were written as zero, and `real64` values below about 1e-133 lost digits. It now uses the same default as `str`, which reads back exactly.

## [2.0.0](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v2.0.0) - 2026-11-07

Version 2.0.0 is a rewrite for correctness, portability and speed. Code written for 1.x should still compile, except as noted under breaking changes, but errors that 1.x let pass silently now stop the program unless `stat` is present.

### Breaking changes

* **Errors no longer pass silently.** If an error occurs and `stat` is not present, the program ends with `error stop`. Pass `stat` to handle errors yourself. Before, these calls returned as if nothing had happened.
* **`cast` rejects invalid numbers.** Text such as `"abc"`, `"12x"`, `"2*3.5"` or an out-of-range integer used to give a wrong value. It is now an error, reported through a new optional `stat` argument, which is elemental. The decimal mark must match the locale, an exponent needs its letter, and empty text is an error.
* **An invalid `fmt` or `locale`** ends the program with `error stop` in `str`, `String` and `cast` (or sets `stat` for `cast`). It used to return an empty string.
* **`decimals` rounds** instead of truncating, so `str(0.999, fmt="f", decimals=2)` gives `1.00`. The default `fmt="f"` output is rounded as well, and negative numbers now have the same number of digits as positive ones.
* **Quotes in delimited text.** `read_file` removes the quotes around a quoted cell and reads `""` as one quote, so `cell_array` no longer holds the quote characters. `write_file` quotes a cell only when it contains a separator character, a quote, `CR` or `LF`. An unclosed quote at the start of a cell is an error.
* **Uneven rows.** The first row sets the number of columns. Shorter rows are filled with empty cells, and a longer row is an error.
* **Empty numeric cells.** In `from_file`, an empty cell reads as NaN for `real` and `complex` data, and is an error for `integer` data.
* **`**` takes a scalar `ncopies`.** Arrays of `character` of any rank still work, but an array-valued `ncopies` is gone, as it was not valid Fortran.
* **`real128` procedures are only built when available:** on `ifx`, and on `gfortran` targets with `REAL(16)`. Other compilers, including `flang`, need `-DIOFL_HAS_R128` to enable them.
* **Building without fpm** now needs the preprocessor (`-cpp` or `-fpp`) and `include` on the include path.
* **The `randoms` module is no longer part of the library.** Its Gaussian sampler was adapted from ROOT and UNU.RAN, and it was only used by the tests.
* Unallocated and empty `String` values now compare and count the same, `trim` removes all whitespace rather than only spaces, `echo` accepts empty text, and a negative `**` count gives an empty result.

### Added

* `<`, `<=`, `>` and `>=` for `String`, and for `String` with `character`. Bytes are compared in order, and a shorter string that matches the start of a longer one comes first, so these agree with `==`. Trailing blanks are significant, unlike the intrinsic comparison of two `character` values.
* `read_line`, which reads the next record from a unit into a `String`: `call line%read_line(unit, stat=stat)`. At the end of the file, `stat` satisfies `is_iostat_end`.
* `count64`, which returns a 64-bit count. Together with `len64`, use it for strings over about 2.1 GB.
* An optional `unit` for `aprint`.
* An optional `format`, `"text"` or `"binary"`, for `to_file`, `from_file`, `read_file`, `write_file` and `echo`, so a file can have any extension or none.
* File extensions are no longer case sensitive, so `.CSV` works.

### Fixed

* **Strings over 2 GB lost data silently.** `as_str`, `str`, `trim`, `replace`, repeating and concatenating all gave wrong or empty results on such strings.
* `to_file` with an exact zero in `EU` locale wrote a file that could not be read back.
* NaN and infinity could crash `str`, or be written in a form that could not be read back. They are now written as `NaN`, `Inf` and `-Inf`.
* `fmt="f"` failed on very large and very small values.
* `from_file` aborted on a bad number even with `stat`. It now reports the text with its row and column.
* `read_file` dropped or crashed on a file with no final newline, and mishandled CRLF. Both now work with the default `row_separator`.
* `write_file` overflowed the heap on an unallocated cell.
* `to_file` with a scalar and no `stat` segfaulted, and `join` lost separators between empty tokens.
* `str` and `String` returned nothing for `complex(real64)` with a zero part and `fmt="f"`.
* `fmt="z"` lost the sign of negative zero.
* `replace` with an unallocated `substring` gave a garbled result. It now acts as an empty string, as documented.
* `aprint` cut the last character from complex elements written with `im`, so `1.00+2.00j` printed as `1.00+2.00`.
* Files were left open after some errors.
* The library now builds on `ifx` 2026 and `flang`, and no longer crashes on `gfortran` 13-15 from its `String` finalizer, which has been removed.

### Performance

`push`, `replace` and `join` are now linear rather than quadratic:

| Operation | v1.3.1 | v2.0.0 |
|---|---|---|
| `push`, 40,000 appends | 5.32 s | 0.002 s |
| `push`, 2.56 million appends | did not finish in 30 s | 0.054 s |
| `replace`, 40,000 matches | 0.407 s | under 0.01 s |
| `replace`, 640,000 matches | did not finish in 40 s | 0.024 s |

Validation costs some speed when reading and writing files: compared with v1.3.1, `from_file` takes about 10-15% longer for `real` data and about 30% longer for `integer` data, and `write_file` about 50% longer on a large table. Absolute times are small (0.55 s for 800,000 reals).

### Tested compilers

CI tests `gfortran` 13-16, `ifx` 2025.3 and 2026, and `flang` 23 on Linux, `gfortran` 16 and `flang` 23 on macOS (Apple Silicon), and `gfortran` 16 and `ifx` 2026 on Windows. See the [compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html) for details.

### Known compiler bugs

These occur with no library code involved, so they can affect your own programs:

* `gfortran` 15: `reshape` of an array constructor of a type with an allocatable component, such as `reshape([String("a"), ...], [2,4])`, gives wrong values.
* `gfortran` 15 and 16: `a = [a(3), a(2), a(1)]` with such a type gives wrong values.

The [compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html) list the others found during this release.

## [1.3.1](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v1.3.1) - 2024-03-17

### Improved

* Fixed a "z" format string conversion bug.
* Updated documentation.
* Modified benchmark programs.

## [1.2.1](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v1.2.1) - 2023-02-07

### Added

* Added implementations for
[cast](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/cast.html).

### Improved

* Improved performance for
[count](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/String-methods.html#count),
[read_file](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/String-methods.html#read_file),
and [write_file](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/String-methods.html#write_file),
as well as [split](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/join-split.html)
for very large strings. The read/write performance is 2-4x faster than
previous (compiler-dependent).

## [1.2.0](https://github.com/acbbullock/IO-Fortran-Library/releases/tag/v1.2.0) - 2023-01-26

### New features

* The `String` type has been greatly expanded and can now serve as an
  interface for advanced string manipulations and text file I/O. See the
  references for
  [type-bound procedures](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/String-methods.html)
  and the [tutorials](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Examples/index.html)
  for more information.
* New [constants](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/constants.html)
  have been added for public use to facilitate file I/O consistently on
  a variety of platforms.
* New interface [String](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/String.html)
  for an elemental version of `str` with a return type of `String`.
* New interface [cast](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/cast.html)
  for casting `character` and `String` data into numeric variables.
* New interfaces [join and split](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/join-split.html)
  for joining and splitting strings.
* New [operator interfaces](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/operators.html)
  for string manipulations involving both `character` and `String`
  types.

### Added

* Slightly modified interfaces for
  [echo](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/echo.html)
  and [from_file](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/from_file.html)
  to accept an additional optional argument each.
* Test programs have been added in `/test`, which are all passing in
  the following configurations with lowest and highest optimizations
  enabled:
  * GNU Fortran Compiler v11.3.0 on Linux
  * Intel Fortran Compiler v2023.0.0 on Linux
  * Intel Fortran Compiler Classic v2021.8.0 on Linux
  * GNU Fortran Compiler v11.2.0 on Windows 11
  * Intel Fortran Compiler v2023.0.0 on Windows 11
  * Intel Fortran Compiler Classic v2021.8.0 on Windows 11

### Improved

* Removed vertical bars in
  [aprint](https://acbbullock.github.io/IO-Fortran-Library/doc/page/Ref/aprint.html)
  which were displayed incorrectly in terminals with display encoding
  different from UTF-8.
* Text file I/O has been optimized for performance and memory usage,
  and expanded to support processing of large files.
