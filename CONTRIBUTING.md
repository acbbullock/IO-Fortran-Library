# Contributing to IO-Fortran-Library

Thank you for helping. This page covers building, testing, and the conventions that keep the library working on
every supported compiler.

## Building and testing

You need a Fortran 2018 compiler and [fpm](https://fpm.fortran-lang.org) 0.13 or later. CI tests `gfortran` 13 to
16, `ifx` 2025.3 and 2026, and `flang` 23 on Linux, with `gfortran` and `flang` on Apple Silicon macOS, `gfortran` on
Intel macOS, and `gfortran` and `ifx` on Windows.

```sh
fpm test                                   # build the library and run the whole suite
fpm test --compiler ifx                    # with another compiler
```

fpm 0.12 also works, but it doesn't enable the preprocessor for `flang`, so use
`fpm test --compiler flang-new --flag -cpp` there.

The tests also need the C compiler that fpm pairs with your Fortran compiler (`gcc`, `icx` or `clang`), for
`test/suite/mingw_locale.c`. It works around a libgfortran bug on Windows that crashes formatted I/O at random
(GCC bug 128183), and compiles to nothing elsewhere. The library itself is Fortran only. With `ifx` on Windows, add
`--c-compiler clang`: fpm can't drive `cl`, and the oneAPI environment there has `icx-cl` but no `icx`.

One build tests every kind and rank. A failing case ends `fpm test` with a nonzero exit status, and prints what the
case wrote. The suite takes well under a minute.

To run part of the suite, pass a pattern after `--`. It matches anywhere in `group/name`:

```sh
fpm test tests -- cast/                    # the cases in one group
fpm test tests -- crlf                     # every case with "crlf" in its name
fpm test tests -- --list                   # list the cases, with their markers
fpm test tests -- --in-process read_crlf   # run in one process, for a debugger
```

Each case normally runs in its own child process. That's how a case can check that a call ends the program with
`error stop`, and it means a crash in one case doesn't stop the others. A child's output is kept in
`test/output/tests/logs/<name>.log`.

## Adding a test case

Cases live in `test/suite/`, one module per group, such as `delimited_cases.f90` for `read_file` and
`write_file`. A case is a subroutine with no arguments. Write it in the group's module, and register it once in that
module's `register_*` subroutine:

```fortran
  subroutine register_delimited(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("delimited")
    ! ...
    call suite%add("read_tab_in_quotes", read_tab_in_quotes)
  end subroutine register_delimited

  subroutine read_tab_in_quotes()
    type(String)              :: s
    type(String), allocatable :: cells(:,:)

    call write_raw(scratch()//".txt", '"a'//IOFL_TAB//'b"'//IOFL_TAB//"c"//IOFL_LF)
    call s%read_file(scratch()//".txt", cell_array=cells, column_separator=IOFL_TAB)
    call expect_equal(size(cells, dim=2), 2, "A tab inside quotes is not a separator.")
    call expect_equal(cells(1,1), "a"//IOFL_TAB//"b", "The quoted cell keeps its tab.")
  end subroutine read_tab_in_quotes
```

What a case can use, from `test_support`:

- `expect_equal(actual, expected, message)` compares values of any kind, `String` or `character`, as scalars or
  rank 1 arrays, and prints both when they differ. Reals and complex values compare exactly; pass `rel_tol` to allow
  a relative difference. Character values must match in length too, unlike the intrinsic `==`.
- `expect_identical(actual, expected, message)` compares reals and complex values as identical: NaN matches NaN, and
  the sign of zero counts.
- `check(condition, message)` tests anything else.
- `scratch()` returns a path prefix unique to the case. Add a suffix and an extension: `scratch()//".csv"`. The
  output directory is emptied at the start of each run, so a case never sees an earlier run's files.
- `write_raw(file, content)` writes bytes exactly, and `read_raw(file)` reads them back.

A case reports every failed check, not just the first, so put the checks that later ones depend on first. A case
that should stop ends at its first failed check, since its `error stop` would otherwise hide the failure.

For data, `test_values` provides `test_values_of(mold, n)`: the edge values of the kind of `mold` (zeros, `huge`,
`tiny`, subnormals, NaN, infinities) followed by `n` random values. Call `reset_random()` first, so the case sees the
same values on every compiler and in any order.

### Markers

`suite%add` takes two optional markers:

- `expect=STOPS`: the case should end the program with `error stop`, as a call with no `stat` does on an error. The
  first line of its message must be readable.
- `status=KNOWN_BUG`: the case is expected to fail, because the bug it describes isn't fixed yet. If it starts
  passing, the suite fails, so the marker is removed in the same change as the fix. Open an issue for the bug and
  name it in the case's comment.
- `status=UNDEFINED`: like `KNOWN_BUG`, for a bug with undefined behavior, such as an out-of-bounds access, which can
  appear to work. Either result is accepted.

The usual way to fix a bug is to add a `KNOWN_BUG` case that shows it, then fix the bug and remove the marker in
one commit.

### Adding a group

Add a module `test/suite/<group>_cases.f90` with a public `register_<group>` subroutine, and call it from
`test/tests.f90`. Keep test modules in `test/suite/` itself: fpm only finds a module used by another module in the
same directory or below it.

## Generated sources

Most of `src`, and `test_support`, `test_values` and the round-trip groups in `test/suite`, are generated from
the [fypp](https://github.com/aradi/fypp) templates in `fypp/`, so that each kind and rank has its own procedure.
A generated file says so on its first line. Don't edit it; edit its template, then run

```sh
fypp/generate.sh
```

and commit both. You need fypp 3.2 (`pipx install fypp==3.2`). Shared lists of kinds and ranks are in
`fypp/common.fypp`. CI regenerates everything and fails if the result differs from what is committed.

`real128` procedures are wrapped in `#ifdef IOFL_HAS_R128`, from `include/iofl_config.inc`, because not every
compiler has that kind. Do the same in tests that use it.

## Conventions

- Standard Fortran 2018, with no compiler extensions, and `implicit none (type, external)` in every scope.
- Lines of at most 120 characters.
- An error is reported through `stat` and `errmsg` when they are present, and ends the program with `error stop`
  when they are not.
- Fortran 2023 features wait until every supported compiler has them (#54).

Compilers disagree more often than you'd expect. Before working around something, check
[the compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html), and when
you find a new compiler bug, reduce it to a small program with no library code and add it to #45.

## Pull requests

Work happens on the branch for the next release, such as `2.1`, and reaches `main` at release. Reference the issue
a change addresses, include a test case for a bug fix, and make sure CI passes.
