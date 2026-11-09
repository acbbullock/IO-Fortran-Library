---
title: Compiler-dependent behavior
author: Austin C Bullock
---

## Supported compilers

The library is tested in CI with `gfortran` 13 to 16, `ifx` 2025.3 and
2026, and `flang` 23 on Linux (x86-64), with `gfortran` 14 and `flang` 23
on ARM64 Linux, `gfortran` 13 on big-endian s390x Linux, `gfortran` 16 and
`flang` 23 on macOS (Apple Silicon), `gfortran` 15 on macOS (Intel), and
`gfortran` 16 and `ifx` 2026 on Windows. CI also builds the library
without fpm, as described below. It is written to the Fortran 2018 standard.

The `real128` and `complex(real128)` routines are built on `ifx`, and on
`gfortran` targets that support `REAL(16)`. They are left out on other
compilers, including `flang` and `gfortran` on Apple Silicon. To override
this, compile with `-DIOFL_HAS_R128` or `-DIOFL_NO_R128`.

The sources must be preprocessed. `fpm` does this automatically. To
build by other means, compile `src/modules/io_fortran_lib_mod.f90`, then
`src/modules/io_strings_mod.f90`, then the submodules in
`src/io_fortran_lib/`, as `ci/build-without-fpm.sh` does. Add `-cpp`
(`gfortran`, `flang`) or `-fpp` (`ifx`),
and add the `include` directory to the include path.

## Unsupported compilers

`nvfortran` (NVIDIA HPC SDK 26.1) and AMD's AOCC `flang` 5.2 can't
compile the library. Both use the classic Flang front end, which lacks
four Fortran 2018 features that the library uses throughout:

* `select rank`, which every array file routine uses. NVIDIA has said it
  doesn't plan to implement it.
* Arrays above rank 7. The library supports up to rank 15.
* `implicit none (type, external)`.
* Elemental procedures that are also `recursive`.

LFortran can't compile the library yet. Versions 0.58 and 0.68 crash on
the first file. A separate workflow runs all three weekly, so a release
that can build the library will show up there.

## Known compiler bugs

These were found while testing the library, and occur without any of its
code, so they can affect programs that use it:

* `gfortran` 15: `reshape` of an array constructor of a derived type with
  an allocatable component, such as `reshape([String("a"), ...], [2,4])`,
  gives wrong values. Assign the elements individually instead.
* `gfortran` 15 and 16: an assignment such as `a = [a(3), a(2), a(1)]`,
  for such a type, gives wrong values. Assign through a temporary array.
* `gfortran` on Windows (MinGW with the Universal C Runtime, any version):
  formatted I/O reads freed memory, so a program that does a lot of it,
  such as one that converts many numbers with `str`, `String` or `cast`,
  can crash at random with `SIGSEGV` inside `setlocale`. This is
  [GCC bug 128183](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=128183).
  libgfortran saves the `LC_NUMERIC` name before each formatted statement
  and restores it afterwards, but the C runtime frees the saved name in
  between. Until it is fixed, compiling this C file into the program
  avoids it, by setting the locale once and keeping libgfortran from
  saving and restoring it. fpm compiles a `.c` file among a project's
  sources automatically:

```c
#if defined(__MINGW32__) && defined(__GNUC__) && !defined(__clang__)
#include <locale.h>
extern int _gfortrani_old_locale_ctr;
__attribute__((constructor)) static void pin_numeric_locale(void)
{
  setlocale(LC_NUMERIC, "C");
  _gfortrani_old_locale_ctr++;
}
#endif
```

  The library's own tests do this, in `test/suite/mingw_locale.c`.

## Character array literals

When writing text files, it's important to note that some compilers
implement extensions to the Fortran standard by default with regards
to character array literals. For example, the array literal

```fortran
header = [ "firstcol", "secondcol" ]
```

is not standard Fortran 2018 since the strings in the array do not have
identical length. Some compilers will accept this and others will not.
If required, simply add padding spaces to the left or right of each
string to match the length of the longest element. These padding
spaces will not be present in the output file.

@note Some compilers may allocate strings dynamically on the stack.
When reading large text files, this may result in a stack overflow or
segmentation fault unless the compiler is directed to allocate
everything on the heap. For example, one would specify `-heap-arrays 0`
for the Intel Fortran compiler on Linux (`/heap-arrays:0` on Windows).
