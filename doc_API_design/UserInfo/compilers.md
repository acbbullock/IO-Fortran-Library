---
title: Compiler-dependent behavior
author: Austin C Bullock
---

## Supported compilers

The library is tested in CI with `gfortran` 13 to 16, `ifx` 2025.3 and
2026, and `flang` 23 on Linux, with `gfortran` 16 and `flang` 23 on macOS
(Apple Silicon), and with `gfortran` 16 and `ifx` 2026 on Windows. It is
written to the Fortran 2018 standard.

The `real128` and `complex(real128)` routines are built on `ifx`, and on
`gfortran` targets that support `REAL(16)`. They are left out on other
compilers, including `flang` and `gfortran` on Apple Silicon. To override
this, compile with `-DIOFL_HAS_R128` or `-DIOFL_NO_R128`.

The sources must be preprocessed. `fpm` does this automatically. To
build by other means, add `-cpp` (`gfortran`, `flang`) or `-fpp` (`ifx`),
and add the `include` directory to the include path.

## Known compiler bugs

These were found while testing the library, and occur without any of its
code, so they can affect programs that use it:

* `gfortran` 15: `reshape` of an array constructor of a derived type with
  an allocatable component, such as `reshape([String("a"), ...], [2,4])`,
  gives wrong values. Assign the elements individually instead.
* `gfortran` 15 and 16: an assignment such as `a = [a(3), a(2), a(1)]`,
  for such a type, gives wrong values. Assign through a temporary array.

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
