---
title: Compiler-dependent behavior
author: Austin C Bullock
---

When writing text files, it's important to note that some compilers implement extensions to the Fortran standard by default with regards to character array literals. For example, the array literal

```fortran
header = [ "firstcol", "secondcol" ]
```

is not standard Fortran 2018 since the strings in the array do not have identical length. Some compilers will accept this and others will not. If required, simply add padding spaces to the left or right of each string to match the length of the longest element. These padding spaces will not be present in the output file.

@note Some compilers may allocate strings dynamically on the stack. When reading large text files, this may result in a stack overflow or segmentation fault unless the compiler is directed to allocate everything on the heap. For example, one would specify `-heap-arrays 0` for the Intel Fortran compiler on Linux (`/heap-arrays:0` on Windows).
