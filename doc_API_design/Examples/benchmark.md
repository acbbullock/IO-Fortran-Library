---
title: Benchmarking
author: Austin C Bullock
---

## Compared with plain Fortran

`benchmarks/benchmark-compare.f90` times the library's common operations
against plain Fortran that does the same work, so you can see what the
library costs. Each operation checks that it got the values back exactly,
and each time is the best of three runs.

The comparisons are:

| Operation | Library | Plain Fortran |
|---|---|---|
| Write a CSV file | `to_file` | formatted `write`, a row at a time |
| Read a CSV file | `from_file` | list-directed `read`, a row at a time |
| Write and read a raw binary file | `to_file`, `from_file` (`.dat`) | unformatted stream `write`, `read` |
| Write and read a NumPy file | `to_file`, `from_file` (`.npy`, `.npz`) | unformatted stream `write`, `read` |
| Numbers to text | `cast` into a `String` array | internal `write` into a `character` array |
| Text to numbers | `cast` from a `String` array | internal `read` |
| Split a CSV file into cells | `String%read_file` | `read` each line, copy the text between commas |
| Build a string | `String%push` | a buffer that doubles when full |

The text operations use 1,000,000 `real64` values, as 20,000 rows of 50,
written with 17 digits after the point (`es25.17e3`), as the library
writes them. The binary operations use 2<sup>25</sup> `real64` values
(256 MiB), because they are much faster. The plain versions have no
error handling, so a ratio near 1 means the library's checks and
messages cost little.

### Results

Run on:
- an Intel Xeon w5-3435X (16 cores, 32 threads, 45 MiB L3 cache), using
  one thread;
- 118 GiB of memory;
- Fedora Linux 46 under WSL2 (kernel 6.18) on Windows, with the files on
  WSL2's ext4 virtual disk.

The files fit in the page cache, so the times are the CPU and memory work
of each operation, not the disk's speed.

The compilers were gfortran 16.2.1, ifx 2026.2.0 and flang 23.1.5, each
with `-O2` through fpm 0.13 (`fpm run benchmark-compare --flag -O2`).

The ratio of the library's time to plain Fortran's (lower is better,
and 1.0 is the same speed):

| Operation | gfortran | ifx | flang |
|---|--:|--:|--:|
| Write a CSV file | 4.9 | 5.6 | 4.2 |
| Read a CSV file | 2.8 | 3.5 | 2.1 |
| Write a raw binary file | 4.9 | 0.9 | 1.0 |
| Read a raw binary file | 3.1 | 1.0 | 1.0 |
| Write a `.npy` file | 5.1 | 1.2 | 1.0 |
| Read a `.npy` file | 2.7 | 0.7 | 0.8 |
| Write a `.npz` file | 11.6 | 7.6 | 3.8 |
| Read a `.npz` file | 3.0 | 0.7 | 0.8 |
| Numbers to text | 2.4 | 3.3 | 3.4 |
| Text to numbers | 1.2 | 2.5 | 1.2 |
| Split a CSV file into cells | 1.2 | 4.6 | 6.6 |
| Build a string | 0.8 | 0.9 | 0.9 |

The times, in milliseconds, as library / plain:

| Operation | gfortran | ifx | flang |
|---|--:|--:|--:|
| Write a CSV file | 1606 / 326 | 1163 / 208 | 1075 / 256 |
| Read a CSV file | 764 / 269 | 967 / 275 | 761 / 368 |
| Write a raw binary file | 439 / 90 | 82 / 88 | 228 / 235 |
| Read a raw binary file | 485 / 158 | 150 / 144 | 297 / 297 |
| Write a `.npy` file | 458 / 90 | 107 / 88 | 230 / 235 |
| Read a `.npy` file | 433 / 158 | 107 / 144 | 246 / 297 |
| Write a `.npz` file | 1044 / 90 | 670 / 88 | 897 / 235 |
| Read a `.npz` file | 466 / 158 | 105 / 144 | 241 / 297 |
| Numbers to text | 1555 / 644 | 1016 / 310 | 1018 / 299 |
| Text to numbers | 627 / 529 | 785 / 309 | 578 / 488 |
| Split a CSV file into cells | 79 / 67 | 115 / 25 | 118 / 18 |
| Build a string | 14 / 18 | 17 / 18 | 10 / 10 |

What the numbers say:
- **Binary files are as fast as plain stream I/O with ifx and flang.**
  With gfortran, `.dat` and `.npy` files take 3–5× as long
  ([#93](https://github.com/acbbullock/IO-Fortran-Library/issues/93)).
- **Writing numbers as text costs 2.4–5.6× plain formatted output**, for
  `to_file` and for `cast`
  ([#91](https://github.com/acbbullock/IO-Fortran-Library/issues/91)).
  Reading a CSV file costs 2–3.5× a list-directed read, which reports
  neither where a bad value is nor reads empty cells as NaN
  ([#92](https://github.com/acbbullock/IO-Fortran-Library/issues/92)).
- **`.npz` writes compute a CRC-32 a byte at a time**, which is most of
  their time
  ([#94](https://github.com/acbbullock/IO-Fortran-Library/issues/94)).
  Reading an array from an archive is as fast as reading a `.npy` file.
- **`read_file` is close to splitting lines by hand with gfortran**, and
  4.6–6.6× slower with ifx and flang
  ([#95](https://github.com/acbbullock/IO-Fortran-Library/issues/95)).
  It also handles quoted cells, `CR LF` line endings and rows of
  different lengths, which the plain version doesn't.
- **`String%push` matches a buffer you grow yourself.**

### Running it

```sh
fpm run benchmark-compare --flag -O2                      # with gfortran
fpm run benchmark-compare --compiler ifx --flag -O2
fpm run benchmark-compare --flag -O2 -- 0.1               # a tenth of the data, for a quick run
```

It prints the tables above in Markdown, with the compiler and its
options, and deletes its files at the end. It needs under 1 GiB of
memory at full size. CI runs it at a quarter of the size with gfortran,
and puts the tables in the job summary. Hosted machines vary too much for
those times to be compared run to run, so they never fail the job, but a
large change would show.

## Big data I/O

The IO-Fortran-Library is capable of reading and writing very large
text files with efficiency, even those whose data size exceeds the
2,147,483,647 byte upper limit of the 32-bit signed integer.
`benchmarks/benchmark-array.f90` (`fpm run benchmark-array`) times the
major internal and external text I/O routines of the IO-Fortran-Library:

```fortran
{!benchmarks/benchmark-array.f90!}
```

Here, we populate an `n`-by-`n` single-precision array `x` with samples
from the uniform distribution on `[-0.5, 0.5)` and convert each to a
hexadecimal string to populate a cell array, write the cell array to a
text file `"bigx.csv"`, read the file back into the program to
re-populate the cell array, then finally cast the cell data into `y`
and compare with `x` to observe an exact match. For `n = 15000`, the
resulting csv file size is `2.47 GB`.

@note With the Intel Fortran compiler `ifx`, we must specify
`-heap-arrays 0` to avoid a segmentation fault when reading a file of
this size, as noted in [compiler-dependent
behavior](../UserInfo/compilers.html).

For a more extreme example, `benchmarks/benchmark-ints.f90`
(`fpm run benchmark-ints`) writes every 32-bit integer, in decimal and
as a hexadecimal string, to a text file `int32.csv`:

```fortran
{!benchmarks/benchmark-ints.f90!}
```

The resulting file size is `94.128 GB`.
