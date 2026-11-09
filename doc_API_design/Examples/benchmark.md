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

The ratio of the library's time to plain Fortran's, in version 2.2 and
in 2.3 (lower is better, and 1.0 is the same speed):

| Operation | gfortran 2.2 | 2.3 | ifx 2.2 | 2.3 | flang 2.2 | 2.3 |
|---|--:|--:|--:|--:|--:|--:|
| Write a CSV file | 5.1 | **1.2** | 5.7 | **2.0** | 4.4 | **1.7** |
| Read a CSV file | 2.8 | **1.9** | 3.6 | **1.5** | 2.1 | **1.3** |
| Write a raw binary file | 5.1 | **1.0** | 1.0 | 1.0 | 1.2 | 0.8 |
| Read a raw binary file | 2.9 | **1.0** | 1.0 | 1.0 | 1.0 | 1.0 |
| Write a `.npy` file | 5.6 | **1.0** | 1.0 | 1.2 | 0.9 | 0.8 |
| Read a `.npy` file | 2.7 | **0.6** | 0.7 | 0.7 | 0.8 | 0.8 |
| Write a `.npz` file | 12.2 | **2.0** | 7.8 | **3.0** | 3.8 | **1.6** |
| Read a `.npz` file | 2.6 | **0.6** | 0.7 | 0.7 | 0.8 | 0.8 |
| Numbers to text | 2.4 | **1.2** | 3.4 | **1.6** | 3.4 | **1.7** |
| Text to numbers | 1.2 | 1.2 | 2.6 | **1.7** | 1.2 | 1.2 |
| Split a CSV file into cells | 1.2 | 1.0 | 4.3 | 4.4 | 7.3 | **4.7** |
| Build a string | 0.8 | 0.8 | 0.9 | 1.0 | 0.9 | 0.6 |

The 2.3 times, in milliseconds, as library / plain:

| Operation | gfortran | ifx | flang |
|---|--:|--:|--:|
| Write a CSV file | 370 / 302 | 386 / 197 | 413 / 244 |
| Read a CSV file | 485 / 255 | 413 / 272 | 440 / 342 |
| Write a raw binary file | 107 / 103 | 83 / 84 | 208 / 259 |
| Read a raw binary file | 154 / 152 | 143 / 141 | 288 / 278 |
| Write a `.npy` file | 106 / 103 | 100 / 84 | 213 / 259 |
| Read a `.npy` file | 94 / 152 | 100 / 141 | 224 / 278 |
| Write a `.npz` file | 208 / 103 | 249 / 84 | 403 / 259 |
| Read a `.npz` file | 98 / 152 | 100 / 141 | 223 / 278 |
| Numbers to text | 721 / 612 | 505 / 315 | 478 / 279 |
| Text to numbers | 596 / 497 | 501 / 304 | 531 / 451 |
| Split a CSV file into cells | 65 / 64 | 106 / 24 | 78 / 17 |
| Build a string | 14 / 18 | 17 / 18 | 9 / 16 |

The two versions were timed one after the other on the same machine,
with nothing else running. Times vary by about 10% from run to run, so
ratios within 0.1–0.2 of each other are the same.

What the numbers say:
- **Binary and NumPy files are as fast as plain stream I/O** on all
  three compilers. In 2.2, gfortran took 3–5× as long, because it moves
  an array inside `select rank` one element at a time
  ([#93](https://github.com/acbbullock/IO-Fortran-Library/issues/93)).
  Reading is sometimes faster than the plain version, which reads into
  an array it allocated beforehand.
- **`.npz` writes cost 1.6–3× a plain write**, for the CRC-32 that a ZIP
  archive needs, which is now computed eight bytes at a time
  ([#94](https://github.com/acbbullock/IO-Fortran-Library/issues/94)).
  Reading an array from an archive is as fast as reading a `.npy` file,
  because the CRC isn't checked on read.
- **Writing numbers as text costs 1.2–2× plain formatted output**, for
  `to_file` and for `cast`
  ([#91](https://github.com/acbbullock/IO-Fortran-Library/issues/91)).
  Each number still goes to its own `String`, which plain Fortran doesn't
  need.
- **Reading a CSV file costs 1.3–1.9× a list-directed read**, which
  reports neither where a bad value is nor reads empty cells as NaN
  ([#92](https://github.com/acbbullock/IO-Fortran-Library/issues/92)).
- **`read_file` matches splitting lines by hand with gfortran**, and takes
  about 4.5× as long with ifx and flang. Most of that is allocating a
  `String` for each cell, against the plain version's fixed-length
  `character` array
  ([#95](https://github.com/acbbullock/IO-Fortran-Library/issues/95)).
  `read_file` also handles quoted cells, `CR LF` line endings and rows of
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
