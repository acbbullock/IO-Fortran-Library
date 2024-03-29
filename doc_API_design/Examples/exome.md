---
title: NGS Human Core Exome Panel
author: Austin C Bullock
---

## Handling genomic data

Fortran can serve as a valuable tool for heavy numerical calculations
in Next Generation Sequencing (NGS) analysis, which may often involve
reading and writing many large `.bed` files in succession. The
IO-Fortran-Library is optimized for both performance and memory
consumption when reading and writing large text files, streamlining
performance for Fortran bioinformatics applications.

To demonstrate the speed of file I/O, the following program reads the
Twist Human Core Exome target `.bed` file for hg38 obtained from
[Twist Bioscience](https://www.twistbioscience.com/resources/data-files/ngs-human-core-exome-panel-bed-file)
into a cell array and then writes the cell array to a new file in a
round-trip, comparing the two files for an exact match and providing
the total time elapsed:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, real64, compiler_version
    use io_fortran_lib, only: String, str, TAB, operator(+), operator(==)
    implicit none (type, external)

    type(String) :: hg38, hg38_new
    type(String), allocatable :: cells(:,:)

    integer(int64) :: t1, t2
    real(real64) :: wall_time, rate

    call system_clock(t1)

    call hg38%read_file("./data/hg38.bed", cell_array=cells, column_separator=TAB)
    call hg38_new%write_file(cells, "./data/hg38_new.bed", column_separator=TAB)

    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,real64)/rate

    write(*,"(a,l)") "New file and original are exact match: ", hg38_new == hg38
    write(*,"(a)")   "Wall time: " + str(wall_time, fmt="f", decimals=3) + " s " + &
                     'using compiler: "' + compiler_version() + '".'
end program main
```

The file `hg38.bed` is provided locally in `/data` and contains
`192262` lines of `TAB`-delimited data.

@note With the Intel Fortran compiler `ifx`, we may need to specify
`-heap-arrays 0` to avoid a segmentation fault when reading a file of
this size, as noted in [compiler-dependent
behavior](../UserInfo/compilers.html).
