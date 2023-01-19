---
title: Benchmarking
author: Austin C Bullock
---

## Big data I/O

The IO-Fortran-Library is capable of reading and writing very large text files with efficiency, even those whose data size exceeds the 2,147,483,647 byte upper limit of the 32-bit signed integer. A program is provided in `/test/benchmark.f90` for benchmarking the major internal and external text I/O routines of the IO-Fortran-Library:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, rk=>real64, dp=>real64, compiler_version, compiler_options
    use io_fortran_lib, only: String, str, LF, operator(+)
    implicit none (type,external)

    type(String) :: csv
    type(String), allocatable, dimension(:,:) :: cells

    integer(int64) :: t1, t2
    real(dp) :: wall_time, rate

    integer, parameter :: n = 10000
    real(rk), allocatable, dimension(:,:) :: x, y

    allocate( x(n,n) ); call random_gauss(x,0.0_rk,1.0_rk)
    write(*,'(a)')  'Compiler version: ' + compiler_version()
    write(*,'(a)')  'Compiler options: ' + compiler_options() + LF

    call system_clock(t1)
    cells = String(x, fmt='e')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,rk)/rate

    write(*,'(a)')  'Wall time for String: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Number of string conversions/second: ' + str(nint(size(x)/wall_time)) + LF

    call system_clock(t1)
    call csv%write_file(cells, 'bigx.csv')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,rk)/rate

    write(*,'(a)')  'Wall time for write_file: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Estimated file size: ' + str(csv%len64()/1e9, fmt='f', decimals=6) + ' GB' + LF

    call system_clock(t1)
    call csv%read_file('bigx.csv', cell_array=cells)
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,rk)/rate

    write(*,'(a)')  'Wall time for read_file: ' + str(wall_time, fmt='f', decimals=3) + ' s' + LF

    call csv%empty(); allocate( y(n,n) )

    call system_clock(t1)
    call cells%cast(into=y, fmt='e')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,rk)/rate

    write(*,'(a)')  'Wall time for cast: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Number of string casts/second: ' + str(nint(size(x)/wall_time))
    write(*,'(a,l)')'Data is exact match: ', all(x == y)

    contains
    ! random_gauss
end program main
```

Here, we populate an `n`-by-`n` double-precision array `x` with samples from the standard Gaussian distribution and convert each to a `String` in normalized exponential format to populate a cell array, write the cell array to a text file `'bigx.csv'`, read the file back into the program to re-populate the cell array, then finally cast the cell data into `y` and compare with `x` to observe an exact match. For `n = 10000`, the total data size is `1e8` and the resulting csv file size is `2.45 GB`.

With highest optimizations enabled for each compiler on Linux (`-O3`), we observe the following sample output:

```text
---
Compiler version: GCC version 11.3.0
Compiler options: -I build/gfortran_93B6DA15423670F8 -mtune=generic -march=x86-64 -O3 -J build/gfortran_93B6DA15423670F8 -fpre-include=/usr/include/finclude/math-vector-fortran.h

Wall time for String: 189.034 s
Number of string conversions/second: 529005

Wall time for write_file: 14.219 s
Estimated file size: 2.450003 GB

Wall time for read_file: 64.513 s

Wall time for cast: 74.897 s
Number of string casts/second: 1335170
Data is exact match: T
---
Compiler version: Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2023.0.0 Build 20221201
Compiler options: -Ibuild/ifx_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifx_810FD198DC3B0576 -o build/ifx_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o

Wall time for String: 108.515 s
Number of string conversions/second: 921534

Wall time for write_file: 16.826 s
Estimated file size: 2.450004 GB

Wall time for read_file: 84.971 s

Wall time for cast: 54.377 s
Number of string casts/second: 1839022
Data is exact match:  T
---
Compiler version: Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.8.0 Build 20221119_000000
Compiler options: -Ibuild/ifort_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifort_810FD198DC3B0576 -o build/ifort_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o

Wall time for String: 110.145 s
Number of string conversions/second: 907893

Wall time for write_file: 14.261 s
Estimated file size: 2.450005 GB

Wall time for read_file: 71.295 s

Wall time for cast: 55.172 s
Number of string casts/second: 1812504
Data is exact match:  T
---
```

After testing, it is clear that different compilers have different strengths and weaknesses, with some performing internal I/O more efficiently while others perform external I/O more efficiently. On Windows, the timings may be slower by a factor of 2-3 times.

@note With the Intel Fortran compiler `ifx`/`ifort`, we must specify `-heap-arrays 0` to avoid a segmentation fault when reading a file of this size, as noted in [compiler-dependent behavior](../UserInfo/compilers.html).
