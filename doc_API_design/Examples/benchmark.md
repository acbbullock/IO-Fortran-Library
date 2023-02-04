---
title: Benchmarking
author: Austin C Bullock
---

## Big data I/O

The IO-Fortran-Library is capable of reading and writing very large text files with efficiency, even those whose data size exceeds the 2,147,483,647 byte upper limit of the 32-bit signed integer. A program is provided in `/test/benchmark.f90` for benchmarking the major internal and external text I/O routines of the IO-Fortran-Library:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, rk=>real32, dp=>real64, compiler_version, compiler_options
    use io_fortran_lib, only: String, cast, str, LF, operator(+)
    implicit none (type,external)

    type(String) :: csv
    type(String), allocatable, dimension(:,:) :: cells

    integer(int64) :: t1, t2
    real(dp) :: wall_time, rate

    integer, parameter :: n = 15000
    real(rk), allocatable, dimension(:,:) :: x, y

    allocate( x(n,n), cells(n,n) ); call random_gauss(x,0.0_rk,1.0_rk)
    write(*,'(a)')  'Compiler version: ' + compiler_version()
    write(*,'(a)')  'Compiler options: ' + compiler_options() + LF

    call system_clock(t1)
    call cast(x, into=cells, fmt='z')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,'(a)')  'Wall time for cast: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Number of string conversions/second: ' + str(nint(size(x)/wall_time)) + LF

    call system_clock(t1)
    call csv%write_file(cells, file_name='bigx.csv')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,'(a)')  'Wall time for write_file: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Estimated file size: ' + str(csv%len64()/1e9, fmt='f', decimals=6) + ' GB' + LF

    call system_clock(t1)
    call csv%read_file('bigx.csv', cell_array=cells)
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,'(a)')  'Wall time for read_file: ' + str(wall_time, fmt='f', decimals=3) + ' s' + LF

    call csv%empty(); allocate( y(n,n) )

    call system_clock(t1)
    call cast(cells, into=y, fmt='z')
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,'(a)')  'Wall time for cast: ' + str(wall_time, fmt='f', decimals=3) + ' s'
    write(*,'(a)')  'Number of string casts/second: ' + str(nint(size(x)/wall_time))
    write(*,'(a,l)')'Data is exact match: ', all(x == y)

    contains
    ! random_gauss
end program main
```

Here, we populate an `n`-by-`n` single-precision array `x` with samples from the standard Gaussian distribution and convert each to a hexadecimal string to populate a cell array, write the cell array to a text file `'bigx.csv'`, read the file back into the program to re-populate the cell array, then finally cast the cell data into `y` and compare with `x` to observe an exact match. For `n = 15000`, the total data size is `225e6` and the resulting csv file size is `2.47 GB`.

With highest optimizations enabled for each compiler on Linux (`-O3`), we observe the following sample output:

```text
---
Compiler version: GCC version 11.3.0
Compiler options: -I build/gfortran_93B6DA15423670F8 -mtune=generic -march=x86-64 -O3 -J build/gfortran_93B6DA15423670F8 -fpre-include=/usr/include/finclude/math-vector-fortran.h

Wall time for cast: 7.622 s
Number of string conversions/second: 29518088

Wall time for write_file: 15.694 s
Estimated file size: 2.474999 GB

Wall time for read_file: 20.032 s

Wall time for cast: 8.337 s
Number of string casts/second: 26987495
Data is exact match: T
---
Compiler version: Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2023.0.0 Build 20221201
Compiler options: -Ibuild/ifx_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifx_810FD198DC3B0576 -o build/ifx_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o 

Wall time for cast: 13.990 s
Number of string conversions/second: 16081786

Wall time for write_file: 13.911 s
Estimated file size: 2.474999 GB

Wall time for read_file: 22.623 s

Wall time for cast: 7.963 s
Number of string casts/second: 28252255
Data is exact match:  T
---
Compiler version: Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.8.0 Build 20221119_000000
Compiler options: -Ibuild/ifort_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifort_810FD198DC3B0576 -o build/ifort_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o

Wall time for cast: 13.676 s
Number of string conversions/second: 16452100

Wall time for write_file: 12.479 s
Estimated file size: 2.474999 GB

Wall time for read_file: 24.115 s

Wall time for cast: 9.906 s
Number of string casts/second: 22712251
Data is exact match:  T
---
```

@note With the Intel Fortran compiler `ifx`/`ifort`, we must specify `-heap-arrays 0` to avoid a segmentation fault when reading a file of this size, as noted in [compiler-dependent behavior](../UserInfo/compilers.html).

For a more extreme example, consider the following program to write every 32-bit integer as a hexadecimal string to a text file `int32.txt`:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use io_fortran_lib, only: String, cast, str, LF, operator(+)
    implicit none (type,external)

    type(String) :: int_file
    type(String), allocatable, dimension(:), target :: hex_ints, hex_ints_extra
    type(String), dimension(:,:), pointer :: hex_ints_map => null()

    integer, allocatable, dimension(:), target :: indices, indices_extra
    integer, dimension(:), pointer :: indices_map => null()
    integer :: largest, smallest, step, start, i, j

    integer(int64) :: t1, t2, total_length
    real(real64) :: wall_time, rate

    largest = huge(1); smallest = - largest - 1; step = (int(largest,int64) - int(smallest,int64))/128

    write(*,'(a)')  'Writing integers from ' + str(smallest) + ' to ' + str(largest) + ' in chunks of ' + str(step)

    total_length = 0_int64
    allocate( indices(step), hex_ints(step) )
    allocate( indices_extra(smallest+128*step:largest), hex_ints_extra(smallest+128*step:largest) )

    call system_clock(t1)

    do j = 1, 128
        start = smallest + (j-1)*step

        indices_map(start:start+step-1) => indices

        do concurrent ( i = start:start+step-1 )
            indices_map(i) = i
        end do

        call cast(indices, into=hex_ints, fmt='z')

        hex_ints_map(1:step,1:1) => hex_ints
        call int_file%write_file(hex_ints_map, file_name='int32.txt', append=.true.)
        total_length = total_length + int_file%len64()

        write(*,'(a)')  'File length: ' + str(total_length/1e9, fmt='f', decimals=3) + ' GB in cycle ' + str(j)
    end do

    do concurrent ( i = smallest+128*step:largest-1 )
        indices_extra(i) = i
    end do; indices_extra(largest) = largest

    call cast(indices_extra, into=hex_ints_extra, fmt='z')

    hex_ints_map(smallest+128*step:largest,1:1) => hex_ints_extra
    call int_file%write_file(hex_ints_map, file_name='int32.txt', append=.true.)
    total_length = total_length + int_file%len64()

    write(*,'(a)')  'File length: ' + str(total_length/1e9, fmt='f', decimals=3) + ' GB at end of file.'

    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,real64)/rate
    write(*,'(a)')  'Total time for write: ' + str(wall_time/60, fmt='f', decimals=3) + ' minutes'

    nullify(indices_map, hex_ints_map); deallocate(indices, indices_extra, hex_ints, hex_ints_extra)
end program main
```

On Linux, this should take around five minutes with `gfortran`, and four minutes with `ifx`/`ifort` using highest optimizations, and the resulting file size is `46.96 GB`.
