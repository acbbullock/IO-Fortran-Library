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

Wall time for String: 92.604 s
Number of string conversions/second: 1079860

Wall time for write_file: 20.808 s
Estimated file size: 2.549994 GB

Wall time for read_file: 44.910 s

Wall time for cast: 77.580 s
Number of string casts/second: 1288986
Data is exact match: T
---
Compiler version: Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2023.0.0 Build 20221201
Compiler options: -Ibuild/ifx_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifx_810FD198DC3B0576 -o build/ifx_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o

Wall time for String: 59.280 s
Number of string conversions/second: 1686893

Wall time for write_file: 21.420 s
Estimated file size: 2.550004 GB

Wall time for read_file: 53.557 s

Wall time for cast: 50.867 s
Number of string casts/second: 1965883
Data is exact match:  T
---
Compiler version: Intel(R) Fortran Intel(R) 64 Compiler Classic for applications running on Intel(R) 64, Version 2021.8.0 Build 20221119_000000
Compiler options: -Ibuild/ifort_810FD198DC3B0576 -c -O3 -heap-arrays 0 -module build/ifort_810FD198DC3B0576 -o build/ifort_810FD198DC3B0576/IO-Fortran-Library/test_benchmark.f90.o    

Wall time for String: 57.567 s
Number of string conversions/second: 1737098

Wall time for write_file: 16.180 s
Estimated file size: 2.550004 GB

Wall time for read_file: 45.013 s

Wall time for cast: 52.301 s
Number of string casts/second: 1911984
Data is exact match:  T
---
```

After testing, it is clear that different compilers have different strengths and weaknesses, with some performing internal I/O more efficiently while others perform external I/O more efficiently. On Windows, the timings may be slower by a factor of 2-3 times.

@note With the Intel Fortran compiler `ifx`/`ifort`, we must specify `-heap-arrays 0` to avoid a segmentation fault when reading a file of this size, as noted in [compiler-dependent behavior](../UserInfo/compilers.html).

For a more extreme example, consider the following program to write every 32-bit hexadecimal integer to a text file `int32.txt`:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use io_fortran_lib, only: String, str, LF, operator(+)
    implicit none (type,external)

    type(String), allocatable :: ints
    type(String), allocatable, dimension(:) :: int_array
    integer :: largest, smallest, step, start, i, j

    integer(int64) :: t1, t2, total_length
    real(real64) :: wall_time, rate

    largest = huge(1); smallest = - largest - 1; step = (int(largest,int64) - int(smallest,int64))/128

    write(*,'(a)')  'Writing integers from ' + str(smallest) + ' to ' + str(largest) + ' in chunks of ' + str(step)

    total_length = 0

    call system_clock(t1)

    do j = 1, 128
        start = smallest + (j-1)*step

        if ( j < 128 ) then
            allocate( int_array(start:start+step-1) )

            do concurrent ( i = start:start+step-1 )
                int_array(i) = String(i, fmt='z')
            end do
        else
            allocate( int_array(start:largest) )

            do concurrent ( i = start:largest-1 )
                int_array(i) = String(i, fmt='z')
            end do
            int_array(largest) = String(largest, fmt='z')
        end if

        allocate(ints)
        call ints%glue(int_array, separator=LF)
        call ints%echo(file_name='int32.txt', append=.true.)
        total_length = total_length + ints%len64()
        deallocate(ints,int_array)

        write(*,'(a)')  'File length: ' + str(total_length/1e9, fmt='f', decimals=3) + ' GB in cycle ' + str(j)
    end do

    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,real64)/rate
    write(*,'(a)')  'Total time for write: ' + str(wall_time/60, fmt='f', decimals=3) + ' minutes'
end program main
```

On Linux, this should take around 50 minutes with `ifort`/`ifx` with highest optimizations enabled, and the resulting file size is `47.2 GB`.
