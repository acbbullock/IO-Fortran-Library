---
title: Benchmarking
author: Austin C Bullock
---

## Big data I/O

The IO-Fortran-Library is capable of reading and writing very large
text files with efficiency, even those whose data size exceeds the
2,147,483,647 byte upper limit of the 32-bit signed integer. A program
is provided in `/test/benchmark.f90` for benchmarking the major
internal and external text I/O routines of the IO-Fortran-Library:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int64, rk=>real32, dp=>real64, compiler_version, compiler_options
    use randoms, only: random_gauss
    use io_fortran_lib, only: String, cast, str, LF, operator(+)
    implicit none (type, external)

    type(String)              :: csv
    type(String), allocatable :: cells(:,:)

    integer(int64) :: t1, t2
    real(dp)       :: wall_time, rate

    integer,  parameter   :: n = 15000
    real(rk), allocatable :: x(:,:), y(:,:)

    allocate( x(n,n), cells(n,n) ); call random_gauss(x, 0e0_rk, 1.0_rk)
    write(*,"(a)")  "Compiler version: " + compiler_version()
    write(*,"(a)")  "Compiler options: " + compiler_options() + LF

    call system_clock(t1)
    call cast(x, into=cells, fmt="z")
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,"(a)")  "Wall time for cast: " + str(wall_time, fmt="f", decimals=3) + " s"
    write(*,"(a)")  "Number of string conversions/second: " + str(nint(size(x)/wall_time)) + LF

    call system_clock(t1)
    call csv%write_file(cells, file="bigx.csv")
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,"(a)")  "Wall time for write_file: " + str(wall_time, fmt="f", decimals=3) + " s"
    write(*,"(a)")  "Estimated file size: " + str(csv%len64()/1e9, fmt="f", decimals=6) + " GB" + LF

    call system_clock(t1)
    call csv%read_file("bigx.csv", cell_array=cells)
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,"(a)")  "Wall time for read_file: " + str(wall_time, fmt="f", decimals=3) + " s" + LF

    call csv%empty(); allocate( y(n,n) )

    call system_clock(t1)
    call cast(cells, into=y, fmt="z")
    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

    write(*,"(a)")   "Wall time for cast: " + str(wall_time, fmt="f", decimals=3) + " s"
    write(*,"(a)")   "Number of string casts/second: " + str(nint(size(x)/wall_time))
    write(*,"(a,l)") "Data is exact match: ", all(x == y)
end program main
```

Here, we populate an `n`-by-`n` single-precision array `x` with samples
from the standard Gaussian distribution and convert each to a
hexadecimal string to populate a cell array, write the cell array to a
text file `"bigx.csv"`, read the file back into the program to
re-populate the cell array, then finally cast the cell data into `y`
and compare with `x` to observe an exact match. For `n = 15000`, the
resulting csv file size is `2.47 GB`.

@note With the Intel Fortran compiler `ifx`, we must specify
`-heap-arrays 0` to avoid a segmentation fault when reading a file of
this size, as noted in [compiler-dependent
behavior](../UserInfo/compilers.html).

For a more extreme example, consider the following program to write
every 32-bit integer as a hexadecimal string to a text file `int32.txt`:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: i64=>int64, dp=>real64
    use io_fortran_lib, only: String, str, operator(+)
    implicit none (type, external)

    integer, parameter :: largest = huge(0), smallest = -largest - 1, nsteps = 128
    integer, parameter :: step_size = int( (int(largest,i64) - int(smallest,i64) )/int(nsteps,i64))

    type(String) :: csv
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: cells_p(:,:)

    integer :: lower, upper, i, j

    integer(i64) :: t1, t2, total_length
    real(dp)     :: wall_time, rate

    write(*,"(a)") "Writing integers from " + str(smallest) + " to " + str(largest)

    allocate( cells(step_size, 2) )
    total_length = 0_i64
    call system_clock(t1)

    do j = 1, nsteps
        lower = smallest + (j-1)*step_size
        upper = lower + step_size - 1

        cells_p(lower:upper,1:2) => cells(:,:)

        do concurrent ( i = lower:upper )
            cells_p(i,1) = String(i, fmt="i")
            cells_p(i,2) = String(i, fmt="z")
        end do

        call csv%write_file(cells_p, file="int32.csv", append=.true.)
        total_length = total_length + csv%len64()

        write(*,"(a)")  "File length: " + str(total_length/1e9, fmt="f", decimals=3) + " GB in cycle " + str(j)
    end do

    lower = upper + 1; upper = largest
    nullify(cells_p); deallocate(cells); allocate( cells(lower:upper,2) )

    do concurrent ( i = lower:upper )
        cells(i,1) = String(i, fmt="i")
        cells(i,2) = String(i, fmt="z")
    end do

    call csv%write_file(cells, file="int32.csv", append=.true.)
    total_length = total_length + csv%len64()

    write(*,"(a)")  "File length: " + str(total_length/1e9, fmt="f", decimals=3) + " GB"

    call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate
    write(*,"(a)")  "Total time for write: " + str(wall_time/60, fmt="f", decimals=3) + " minutes"
end program main
```

The resulting file size is `94.128 GB`.
