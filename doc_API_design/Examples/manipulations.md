---
title: String manipulations
author: Austin C Bullock
---

## String Queries

Sometimes it is useful or necessary to write execution conditions based on compiler vendors or version, such as when a certain compiler version has a known bug or when a piece of code is required for a specific compiler. The following program shows how one may use the [count](../Ref/string-methods.html#count) procedure to determine a compiler at runtime:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: compiler_version
    use io_fortran_lib, only: String
    implicit none (type,external)

    type(String) :: compiler
    logical :: GCC, GCC_RECENT, INTEL, INTEL_RECENT
    
    compiler = String(compiler_version())

    GCC = compiler%count(match='GCC') > 0
    GCC_RECENT = GCC .and. ( compiler%count(match='11.3.0') > 0 )
    INTEL = compiler%count(match='Intel') > 0
    INTEL_RECENT = INTEL .and. ( compiler%count(match='2023.0.0') > 0 )
    
    write(*,*) compiler

    if ( GCC ) then
        if ( GCC_RECENT ) then
            write(*,*) 'Hello from a recent GNU Fortran Compiler.'
        else
            write(*,*) 'Hello from an older GNU Fortran Compiler.'
        end if
    else if ( INTEL ) then
        if ( INTEL_RECENT ) then
            write(*,*) 'Hello from a recent Intel Fortran Compiler.'
        else
            write(*,*) 'Hello from an older Intel Fortran Compiler.'
        end if
    end if
end program main
```

## Dynamic string manipulation

Using [operator](../Ref/operators.html) techniques and [string methods](../Ref/string-methods.html), we may easily perform complex string manipulations during run time.

The following program demonstrates the use of the [String](../../type/string.html) type and some [type-bound procedures](../Ref/string-methods.html) for manipulating a timestamp:

```fortran
program main
    use io_fortran_lib, only: String, join, split, LF, operator(+), operator(-)
    implicit none (type,external)

    type(String) :: time_stamp, new_time_stamp
    type(String), allocatable, dimension(:) :: tokens

    character(len=10) :: date, time

    call date_and_time(date=date, time=time)

    time_stamp = String('Date : ' + date + LF + 'Time : ' + time)

    write(*,'(a)') 'ORIGINAL TIME STAMP:' + LF + time_stamp%as_str() + LF

    tokens = split(time_stamp, separator=LF) - 'Date : ' - 'Time : ' + [' : Date', ' : Time']
    new_time_stamp = join(tokens, separator=' | ')

    write(*,'(a)') 'RECONSTRUCTED TIME STAMP:' + LF + new_time_stamp%as_str()
end program main
```

This program produces the following sample output:

```text
ORIGINAL TIME STAMP:
Date : 20230101
Time : 143355.897

RECONSTRUCTED TIME STAMP:
20230101 : Date | 143355.897 : Time
```
