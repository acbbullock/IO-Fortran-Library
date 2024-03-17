---
title: String manipulations
author: Austin C Bullock
---

## String Queries

Sometimes it is useful or necessary to write execution conditions based
on compiler vendors or version, such as when a certain compiler version
has a known bug or when a piece of code is required for a specific
compiler. The following program shows how one may use the
[count](../Ref/String-methods.html#count) procedure to determine a
compiler and version at runtime:

```fortran
program main
  use, intrinsic :: iso_fortran_env, only: compiler_version
  use io_fortran_lib, only: String
  implicit none (type, external)

  type(String) :: compiler
  logical :: GCC, GCC_SUPPORTED, INTEL, INTEL_SUPPORTED

  compiler = String(compiler_version())

  GCC = compiler%count(match="GCC") > 0
  GCC_SUPPORTED = GCC .and. ( compiler%count(match="13.2.1") > 0 )

  INTEL = compiler%count(match="Intel") > 0
  INTEL_SUPPORTED = INTEL .and. ( compiler%count(match="2024.0.2") > 0 )

  write(*,*) compiler

  if ( GCC ) then
    if ( GCC_SUPPORTED ) then
      write(*,*) "This GNU Fortran Compiler version is known to be supported."
    else
      write(*,*) "This GNU Fortran Compiler version may not be supported."
    end if
  else if ( INTEL ) then
    if ( INTEL_SUPPORTED ) then
      write(*,*) "This Intel Fortran Compiler version is known to be supported."
    else
      write(*,*) "This Intel Fortran Compiler version may not be supported."
    end if
  end if
end program main
```

## Dynamic string manipulation

Using [operator](../Ref/operators.html) techniques and
[string methods](../Ref/String-methods.html), we may easily perform
complex string manipulations during run time.

The following program demonstrates the use of the
[String](../../type/string.html) type and some
[type-bound procedures](../Ref/String-methods.html) for manipulating a
timestamp:

```fortran
program main
    use io_fortran_lib, only: String, join, split, LF, operator(+), operator(-)
    implicit none (type, external)

    type(String) :: time_stamp, new_time_stamp
    type(String), allocatable :: tokens(:)

    character(len=10) :: date, time

    call date_and_time(date=date, time=time)

    time_stamp = String("Date : " + date + LF + "Time : " + time)

    write(*,"(a)") "ORIGINAL TIME STAMP:" + LF + time_stamp%as_str() + LF

    tokens = split(time_stamp, separator=LF) - "Date : " - "Time : " + [" : Date", " : Time"]
    new_time_stamp = join(tokens, separator=" | ")

    write(*,"(a)") "RECONSTRUCTED TIME STAMP:" + LF + new_time_stamp%as_str()
end program main
```

This program produces the following sample output:

```text
ORIGINAL TIME STAMP:
Date : 20240316
Time : 163641.569

RECONSTRUCTED TIME STAMP:
20240316   : Date | 163641.569 : Time
```
