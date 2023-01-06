---
title: String manipulation
author: Austin C Bullock
---

The following program demonstrates the use of the [String](../../type/string.html) type and some [type-bound procedures](../Ref/string-methods.html) for manipulating a timestamp, supplemented with the use of [LF](../../module/io_fortran_lib.html#variable-lf) and the [operators](../Ref/operators.html) `+` and `-`:

```fortran
program main
    use io_fortran_lib, only: String, LF, operator(+), operator(-)
    implicit none (type,external)

    character(len=10) :: date, time
    type(String) :: time_stamp, new_time_stamp
    type(String), allocatable, dimension(:) :: tokens

    call date_and_time(date=date, time=time)

    time_stamp = String('Date : ' + date + LF + 'Time : ' + time)

    write(*,'(a)') 'ORIGINAL TIME STAMP:' + LF + time_stamp%as_str() + LF

    tokens = time_stamp%split(separator=LF) - 'Date : ' - 'Time : ' + [' : Date', ' : Time']
    call new_time_stamp%glue(tokens=tokens, separator=' | ')

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
