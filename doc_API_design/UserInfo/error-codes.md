---
title: Error Codes
author: Austin C Bullock
---

When writing to file or reading from file, the `optional` arguments
`stat` and `errmsg` may be present, which will detail any errors that
may occur during execution of the procedure. The module exports the
error codes as named constants:

```Fortran
integer, parameter :: IOFL_READ_ERR  = 1 ! Read error code
integer, parameter :: IOFL_WRITE_ERR = 2 ! Write error code
integer, parameter :: IOFL_ALLOC_ERR = 3 ! Allocation error code
integer, parameter :: IOFL_ARG_ERR   = 4 ! Argument error code
```

so that a program can test for one by name:

```Fortran
use io_fortran_lib, only: from_file, IOFL_READ_ERR

call from_file("data.csv", into=x, stat=stat, errmsg=errmsg)
if ( stat == IOFL_READ_ERR ) write(*,"(a)") trim(errmsg)
```

An `IOFL_READ_ERR` code indicates that an error has occured during a
`read` statement, `inquire` statement, or a `close` statement in the
execution of [from_file](../Ref/from_file.html) or
[read_file](../Ref/String-methods.html#read_file), or that
[cast](../Ref/cast.html) was given text that is not a valid number. An
`IOFL_WRITE_ERR` code indicates that an error has occured during a
`write` statement or a `close` statement in the execution of
[to_file](../Ref/to_file.html), [echo](../Ref/echo.html), or
[write_file](../Ref/String-methods.html#write_file). An
`IOFL_ALLOC_ERR` code indicates that an `allocate` or `deallocate`
statement has failed. An `IOFL_ARG_ERR` code indicates that the user
has provided an incorrect argument to the procedure.

@note Before version 2.1 the error codes were not exported, so programs
compared `stat` with the numbers 1 to 4. The values are unchanged.

In all cases, the `errmsg` will contain more detailed information about
the error that occured. If no error condition occurs, the `stat` will
return `0` and the `errmsg` will be empty.

If an error occurs and `stat` is not present, the program ends with
`error stop`, showing the same message. Pass `stat` to handle an error
in your own program.

An invalid number in a file is an `IOFL_READ_ERR`, and the `errmsg` gives the
text and its row and column.
