---
title: Error Codes
author: Austin C Bullock
---

When writing to file or reading from file, the `optional` arguments `stat` and `errmsg` may be present, which will detail any errors that may occur during execution of the procedure. The error codes have the following explanation:

```Fortran
integer, parameter :: READ_ERR  = 1 ! Read error code
integer, parameter :: WRITE_ERR = 2 ! Write error code
integer, parameter :: ALLOC_ERR = 3 ! Allocation error code
integer, parameter :: ARG_ERR   = 4 ! Argument error code
```

A `READ_ERR` code indicates that an error has occured during a `read` statement, `inquire` statement, or a `close` statement in the execution of [from_file](../Ref/from_file.html) or [read_file](../Ref/String-methods.html#read_file). A `WRITE_ERR` code indicates that an error has occured during a `write` statement or a `close` statement in the execution of [to_file](../Ref/to_file.html), [echo](../Ref/echo.html), or [write_file](../Ref/String-methods.html#write_file). An `ALLOC_ERR` code indicates that an `allocate` or `deallocate` statement has failed. An `ARG_ERR` code indicates that the user has provided an incorrect argument to the procedure.

In all cases, the `errmsg` will contain more detailed information about the error that occured. If no error condition occurs, the `stat` will return `0` and the `errmsg` will be empty.
