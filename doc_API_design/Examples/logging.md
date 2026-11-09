---
title: Handling log files
author: Austin C Bullock
---

## Log file I/O

The routine [echo](../Ref/echo.html) is the preferred method for
writing general text data to a log file.

The following program demonstrates a simple use of `echo` for writing
messages to a log file:

```fortran
{!example/logging_echo.f90!}
```

Depending on style, one may wish to accumulate log messages into a
`String` and then call the type-bound procedure
[echo](../Ref/String-methods.html#echo) conditionally:

```fortran
{!example/logging_string.f90!}
```

Here, the `error stop` will dump the entire contents of `logmsg` to
stdout. We also take advantage of the
[operators](../Ref/operators.html) `+` and `**` for concatenation and
repetition.
