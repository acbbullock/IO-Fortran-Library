---
title: echo
author: Austin C Bullock
---

## [interface echo](../../interface/echo.html)

*Description*: Subroutine for streaming scalar `character` data to an external text file.

For writing a `character` string to an external file:

```fortran
call echo(string, file_name, append, terminator)
```

* `string` is of type `character(len=*)`
* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical`
* `terminator` is `optional` and of type `character(len=*)`

@note `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

For the `String` counterpart of `echo`, see [echo](string-methods.html#echo).

### Optional Arguments

Append (default is `.true.`): `append` specifies whether to append `string` to file `file_name`.

Terminator (default is `LF`): `terminator` is a string terminator inserted at the end of the input string.
