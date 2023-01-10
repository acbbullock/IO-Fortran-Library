---
title: echo
author: Austin C Bullock
---

## [interface echo](../../interface/echo.html)

*Description*: Subroutine for streaming a scalar `character` or `String` type to an external text file.

For writing a scalar `character` string to an external file:

```fortran
call echo(substring, file_name, append, terminator)
```

* `substring` is of type `character(len=*)`
* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical`
* `terminator` is `optional` and of type `character(len=*)`

For writing a scalar `String` type to an external file:

```fortran
call echo(self, file_name, append, terminator)
```

```fortran
call self%echo(file_name, append, terminator)
```

* `self` is of type `String`
* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical`
* `terminator` is `optional` and of type `character(len=*)`

@note The type-bound procedure access of the form `call self%echo()` is valid when `self` is a `String` variable. To echo a `String` expression, the expression must be passed to `echo` by the form `call echo()`.

@note `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### Optional Arguments

Append (default is `.true.`): `append` specifies whether to append or to replace the file `file_name`. Either way, the file will be created if it does not exist.

Terminator (default is `LF`): `terminator` is a string terminator inserted at the end of the input string.
