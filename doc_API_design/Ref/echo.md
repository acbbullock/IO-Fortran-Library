---
title: echo
author: Austin C Bullock
---

## [interface echo](../../interface/echo.html)

*Description*: Subroutine for writing a scalar `character` or `String` to an external text file.

For `substring` a scalar of type `character` or `String`:

```fortran
call echo(substring, file, append, terminator)
```

* `substring` is of type `character(len=*)` or `String`
* `file` is of type `character(len=*)`
* `append` is `optional` and of type `logical`
* `terminator` is `optional` and of type `character(len=*)`

For `substring` a scalar variable of type `String`:

```fortran
call substring%echo(file, append, terminator)
```

@note The type-bound procedure access of the form `call substring%echo()` is valid when `substring` is a `String` variable. To echo a `String`-valued expression, the expression must be passed to `echo` by the form `call echo()`.

@note `file` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### Optional Arguments

Append (default is `.true.`): `append` specifies whether to append or to replace the file `file`. Either way, the file will be created if it does not exist.

Terminator (default is `LF`): `terminator` is a string terminator inserted at the end of the input string.
