---
title: echo
author: Austin C Bullock
---

## [interface echo](../../interface/echo.html)

```fortran
call echo(string, file_name, append)
```

* `string` is of type `character(len=*)`
* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical`

@warning `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### Optional Arguments

Append (default is `.true.`): `append` specifies whether to append `string` to file `file_name`.
