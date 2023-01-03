---
title: String methods
author: Austin C Bullock
---

## [type String](../../type/string.html)

### [as_str](../../type/string.html#boundprocedure-as_str)

For `self` a scalar of type `String`:

```fortran
    result = self%as_str()
```

### [echo](../../type/string.html#boundprocedure-echo)

For `self` a scalar of type `String`:

```fortran
    call self%echo(file_name, append)
```

* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical`

@note `echo` is identical in function to [echo](echo.html) for strings of type `character`.

### [empty](../../type/string.html#boundprocedure-empty)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%empty()
```

### [glue](../../type/string.html#boundprocedure-glue)

For `self` a scalar of type `String`:

```fortran
    call self%glue(tokens, separator)
```

* `tokens` is of type `type(String), dimension(:)`
* `separator` is `optional` and of type `character(len=*)`

### [len](../../type/string.html#boundprocedure-len)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%len()
```

### [print](../../type/string.html#boundprocedure-print)

For `self` a scalar of type `String`:

```fortran
    call self%print()
```

### [push](../../type/string.html#boundprocedure-push)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%push(chars)
```

* `chars` is of type `character(len=*)`

### [read_file](../../type/string.html#boundprocedure-read_file)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%read_file(file_name)
```

* `file_name` is of type `character(len=*)`

@note The file extension of `file_name` must correspond to one of the valid [text file extensions](../UserInfo/file-ext.html).

### [replace](../../type/string.html#boundprocedure-replace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%replace(search_for, replace_with)
```

* `search_for` is of type `character(len=*)`
* `replace_with` is of type `character(len=*)`

### [replace_inplace](../../type/string.html#boundprocedure-replace_inplace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%replace_inplace(search_for, replace_with)
```

* `search_for` is of type `character(len=*)`
* `replace_with` is of type `character(len=*)`

### [split](../../type/string.html#boundprocedure-split)

For `self` a scalar of type `String`:

```fortran
    result = self%split(separator)
```

* `separator` is `optional` and of type `character(len=*)`

### [trim](../../type/string.html#boundprocedure-trim)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%trim()
```

### [trim_inplace](../../type/string.html#boundprocedure-trim_inplace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%trim_inplace()
```
