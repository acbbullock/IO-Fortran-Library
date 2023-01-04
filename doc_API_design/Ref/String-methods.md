---
title: String methods
author: Austin C Bullock
---

## [type String](../../type/string.html)

*Description*: A derived type with a single (private) component:

```fortran
character(len=:), allocatable :: s
```

This type is provided for flexible and advanced character handling when the intrinsic `character` type is insufficient. For instance, a `String` may be used in array contexts for which the user requires arrays of strings which may have non-identical lengths, whose lengths may not be known, or whose lengths may need to vary during run time. One may also use the `String` type as an interface to read/write external text files, in particular for cases in which `.csv` data contains data of mixed type. For reading/writing data of uniform type, it is simpler to use the routines [to_file](to_file.html) and [from_file](from_file.html).

## Type-bound procedures

### [as_str](../../type/string.html#boundprocedure-as_str)

For `self` a scalar of type `String`:

```fortran
    result = self%as_str()
```

*Description*: Returns a copy of the string slice component of a scalar `String`.

### [echo](../../type/string.html#boundprocedure-echo)

For `self` a scalar of type `String`:

```fortran
    call self%echo(file_name, append)
```

* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical` (default is `.true.`)

*Description*: Streams the content of a `String` to an external text file. This method is identical in function to the routine [echo](echo.html) for `character` strings.

@note `echo` is identical in function to [echo](echo.html) for strings of type `character`.

### [empty](../../type/string.html#boundprocedure-empty)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%empty()
```

*Description*: Sets the string slice component to the empty string elementally.

### [glue](../../type/string.html#boundprocedure-glue)

For `self` a scalar of type `String`:

```fortran
    call self%glue(tokens, separator)
```

* `tokens` is of type `type(String), dimension(:)`
* `separator` is `optional` and of type `character(len=*)` (default is SPACE)

*Description*: Glues a string vector into `self` with given separator. Default separator is SPACE. The string slice component will be replaced if already allocated.

### [len](../../type/string.html#boundprocedure-len)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%len()
```

*Description*: Returns the length of the string slice component elementally. Unallocated components return -1.

### [push](../../type/string.html#boundprocedure-push)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%push(chars)
```

* `chars` is of type `character(len=*)`

*Description*: Appends characters to the string slice component elementally. This procedure is identical in function to the [concatenation operators](operators.html#concatenation) `self // chars` and `self + chars`.

### [read_file](../../type/string.html#boundprocedure-read_file)

For `self` a scalar of type `String`:

```fortran
    call self%read_file(file_name, cell_array, row_separator, column_separator)
```

* `file_name` is of type `character(len=*)`
* `cell_array` is `optional` and of type `type(String), allocatable, dimension(:,:)`
* `row_separator` is `optional` and of type `character(len=*)` (default is NEW_LINE)
* `column_separator` is `optional` and of type `character(len=*)` (default is `','`)

*Description*: The `read_file` method is provided primarily for the purpose of reading in `.csv` files containing data of **mixed type**, which cannot be handled with a simple call to [from_file](from_file.html) (which assumes data of uniform type). The file's entire contents are populated into `self`, and one may manually parse and manipulate the file's contents using the methods referenced on this page. Optionally, one may provide a rank `2` allocatable array `cell_array` of type `String`, which will be populated with the cells of the given file using the designated `row_separator` and `column_separator` whose default values are NEW_LINE and `','` respectively.

@note The file extension of `file_name` must correspond to one of the valid [text file extensions](../UserInfo/file-ext.html).

@warning `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### [replace](../../type/string.html#boundprocedure-replace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%replace(search_for, replace_with)
```

* `search_for` is of type `character(len=*)`
* `replace_with` is of type `character(len=*)`

*Description*: Returns a copy of a `String` elementally in which each string slice component has had a substring searched and replaced.

### [replace_inplace](../../type/string.html#boundprocedure-replace_inplace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%replace_inplace(search_for, replace_with)
```

* `search_for` is of type `character(len=*)`
* `replace_with` is of type `character(len=*)`

*Description*: Searches and replaces a substring elementally in place.

### [split](../../type/string.html#boundprocedure-split)

For `self` a scalar of type `String`:

```fortran
    result = self%split(separator)
```

* `separator` is `optional` and of type `character(len=*)` (default is SPACE)

*Description*: Splits a string into a vector of `tokens` with given separator. Default separator is SPACE.

### [trim](../../type/string.html#boundprocedure-trim)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%trim()
```

*Description*: Returns a copy of a `String` elementally in which each string slice component has been trimmed of any leading or trailing whitespace.

### [trim_inplace](../../type/string.html#boundprocedure-trim_inplace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%trim_inplace()
```

*Description*: Removes any leading or trailing whitespace of the string slice component of a `String` elementally and in place.

### [write_file](../../type/string.html#boundprocedure-write_file)

For `self` a scalar of type `String`:

```fortran
    call self%write_file(cell_array, file_name, row_separator, column_separator)
```

* `cell_array` is of type `type(String), dimension(:,:)`
* `file_name` is of type `character(len=*)`
* `row_separator` is `optional` and of type `character(len=*)` (default is NEW_LINE)
* `column_separator` is `optional` and of type `character(len=*)` (default is `','`)

*Description*: The `write_file` method is provided primarily for the purpose of writing `.csv` files containing data of **mixed type**, which cannot be handled with a simple call to [to_file](to_file.html) (which accepts numeric arrays of uniform type). The cell array's entire contents are populated into `self` and then streamed to an external text file using the designated `row_separator` and `column_separator` whose default values are NEW_LINE and `','` respectively.

@note The file extension of `file_name` must correspond to one of the valid [text file extensions](../UserInfo/file-ext.html). Additionally, `file_name` will be created if it does not already exist and will be overwritten if it does exist.

@warning `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### [write(formatted)](../../type/string.html#boundprocedure-write%28formatted%29)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    print '(DT)', self
```

```fortran
    write(unit, '(DT)') self
```

*Description*: Formatted write DTIO procedure for type `String`.

@note When performing a formatted write to `unit`, use the derived-type edit descriptor format `fmt='(DT)'`.
