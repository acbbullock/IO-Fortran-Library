---
title: String methods
author: Austin C Bullock
---

## [type String](../../type/string.html)

*Description*: A growable string type for advanced character manipulations and text file I/O.

@note Aside from the functionality provided through type-bound procedures, the `String` type may be useful in array contexts for which the user requires arrays of strings which may have non-identical lengths, whose lengths may not be known, whose lengths may need to vary during run time, or in any other context in which the intrinsic `character` type is insufficient.

@note The `String` type is memory safe. The user is forbidden from attempting to access invalid memory due to the `private` attribute of the component and the exceptions put into place in the type-bound procedures.

## Type-bound procedures

### [as_str](../../type/string.html#boundprocedure-as_str)

For `self` a scalar of type `String`:

```fortran
    result = self%as_str()
```

*Description*: Returns a copy of the string slice component of a scalar `String`.

### [cast](../../type/string.html#boundprocedure-cast)

*Description*: A generic binding for the interface [cast_string](cast_string.html).

### [count](../../type/string.html#boundprocedure-count)

For `self` of type `String` and `match` of type `character` or `String` both scalars or arrays of any compatible rank:

```fortran
    result = self%count(match)
```

* `match` is of type `character(len=*)` or `String`

*Description*: Returns number of non-overlapping occurrences of a substring elementally.

### [echo](../../type/string.html#boundprocedure-echo)

For `self` a scalar of type `String`:

```fortran
    call self%echo(file_name, append, terminator)
```

* `file_name` is of type `character(len=*)`
* `append` is `optional` and of type `logical` (default is `.true.`)
* `terminator` is `optional` and of type `character(len=*)` (default is `LF`)

*Description*: Streams the content of a `String` to an external text file. This method is identical in function to the routine [echo](echo.html) for `character` strings.

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
* `separator` is `optional` and of type `character(len=*)` (default is `SPACE`)

*Description*: Glues a `String` vector into `self` with given separator. Default separator is `SPACE`. The string slice component will be replaced if already allocated.

### [len](../../type/string.html#boundprocedure-len)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%len()
```

*Description*: Returns the length of the string slice component elementally. Unallocated components return `-1`.

### [push](../../type/string.html#boundprocedure-push)

For `self` of type `String` and `chars` of type `character` or `String` both scalars or arrays of any compatible rank:

```fortran
    call self%push(chars)
```

* `chars` is of type `character(len=*)` or `String`

*Description*: Appends characters to the string slice component elementally in place. This procedure is identical in function to the [concatenation operators](operators.html#concatenation) with self assignment: `self = self // chars` and `self = self + chars`.

### [read_file](../../type/string.html#boundprocedure-read_file)

For `self` a scalar of type `String`:

```fortran
    call self%read_file(file_name, cell_array, row_separator, column_separator)
```

* `file_name` is of type `character(len=*)`
* `cell_array` is `optional` and of type `type(String), allocatable, dimension(:,:)`
* `row_separator` is `optional` and of type `character(len=*)` (default is `LF`)
* `column_separator` is `optional` and of type `character(len=*)` (default is `','`)

*Description*: Reads an external text file into `self` and optionally populates a cell array using the designated `row_separator` and `column_separator` whose default values are `LF` and `','` respectively.

@note `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

@note The `cell_array` must be `allocatable` and will be re-allocated internally (if already allocated).

### [replace](../../type/string.html#boundprocedure-replace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    result = self%replace(match, substring, back)
```

* `match` is of type `character(len=*)` or `String`
* `substring` is of type `character(len=*)` or `String`
* `back` is `optional` and of type `logical` (default is `.false.`)

*Description*: Matches and replaces all occurrences of a substring elementally. If `back` is `.true.`, then `self` is scanned from back to front, which may result in a different outcome in the case that `match` is overlapping itself.

@note `match` and `substring` may be any combination of `character` or `String` and may be of any rank compatible with each other and with `self`.

### [replace_inplace](../../type/string.html#boundprocedure-replace_inplace)

For `self` a scalar or array of any rank and of type `String`:

```fortran
    call self%replace_inplace(match, substring, back)
```

* `match` is of type `character(len=*)` or `String`
* `substring` is of type `character(len=*)` or `String`
* `back` is `optional` and of type `logical` (default is `.false.`)

*Description*: Matches and replaces all occurrences of a substring elementally in place. If `back` is `.true.`, then `self` is scanned from back to front, which may result in a different outcome in the case that `match` is overlapping itself.

@note `match` and `substring` may be any combination of `character` or `String` and may be of any rank compatible with each other and with `self`.

### [split](../../type/string.html#boundprocedure-split)

For `self` a scalar of type `String`:

```fortran
    result = self%split(separator)
```

* `separator` is `optional` and of type `character(len=*)` (default is `SPACE`)

*Description*: Splits a string into a vector of `tokens` with given separator. Default separator is `SPACE`.

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
* `row_separator` is `optional` and of type `character(len=*)` (default is `LF`)
* `column_separator` is `optional` and of type `character(len=*)` (default is `','`)

*Description*: Writes the content of a cell array to a text file. The cell array's entire contents are populated into `self` and then streamed to an external text file using the designated `row_separator` and `column_separator` whose default values are `LF` and `','` respectively.

@note `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform. Additionally, the file will be created if it does not already exist and will be overwritten if it does exist.

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
